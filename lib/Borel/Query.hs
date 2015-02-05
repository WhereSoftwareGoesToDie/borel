{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TransformListComp   #-}
{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}

-- | Evaluates Borel requests and construct a Borel queries for them.
--
module Borel.Query
  (
  ) where

import           Control.Monad.Logger
import           Control.Monad.Reader
import           Data.List (nub)
import           Data.Monoid
import           Data.Word
import           Data.Set (Set)
import qualified Data.Set as S
import           Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Bimap as BM
import           Pipes hiding (Proxy)
import qualified Pipes.Prelude as P
import           Pipes.Safe
import Data.Typeable
import Control.Lens
import Control.Applicative
import Data.Bifunctor
import Data.Maybe
import Data.Text (Text)
import qualified Data.List as L

-- friends
import           Vaultaire.Types
import           Vaultaire.Query
import           Ceilometer.Types
import           Ceilometer.Client

-- family
import           Borel.Types


type Result = (Metric, Word64)

-- should be something else
type TenancyID = Word64

query :: Set TenancyID -> BorelM m [Result]
query tenancies = do
   params <- ask
   let points     = _
   let ceilometer = Env (params ^. paramFlavorMap)
                        _
                        (params ^. paramStart)
                        (params ^. paramEnd)
   go (params ^. paramMetrics) (params ^. paramFlavorMap) ceilometer _

go :: (Monad m, Applicative m)
    => [Metric] -> FlavorMap
    -> Env
    -> Producer SimplePoint m ()
    -> m [Result]
go metrics fm ceilometer points = case metrics of
  (m:[]) -> if
    | m == cpu     -> poke (single m) $ decodeAndFold (undefined :: proxy PDCPU)            ceilometer points
    | m == volumes -> poke (single m) $ decodeAndFold (undefined :: proxy PDVolume)         ceilometer points
    | m == vcpus   -> poke (sum    m) $ decodeAndFold (undefined :: proxy PDInstanceVCPU)   ceilometer points
    | otherwise    -> return []
  ms@_   -> if
    -- the flavors queried are known in our flavor map config.
    | fs <- intersect ms fm, not $ null fs
                   -> poke (group fs) $ decodeAndFold (undefined :: proxy PDInstanceFlavor) ceilometer points
    -- we have no idea what they're talking about!
    | otherwise    -> return []
  where single     :: Metric -> Word64 -> [Result]
        single m v =  [(m, v)]

        sum        :: Metric -> Map PFValue32 Word64 -> [Result]
        sum m vs   = [(m, M.foldlWithKey (\a k v -> a + (fromIntegral k * v)) 0 vs)]

        group      :: [(Metric, Flavor)] -> Map PFValueText Word64 -> [Result]
        group ms vs = map (\(metric, flavor) -> (metric,) $ fromMaybe 0 $ M.lookup flavor vs) ms

        poke :: (Monad m, Monoid y) => (x -> y) -> m (Maybe x) -> m y
        poke f = \mx -> mx >>= \case Nothing -> return mempty
                                     Just x  -> return $ f x

        --  Intersect the flavor map and the list of metrics requested. Flatten the result.
        intersect :: [Metric] -> FlavorMap -> [(Metric, Flavor)]
        intersect ms = BM.fold (\k1 _ acc -> maybe acc ((:acc) . (,k1))
                                          $  L.find (== mkInstance k1) ms) []
