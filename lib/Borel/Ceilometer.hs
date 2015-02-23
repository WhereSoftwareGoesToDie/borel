{-# LANGUAGE TupleSections #-}

module Borel.Ceilometer
     ( ceilometer )
where

import           Control.Applicative
import           Control.Monad.Reader
import qualified Data.Bimap           as BM
import qualified Data.List            as L
import qualified Data.Map             as M
import           Data.Maybe
import           Data.Monoid
import           Pipes                hiding (Proxy)
import           System.Log.Logger

-- friends
import           Ceilometer.Client
import           Vaultaire.Query

import           Borel.Types


--------------------------------------------------------------------------------

-- | Use Ceilometer to decode and aggregate a stream of raw data points.
--
ceilometer
    :: (MonadIO m, Applicative m)
    => FlavorMap                 -- ^ Instance flavor mapping
    -> GroupedMetric             -- ^ Requested metrics, this determines how we present the fold result
    -> Env                       -- ^ Ceilometer arguments
    -> Producer SimplePoint m () -- ^ Raw points
    -> m [Result]
ceilometer flavors metrics cenv@(Env _ sd _ _) raw = do
  liftIO $ debugM "borel" ("using ceilometer to decode data with " <> show sd)
  fromMaybe [] <$> fmap handleResult <$> decodeFold cenv raw

  where handleResult :: FoldResult -> [Result]
        handleResult x = case (x, metrics) of
          (RSingle   val,  [m]) -> [(m,val)]
          (RMapNum32 vals, [m]) -> [(m, M.foldlWithKey (\a k v -> a + (fromIntegral k * v)) 0 vals)]
          (RMapNum64 vals, [m]) -> [(m, M.foldlWithKey (\a k v -> a + (fromIntegral k * v)) 0 vals)]
          (RMapText  vals,  _)  -> let ms = intersect metrics flavors
                                  in  map (\(metric, flavor) -> (metric,) $ fromMaybe 0 $ M.lookup flavor vals) ms
          _                    -> []

        --  Intersect the flavor map and the list of metrics requested. Flatten the result.
        intersect :: [Metric] -> FlavorMap -> [(Metric, Flavor)]
        intersect ms = BM.fold (\k1 _ acc -> maybe acc ((:acc) . (,k1))
                                          $  L.find (== mkInstance k1) ms) []
