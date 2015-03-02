{-# LANGUAGE CPP                 #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE ViewPatterns        #-}

module Borel.Marquise
     ( marquise )

where

import           Control.Lens
import           Control.Monad
import           Data.Monoid
import           Network.URI
import           Pipes
import           Pipes.Safe        as P
import           System.Log.Logger
import qualified System.ZMQ4       as Z

-- friends
import qualified Marquise.Client   as M
import           Vaultaire.Types

-- family
import           Borel.Types


-- | Use Marquise to fetch raw data points.
--
marquise :: (MonadIO m, MonadSafe m)
         => BorelEnv
         -> (GroupedMetric, Origin, Address)
         -> Producer SimplePoint m ()
marquise params (metrics, origin, addr) = do
  liftIO $ debugM "borel" ("fetching from marquise with origin="
                           <> show origin
                           <> " addr="
                           <> show addr)

  case metrics of
      [metric] -> if
        | metric == volumes  -> events
        | metric == ipv4     -> events
        | metric == snapshot -> events
        | otherwise          -> points
      _                      -> points
  where events = rangeData context uri origin addr (TimeStamp 0)          end
        points = rangeData context uri origin addr (params ^. paramStart) end
        context = params ^. paramBorelConfig . paramZMQContext
        uri     = params ^. paramBorelConfig . paramMarquiseURI
        end     = params ^. paramEnd

rangeData
  :: (MonadIO m, MonadSafe m)
  => Z.Context -> URI
  -> Origin -> Address -> TimeStamp -> TimeStamp
  -> Producer SimplePoint m ()
rangeData ctx uri origin addr start end
  = runMarquiseReader ctx uri
  $ \conn -> hoist liftIO
           $ void $ M.catchMarquiseAll
             (M.readSimplePoints M.NoRetry addr start end origin conn)
             (lift . lift . errorM "borel" . show)


--------------------------------------------------------------------------------

-- Marquise runners wrap marquise functionalities in a composable, safe pipe.

type MarquiseReader = M.SocketState

runMarquiseReader
  :: (MonadSafe m)
  => Z.Context -> URI
  -> (MarquiseReader -> Proxy a a' b b' m x)
  -> Proxy a a' b b' m  x
runMarquiseReader ctx (show -> uri) f
  = P.bracket (liftIO $ Z.socket ctx Z.Dealer) (liftIO . Z.close) $ \sock ->
    P.bracket (liftIO $ Z.connect sock uri)    (const $ return ())$ \_    ->
      f (M.SocketState sock uri)
