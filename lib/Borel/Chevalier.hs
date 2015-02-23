{-# LANGUAGE CPP                 #-}
{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE ViewPatterns        #-}

module Borel.Chevalier
     ( chevalier )
where

import           Control.Lens         ((^.))
import           Control.Monad.Reader
import           Data.Either
import           Data.Monoid
import           Data.Set             (Set)
import qualified Data.Set             as S
import           Data.Text            (Text)
import qualified Data.Text            as T
import qualified Data.Text.Encoding   as E
import           Network.URI
import           Pipes                as P
import           Pipes.Safe           as P
import           System.Log.Logger
import qualified System.ZMQ4          as Z

import           Ceilometer.Tags
import qualified Chevalier.Types      as C
import qualified Chevalier.Util       as C
import           Vaultaire.Types

import           Borel.Types


-- | Use Chevalier to find origin, address, sourcedict that contains data relevant
--   to this OpenStack tenancy.
--
chevalier :: (MonadIO m, MonadSafe m)
          => (GroupedMetric, TenancyID)
          -> Producer (Origin, Address, SourceDict) (BorelS m) ()
chevalier (metrics, tid) = do
  liftIO $ debugM "borel" ("searching chevalier with tenancy="
                          <> show tid
                          <> " requested="
                          <> show (map deserialise metrics))
  params <- lift ask
  let req = C.buildRequestFromPairs $ chevalierTags
            (params ^. paramBorelConfig . allInstances)
            (metrics, tid)
  P.enumerate
    [ (org, addr, sd)
    | org        <- Select $ P.each (params ^. paramBorelConfig . paramOrigins)
    , (addr, sd) <- Select $ addressesWith
                             (params ^. paramBorelConfig . paramZMQContext)
                             (params ^. paramBorelConfig . paramChevalierURI)
                             org req
    ]

chevalierTags :: Set Metric -> (GroupedMetric, TenancyID) -> [(Text, Text)]
chevalierTags instances (ms, TenancyID tid) = case ms of
  [metric] -> if
    | metric == cpu        -> [tagID tid , tagName "cpu"                                  ]
    | metric == volumes    -> [tagID tid , tagName "volume.size"            , tagEvent    ]
    | metric == diskReads  -> [tagID tid , tagName "disk.read.bytes"                      ]
    | metric == diskWrites -> [tagID tid , tagName "disk.write.bytes"                     ]
    | metric == neutronIn  -> [tagID tid , tagName "network.incoming.bytes"               ]
    | metric == neutronOut -> [tagID tid , tagName "network.outgoing.bytes"               ]
    | metric == ipv4       -> [tagID tid , tagName "ip.floating"            , tagEvent    ]
    | metric == vcpus      -> [tagID tid , tagName "instance_vcpus"         , tagPollster ]
    | metric == memory     -> [tagID tid , tagName "instance_ram"           , tagPollster ]
    | metric == snapshot   -> [tagID tid , tagName "snapshot.size"          , tagEvent    ]
    | metric == image      -> [tagID tid , tagName "image.size"             , tagPollster ]
    | otherwise            -> []
  _ -> if
    | all (`S.member` instances) ms
                           -> [tagID tid , tagName "instance_flavor"        , tagPollster ]
    | otherwise            -> []
  where tagID       = (keyTenancyID,)
        tagName     = (keyMetricName,)
        tagEvent    = (keyEvent, valTrue)
        tagPollster = (keyEvent, valFalse)

--------------------------------------------------------------------------------

addressesWith
  :: (MonadSafe m)
  => Z.Context -> URI
  -> Origin -> C.SourceRequest
  -> Producer (Address, SourceDict) m ()
addressesWith ctx uri origin request
  = runChevalier ctx uri
  $ \conn -> hoist liftIO $ do
      resp <- liftIO (sendrecv conn)
      -- this doesn't actually stream because chevalier doesn't
      each $ either (error . show) (rights . map C.convertSource) (C.decodeResponse resp)
  where sendrecv sock = do
            Z.send sock [Z.SendMore] $ encodeOrigin origin
            Z.send sock []           $ C.encodeRequest request
            Z.receive sock
        encodeOrigin (Origin x) = E.encodeUtf8 $ T.pack $ show x

type Chevalier = Z.Socket Z.Req

runChevalier
  :: MonadSafe m
  => Z.Context -> URI
  -> (Chevalier -> Proxy a a' b b' m x)
  -> Proxy a a' b b' m x
runChevalier ctx (show -> uri) act
  = P.bracket (liftIO $ Z.socket ctx Z.Req) (liftIO . Z.close) $ \sock ->
    P.bracket (liftIO $ Z.connect sock uri) (const $ return ())$ \_    ->
      act sock
