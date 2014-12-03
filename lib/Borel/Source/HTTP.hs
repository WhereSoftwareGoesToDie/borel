{-# LANGUAGE ExistentialQuantification  #-}

-- | Borel communicates with numerous data sources, some of which is via HTTP.
--   This module contains generic utilities to stream from such backends.
--
module Borel.Source.HTTP
     ( RespBody
     , safeHTTP, from
     ) where

import           Control.Monad
import           Data.ByteString.Char8      (ByteString)
import qualified Data.ByteString.Char8      as B
import           Network.HTTP.Client
import           Pipes
import           Pipes.Safe

type RespBody = B.ByteString

safeHTTP :: MonadSafe m
         => Request
         -> Manager
         -> Producer RespBody m ()
safeHTTP req man = bracket
  (liftIO $ responseOpen req man)
  (liftIO . responseClose)
  (hoist liftIO . from . brRead . responseBody)

-- Copypasted from pipes-http
from :: IO ByteString -> Producer ByteString IO ()
from io = go
  where go = do bs <- lift io
                unless (B.null bs) $ do
                  yield bs
                  go
