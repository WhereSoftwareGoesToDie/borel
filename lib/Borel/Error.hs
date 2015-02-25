{-# LANGUAGE OverloadedStrings #-}
-- | Copyright 2014-2015 Anchor Systems, Pty Ltd and Others
--
-- The code in this file, and the program it is a part of, is
-- made available to you by its authors as open source software:
-- you can redistribute it and/or modify it under the terms of
-- the 3-clause BSD licence.
--
-- /Description/
-- This module defines error types in Borel.
--
-- Because Borel provides a safe pipeline to process data from
-- Marquise, we cannot insert abitrary exit points with Except/Error.
-- (e.g. `Producer x (ExceptT BorelError m) r` is not possible because
-- `m` is required to be a `MonadSafe` so that resources can be
-- cleaned up after a failure in the stream.)
--
-- Instead Borel errors need to be handled inside the pipeline. We can
-- choose to stop streaming or log the error.
--
module Borel.Error
  ( BorelError(..)
  , lookupSD
  , noteBorelError
  , stopBorelError
  ) where

import           Control.Error.Util
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Monoid
import           Data.Text              (Text)
import           Pipes

import           Vaultaire.Types

type Message = Text

data BorelError
  = SourceDictLookup SourceDict Message
  | ConfigLoad                  Message
  deriving (Eq, Show)

lookupSD :: Text -> SourceDict -> Either BorelError Text
lookupSD key sd
  = note (SourceDictLookup sd $ "lookup failed: with key=" <> key)
         (lookupSource key sd)

noteBorelError :: MonadIO m => Pipe (Either BorelError a) a m r
noteBorelError = forever $ await >>= either (const (return ()) . show) yield

stopBorelError :: Either BorelError x -> x
stopBorelError (Left  e) = error $ "implode" <> show e
stopBorelError (Right x) = x
