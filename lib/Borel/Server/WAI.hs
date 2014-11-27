-- A clone of some of pipes-wai's functionality to work with Scotty.
-- once https://github.com/scotty-web/scotty/issues/80 is resolved, we
-- can get rid of this and use pipes-wai.
module Borel.Server.WAI
       ( Flush(..)
       , responseProducer )
where

import           Blaze.ByteString.Builder (Builder)
import           Pipes

data Flush a = Chunk a | Flush

responseProducer :: (MonadIO m)
                 => Producer (Flush Builder) m ()  -- pipe to stream from
                 -> (Builder -> IO ())             -- sender function
                 -> IO ()                          -- flusher function
                 -> m ()
responseProducer src send flush =
  runEffect $ for src $ \mbuilder -> case mbuilder of
    Chunk a -> liftIO $ send a
    Flush   -> liftIO flush
