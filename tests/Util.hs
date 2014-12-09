-- | Strictly testing-only utilities
--
module Util
  ( toListP
  ) where

import           Pipes
import           Pipes.Internal

-- | Like pipes's toListM, but keeps the return value
--
toListP :: Monad m => Producer a m x -> m (x, [a])
toListP = loop
  where
    loop p = case p of
        Request v _  -> closed v
        Respond a fu -> do
            (x, as) <- loop (fu ())
            return (x, a:as)
        M         m  -> m >>= loop
        Pure    x    -> return (x, [])
{-# INLINABLE toListP #-}
