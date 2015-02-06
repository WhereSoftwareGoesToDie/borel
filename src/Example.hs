{-# LANGUAGE OverloadedStrings #-}
import           Control.Lens
import qualified Data.Bimap              as BM
import qualified Data.ByteString.Char8   as B
import           Data.Either.Combinators
import           Data.Maybe
import qualified Data.Set                as S
import qualified Data.Text               as T
import           Network.URI
import           Pipes
import qualified Pipes.ByteString        as P
import qualified Pipes.Prelude           as P
import qualified Pipes.Safe              as P
import           System.Environment

import           Vaultaire.Types

import           Borel

main :: IO ()
main = do
  marquise:chevalier:origin:tenancy:_ <- getArgs
  let conf = mkConfig (S.singleton $ fromRight' $ makeOrigin $ B.pack origin)
                      (fromJust $ parseURI marquise)
                      (fromJust $ parseURI chevalier)
                      flavors
  P.runSafeT
    $ runEffect
    $ toJSONStream (run conf (_metrics conf) (T.pack tenancy) start end)
    >-> P.stdout
  where start = read "2014-12-01" :: TimeStamp
        end   = read "2014-12-07" :: TimeStamp
        flavors = BM.empty
