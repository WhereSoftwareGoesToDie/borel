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
import qualified Pipes.Prelude           as P
import qualified Pipes.Safe              as P
import           System.Environment
import qualified System.ZMQ4             as Z

import           Vaultaire.Types

import           Borel

main :: IO ()
main = do
  tenancy:_ <- getArgs
  Right (conf, _) <- loadBorelConfig "borel-openstack.conf"
  context <- Z.context
  P.runSafeT
    $ runEffect
    $ run conf (conf ^. allMetrics) (TenancyID $ T.pack tenancy) start end
    >-> P.print
  Z.term context
  where start = read "2014-12-01" :: TimeStamp
        end   = read "2014-12-07" :: TimeStamp
        flavors = BM.empty
