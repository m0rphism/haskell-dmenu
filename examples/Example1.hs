import qualified DMenu
import Control.Lens
import Control.Monad.IO.Class
import Control.Monad.State

config :: MonadState DMenu.Config m => m ()
config = do
  DMenu.binaryPath      .= "/nix/store/2v3wxf5f1xfz90igayz14kzigffq03y0-user-environment/bin/dmenu"
  DMenu.numLines        .= 10
  DMenu.font            .= "FiraMono:size=11"
  DMenu.prompt          .= "run"
  DMenu.caseInsensitive .= True

main :: IO ()
main = print =<< DMenu.runAsk config ["A","B","C"]
