import qualified DMenu
import Control.Lens

config :: DMenu.MonadDMenu m => m ()
config = do
  DMenu.binaryPath      .= "/nix/store/2v3wxf5f1xfz90igayz14kzigffq03y0-user-environment/bin/dmenu"
  DMenu.numLines        .= 10
  DMenu.font            .= "FiraMono:size=11"
  DMenu.prompt          .= "run"
  DMenu.caseInsensitive .= True

main :: IO ()
main = print =<< DMenu.runAsk config ["A","B","C"]

repl :: IO ()
repl = DMenu.repl config ["A","B","C"] $ \case
  Left _pe → pure Nothing
  Right ss → do
    print ss
    pure $ Just $ map (head ss ++ ) ["1","2","3"]
