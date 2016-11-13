import qualified DMenu
import Control.Lens

cmdOpts :: DMenu.MonadDMenu m => m ()
cmdOpts = do
  DMenu.numLines        .= 10
  DMenu.font            .= "FiraMono:size=11"
  DMenu.prompt          .= "run"
  DMenu.caseInsensitive .= True

main :: IO ()
main = print =<< DMenu.runAsk cmdOpts ["A","B","C"]

repl :: IO ()
repl = DMenu.repl cmdOpts ["A","B","C"] $ \case
  Left _pe → pure Nothing
  Right ss → do
    print ss
    pure $ Just $ map (head ss ++ ) ["1","2","3"]

runSelect:: IO ()
runSelect = print =<< DMenu.runSelect cmdOpts show [1..10::Int]
