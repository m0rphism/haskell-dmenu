import DMenu
import Control.Lens

main :: IO ()
main = do
  let entries = ["A","B","C"]
  eSelEntries ← flip runDMenuM entries $ do
    dmenuBinaryPath .= "/nix/store/2v3wxf5f1xfz90igayz14kzigffq03y0-user-environment/bin/dmenu"
    numLines        .= 10
    font            .= "FiraMono:size=11"
    prompt          .= "run"
    caseInsensitive .= True
  print eSelEntries
