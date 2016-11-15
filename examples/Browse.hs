import Control.Lens
import System.Process

import qualified DMenu

main :: IO ()
main = do
  let templates = [ ("https://www.google.com/search?q=", "google")
                  , ("https://www.haskell.org/hoogle/?hoogle=", "hoogle")
                  ]
  let browserCmd = "chromium -new-window"

  DMenu.select' (DMenu.prompt .= "search with") snd templates >>= \case
    Right [(template, _)] →
      DMenu.select (DMenu.prompt .= "search for") [] >>= \case
        Right [query] → callCommand $ browserCmd ++ " " ++ template ++ query
        _→ pure ()
    _ → pure ()

-- usage :: String
-- usage = unlines
--   [ "Usage: dmenu-edit"
--   ]
