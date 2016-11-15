-- FIXME: not working yet...

import Control.Monad
import Control.Monad.IO.Class
import Control.Lens
import System.Environment
import System.Exit
import System.Process
import Data.List (intersperse)
import Text.Read (readMaybe)
import GHC.Exts (sortWith)

import qualified DMenu

main :: IO ()
main = do
  Right [pw] ← flip DMenu.select [] $ do
    DMenu.prompt .= "Root Password"
    assign DMenu.normalFGColor =<< use DMenu.normalBGColor
    assign DMenu.selectedFGColor =<< use DMenu.selectedBGColor
  Right [cmd] ← DMenu.select (DMenu.prompt .= "Root Command") []
  callCommand $ "echo " ++ pw ++ " | sudo --std-in " ++ cmd

usage :: String
usage = unlines
  [ "Usage: dmenu-pkill [OPTIONS]"
  , ""
  , "Get current processes with `ps aux`, optionally sort them by CPU or RAM"
  , "usage, and ask via dmenu to kill one of the processes via `kill -9 <pid>`."
  , ""
  , "Options:"
  , "  -cpu: sort process list by CPU usage."
  , "  -mem: sort process list by memory usage."
  , "  -pid: sort process list by pid. (default)"
  ]
