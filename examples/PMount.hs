import Control.Monad
import Control.Monad.IO.Class
import Control.Lens
import System.Directory
import System.Environment
import System.Exit
import System.Process
import Control.Exception
import Data.Maybe
import Data.List (isPrefixOf)

import qualified DMenu

cmdOpts :: DMenu.MonadDMenu m => m ()
cmdOpts = do
  DMenu.numLines        .= 10
  DMenu.font            .= "FiraMono:size=11"
  DMenu.caseInsensitive .= True

getDevs :: MonadIO m => m [String]
getDevs = liftIO $ listDirectoryDef "/dev"

filterWithPrefixes :: [String] → [String] → [String]
filterWithPrefixes ps = filter $ \s → any (`isPrefixOf` s) ps

devPathPrefixes :: [String]
devPathPrefixes = [ "sd" ]

devPathPrefixesU :: [String]
devPathPrefixesU = map ("/dev/" ++) [ "sd" ]

listDirectoryDef :: MonadIO m => FilePath → m [FilePath]
listDirectoryDef p = liftIO $ listDirectory p `catch` (\(_ :: SomeException) → pure [])

getMountedDevs :: MonadIO m => m [FilePath]
getMountedDevs = liftIO $ do
  (exitCode, sOut, _sErr) ← readCreateProcessWithExitCode
    (proc "pmount" [])
    ""
  pure $ case exitCode of
    ExitSuccess   → map ((!! 0) . words) $ drop 1 $ init $ lines sOut
    ExitFailure _ → []

main :: IO ()
main = getArgs >>= \case
  [] → do
    devs ← filterWithPrefixes devPathPrefixes <$> getDevs
    DMenu.runAsk (do cmdOpts; DMenu.prompt .= "mount") devs >>= \case
      Right (dev:_) | not (null dev) → callProcess "pmount" [dev]
      _             → pure ()
  ["-u"] → do
    devs ← filterWithPrefixes devPathPrefixesU <$> getMountedDevs
    DMenu.runAsk (do cmdOpts; DMenu.prompt .= "umount") devs >>= \case
      Right (dev:_) | not (null dev) → callProcess "pumount" [dev]
      _             → pure ()
  _ → do
    putStrLn usage
    exitFailure

usage :: String
usage = unlines
  [ "Usage: dmenu-pmount [-u]"
  , ""
  , "Options:"
  , "  -u: unmount devices with pumount."
  ]
