import Control.Monad
import Control.Monad.IO.Class
import Control.Lens
import System.Directory
import System.Environment
import System.Exit
import System.Process
import Control.Exception
import Data.Maybe
import Data.List (isPrefixOf, sort, intersperse)
import Text.Read (readMaybe)
import GHC.Exts (sortWith)

-- FIXME: new `ps aux` version does not show all processes (e.g. sxiv)
-- TODO: mem and cpu sort

import qualified DMenu

runProc :: MonadIO m => String → [String] → String → m (Either String String)
runProc prog args sIn = liftIO $ do
  (exitCode, sOut, sErr) ←
    readCreateProcessWithExitCode (proc prog args) sIn
  pure $ case exitCode of
    ExitSuccess   → Right sOut
    ExitFailure _ → Left sErr

runProcOr :: MonadIO m => String → [String] → String → String → m String
runProcOr prog args sIn sDef = either (const sDef) id <$> runProc prog args sIn

skipNumbersAndWS :: Int → String → String
skipNumbersAndWS = go False where
  go _ 0 s                           = dropWhile (`elem` [' ', '\t']) $ s
  go _ _ ""                          = ""
  go b n (c:s) | c `elem` ['0'..'9'] = go True n s
               | otherwise           = go False (if b then n-1 else n) s

-- returns list of pairs of process ids and description
getProcs :: MonadIO m => m [(Integer, String)]
getProcs = do
  mUser ← liftIO $ lookupEnv "USER"
  case mUser of
    Nothing → pure []
    Just user → do
      sOut ← runProcOr "pgrep" ["-u", user, "--list-full"] "" ""
      fmap concat $ forM (lines sOut) $ \l → do
        let mPid :: Maybe Integer = readMaybe $ head $ words $ skipNumbersAndWS 0 l
        case mPid of
          Nothing → pure []
          Just pid → do
            let mInfo = skipNumbersAndWS 1 l
            pure [(pid, mInfo)]

data ProcInfo = ProcInfo
  { piUser        :: String
  , piPid         :: Integer
  , piCpuUsage    :: Double
  , piMemoryUsage :: Double
  , piVSZ         :: Integer
  , piRSS         :: Integer
  , piTTY         :: Maybe String
  , piStat        :: String
  , piStart       :: String
  , piTime        :: String
  , piCommand     :: String
  }

readProcInfo :: String → Maybe ProcInfo
readProcInfo s = case words s of
  [user, pid, cpu, mem, vsz, rss, tty, stat, start, time, cmd]
    | Just pid' ← readMaybe pid
    , Just cpu' ← readMaybe cpu
    , Just mem' ← readMaybe mem
    , Just vsz' ← readMaybe vsz
    , Just rss' ← readMaybe rss
    → let tty' | tty == "?" = Nothing
               | otherwise = Just tty
      in Just $ ProcInfo user pid' cpu' mem' vsz' rss' tty' stat start time cmd
  _ → Nothing

showProcInfos :: [ProcInfo] → [String]
showProcInfos pis = map f pairs
 where
  users = fillWithSP $ map piUser pis
  pids  = fillWithSPR $ map (show . piPid) pis
  cpus  = fillWithSPR $ map (show . piCpuUsage) pis
  mems  = fillWithSPR $ map (show . piMemoryUsage) pis
  cmds  = fillWithSPR $ map piCommand pis
  pairs = zip users $ zip pids $ zip cpus $ zip mems cmds
  f (user,(pid,(cpu,(mem,cmd)))) = concat $ intersperse "  " [ pid, user, cpu ++ "% CPU", mem ++ "% MEM", cmd ]

-- returns list of pairs of process ids and description
getProcs' :: MonadIO m => m [ProcInfo]
getProcs' = do
  sOut ← runProcOr "ps" ["aux"] "" ""
  fmap concat $ forM (drop 1 $ lines sOut) $ \l → do
    case readProcInfo l of
      Nothing → pure []
      Just pi' → pure [pi']

fillWithSP :: [String] → [String]
fillWithSP ss = map f ss where
  f s = s ++ replicate (maxLength - length s) ' '
  maxLength = maximum (map length ss)

fillWithSPR :: [String] → [String]
fillWithSPR ss = map f ss where
  f s = replicate (maxLength - length s) ' ' ++ s
  maxLength = maximum (map length ss)

main :: IO ()
main = do
  args ← getArgs
  sortProcs ← case args of
    ["-cpu"] → pure $ reverse . sortWith piCpuUsage
    ["-mem"] → pure $ reverse . sortWith piMemoryUsage
    ["-pid"] → pure id
    []       → pure id
    _        → putStrLn usage >> exitFailure
  procs ← sortProcs <$> getProcs'
  let procs' = zip (map piPid procs) (showProcInfos procs)
  DMenu.runSelect (DMenu.prompt .= "kill -9") snd procs' >>= \case
    Right ((pid,_):_) → callCommand $ "kill -9 " ++ show pid
    _                 → pure ()

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
