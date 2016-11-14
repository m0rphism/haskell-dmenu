{-# LANGUAGE RecordWildCards, TemplateHaskell #-}

module DMenu.Run where

import Control.Exception
import Control.Monad.State.Strict
import Data.Maybe
import System.Exit
import System.Process
import System.Directory

import DMenu.Options

-- | A state monad transformer in which the command line options of @dmenu@ can
-- be cmdOptsured.
type DMenuT = StateT Options

-- | The @MonadIO@ constraint additionally allows to spawn processes with
-- @System.Process@ in between.
type MonadDMenu m = (MonadIO m, MonadState Options m)

-- | When a spawned process fails, this type is used to represent the exit code
-- and @stderr@ output.
type ProcessError = (Int, String)

-- | Run DMenu to let the user choose a sub-list of @String@s.
--
-- The exit code in the @ProcessError@ is @1@ if the user cancels the selection,
-- e.g. by pressing the escape key.
ask :: MonadDMenu m => [String] → m (Either ProcessError [String])
ask entries = do
  cfg ← get
  liftIO $ do
    (exitCode, sOut, sErr) ← readCreateProcessWithExitCode
      (proc (_binaryPath cfg) (cmdOptsToArgs cfg))
      (unlines entries)
    pure $ case exitCode of
      ExitSuccess → Right $ lines sOut
      ExitFailure i → Left (i, sErr)

-- | Run a @StateT Options m a@ action using the command line options from the
-- config file or an empty set of options as initial state.
--
-- The config file is located at @~/.haskell-dmenu.conf@.
-- For an example see te @haskell-dmenu.conf@ file in the git repository.
--
-- For example
--
-- > import qualified DMenu
-- >
-- > main :: IO ()
-- > main = print =<< DMenu.run (do cmdOpts; DMenu.ask ["A","B","C"])
-- >
-- > cmdOpts :: DMenu.MonadDMenu m => m ()
-- > cmdOpts = do
-- >   DMenu.numLines .= 10
-- >   DMenu.prompt   .= "run"
run :: MonadIO m => DMenuT m a → m a
run ma = evalStateT ma =<< readConfigOrDef =<< getDefConfigPath

getDefConfigPath :: MonadIO m => m FilePath
getDefConfigPath = (++"/.haskell-dmenu.conf") <$> liftIO getHomeDirectory

-- | Convenience function combining @run@ and @ask@.
--
-- The following example is equivalent to the example for @run@:
--
-- > import qualified DMenu
-- >
-- > main :: IO ()
-- > main = print =<< DMenu.runAsk cmdOpts ["A","B","C"]
-- >
-- > cmdOpts :: DMenu.MonadDMenu m => m ()
-- > cmdOpts = do
-- >   DMenu.numLines .= 10
-- >   DMenu.prompt   .= "run"
runAsk :: MonadIO m => DMenuT m () → [String] → m (Either ProcessError [String])
runAsk m0 entries = run $ m0 >> ask entries

-- | Same as @ask@, but allows the user to select from a list of arbitrary
-- elements @xs@, which have a @String@ representation @f@.
select :: MonadDMenu m => (a → String) → [a] → m (Either ProcessError [a])
select f xs = fmap (fmap (fromJust . flip lookup m)) <$> ask (map f xs)
  where m = [ (f x, x) | x ← xs ]

-- | Same as @runAsk@, but allows the user to select from a list of arbitrary
-- elements @xs@, which have a @String@ representation @f@.
--
-- For example
--
-- > import qualified DMenu
-- >
-- > main :: IO ()
-- > main = print =<< DMenu.runSelect cmdOpts show [1..10::Int]
-- >
-- > cmdOpts :: DMenu.MonadDMenu m => m ()
-- > cmdOpts = do
-- >   DMenu.numLines .= 10
-- >   DMenu.prompt   .= "run"
runSelect :: MonadIO m => DMenuT m () → (a → String) → [a] → m (Either ProcessError [a])
runSelect m0 f xs = run $ m0 >> select f xs

-- | Run a repl. For example
--
-- > import qualified DMenu
-- >
-- > main :: IO ()
-- > main = DMenu.repl cmdOpts ["A","B","C"] $ \case
-- >   Left _pe → pure Nothing
-- >   Right ss → do
-- >     print ss
-- >     pure $ Just $ map (head ss ++ ) ["1","2","3"]
repl :: MonadIO m => DMenuT m a → [String] → (Either ProcessError [String] → m (Maybe [String])) → m ()
repl m0 ss0 f = run $ m0 >> go (Right ss0) where
  go ess = do
    mss ← lift $ f ess
    forM_ mss $ \ss → do
      ess' ← ask ss
      go ess'


splitFirstWord :: String → (String, String)
splitFirstWord = go "" where
  go s []                           = (s, [])
  go s (c:cs) | c `elem` [' ','\t'] = (s, dropWhile (`elem` [' ','\t']) cs)
              | otherwise           = go (s++[c]) cs

readFileMay :: MonadIO m => FilePath → m (Maybe String)
readFileMay path = liftIO $
  (Just <$> readFile path) `catch` (\(_ :: SomeException) → pure Nothing)

readConfigOrDef :: MonadIO m => FilePath → m Options
readConfigOrDef = fmap f . readFileMay where
  f = \case
    Nothing → defOptions
    Just content → parseOptions content
