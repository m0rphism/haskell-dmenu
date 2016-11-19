{-# LANGUAGE UnicodeSyntax, LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables, ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
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
-- be configured.
type DMenuT = StateT Options

-- | The @MonadIO@ constraint additionally allows to spawn processes with
-- @System.Process@ in between.
type MonadDMenu m = (MonadIO m, MonadState Options m)

-- | When a spawned process fails, this type is used to represent the exit code
-- and @stderr@ output.
type ProcessError = (Int, String)

-- | Run a @StateT Options m a@ action using the command line options from the
-- config file or an empty set of options as initial state.
--
-- The config file is located at @~/.haskell-dmenu.conf@.
-- For an example see the @haskell-dmenu.conf@ file in the git repository.
--
-- For example
--
-- > import qualified DMenu
-- >
-- > main :: IO ()
-- > main = DMenu.run $ do
-- >   DMenu.numLines .= 10
-- >   DMenu.prompt   .= "run"
-- >   liftIO . print =<< DMenu.selectM ["A","B","C"]
run :: MonadIO m => DMenuT m a → m a
run ma = evalStateT ma =<< readConfigOrDef =<< getDefConfigPath

getDefConfigPath :: MonadIO m => m FilePath
getDefConfigPath = (++"/.haskell-dmenu.conf") <$> liftIO getHomeDirectory

-- | Run DMenu with the command line options from @m@ and a list of @String@s
-- from which the user should choose.
selectM
  :: MonadDMenu m
  => [String]
     -- ^ List from which the user should select.
   → m (Either ProcessError [String])
     -- ^ The selection made by the user, or a @ProcessError@, if the user
     -- canceled.
selectM entries = do
  cfg ← get
  liftIO $ do
    (exitCode, sOut, sErr) ← readCreateProcessWithExitCode
      (proc (_binaryPath cfg) (optionsToArgs cfg))
      (unlines entries)
    pure $ case exitCode of
      ExitSuccess → Right $ lines sOut
      ExitFailure i → Left (i, sErr)

-- | Convenience function combining @run@ and @selectM@.
--
-- The following example has the same behavior as the example for @run@:
--
-- > import qualified DMenu
-- >
-- > main :: IO ()
-- > main = print =<< DMenu.select setOptions ["A","B","C"]
-- >
-- > setOptions :: DMenu.MonadDMenu m => m ()
-- > setOptions = do
-- >   DMenu.numLines .= 10
-- >   DMenu.prompt   .= "run"
select
  :: MonadIO m
  => DMenuT m ()
     -- ^ @State Options@ action which changes the default command line
     -- options.
   → [String]
     -- ^ List from which the user should select.
   → m (Either ProcessError [String])
     -- ^ The selection made by the user, or a @ProcessError@, if the user
     -- canceled.
select m0 entries = run $ m0 >> selectM entries

-- | Same as @selectM@, but allows the user to select from a list of arbitrary
-- elements, which have a @String@ representation.
selectWithM
  :: MonadDMenu m
  => (a → String)
     -- ^ How to display an @a@ in @dmenu@.
   → [a]
     -- ^ List from which the user should select.
   → m (Either ProcessError [a])
     -- ^ The selection made by the user, or a @ProcessError@, if the user
     -- canceled.
selectWithM f xs = fmap (fmap (fromJust . flip lookup m)) <$> selectM (map f xs)
  where m = [ (f x, x) | x ← xs ]

-- | Same as @select@, but allows the user to select from a list of arbitrary
-- elements, which have a @String@ representation.
--
-- For example
--
-- > import qualified DMenu
-- >
-- > main :: IO ()
-- > main = print =<< DMenu.selectWith setOptions show [1..10::Int]
-- >
-- > setOptions :: DMenu.MonadDMenu m => m ()
-- > setOptions = do
-- >   DMenu.numLines .= 10
-- >   DMenu.prompt   .= "run"
selectWith
  :: MonadIO m
  => DMenuT m ()
     -- ^ @State Options@ action which changes the default command line
     -- options.
   → (a → String)
     -- ^ How to display an @a@ in @dmenu@.
   → [a]
     -- ^ List from which the user should select.
   → m (Either ProcessError [a])
     -- ^ The selection made by the user, or a @ProcessError@, if the user
     -- canceled.
selectWith m0 f xs = run $ m0 >> selectWithM f xs


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
