{-# LANGUAGE RecordWildCards, TemplateHaskell #-}

module DMenu (
    -- * Running DMenu
    DMenuT, MonadDMenu, ProcessError, ask, run, runAsk, repl,
    -- * Configuration
    Config(..),
    -- ** Lenses
    binaryPath,
    displayAtBottom,
    displayNoItemsIfEmpty,
    grabKeyboardBeforeStdin,
    filterMode,
    caseInsensitive,
    fuzzyMatching,
    tokenMatching,
    maskInputWithStar,
    ignoreStdin,
    screenIx,
    windowName,
    windowClass,
    windowOpacity,
    windowDimOpacity,
    windowDimColor,
    numLines,
    heightInPixels,
    underlineHeightInPixels,
    prompt,
    font,
    windowOffsetX,
    windowOffsetY,
    width,
    normalBGColor,
    normalFGColor,
    selectedBGColor,
    selectedFGColor,
    underlineColor,
    historyFile,
    printVersionAndExit,
    -- * Color
    Color(..),
    -- * Reexports from @lens@
    (.=),
  ) where

import Control.Monad.State.Strict
import Control.Lens
import System.Exit
import System.Process
import Numeric (showHex)

-- | A state monad transformer in which the command line options of @dmenu@ can
-- be configured.
type DMenuT = StateT Config

-- | The @MonadIO@ constraint additionally allows to spawn processes with
-- @System.Process@ in between.
type MonadDMenu m = (MonadIO m, MonadState Config m)

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
      (proc (_binaryPath cfg) (configToArgs cfg))
      (unlines entries)
    unless (null sErr) $ do
      putStrLn $ "Error: " ++ sErr
      putStrLn $ "Binary: " ++ show (_binaryPath cfg)
      putStrLn $ "Args: " ++ show (configToArgs cfg)
      putStrLn $ "Entries: " ++ show entries
    pure $ case exitCode of
      ExitSuccess → Right $ lines sOut
      ExitFailure i → Left (i, sErr)

-- readCreateCommandWithExitCode :: String → String → IO (ExistCode, String, String)
-- readCreateCommandWithExitCode cmd sIn = do
--   procHandle ← spawnCommand cmd
--   exitCode ← waitForProcess procHandle

-- | Run a @StateT Config m a@ action using an empty set of command line options as initial state. For example
--
-- > import qualified DMenu
-- >
-- > main :: IO ()
-- > main = print =<< DMenu.run (do config; DMenu.ask ["A","B","C"])
-- >
-- > config :: DMenu.MonadDMenu m => m ()
-- > config = do
-- >   DMenu.numLines .= 10
-- >   DMenu.prompt   .= "run"
run :: MonadIO m => DMenuT m a → m a
run = flip evalStateT defConfig

-- | Convenience function combining @run@ and @ask@.
--
-- The following example is equivalent to the example for @run@:
--
-- > import qualified DMenu
-- >
-- > main :: IO ()
-- > main = print =<< DMenu.runAsk config ["A","B","C"]
-- >
-- > config :: DMenu.MonadDMenu m => m ()
-- > config = do
-- >   DMenu.numLines .= 10
-- >   DMenu.prompt   .= "run"
runAsk :: MonadIO m => DMenuT m a → [String] → m (Either ProcessError [String])
runAsk ma entries = run $ ma >> ask entries

-- | Run a repl. For example
--
-- > import qualified DMenu
-- >
-- > main :: IO ()
-- > main = DMenu.repl config ["A","B","C"] $ \case
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

-- | Contains the binary path and command line options of dmenu.
-- The option descriptions are copied from the dmenu @man@ page.
data Config = Config
  { -- | Path to the the dmenu executable file.
    _binaryPath :: FilePath
    -- | @-b@; dmenu appears at the bottom of the screen.
  , _displayAtBottom :: Bool
    -- | @-q@; dmenu will not show any items if the search string is empty.
  , _displayNoItemsIfEmpty :: Bool
    -- | @-f@; dmenu grabs the keyboard before reading stdin.  This is faster, but will lock up X until stdin reaches end-of-file.
  , _grabKeyboardBeforeStdin :: Bool
    -- | @-r@; activates filter mode. All matching items currently shown in the list will be selected, starting with the item that is highlighted and wrapping around to the beginning of the list.
  , _filterMode :: Bool
    -- | @-i@; dmenu matches menu items case insensitively.
  , _caseInsensitive :: Bool
    -- | @-z@; dmenu uses fuzzy matching. It matches items that have all characters entered, in sequence they are entered, but there may be any number of characters between matched characters.  For example it takes "txt" makes it to "*t*x*t" glob pattern and checks if it matches.
  , _fuzzyMatching :: Bool
    -- | @-t@; dmenu uses space-separated tokens to match menu items. Using this overrides -z option.
  , _tokenMatching :: Bool
    -- | @-mask@; dmenu masks input with asterisk characters (*).
  , _maskInputWithStar :: Bool
    -- | @-noinput@; dmenu ignores input from stdin (equivalent to: echo | dmenu).
  , _ignoreStdin :: Bool
    -- | @-s screen@; dmenu apears on the specified screen number. Number given corespondes to screen number in X configuration.
  , _screenIx :: Int
    -- | @-name name@; defines window name for dmenu. Defaults to "dmenu".
  , _windowName :: String
    -- | @-class class@; defines window class for dmenu. Defaults to "Dmenu".
  , _windowClass :: String
    -- | @-o opacity@; defines window opacity for dmenu. Defaults to 1.0.
  , _windowOpacity :: Double
    -- | @-dim opacity@; enables screen dimming when dmenu appers. Takes dim opacity as argument.
  , _windowDimOpacity :: Double
    -- | @-dc color@; defines color of screen dimming. Active only when -dim in effect. Defautls to black (#000000)
  , _windowDimColor :: Color
    -- | @-l lines@; dmenu lists items vertically, with the given number of lines.
  , _numLines :: Int
    -- | @-h height@; defines the height of the bar in pixels.
  , _heightInPixels :: Int
    -- | @-uh height@; defines the height of the underline in pixels.
  , _underlineHeightInPixels :: Int
    -- | @-p prompt@; defines the prompt to be displayed to the left of the input field.
  , _prompt :: String
    -- | @-fn font@; defines the font or font set used. eg. "fixed" or "Monospace-12:normal" (an xft font)
  , _font :: String
    -- | @-x xoffset@; defines the offset from the left border of the screen.
  , _windowOffsetX :: Int
    -- | @-y yoffset@; defines the offset from the top border of the screen.
  , _windowOffsetY :: Int
    -- | @-w width@; defines the desired menu window width.
  , _width :: Int
    -- | @-nb color@; defines the normal background color.  #RGB, #RRGGBB, and X color names are supported.
  , _normalBGColor :: Color
    -- | @-nf color@; defines the normal foreground color.
  , _normalFGColor :: Color
    -- | @-sb color@; defines the selected background color.
  , _selectedBGColor :: Color
    -- | @-sf color@; defines the selected foreground color.
  , _selectedFGColor :: Color
    -- | @-uc color@; defines the underline color.
  , _underlineColor :: Color
    -- | @-hist <histfile>@; the file to use for history
  , _historyFile :: FilePath
    -- | @-v@; prints version information to stdout, then exits.
  , _printVersionAndExit :: Bool
  }

defConfig :: Config
defConfig = Config
  { _binaryPath = "dmenu"
  , _displayAtBottom = False
  , _displayNoItemsIfEmpty = False
  , _grabKeyboardBeforeStdin = False
  , _filterMode = False
  , _caseInsensitive = False
  , _fuzzyMatching = False
  , _tokenMatching = False
  , _maskInputWithStar = False
  , _ignoreStdin = False
  , _screenIx = (-1)
  , _windowName = ""
  , _windowClass = ""
  , _windowOpacity = (-1)
  , _windowDimOpacity = (-1)
  , _windowDimColor = HexColor (-1)
  , _numLines = (-1)
  , _heightInPixels = (-1)
  , _underlineHeightInPixels = (-1)
  , _prompt = ""
  , _font = ""
  , _windowOffsetX = 0
  , _windowOffsetY = 0
  , _width = (-1)
  , _normalBGColor = HexColor (-1)
  , _normalFGColor = HexColor (-1)
  , _selectedBGColor = HexColor (-1)
  , _selectedFGColor = HexColor (-1)
  , _underlineColor = HexColor (-1)
  , _historyFile = ""
  , _printVersionAndExit = False
  }

configToArgs :: Config → [String]
configToArgs (Config{..}) = concat $ concat
  [ [ [ "-b"                                   ] | _displayAtBottom ]
  , [ [ "-q"                                   ] | _displayNoItemsIfEmpty ]
  , [ [ "-f"                                   ] | _grabKeyboardBeforeStdin ]
  , [ [ "-r"                                   ] | _filterMode ]
  , [ [ "-i"                                   ] | _caseInsensitive ]
  , [ [ "-z"                                   ] | _fuzzyMatching ]
  , [ [ "-t"                                   ] | _tokenMatching ]
  , [ [ "-mask"                                ] | _maskInputWithStar ]
  , [ [ "-noinput"                             ] | _ignoreStdin ]
  , [ [ "-s", show _screenIx                   ] | _screenIx /= (-1) ]
  , [ [ "-name", show _windowName              ] | _windowName /= "" ]
  , [ [ "-class", show _windowClass            ] | _windowClass /= "" ]
  , [ [ "-o", show _windowOpacity              ] | _windowOpacity /= (-1) ]
  , [ [ "-dim"                                 ] | _windowDimOpacity /= (-1) ]
  , [ [ "-dc", showColorAsHex _windowDimColor  ] | _windowDimColor /= HexColor (-1) ]
  , [ [ "-l", show _numLines                   ] | _numLines /= (-1) ]
  , [ [ "-h", show _heightInPixels             ] | _heightInPixels /= (-1) ]
  , [ [ "-uh", show _underlineHeightInPixels   ] | _underlineHeightInPixels /= (-1) ]
  , [ [ "-p", show _prompt                     ] | _prompt /= "" ]
  , [ [ "-fn", _font                           ] | _font /= "" ]
  , [ [ "-x", show _windowOffsetX              ] | _windowOffsetX /= (-1) ]
  , [ [ "-y", show _windowOffsetY              ] | _windowOffsetY /= (-1) ]
  , [ [ "-w", show _width                      ] | _width /= (-1) ]
  , [ [ "-nb", showColorAsHex _normalBGColor   ] | _normalBGColor /= HexColor (-1) ]
  , [ [ "-nf", showColorAsHex _normalFGColor   ] | _normalFGColor /= HexColor (-1) ]
  , [ [ "-sb", showColorAsHex _selectedBGColor ] | _selectedBGColor /= HexColor (-1) ]
  , [ [ "-sf", showColorAsHex _selectedFGColor ] | _selectedFGColor /= HexColor (-1) ]
  , [ [ "-uc", showColorAsHex _underlineColor  ] | _underlineColor /= HexColor (-1) ]
  , [ [ "-hist", show _historyFile             ] | _historyFile /= "" ]
  , [ [ "-v"                                   ] | _printVersionAndExit ]
  ]

-- | Multiple representations for colors.
--
-- For example, green can be defined as
--
-- > green1 = HexColor 0x00FF00
-- > green2 = RGBColor 0 255 0
-- > green3 = RGBColorF 0 1 0
data Color
  = HexColor Int
  | RGBColor Int Int Int
  | RGBColorF Float Float Float
  deriving (Eq, Ord, Read, Show)

showColorAsHex :: Color → String
showColorAsHex (HexColor i) = showHex i ""
showColorAsHex (RGBColor r g b) = showColorAsHex $ HexColor $ r*256*256 + g*256 + b
showColorAsHex (RGBColorF r g b) = showColorAsHex $ RGBColor (f r) (f g) (f b)
  where f = floor . (* 255)


makeLenses ''Config
