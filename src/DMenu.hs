{-
  TODO
    - Add a remark, that stack exec may cause System.Process to fail finding
      programs, and this can be circumvented by specifying an absolute path.
    - Support for a .dmenurc file.
    - apps:
      - select config file to edit, ask for sudo pw for root configs in a secure way.
-}

{-# LANGUAGE RecordWildCards, TemplateHaskell #-}

module DMenu (
    -- * Running DMenu
    DMenuT, MonadDMenu, ProcessError, ask, run, runAsk, select, runSelect, repl,
    -- * Command Line Options
    CmdOpts(..),
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

import Control.Exception
import Control.Monad.State.Strict
import Control.Lens
import Data.Maybe
import System.Exit
import System.Process
import System.Directory
import System.IO
import Numeric (showHex)

-- | A state monad transformer in which the command line options of @dmenu@ can
-- be cmdOptsured.
type DMenuT = StateT CmdOpts

-- | The @MonadIO@ constraint additionally allows to spawn processes with
-- @System.Process@ in between.
type MonadDMenu m = (MonadIO m, MonadState CmdOpts m)

-- | When a spawned process fails, this type is used to represent the exit code
-- and @stderr@ output.
type ProcessError = (Int, String)

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

fillLeft :: Char → Int → String → String
fillLeft c i s = replicate (i - length s) c ++ s

showColorAsHex :: Color → String
showColorAsHex (HexColor i) = "#" ++ fillLeft '0' 6 (showHex i "")
showColorAsHex (RGBColor r g b) = showColorAsHex $ HexColor $ r*256*256 + g*256 + b
showColorAsHex (RGBColorF r g b) = showColorAsHex $ RGBColor (f r) (f g) (f b)
  where f = floor . (* 255)

-- | Contains the binary path and command line options of dmenu.
-- The option descriptions are copied from the dmenu @man@ page.
data CmdOpts = CmdOpts
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
    -- | @-s screen@; dmenu apears on the specified screen number. Number given corespondes to screen number in X cmdOptsuration.
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

makeLenses ''CmdOpts

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
    unless (null sErr) $ do
      putStrLn $ "Error: " ++ sErr
      putStrLn $ "Binary: " ++ show (_binaryPath cfg)
      putStrLn $ "Args: " ++ show (cmdOptsToArgs cfg)
      putStrLn $ "Entries: " ++ show entries
    pure $ case exitCode of
      ExitSuccess → Right $ lines sOut
      ExitFailure i → Left (i, sErr)

-- | Run a @StateT CmdOpts m a@ action using the command line options from the
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

defCmdOpts :: CmdOpts
defCmdOpts = CmdOpts
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

cmdOptsToArgs :: CmdOpts → [String]
cmdOptsToArgs (CmdOpts{..}) = concat $ concat
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
  , [ [ "-p", _prompt                          ] | _prompt /= "" ]
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


readFileMay :: MonadIO m => FilePath → m (Maybe String)
readFileMay path = liftIO $
  (Just <$> readFile path) `catch` (\(_ :: SomeException) → pure Nothing)

readConfigOrDef :: MonadIO m => FilePath → m CmdOpts
readConfigOrDef = fmap f . readFileMay where
  f = \case
    Nothing → defCmdOpts
    Just content → parseCmdOpts content

parseCmdOpts :: String → CmdOpts
parseCmdOpts = foldl f defCmdOpts . map splitFirstWord . lines where
  f :: CmdOpts → (String, String) → CmdOpts
  f opts (cmd, args) = opts & case cmd of
    "binaryPath"              → binaryPath              .~ args
    "displayAtBottom"         → displayAtBottom         .~ True
    "displayNoItemsIfEmpty"   → displayNoItemsIfEmpty   .~ True
    "grabKeyboardBeforeStdin" → grabKeyboardBeforeStdin .~ True
    "filterMode"              → filterMode              .~ True
    "caseInsensitive"         → caseInsensitive         .~ True
    "fuzzyMatching"           → fuzzyMatching           .~ True
    "tokenMatching"           → tokenMatching           .~ True
    "maskInputWithStar"       → maskInputWithStar       .~ True
    "ignoreStdin"             → ignoreStdin             .~ True
    "screenIx"                → screenIx                .~ read args
    "windowName"              → windowName              .~ args
    "windowClass"             → windowClass             .~ args
    "windowOpacity"           → windowOpacity           .~ read args
    "windowDimOpacity"        → windowDimOpacity        .~ read args
    "windowDimColor"          → windowDimColor          .~ read args
    "numLines"                → numLines                .~ read args
    "heightInPixels"          → heightInPixels          .~ read args
    "underlineHeightInPixels" → underlineHeightInPixels .~ read args
    "prompt"                  → prompt                  .~ args
    "font"                    → font                    .~ args
    "windowOffsetX"           → windowOffsetX           .~ read args
    "windowOffsetY"           → windowOffsetY           .~ read args
    "width"                   → width                   .~ read args
    "normalBGColor"           → normalBGColor           .~ read args
    "normalFGColor"           → normalFGColor           .~ read args
    "selectedBGColor"         → selectedBGColor         .~ read args
    "selectedFGColor"         → selectedFGColor         .~ read args
    "underlineColor"          → underlineColor          .~ read args
    "historyFile"             → historyFile             .~ args
    "printVersionAndExit"     → printVersionAndExit     .~ True
    ""                        → id
    _                         → error $ "Invalid command found when parsing dmenu config file: " ++ cmd

printCmdOpts :: CmdOpts → String
printCmdOpts CmdOpts{..} = unlines $ concat
  [ [ "binaryPath " ++ _binaryPath                                | _binaryPath /= "" ]
  , [ "displayAtBottom"                                           | _displayAtBottom ]
  , [ "displayNoItemsIfEmpty"                                     | _displayNoItemsIfEmpty ]
  , [ "grabKeyboardBeforeStdin"                                   | _grabKeyboardBeforeStdin ]
  , [ "filterMode"                                                | _filterMode ]
  , [ "caseInsensitive"                                           | _caseInsensitive ]
  , [ "fuzzyMatching"                                             | _fuzzyMatching ]
  , [ "tokenMatching"                                             | _tokenMatching ]
  , [ "maskInputWithStar"                                         | _maskInputWithStar ]
  , [ "ignoreStdin"                                               | _ignoreStdin ]
  , [ "screenIx " ++ show _screenIx                               | _screenIx /= (-1) ]
  , [ "windowName " ++ _windowName                                | _windowName /= "" ]
  , [ "windowClass " ++ _windowClass                              | _windowClass /= "" ]
  , [ "windowOpacity " ++ show _windowOpacity                     | _windowOpacity /= (-1) ]
  , [ "windowDimOpacity " ++ show _windowDimOpacity               | _windowDimOpacity /= (-1) ]
  , [ "windowDimColor " ++ show _windowDimColor                   | _windowDimColor /= HexColor (-1) ]
  , [ "numLines " ++ show _numLines                               | _numLines /= (-1) ]
  , [ "heightInPixels " ++ show _heightInPixels                   | _heightInPixels /= (-1) ]
  , [ "underlineHeightInPixels " ++ show _underlineHeightInPixels | _underlineHeightInPixels /= (-1) ]
  , [ "prompt " ++ show _prompt                                   | _prompt /= "" ]
  , [ "font " ++ show _font                                       | _font /= "" ]
  , [ "windowOffsetX " ++ show _windowOffsetX                     | _windowOffsetX /= (-1) ]
  , [ "windowOffsetY " ++ show _windowOffsetY                     | _windowOffsetY /= (-1) ]
  , [ "width " ++ show _width                                     | _width /= (-1) ]
  , [ "normalBGColor "   ++ show _normalBGColor                   | _normalBGColor /= HexColor (-1) ]
  , [ "normalFGColor "   ++ show _normalFGColor                   | _normalFGColor /= HexColor (-1) ]
  , [ "selectedBGColor " ++ show _selectedBGColor                 | _selectedBGColor /= HexColor (-1) ]
  , [ "selectedFGColor " ++ show _selectedFGColor                 | _selectedFGColor /= HexColor (-1) ]
  , [ "underlineColor "  ++ show _underlineColor                  | _underlineColor /= HexColor (-1) ]
  , [ "historyFile " ++ show _historyFile                         | _historyFile /= "" ]
  , [ "printVersionAndExit"                                       | _printVersionAndExit ]
  ]
