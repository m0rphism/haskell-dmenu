{-# LANGUAGE UnicodeSyntax, LambdaCase #-}
{-# LANGUAGE RecordWildCards, TemplateHaskell #-}

module DMenu.Options where

import Control.Lens
import Text.Read (readMaybe)

import DMenu.Color
import DMenu.Lens

-- | Contains the binary path and command line options of dmenu.
-- The option descriptions are copied from the @dmenu@ @man@ page.
data Options = Options
  { _binaryPath              :: FilePath
  , _displayAtBottom         :: Bool
  , _grabKeyboardBeforeStdin :: Bool
  , _caseInsensitive         :: Bool
  , _spawnOnMonitor          :: Int
  , _numLines                :: Int
  , _prompt                  :: String
  , _font                    :: String
  , _normalBGColor           :: Color
  , _normalFGColor           :: Color
  , _selectedBGColor         :: Color
  , _selectedFGColor         :: Color
  , _printVersionAndExit     :: Bool
  , _dmenu2                  :: Options2
  , _noDMenu2                :: Bool
  , _extraArgs               :: [String]
  }

-- | Contains the command line options of @dmenu2@ which are not part of
-- @dmenu@. The option descriptions are copied from the @dmenu2@ @man@ page.
data Options2 = Options2
  { _displayNoItemsIfEmpty   :: Bool
  , _filterMode              :: Bool
  , _fuzzyMatching           :: Bool
  , _tokenMatching           :: Bool
  , _maskInputWithStar       :: Bool
  , _ignoreStdin             :: Bool
  , _spawnOnScreen           :: Int
  , _windowName              :: String
  , _windowClass             :: String
  , _windowOpacity           :: Double
  , _windowDimOpacity        :: Double
  , _windowDimColor          :: Color
  , _heightInPixels          :: Int
  , _underlineHeightInPixels :: Int
  , _windowOffsetX           :: Int
  , _windowOffsetY           :: Int
  , _width                   :: Int
  , _underlineColor          :: Color
  , _historyFile             :: FilePath
  }

-- We create temporary lenses with suffix `L`, and then write wrappers for them
-- to attach documentation.
makeLensesL ''Options
makeLensesL ''Options2

-- | Path to the the dmenu executable file.
-- Default looks for @dmenu@ in the @PATH@ enviroment variable.
binaryPath :: Lens' Options FilePath
binaryPath = _binaryPathL
-- | @-b@; dmenu appears at the bottom of the screen.
displayAtBottom :: Lens' Options Bool
displayAtBottom = _displayAtBottomL
-- | @-f@; dmenu grabs the keyboard before reading stdin. This is faster, but
-- will lock up X until stdin reaches end-of-file.
grabKeyboardBeforeStdin :: Lens' Options Bool
grabKeyboardBeforeStdin = _grabKeyboardBeforeStdinL
-- | @-i@; dmenu matches menu items case insensitively.
caseInsensitive :: Lens' Options Bool
caseInsensitive = _caseInsensitiveL
-- | @-m screen@; dmenu is displayed on the monitor number supplied. Monitor
-- numbers are starting from 0.
spawnOnMonitor :: Lens' Options Int
spawnOnMonitor = _spawnOnMonitorL
-- | @-l lines@; dmenu lists items vertically, with the given number of lines.
numLines :: Lens' Options Int
numLines = _numLinesL
-- | @-p prompt@; defines the prompt to be displayed to the left of the input
-- field.
prompt :: Lens' Options String
prompt = _promptL
-- | @-fn font@; defines the font or font set used. eg. @\"fixed\"@ or
-- @\"Monospace-12:normal\"@ (an xft font)
font :: Lens' Options String
font = _fontL
-- | @-nb color@; defines the normal background color. @#RGB@, @#RRGGBB@, and X
-- color names are supported.
normalBGColor :: Lens' Options Color
normalBGColor = _normalBGColorL
-- | @-nf color@; defines the normal foreground color.
normalFGColor :: Lens' Options Color
normalFGColor = _normalFGColorL
-- | @-sb color@; defines the selected background color.
selectedBGColor :: Lens' Options Color
selectedBGColor = _selectedBGColorL
-- | @-sf color@; defines the selected foreground color.
selectedFGColor :: Lens' Options Color
selectedFGColor = _selectedFGColorL
-- | @-v@; prints version information to stdout, then exits.
printVersionAndExit :: Lens' Options Bool
printVersionAndExit = _printVersionAndExitL
-- | Extra options only available in the dmenu2 fork.
dmenu2 :: Lens' Options Options2
dmenu2 = _dmenu2L
-- | When set to @True@, the 'dmenu2' options are ignored. This
-- ensures compatibility with the normal @dmenu@. A user may set this flag
-- in the configuration file.
noDMenu2 :: Lens' Options Bool
noDMenu2 = _noDMenu2L

-- | List of extra command line arguments to pass to @dmenu@.
-- This can be useful, when the client wants to forward some of its own command
-- line arguments directly to the executed @dmenu@ processes.
--
-- Default: @[]@
extraArgs :: Lens' Options [String]
extraArgs = _extraArgsL

-- | @-q@; dmenu will not show any items if the search string is empty.
displayNoItemsIfEmpty :: Lens' Options2 Bool
displayNoItemsIfEmpty = _displayNoItemsIfEmptyL
-- | @-r@; activates filter mode. All matching items currently shown in the list
-- will be selected, starting with the item that is highlighted and wrapping around
-- to the beginning of the list. (/Note/: Instead of setting this flag yourself,
-- the @dmenu@ @filter@ functions can be used instead of the @select@ functions.)
filterMode :: Lens' Options2 Bool
filterMode = _filterModeL
-- | @-z@; dmenu uses fuzzy matching. It matches items that have all characters
-- entered, in sequence they are entered, but there may be any number of characters
-- between matched characters. For example it takes @\"txt\"@ makes it to
-- @\"*t*x*t\"@ glob pattern and checks if it matches.
fuzzyMatching :: Lens' Options2 Bool
fuzzyMatching = _fuzzyMatchingL
-- | @-t@; dmenu uses space-separated tokens to match menu items. Using this
-- overrides @-z@ option.
tokenMatching :: Lens' Options2 Bool
tokenMatching = _tokenMatchingL
-- | @-mask@; dmenu masks input with asterisk characters (@*@).
maskInputWithStar :: Lens' Options2 Bool
maskInputWithStar = _maskInputWithStarL
-- | @-noinput@; dmenu ignores input from stdin (equivalent to: @echo | dmenu@).
ignoreStdin :: Lens' Options2 Bool
ignoreStdin = _ignoreStdinL
-- | @-s screen@; dmenu apears on the specified screen number. Number given
-- corespondes to screen number in X configuration.
spawnOnScreen :: Lens' Options2 Int
spawnOnScreen = _spawnOnScreenL
-- | @-name name@; defines window name for dmenu. Defaults to @\"dmenu\"@.
windowName :: Lens' Options2 String
windowName = _windowNameL
-- | @-class class@; defines window class for dmenu. Defaults to @\"Dmenu"@.
windowClass :: Lens' Options2 String
windowClass = _windowClassL
-- | @-o opacity@; defines window opacity for dmenu. Defaults to @1.0@.
windowOpacity :: Lens' Options2 Double
windowOpacity = _windowOpacityL
-- | @-dim opacity@; enables screen dimming when dmenu appers. Takes dim opacity
-- as argument.
windowDimOpacity :: Lens' Options2 Double
windowDimOpacity = _windowDimOpacityL
-- | @-dc color@; defines color of screen dimming. Active only when @-dim@ in
-- effect. Defautls to black (@#000000@)
windowDimColor :: Lens' Options2 Color
windowDimColor = _windowDimColorL
-- | @-h height@; defines the height of the bar in pixels.
heightInPixels :: Lens' Options2 Int
heightInPixels = _heightInPixelsL
-- | @-uh height@; defines the height of the underline in pixels.
underlineHeightInPixels :: Lens' Options2 Int
underlineHeightInPixels = _underlineHeightInPixelsL
-- | @-x xoffset@; defines the offset from the left border of the screen.
windowOffsetX :: Lens' Options2 Int
windowOffsetX = _windowOffsetXL
-- | @-y yoffset@; defines the offset from the top border of the screen.
windowOffsetY :: Lens' Options2 Int
windowOffsetY = _windowOffsetYL
-- | @-w width@; defines the desired menu window width.
width :: Lens' Options2 Int
width = _widthL
-- | @-uc color@; defines the underline color.
underlineColor :: Lens' Options2 Color
underlineColor = _underlineColorL
-- | @-hist <histfile>@; the file to use for history
historyFile :: Lens' Options2 FilePath
historyFile = _historyFileL


defOptions :: Options
defOptions = Options
  { _binaryPath = "dmenu"
  , _displayAtBottom = False
  , _grabKeyboardBeforeStdin = False
  , _caseInsensitive = False
  , _numLines = (-1)
  , _prompt = ""
  , _font = ""
  , _spawnOnMonitor = (-1)
  , _normalBGColor = HexColor (-1)
  , _normalFGColor = HexColor (-1)
  , _selectedBGColor = HexColor (-1)
  , _selectedFGColor = HexColor (-1)
  , _printVersionAndExit = False
  , _dmenu2 = defOptions2
  , _noDMenu2 = False
  , _extraArgs = []
  }

defOptions2 :: Options2
defOptions2 = Options2
  { _filterMode = False
  , _fuzzyMatching = False
  , _displayNoItemsIfEmpty = False
  , _tokenMatching = False
  , _maskInputWithStar = False
  , _ignoreStdin = False
  , _spawnOnScreen = (-1)
  , _windowName = ""
  , _windowClass = ""
  , _windowOpacity = (-1)
  , _windowDimOpacity = (-1)
  , _windowDimColor = HexColor (-1)
  , _heightInPixels = (-1)
  , _underlineHeightInPixels = (-1)
  , _windowOffsetX = (-1)
  , _windowOffsetY = (-1)
  , _width = (-1)
  , _underlineColor = HexColor (-1)
  , _historyFile = ""
  }

optionsToArgs :: Options → [String]
optionsToArgs (Options{..}) = dmenuArgs ++ dmenu2Args ++ _extraArgs where
  dmenuArgs = concat $ concat
    [ [ [ "-b"                                   ] | _displayAtBottom ]
    , [ [ "-f"                                   ] | _grabKeyboardBeforeStdin ]
    , [ [ "-i"                                   ] | _caseInsensitive ]
    , [ [ "-m", show _spawnOnMonitor             ] | _spawnOnMonitor /= (-1) ]
    , [ [ "-l", show _numLines                   ] | _numLines /= (-1) ]
    , [ [ "-p", _prompt                          ] | _prompt /= "" ]
    , [ [ "-fn", _font                           ] | _font /= "" ]
    , [ [ "-nb", showColorAsHex _normalBGColor   ] | _normalBGColor /= HexColor (-1) ]
    , [ [ "-nf", showColorAsHex _normalFGColor   ] | _normalFGColor /= HexColor (-1) ]
    , [ [ "-sb", showColorAsHex _selectedBGColor ] | _selectedBGColor /= HexColor (-1) ]
    , [ [ "-sf", showColorAsHex _selectedFGColor ] | _selectedFGColor /= HexColor (-1) ]
    , [ [ "-v"                                   ] | _printVersionAndExit ]
    ]
  dmenu2Args | _noDMenu2 = []
             | otherwise = options2ToArgs _dmenu2

options2ToArgs :: Options2 → [String]
options2ToArgs (Options2{..}) = concat $ concat
  [ [ [ "-q"                                   ] | _displayNoItemsIfEmpty ]
  , [ [ "-r"                                   ] | _filterMode ]
  , [ [ "-z"                                   ] | _fuzzyMatching ]
  , [ [ "-t"                                   ] | _tokenMatching ]
  , [ [ "-mask"                                ] | _maskInputWithStar ]
  , [ [ "-noinput"                             ] | _ignoreStdin ]
  , [ [ "-s", show _spawnOnScreen              ] | _spawnOnScreen /= (-1) ]
  , [ [ "-name", show _windowName              ] | _windowName /= "" ]
  , [ [ "-class", show _windowClass            ] | _windowClass /= "" ]
  , [ [ "-o", show _windowOpacity              ] | _windowOpacity /= (-1) ]
  , [ [ "-dim"                                 ] | _windowDimOpacity /= (-1) ]
  , [ [ "-dc", showColorAsHex _windowDimColor  ] | _windowDimColor /= HexColor (-1) ]
  , [ [ "-h", show _heightInPixels             ] | _heightInPixels /= (-1) ]
  , [ [ "-uh", show _underlineHeightInPixels   ] | _underlineHeightInPixels /= (-1) ]
  , [ [ "-x", show _windowOffsetX              ] | _windowOffsetX /= (-1) ]
  , [ [ "-y", show _windowOffsetY              ] | _windowOffsetY /= (-1) ]
  , [ [ "-w", show _width                      ] | _width /= (-1) ]
  , [ [ "-uc", showColorAsHex _underlineColor  ] | _underlineColor /= HexColor (-1) ]
  , [ [ "-hist", show _historyFile             ] | _historyFile /= "" ]
  ]

readOr :: Read a => String → String → Either String a
readOr s err = case readMaybe s of
  Nothing -> Left err
  Just x  -> Right x

parseOptions :: String → Either String Options
parseOptions = foldl f (Right defOptions) . map splitFirstWord . lines where
  stringErr cmd =  "`" ++ cmd ++ "` must be a string, e.g. `\"/foo/bar\"`"
  boolErr   cmd =  "`" ++ cmd ++ "` must be a boolean, i.e. `True` or `False`."
  natErr    cmd =  "`" ++ cmd ++ "` must be a natural number, i.e. `0` or `20`."
  floatErr  cmd =  "`" ++ cmd ++ "` must be a floating point number, i.e. `0` or `20.23`."
  colorErr  cmd =  "`" ++ cmd ++ "` must be a color, i.e. `RGBColorF 1 0 0` for red."
  f :: Either String Options → (String, String) → Either String Options
  f opts (cmd, args) = opts >>= case cmd of
    "binaryPath"              → mapM (binaryPath                       .~) $ readOr args (stringErr cmd)
    "displayAtBottom"         → mapM (displayAtBottom                  .~) $ readOr args (boolErr cmd)
    "displayNoItemsIfEmpty"   → mapM (dmenu2 . displayNoItemsIfEmpty   .~) $ readOr args (boolErr cmd)
    "grabKeyboardBeforeStdin" → mapM (grabKeyboardBeforeStdin          .~) $ readOr args (boolErr cmd)
    "filterMode"              → mapM (dmenu2 . filterMode              .~) $ readOr args (boolErr cmd)
    "caseInsensitive"         → mapM (caseInsensitive                  .~) $ readOr args (boolErr cmd)
    "fuzzyMatching"           → mapM (dmenu2 . fuzzyMatching           .~) $ readOr args (boolErr cmd)
    "tokenMatching"           → mapM (dmenu2 . tokenMatching           .~) $ readOr args (boolErr cmd)
    "maskInputWithStar"       → mapM (dmenu2 . maskInputWithStar       .~) $ readOr args (boolErr cmd)
    "ignoreStdin"             → mapM (dmenu2 . ignoreStdin             .~) $ readOr args (boolErr cmd)
    "spawnOnScreen"           → mapM (dmenu2 . spawnOnScreen           .~) $ readOr args (natErr cmd)
    "spawnOnMonitor"          → mapM (spawnOnMonitor                   .~) $ readOr args (natErr cmd)
    "windowName"              → mapM (dmenu2 . windowName              .~) $ readOr args (stringErr cmd)
    "windowClass"             → mapM (dmenu2 . windowClass             .~) $ readOr args (stringErr cmd)
    "windowOpacity"           → mapM (dmenu2 . windowOpacity           .~) $ readOr args (floatErr cmd)
    "windowDimOpacity"        → mapM (dmenu2 . windowDimOpacity        .~) $ readOr args (floatErr cmd)
    "windowDimColor"          → mapM (dmenu2 . windowDimColor          .~) $ readOr args (colorErr cmd)
    "numLines"                → mapM (numLines                         .~) $ readOr args (natErr cmd)
    "heightInPixels"          → mapM (dmenu2 . heightInPixels          .~) $ readOr args (natErr cmd)
    "underlineHeightInPixels" → mapM (dmenu2 . underlineHeightInPixels .~) $ readOr args (natErr cmd)
    "prompt"                  → mapM (prompt                           .~) $ readOr args (stringErr cmd)
    "font"                    → mapM (font                             .~) $ readOr args (stringErr cmd)
    "windowOffsetX"           → mapM (dmenu2 . windowOffsetX           .~) $ readOr args (natErr cmd)
    "windowOffsetY"           → mapM (dmenu2 . windowOffsetY           .~) $ readOr args (natErr cmd)
    "width"                   → mapM (dmenu2 . width                   .~) $ readOr args (natErr cmd)
    "normalBGColor"           → mapM (normalBGColor                    .~) $ readOr args (colorErr cmd)
    "normalFGColor"           → mapM (normalFGColor                    .~) $ readOr args (colorErr cmd)
    "selectedBGColor"         → mapM (selectedBGColor                  .~) $ readOr args (colorErr cmd)
    "selectedFGColor"         → mapM (selectedFGColor                  .~) $ readOr args (colorErr cmd)
    "underlineColor"          → mapM (dmenu2 . underlineColor          .~) $ readOr args (colorErr cmd)
    "historyFile"             → mapM (dmenu2 . historyFile             .~) $ readOr args (stringErr cmd)
    "printVersionAndExit"     → mapM (printVersionAndExit              .~) $ readOr args (boolErr cmd)
    "noDMenu2"                → mapM (noDMenu2                         .~) $ readOr args (boolErr cmd)
    ""                        → pure
    _                         → const $ Left $ "Invalid command: " ++ cmd

-- | Description of the configuration file syntax of ~/.haskell-dmenu, as written
-- in the <https://github.com/m0rphism/haskell-dmenu/blob/master/CONFIG.md CONFIG.md> file.
--
-- This 'String' may be useful to inform clients about the config file, e.g. in
-- the usage information.
configFileUsage :: String
configFileUsage = unlines
  [ "The `dmenu` Haskell bindings support specifying default command line arguments"
  , "passed to `dmenu` in the `~/.haskell-dmenu` file."
  , ""
  , "The following shows an example ~/.haskell-dmenu file:"
  , ""
  , "  numLines         15"
  , "  font             \"FiraMono:size=11\""
  , "  caseInsensitive  True"
  , "  normalBGColor    RGBColorF 0.02 0.02 0.02"
  , ""
  , "Each line specifies the value of a dmenu option."
  , "The first word of a line specifies the option, the rest of the line the value."
  , "Depending on the option, the value has one of the following types and forms:"
  , ""
  , "  Nat      A natural number, e.g. 0, 1, 2, etc."
  , "  Float    A floating point number, e.g. -12 or 13.43"
  , "  String   A string literal, e.g. \"foo bar\""
  , "  Color    An RGB color. For example, the color red can be specified as"
  , "             HexColor 0xFF0000        (hexadecimal)"
  , "             RGBColor 255 0 0         (dezimal, split components)"
  , "             RGBColorF 1.0 0.0 0.0    (normalized, split components)"
  , ""
  , "All dmenu and dmenu2 options are supported:"
  , ""
  , "  binaryPath              : String"
  , "  displayAtBottom         : Bool"
  , "  displayNoItemsIfEmpty   : Bool"
  , "  grabKeyboardBeforeStdin : Bool"
  , "  filterMode              : Bool"
  , "  caseInsensitive         : Bool"
  , "  fuzzyMatching           : Bool"
  , "  tokenMatching           : Bool"
  , "  maskInputWithStar       : Bool"
  , "  ignoreStdin             : Bool"
  , "  spawnOnScreen           : Nat"
  , "  spawnOnMonitor          : Nat"
  , "  windowName              : String"
  , "  windowClass             : String"
  , "  windowOpacity           : Float"
  , "  windowDimOpacity        : Float"
  , "  windowDimColor          : Color"
  , "  numLines                : Nat"
  , "  heightInPixels          : Nat"
  , "  underlineHeightInPixels : Nat"
  , "  prompt                  : String"
  , "  font                    : String"
  , "  windowOffsetX           : Nat"
  , "  windowOffsetY           : Nat"
  , "  width                   : Nat"
  , "  normalBGColor           : Color"
  , "  normalFGColor           : Color"
  , "  selectedBGColor         : Color"
  , "  selectedFGColor         : Color"
  , "  underlineColor          : Color"
  , "  historyFile             : String"
  , "  printVersionAndExit     : Bool"
  , "  noDMenu2                : Bool"
  ]

-- printOptions :: Options → String
-- printOptions Options{..} = unlines $ concat
--   [ [ "binaryPath " ++ _binaryPath                                | _binaryPath /= "" ]
--   , [ "displayAtBottom"                                           | _displayAtBottom ]
--   , [ "displayNoItemsIfEmpty"                                     | _displayNoItemsIfEmpty ]
--   , [ "grabKeyboardBeforeStdin"                                   | _grabKeyboardBeforeStdin ]
--   , [ "filterMode"                                                | _filterMode ]
--   , [ "caseInsensitive"                                           | _caseInsensitive ]
--   , [ "fuzzyMatching"                                             | _fuzzyMatching ]
--   , [ "tokenMatching"                                             | _tokenMatching ]
--   , [ "maskInputWithStar"                                         | _maskInputWithStar ]
--   , [ "ignoreStdin"                                               | _ignoreStdin ]
--   , [ "spawnOnScreen " ++ show _spawnOnScreen                     | _spawnOnScreen /= (-1) ]
--   , [ "windowName " ++ _windowName                                | _windowName /= "" ]
--   , [ "windowClass " ++ _windowClass                              | _windowClass /= "" ]
--   , [ "windowOpacity " ++ show _windowOpacity                     | _windowOpacity /= (-1) ]
--   , [ "windowDimOpacity " ++ show _windowDimOpacity               | _windowDimOpacity /= (-1) ]
--   , [ "windowDimColor " ++ show _windowDimColor                   | _windowDimColor /= HexColor (-1) ]
--   , [ "numLines " ++ show _numLines                               | _numLines /= (-1) ]
--   , [ "heightInPixels " ++ show _heightInPixels                   | _heightInPixels /= (-1) ]
--   , [ "underlineHeightInPixels " ++ show _underlineHeightInPixels | _underlineHeightInPixels /= (-1) ]
--   , [ "prompt " ++ show _prompt                                   | _prompt /= "" ]
--   , [ "font " ++ show _font                                       | _font /= "" ]
--   , [ "windowOffsetX " ++ show _windowOffsetX                     | _windowOffsetX /= (-1) ]
--   , [ "windowOffsetY " ++ show _windowOffsetY                     | _windowOffsetY /= (-1) ]
--   , [ "width " ++ show _width                                     | _width /= (-1) ]
--   , [ "normalBGColor "   ++ show _normalBGColor                   | _normalBGColor /= HexColor (-1) ]
--   , [ "normalFGColor "   ++ show _normalFGColor                   | _normalFGColor /= HexColor (-1) ]
--   , [ "selectedBGColor " ++ show _selectedBGColor                 | _selectedBGColor /= HexColor (-1) ]
--   , [ "selectedFGColor " ++ show _selectedFGColor                 | _selectedFGColor /= HexColor (-1) ]
--   , [ "underlineColor "  ++ show _underlineColor                  | _underlineColor /= HexColor (-1) ]
--   , [ "historyFile " ++ show _historyFile                         | _historyFile /= "" ]
--   , [ "printVersionAndExit"                                       | _printVersionAndExit ]
--   ]

splitFirstWord :: String → (String, String)
splitFirstWord = go "" where
  go s []                           = (s, [])
  go s (c:cs) | c `elem` [' ','\t'] = (s, dropWhile (`elem` [' ','\t']) cs)
              | otherwise           = go (s++[c]) cs
