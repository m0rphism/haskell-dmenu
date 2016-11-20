{-# LANGUAGE UnicodeSyntax, LambdaCase #-}
{-# LANGUAGE RecordWildCards, TemplateHaskell #-}

module DMenu.Options where

import Control.Lens

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
  }

-- | Contains the command line options of @dmenu2@ which are not part of
-- @dmenu@. The @_filterMode@ option is not listed; it can be implicitly used by
-- using @DMenu.filter@ instead of @DMenu.select@. The option descriptions are
-- copied from the @dmenu2@ @man@ page.
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
--   Default looks for @dmenu@ in the @PATH@ enviroment variable.
binaryPath :: Lens' Options FilePath
binaryPath = _binaryPathL
-- | @-b@; dmenu appears at the bottom of the screen.
displayAtBottom :: Lens' Options Bool
displayAtBottom = _displayAtBottomL
-- | @-f@; dmenu grabs the keyboard before reading stdin.  This is faster, but will lock up X until stdin reaches end-of-file.
grabKeyboardBeforeStdin :: Lens' Options Bool
grabKeyboardBeforeStdin = _grabKeyboardBeforeStdinL
-- | @-i@; dmenu matches menu items case insensitively.
caseInsensitive :: Lens' Options Bool
caseInsensitive = _caseInsensitiveL
-- | @-m screen@; dmenu is displayed on the monitor number supplied. Monitor numbers are starting from 0.
spawnOnMonitor :: Lens' Options Int
spawnOnMonitor = _spawnOnMonitorL
-- | @-l lines@; dmenu lists items vertically, with the given number of lines.
numLines :: Lens' Options Int
numLines = _numLinesL
-- | @-p prompt@; defines the prompt to be displayed to the left of the input field.
prompt :: Lens' Options String
prompt = _promptL
-- | @-fn font@; defines the font or font set used. eg. @\"fixed\"@ or @\"Monospace-12:normal\"@ (an xft font)
font :: Lens' Options String
font = _fontL
-- | @-nb color@; defines the normal background color.  @#RGB@, @#RRGGBB@, and X color names are supported.
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
-- | When set to @True@, the @dmenu2@ options in '_dmenu2' are ignored. This
-- ensures compatibility with the normal @dmenu@. A user may set this flag
-- in the configuration file.
noDMenu2 :: Lens' Options Bool
noDMenu2 = _noDMenu2L

-- | @-q@; dmenu will not show any items if the search string is empty.
displayNoItemsIfEmpty :: Lens' Options2 Bool
displayNoItemsIfEmpty = _displayNoItemsIfEmptyL
-- | @-r@; activates filter mode. All matching items currently shown in the list will be selected, starting with the item that is highlighted and wrapping around to the beginning of the list. (/Note/: Instead of setting this flag yourself, the @dmenu@ @filter@ functions can be used instead of the @select@ functions.)
filterMode :: Lens' Options2 Bool
filterMode = _filterModeL
-- | @-z@; dmenu uses fuzzy matching. It matches items that have all characters entered, in sequence they are entered, but there may be any number of characters between matched characters.  For example it takes @\"txt\"@ makes it to @\"*t*x*t\"@ glob pattern and checks if it matches.
fuzzyMatching :: Lens' Options2 Bool
fuzzyMatching = _fuzzyMatchingL
-- | @-t@; dmenu uses space-separated tokens to match menu items. Using this overrides @-z@ option.
tokenMatching :: Lens' Options2 Bool
tokenMatching = _tokenMatchingL
-- | @-mask@; dmenu masks input with asterisk characters (@*@).
maskInputWithStar :: Lens' Options2 Bool
maskInputWithStar = _maskInputWithStarL
-- | @-noinput@; dmenu ignores input from stdin (equivalent to: @echo | dmenu@).
ignoreStdin :: Lens' Options2 Bool
ignoreStdin = _ignoreStdinL
-- | @-s screen@; dmenu apears on the specified screen number. Number given corespondes to screen number in X configuration.
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
-- | @-dim opacity@; enables screen dimming when dmenu appers. Takes dim opacity as argument.
windowDimOpacity :: Lens' Options2 Double
windowDimOpacity = _windowDimOpacityL
-- | @-dc color@; defines color of screen dimming. Active only when @-dim@ in effect. Defautls to black (@#000000@)
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
optionsToArgs (Options{..}) = concat $ concat $
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
  ] ++ if _noDMenu2 then [] else options2ToArgs _dmenu2

options2ToArgs :: Options2 → [[[String]]]
options2ToArgs (Options2{..}) =
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

parseOptions :: String → Options
parseOptions = foldl f defOptions . map splitFirstWord . lines where
  f :: Options → (String, String) → Options
  f opts (cmd, args) = opts & case cmd of
    "binaryPath"              → binaryPath                       .~ args
    "displayAtBottom"         → displayAtBottom                  .~ read args
    "displayNoItemsIfEmpty"   → dmenu2 . displayNoItemsIfEmpty   .~ read args
    "grabKeyboardBeforeStdin" → grabKeyboardBeforeStdin          .~ read args
    "filterMode"              → dmenu2 . filterMode              .~ read args
    "caseInsensitive"         → caseInsensitive                  .~ read args
    "fuzzyMatching"           → dmenu2 . fuzzyMatching           .~ read args
    "tokenMatching"           → dmenu2 . tokenMatching           .~ read args
    "maskInputWithStar"       → dmenu2 . maskInputWithStar       .~ read args
    "ignoreStdin"             → dmenu2 . ignoreStdin             .~ read args
    "spawnOnScreen"           → dmenu2 . spawnOnScreen           .~ read args
    "spawnOnMonitor"          → spawnOnMonitor                   .~ read args
    "windowName"              → dmenu2 . windowName              .~ args
    "windowClass"             → dmenu2 . windowClass             .~ args
    "windowOpacity"           → dmenu2 . windowOpacity           .~ read args
    "windowDimOpacity"        → dmenu2 . windowDimOpacity        .~ read args
    "windowDimColor"          → dmenu2 . windowDimColor          .~ read args
    "numLines"                → numLines                         .~ read args
    "heightInPixels"          → dmenu2 . heightInPixels          .~ read args
    "underlineHeightInPixels" → dmenu2 . underlineHeightInPixels .~ read args
    "prompt"                  → prompt                           .~ args
    "font"                    → font                             .~ args
    "windowOffsetX"           → dmenu2 . windowOffsetX           .~ read args
    "windowOffsetY"           → dmenu2 . windowOffsetY           .~ read args
    "width"                   → dmenu2 . width                   .~ read args
    "normalBGColor"           → normalBGColor                    .~ read args
    "normalFGColor"           → normalFGColor                    .~ read args
    "selectedBGColor"         → selectedBGColor                  .~ read args
    "selectedFGColor"         → selectedFGColor                  .~ read args
    "underlineColor"          → dmenu2 . underlineColor          .~ read args
    "historyFile"             → dmenu2 . historyFile             .~ args
    "printVersionAndExit"     → printVersionAndExit              .~ read args
    "noDMenu2"                → noDMenu2                         .~ read args
    ""                        → id
    _                         → error $ "Invalid command found when parsing dmenu config file: " ++ cmd

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
