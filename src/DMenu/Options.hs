{-# LANGUAGE UnicodeSyntax, LambdaCase #-}
{-# LANGUAGE RecordWildCards, TemplateHaskell #-}

module DMenu.Options where

import Control.Lens

import DMenu.Color

-- | Contains the binary path and command line options of dmenu.
-- The option descriptions are copied from the @dmenu@ @man@ page.
data Options = Options
  { -- | Path to the the dmenu executable file.
    --   Default looks for @dmenu@ in the @PATH@ enviroment variable.
    _binaryPath :: FilePath
    -- | @-b@; dmenu appears at the bottom of the screen.
  , _displayAtBottom :: Bool
    -- | @-f@; dmenu grabs the keyboard before reading stdin.  This is faster, but will lock up X until stdin reaches end-of-file.
  , _grabKeyboardBeforeStdin :: Bool
    -- | @-i@; dmenu matches menu items case insensitively.
  , _caseInsensitive :: Bool
    -- | @-m screen@; dmenu is displayed on the monitor number supplied. Monitor numbers are starting from 0.
  , _spawnOnMonitor :: Int
    -- | @-l lines@; dmenu lists items vertically, with the given number of lines.
  , _numLines :: Int
    -- | @-p prompt@; defines the prompt to be displayed to the left of the input field.
  , _prompt :: String
    -- | @-fn font@; defines the font or font set used. eg. @\"fixed\"@ or @\"Monospace-12:normal\"@ (an xft font)
  , _font :: String
    -- | @-nb color@; defines the normal background color.  @#RGB@, @#RRGGBB@, and X color names are supported.
  , _normalBGColor :: Color
    -- | @-nf color@; defines the normal foreground color.
  , _normalFGColor :: Color
    -- | @-sb color@; defines the selected background color.
  , _selectedBGColor :: Color
    -- | @-sf color@; defines the selected foreground color.
  , _selectedFGColor :: Color
    -- | @-v@; prints version information to stdout, then exits.
  , _printVersionAndExit :: Bool
    -- | Extra options only available in the dmenu2 fork.
  , _dmenu2 :: Options2
    -- | When set to @True@, the @dmenu2@ options in '_dmenu2' are ignored. This
    -- ensures compatibility with the normal @dmenu@. A user may set this flag
    -- in the configuration file.
  , _noDMenu2 :: Bool
  }

-- | Contains the command line options of @dmenu2@ which are not part of
-- @dmenu@. The @_filterMode@ option is not listed; it can be implicitly used by
-- using @DMenu.filter@ instead of @DMenu.select@. The option descriptions are
-- copied from the @dmenu2@ @man@ page.
data Options2 = Options2
  { -- | @-q@; dmenu will not show any items if the search string is empty.
    _displayNoItemsIfEmpty :: Bool
    -- | @-r@; activates filter mode. All matching items currently shown in the list will be selected, starting with the item that is highlighted and wrapping around to the beginning of the list. (/Note/: Instead of setting this flag yourself, the @dmenu@ @filter@ functions can be used instead of the @select@ functions.)
  , _filterMode :: Bool
    -- | @-z@; dmenu uses fuzzy matching. It matches items that have all characters entered, in sequence they are entered, but there may be any number of characters between matched characters.  For example it takes @\"txt\"@ makes it to @\"*t*x*t\"@ glob pattern and checks if it matches.
  , _fuzzyMatching :: Bool
    -- | @-t@; dmenu uses space-separated tokens to match menu items. Using this overrides @-z@ option.
  , _tokenMatching :: Bool
    -- | @-mask@; dmenu masks input with asterisk characters (@*@).
  , _maskInputWithStar :: Bool
    -- | @-noinput@; dmenu ignores input from stdin (equivalent to: @echo | dmenu@).
  , _ignoreStdin :: Bool
    -- | @-s screen@; dmenu apears on the specified screen number. Number given corespondes to screen number in X configuration.
  , _spawnOnScreen :: Int
    -- | @-name name@; defines window name for dmenu. Defaults to @\"dmenu\"@.
  , _windowName :: String
    -- | @-class class@; defines window class for dmenu. Defaults to @\"Dmenu"@.
  , _windowClass :: String
    -- | @-o opacity@; defines window opacity for dmenu. Defaults to @1.0@.
  , _windowOpacity :: Double
    -- | @-dim opacity@; enables screen dimming when dmenu appers. Takes dim opacity as argument.
  , _windowDimOpacity :: Double
    -- | @-dc color@; defines color of screen dimming. Active only when @-dim@ in effect. Defautls to black (@#000000@)
  , _windowDimColor :: Color
    -- | @-h height@; defines the height of the bar in pixels.
  , _heightInPixels :: Int
    -- | @-uh height@; defines the height of the underline in pixels.
  , _underlineHeightInPixels :: Int
    -- | @-x xoffset@; defines the offset from the left border of the screen.
  , _windowOffsetX :: Int
    -- | @-y yoffset@; defines the offset from the top border of the screen.
  , _windowOffsetY :: Int
    -- | @-w width@; defines the desired menu window width.
  , _width :: Int
    -- | @-uc color@; defines the underline color.
  , _underlineColor :: Color
    -- | @-hist <histfile>@; the file to use for history
  , _historyFile :: FilePath
  }

makeLenses ''Options
makeLenses ''Options2

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
