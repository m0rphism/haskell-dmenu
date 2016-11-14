{-# LANGUAGE RecordWildCards, TemplateHaskell #-}

module DMenu.Options where

import Control.Exception
import Control.Monad.State.Strict
import Control.Lens
import Data.Maybe
import System.Exit
import System.Process
import System.Directory
import System.IO
import Numeric (showHex)

import DMenu.Color

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

splitFirstWord :: String → (String, String)
splitFirstWord = go "" where
  go s []                           = (s, [])
  go s (c:cs) | c `elem` [' ','\t'] = (s, dropWhile (`elem` [' ','\t']) cs)
              | otherwise           = go (s++[c]) cs
