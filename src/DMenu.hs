{-
  TODO
    - Add a remark, that stack exec may cause System.Process to fail finding
      programs, and this can be circumvented by specifying an absolute path.
    - Add documentation for .haskell-dmenu.conf
    - Add support for regular command line syntax config and/or env variable
-}

module DMenu (
    -- * Running DMenu
    DMenuT, MonadDMenu, ProcessError,
    run, selectM, select, selectM', select', repl,
    -- * Command Line Options
    Options(..),
    -- ** Lenses
    binaryPath,
    displayAtBottom,
    grabKeyboardBeforeStdin,
    caseInsensitive,
    spawnOnMonitor,
    numLines,
    prompt,
    font,
    normalBGColor,
    normalFGColor,
    selectedBGColor,
    selectedFGColor,
    printVersionAndExit,
    -- * Extra Options for the dmenu2 fork
    Options2(..),
    -- ** Lenses
    displayNoItemsIfEmpty,
    filterMode,
    fuzzyMatching,
    tokenMatching,
    maskInputWithStar,
    ignoreStdin,
    spawnOnScreen,
    windowName,
    windowClass,
    windowOpacity,
    windowDimOpacity,
    windowDimColor,
    heightInPixels,
    underlineHeightInPixels,
    windowOffsetX,
    windowOffsetY,
    width,
    underlineColor,
    historyFile,
    -- * Color
    Color(..),
    -- * Reexports from @lens@
    (.=),
  ) where

import Control.Lens

import DMenu.Color
import DMenu.Options
import DMenu.Run
