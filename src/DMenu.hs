{-
  TODO
    - Add a remark, that stack exec may cause System.Process to fail finding
      programs, and this can be circumvented by specifying an absolute path.
    - Support for a .dmenurc file.
    - apps:
      - select config file to edit, ask for sudo pw for root configs in a secure way.
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
    displayNoItemsIfEmpty,
    grabKeyboardBeforeStdin,
    filterMode,
    caseInsensitive,
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

import Control.Lens

import DMenu.Color
import DMenu.Options
import DMenu.Run
