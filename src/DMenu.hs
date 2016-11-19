{-
  TODO
    - Add documentation for .haskell-dmenu.conf
    - Add support for regular command line syntax config and/or env variable
-}

module DMenu (
    -- * Overview
    -- $overview

    -- * Running DMenu
    DMenuT, MonadDMenu, ProcessError,
    run, selectM, select, selectWithM, selectWith,

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

{- $overview
  This module provides complete bindings to the
  <http://tools.suckless.org/dmenu/ dmenu> and
  <https://bitbucket.org/melek/dmenu2 dmenu2> command-line tools.

  The @dmenu@ command line tool

  1.  takes command line 'Options' and reads a list of strings from @stdin@,
  2.  presents the list in a special overlay window, in which the user can select
      from the list via fuzzy matching, and
  3.  prints the selected string to @stdout@ or fails with exit code @1@ if the
      user hit the @ESC@ key.

  Typical uses of @dmenu@ are for example

  1.  as a program launcher by piping the program names from @PATH@ into @dmenu@
      and executing the selected program.
  2.  as an interface for killing programs by piping process information from
      @ps aux@ into @dmenu@, and running @kill -9@ on the selected process id.
  3.  as an interface for mounting devices by piping the device files from @\/dev\/@
      into @dmenu@, and running @pmount@ on the selected device.

  @dmenu2@ is a fork of @dmenu@, which provides additional options covered in
  the 'Options2' data type.

  Additionally to the functionality of @dmenu@ and @dmenu2@, this library
  supports specifying default command line options for @dmenu@ in a configuration
  file.

  The simplest way to run @dmenu@ is with the 'select' function.

  Note for @stack@ users: When running programs using this library via
  @stack exec@, the program may fail to find @dmenu@ in the @PATH@.
  This problem can be solved by running the program directly without @stack@, or
  by temporarily using an absolute path for @dmenu@ in the 'binaryPath' option.
-}
