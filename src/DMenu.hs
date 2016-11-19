{-
  TODO
    - Add a remark, that stack exec may cause System.Process to fail finding
      programs, and this can be circumvented by specifying an absolute path.
    - Add documentation for .haskell-dmenu.conf
    - Add support for regular command line syntax config and/or env variable
-}

module DMenu (
    -- * Overview
    -- $overview

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

{- $overview
  This module provides complete bindings to the
  <http://tools.suckless.org/dmenu/ dmenu> and
  <https://bitbucket.org/melek/dmenu2 dmenu2> command-line tools.

  The @dmenu@ command line utility

  1.  takes @Options@ as arguments and reads a list of strings from @stdin@,
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
  the @Options2@ data type.

  Additionally to the functionality of @dmenu@ and @dmenu2@, this library
  supports specifying default command line options for @dmenu@ in a configuration
  file.

  The following example uses this library to let the user choose between the
  strings @\"A\"@, @\"B\"@, and @\"C\"@.

  > import qualified DMenu
  >
  > main :: IO ()
  > main = print =<< DMenu.select (pure ()) ["A","B","C"]

  The simplest way to use this library is the @"select"@ function.
  It takes the @dmenu@ options and a list of strings as arguments
  and returns @IO (Either ProcessError [String])@.
  The options are specified as a @State Options@ action, so @(pure ())@
  leaves the @Options@ unchanged. This means that

-}
