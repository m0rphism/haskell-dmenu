{-
  TODO
    - Add support for regular command line syntax config and/or env variable
-}

module DMenu (
    -- * Overview
    -- $overview

    -- * Types
    DMenuT, MonadDMenu, ProcessError,
    -- * Running @dmenu@
    run,
    -- ** Selecting a single item
    selectM, select, selectWithM, selectWith,
    -- ** Selecting multiple items
    filterM, filter, filterWithM, filterWith,

    -- * @dmenu@ Command Line Options
    Options(),
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
    dmenu2,
    noDMenu2,

    -- * @dmenu2@-specific Command Line Options
    Options2(),
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

    -- * Configuration File
    -- $configFile
  ) where

import Control.Lens

import DMenu.Color
import DMenu.Options
import DMenu.Run
import Prelude hiding (filter)

{- $overview
  This module provides complete bindings to the
  <http://tools.suckless.org/dmenu/ dmenu> and
  <https://bitbucket.org/melek/dmenu2 dmenu2> command-line tools.

  <<doc/dmenu-pmount.png>>

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
      into @dmenu@, and running @pmount@ on the selected device (shown in the
      image above).

  @dmenu2@ is a fork of @dmenu@, which provides additional options, e.g. selecting
  multiple items at once.

  Ontop of the functionality of @dmenu@ and @dmenu2@, this library
  supports a configuration file for specifying default command line options for
  @dmenu@. See the last section for more on the configuration file.

  The simplest way to run @dmenu@ is with the 'select' function.

  Note for @stack@ users: When running programs using this library via
  @stack exec@, the program may fail to find @dmenu@ in the @PATH@.
  This problem can be solved by running the program directly without @stack@, or
  by temporarily using an absolute path for @dmenu@ in the 'binaryPath' option.
-}
{- $configFile
  The default @Options@ used by 'run', 'select', etc. can be specified
  in the @~/.haskell-dmenu@ file.

  The following example shows the @~/.haskell-dmenu@ file used for the image from
  the first section:

  > numLines         15
  > font             FiraMono:size=11
  > caseInsensitive  True
  > normalBGColor    RGBColorF 0.02 0.02 0.02

  The configuration file contains one line per option.
  Each line consists of an option name and a value for the option.
  The option names are identical to the corresponding lens names.
  The values are read with @Prelude.read@ except for @Strings@ which don't need double quotes.
-}
