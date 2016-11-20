{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module DMenu.Lens where

import Control.Lens

makeLensesL = makeLensesWith $ lensRules
  & lensField .~ mappingNamer ((:[]) . (++"L"))
