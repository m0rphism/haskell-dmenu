{-# LANGUAGE UnicodeSyntax, LambdaCase #-}

module DMenu.Color (
    Color(..), showColorAsHex,
  ) where

import Numeric (showHex)

-- | Multiple representations for colors.
--
-- For example, green can be defined as
--
-- > green1 = HexColor 0x00FF00
-- > green2 = RGBColor 0 255 0
-- > green3 = RGBColorF 0 1 0
data Color
  = HexColor Int
  | RGBColor Int Int Int
  | RGBColorF Float Float Float
  deriving (Eq, Ord, Read, Show)

-- | Render a color to a hexadecimal @String@ representation. For example
--
-- >>> showColorAsHex (RGBColor 5 5 5)
-- "#050505"
showColorAsHex :: Color → String
showColorAsHex = \case
  HexColor i      → "#" ++ fillLeft '0' 6 (showHex i "")
  RGBColor r g b  → showColorAsHex $ HexColor $ r*256*256 + g*256 + b
  RGBColorF r g b → showColorAsHex $ RGBColor (f r) (f g) (f b)
 where
  f = floor . (* 255)
  fillLeft c i s = replicate (i - length s) c ++ s
