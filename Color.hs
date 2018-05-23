{-# LANGUAGE OverloadedStrings #-}
module Color where

import           Data.Monoid
import           Data.Text.Lazy

data Color = Color { r :: Int, g :: Int, b :: Int }
  deriving (Eq, Show)

toS :: (Show a) => a -> Text
toS = pack . show

hsv_to_rgb ::
  Double -> Double -> Double -> Color
hsv_to_rgb h s v =
  case hi of
    0 -> color v t p
    1 -> color q v p
    2 -> color p v t
    3 -> color p q v
    4 -> color t p v
    5 -> color v p q
    _ -> error "does not happen"
 where
   color r g b = Color (truncate $ r * 256) (truncate $ g * 256) (truncate $ b * 256)
   hi = floor $ h * 6 :: Int
   f = h * 6 - fromIntegral hi
   p  = v *(1 - s)
   q  = v *(1 - f * s)
   t  = v *(1 - (1 - f) * s)

mod1 :: RealFrac a => a -> a
mod1 x = x - fromInteger (floor x)

genColors :: [ Color ]
genColors = genColors' startColor
  where
    startColor = 0.6180339
    genColors' c = hsv_to_rgb (mod1 c) 0.5 0.95 : genColors' nextColor
      where
        nextColor = c + startColor

colorise :: Color -> Text -> Text
colorise (Color r g b) t = "\x1b[38;2;" <> toS r <> ";" <> toS g <> ";" <> toS b <> "m" <> t <> "\x1b[0m"
