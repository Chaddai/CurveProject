{-# LANGUAGE ViewPatterns, OverloadedStrings #-}
module Math.GeneratorTools where

import Math.CurveGenerator

import Data.Text.Lazy (Text)
import Data.Text.Lazy.Builder
import Data.Text.Lazy.Builder.RealFloat
import Data.Text.Lazy.Builder.Int
import Data.Monoid
import Text.Printf
import Data.List

import Diagrams.Coordinates
import Diagrams.Prelude (P2, (.-^), (.+^))
import Data.VectorSpace

showF :: Int -> Double -> Builder
showF n = fromString . post . printf ("%." ++ show n ++ "f")

showListF :: Int -> [Double] -> Builder
showListF n = mconcat . intersperse ", " . map (showF n)

post :: String -> String
post = reverse . go . reverse
  where go ('.':cs) = cs
        go ('0':cs) = go cs
        go cs       = cs

inRange cmin cmax x = cmin < x && x < cmax

point :: P2 -> Builder
point (coords -> x :& y) = "(" <> showF 3 x <> " , " <> showF 3 y <> ")"

mWhen :: (Monoid m) => Bool -> m -> m
mWhen b m = if b then m else mempty
