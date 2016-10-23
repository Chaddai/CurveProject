{-# LANGUAGE ViewPatterns, OverloadedStrings #-}
module Math.GeneratorTools where

import Data.Text.Lazy.Builder
import Data.Monoid
import Text.Printf
import Data.List

import Diagrams.Coordinates
import Diagrams.Prelude (P2)

showF :: Int -> Double -> Builder
showF n = fromString . post . printf ("%." ++ show n ++ "f")

showListF :: Int -> [Double] -> Builder
showListF n = mconcat . intersperse ", " . map (showF n)

post :: String -> String
post = reverse . go . reverse
  where go ('.':cs) = cs
        go ('0':cs) = go cs
        go cs       = cs

inRange :: (Ord a) => a -> a -> a -> Bool
inRange cmin cmax x = cmin < x && x < cmax

point :: P2 Double -> Builder
point (coords -> x :& y) = "(" <> showF 3 x <> " , " <> showF 3 y <> ")"

mWhen :: (Monoid m) => Bool -> m -> m
mWhen b m = if b then m else mempty

