module Main where

import Math.CurveGenerator
import Math.SVGGenerator as S
import Math.PSTricksGenerator as P
import Math.TikzGenerator as Tikz
import qualified Data.Text.Lazy.IO as T
import Data.Default
import Diagrams.Prelude
import Diagrams.Coordinates

main = do
  T.putStr $ S.drawAll curve (Width 800)
  T.putStr $ P.drawAll curve
  T.putStr $ Tikz.drawAll curve
  where
    curve= def -- { curveInputs=[( CurvePointsAndT 0.4 [p1, p2, p3, p4], def)] }
    -- p1= ((-2) & 3, Nothing, True)
    -- p2= (0 & 0, Nothing, False)
    -- p3= (2 & 2, Nothing, True)
    -- p4= (5 & 0, Nothing, True)
