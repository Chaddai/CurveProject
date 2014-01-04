{-# LANGUAGE OverloadedStrings, ViewPatterns, RecordWildCards, NamedFieldPuns #-}

module Math.PSTricksGenerator (module Math.CurveGenerator
                              , drawAll
                              , drawAxis
                              , drawGrid
                              , drawCurve
                              , drawTangents) where

import Math.CurveGenerator
import Math.GeneratorTools

import Data.Text.Lazy (Text)
import Data.Text.Lazy.Builder
import Data.Text.Lazy.Builder.RealFloat
import Data.Text.Lazy.Builder.Int
import Data.Monoid
import Text.Printf
import Data.List

import Diagrams.Coordinates
import Diagrams.Prelude (P2, (.-^), (.+^), origin)
import Data.VectorSpace

drawAll :: CGConfig -> Text
drawAll config@CGConfig{axisOptions=AxisOpts{..}, comments, curveInputs=cs} =
  toLazyText $ mWhen comments "% Limite le dessin au repère\n"
  <> "\\begin{pspicture*}" <> point (xMin ^& yMin) <> point (xMax ^& yMax) <> "\n\n"
  <> drawGrid config <> drawAxis config
  <> mconcat (zipWith drawNCT [1..] cs)
  <> "\\end{pspicture*}\n"
  where drawNCT i (createCurve -> curve, curveConfig) =
          drawCurve i config curveConfig curve <> drawTangents i config curve

drawAxis :: CGConfig -> Builder
drawAxis CGConfig{axisOptions=AxisOpts {..}, comments} =
  mWhen comments "% Axes du repère\n"
  <> "\\psaxes[linewidth=1pt, showorigin=false,Dx=" <> showF 3 xTicks <> ",Dy=" <> showF 3 yTicks
  <> ", Ox=" <> showF 3 xOrig <> ", Oy=" <> showF 3 yOrig <> "]{->}"
  <> point (xOrig ^& yOrig) <> point (xMin ^& yMin) <> point (xMax ^& yMax) <> "\n"

drawGrid :: CGConfig -> Builder
drawGrid CGConfig{axisOptions=AxisOpts{..}, gridOptions=GridOpts{..}, comments}
  | majorGrid || minorGrid =
    mWhen comments "% Grille du repère\n"
    <> "\\psgrid[gridlabels=0, gridcolor=gray, gridwidth=.2pt, "
    <> "xunit=" <> showF 3 dxMajor <> ", yunit=" <> showF 3 dyMajor
    -- PSTricks can't do different subgrids in the x and y dimensions
    <> ", subgriddiv=" <> if minorGrid then decimal (floor $ dxMajor/dxMinor) else "0" 
    <> ", subgridwidth=.1pt, subgridcolor=lightgray]"
    <> point (xOrig ^& yOrig) <> point (xMin ^& yMin) <> point (xMax ^& yMax) <> "\n"
  | otherwise = mempty  
                       
drawCurve :: Int -> CGConfig -> CurveOptions -> Curve -> Builder
drawCurve i CGConfig{comments} CurveOpts{..} (BezierJoints (map piPoint -> ps@(_:_:_))) =
  mWhen comments ("% Courbe n°" <> decimal i <> "\n")
  <> go ps
  where
    go (p1:c1:c2:ps@(p2:_)) =
      "  \\psbezier" <> point p1 <> point c1 <> point c2 <> point p2 <> "\n" <> go ps
    go _ = "\n"
drawCurve _ _ _ _ = mempty

drawTangents :: Int -> CGConfig -> Curve -> Builder
drawTangents i CGConfig{tangentsOptions=TanOpts {..}, comments} (BezierJoints pts) = go pts
  where
    go (Through p (normalized -> t) True : pts) =
      mWhen comments ("% Tangente en " <> point p <> " à la courbe n°" <> decimal i <> "\n")
      <> "\\psline[linecolor=" <> fromString tangentColor <> ", linestyle=" <> fromString tangentStyle
      <> "]{<->}" <> point (p .-^ tangentLen *^ t) <> point (p .+^ tangentLen *^ t) <> "\n"
      <> "\\psdot[linecolor=" <> fromString tangentColor <> "]" <> point p <> "\n" <> go pts
    go (_ : pts) = go pts
    go _ = mempty
drawTangents _ _ _ = mempty

