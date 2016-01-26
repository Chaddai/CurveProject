{-# LANGUAGE OverloadedStrings, ViewPatterns, RecordWildCards #-}

module Math.SVGGenerator (module Math.CurveGenerator
                          , drawAll
                          , drawAxis
                          , drawGrid
                          , drawCurve
                          , drawTangents
                          , renderSvg
                          , SizeSpec(..)) where

import Math.CurveGenerator
import Math.GeneratorTools

import Data.Text.Lazy (Text)

import Diagrams.Coordinates
import Linear.Metric
import Linear.Vector

import Diagrams.TwoD.Arrow
import Diagrams.Prelude
import Diagrams.Core.Types 

import Diagrams.Backend.SVG (SVG(..), Options(..))
import Lucid.Base (renderText)

import Control.Lens ((^.))

import Data.Maybe
import Text.Printf

drawAll :: SizeSpec V2 Double -> CGConfig -> Text
drawAll ss2D config@CGConfig{axisOptions=AxisOpts{..}, curveInputs=cs} =
  renderSvg ss2D . rectEnvelope pMin (pMax .-. pMin) $
  mconcat (zipWith drawNCT [1..] cs)
  <> drawAxis config <> drawGrid config
  where drawNCT i (createCurve -> curve, curveConfig) = drawCurve i curveConfig curve <> drawTangents i config curve
        pMin = xMin ^& yMin
        pMax = xMax ^& yMax

renderSvg :: SizeSpec V2 Double -> Diagram SVG -> Text
renderSvg ss2D = renderText . renderDia SVG (SVGOptions ss2D Nothing "")

drawAxis :: CGConfig -> Diagram SVG
drawAxis CGConfig{axisOptions=AxisOpts {..}} =
  lw thin $ axis (xMin ^& yOrig) (xMax ^& yOrig)
           <> axis (xOrig ^& yMin) (xOrig ^& yMax)
           <> ticks (xMin ^& yOrig) (xMax ^& yOrig) (0 ^& yOrig)
                xMin xMax xOrig xTicks 0.5
            <> ticks (xOrig ^& yMin) (xOrig ^& yMax) (xOrig ^& 0)
                yMin yMax yOrig yTicks (-0.5)

axis p1 p2 = arrowBetween' (with & arrowHead .~ spike
                                 & lengths .~ verySmall)
                            p1 p2

ticks :: P2 Double -> P2 Double -> P2 Double -> Double -> Double -> Double -> Double -> Double -> Diagram SVG
ticks p1 p2 myOrigin cmin cmax corig space dist =
  mconcat . map drawTick . filter inRange $
    [corig+space, corig+2*space..cmax]
    ++ [corig-space, corig-2*space..cmin]
  where
    inRange x = cmin < x && x < cmax
    v = normalize (p2 .-. p1)
    pv = perp v
    drawTick c =
      ((tCenter .-^ 0.2 *^ pv) ~~ (tCenter .+^ 0.2 *^ pv))
      <> text (post $ printf "%.2f" c) # scale 0.3 # translate ((myOrigin .-. origin) ^+^ c *^ v ^-^ dist *^ pv)
      where tCenter = myOrigin .+^ (c *^ v)

drawGrid :: CGConfig -> Diagram SVG
drawGrid config@CGConfig{gridOptions=GridOpts{..}} =
  mWhen majorGrid (grid config dxMajor dyMajor # lw veryThin # lc gray) 
  <> mWhen minorGrid (grid config dxMinor dyMinor # lw ultraThin # lc gray)

grid :: CGConfig -> Double -> Double -> Diagram SVG
grid CGConfig{axisOptions=AxisOpts{..}} dx dy =
  mconcat $ map xGrid ([xOrig+dx, xOrig+2*dx..xMax] ++ [xOrig-dx, xOrig-2*dx..xMin])
  ++ map yGrid ([yOrig+dy, yOrig+2*dy..yMax] ++ [yOrig-dy, yOrig-2*dy..yMin])
  where
    xGrid x = (x ^& yMin) ~~ (x ^& yMax)
    yGrid y = (xMin ^& y) ~~ (xMax ^& y)

drawCurve :: Int -> CurveOptions -> Curve -> Diagram SVG
drawCurve i CurveOpts{..} (BezierJoints (map piPoint -> ps@(_:_:_))) =
  lw thin . lc (colourFrom curveColor) . dashPatternFrom curveStyle
  . fromLocSegments . (`at` p1) . go $ ps
  where
    p1 = fromMaybe origin $ listToMaybe ps
    go (p1 : c1 : c2 : p2 : ps) =
      bezier3 (c1 .-. p1) (c2 .-. p1) (p2 .-. p1) : go (p2:ps)
    go _ = []
drawCurve _ _ _ = mempty

 
drawTangents :: Int -> CGConfig -> Curve -> Diagram SVG
drawTangents i CGConfig{tangentsOptions=TanOpts {..}} (BezierJoints pts) =
  go pts
  where
    go (Through p (normalize -> t) True : pts) =
      position [(p, circle 0.05 # fc tColour)] 
      <> tangentLine (p .-^ tangentLen *^ t) (p .+^ tangentLen *^ t) tColour (dashPatternFrom tangentStyle)
      <> go pts
    go (_:pts) = go pts
    go [] = mempty
    tColour = colourFrom tangentColor

tangentLine p1 p2 c d = arrowBetween' (with & arrowHead .~ spike
                                            & arrowTail .~ spike'
                                            & lengths .~ verySmall)
                                      p1 p2 # d # lw veryThin # lc c # fc c
  
colourFrom c = fromMaybe black . readColourName $ c
dashPatternFrom s = case s of
  "solid" -> dashing [] (local 0)
  "dashed" -> dashing [verySmall, tiny] (local 0)
  "dotted" -> dashing [medium, medium] (local 0)
  _        -> dashing [] (local 0)
