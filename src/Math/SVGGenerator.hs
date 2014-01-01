{-# LANGUAGE OverloadedStrings, ViewPatterns, RecordWildCards #-}

module Math.SVGGenerator (module Math.CurveGenerator
                          , drawAll
                          , drawAxis
                          , drawGrid
                          , drawCurve
                          , drawTangents
                          , renderSvg
                          , SizeSpec2D(..)) where

import Math.CurveGenerator
import Math.GeneratorTools

import Data.Text.Lazy (Text)

import Diagrams.Coordinates
import Diagrams.TwoD.Vector
import Diagrams.Prelude
import Diagrams.Backend.SVG (SVG(..), Options(..))
import Diagrams.Core.Types 
import qualified Text.Blaze.Svg.Renderer.Text as TBS

import Data.Maybe
import Text.Printf

drawAll :: SizeSpec2D -> CGConfig -> Text
drawAll ss2D config@CGConfig{axisOptions=AxisOpts{..}, curveInputs=cs} =
  renderSvg ss2D . view pMin (pMax .-. pMin) $
  mconcat (zipWith drawNCT [1..] cs)
  <> drawAxis config <> drawGrid config
  where drawNCT i (createCurve -> curve, curveConfig) = drawCurve i curveConfig curve <> drawTangents i config curve
        pMin = xMin ^& yMin
        pMax = xMax ^& yMax

renderSvg :: SizeSpec2D -> Diagram SVG R2 -> Text
renderSvg ss2D = TBS.renderSvg . renderDia SVG (SVGOptions ss2D Nothing)

drawAxis :: CGConfig -> Diagram SVG R2
drawAxis CGConfig{axisOptions=AxisOpts {..}} =
  axis (xMin ^& yOrig) (xMax ^& yOrig)
  <> axis (xOrig ^& yMin) (xOrig ^& yMax)
  <> ticks (xMin ^& yOrig) (xMax ^& yOrig) (0 ^& yOrig)
       xMin xMax xOrig xTicks 0.5
  <> ticks (xOrig ^& yMin) (xOrig ^& yMax) (xOrig ^& 0)
       yMin yMax yOrig yTicks (-0.5)

axis p1 p2 = (arrowHead <> shaft) # fc black
  where
    v = p2 .-. p1
    shaft     = p1 ~~ p2
    arrowHead = eqTriangle 0.1
               # rotateBy (direction v - 1/4)
               # translate (p2 .-. origin)

ticks :: P2 -> P2 -> P2 -> Double -> Double -> Double -> Double -> Double -> Diagram SVG R2
ticks p1 p2 myOrigin cmin cmax corig space dist =
  mconcat . map drawTick . filter inRange $
    [corig+space, corig+2*space..cmax]
    ++ [corig-space, corig-2*space..cmin]
  where
    inRange x = cmin < x && x < cmax
    v = normalized (p2 .-. p1)
    pv = perp v
    drawTick c =
      ((tCenter .-^ 0.2 *^ pv) ~~ (tCenter .+^ 0.2 *^ pv))
      <> text (post $ printf "%.2f" c) # scale 0.3 # translate ((myOrigin .-. origin) ^+^ c *^ v ^-^ dist *^ pv)
      where tCenter = myOrigin .+^ (c *^ v)

drawGrid :: CGConfig -> Diagram SVG R2
drawGrid config@CGConfig{gridOptions=GridOpts{..}} =
  mWhen majorGrid (grid config dxMajor dyMajor # lw 0.007 # lc gray) 
  <> mWhen minorGrid (grid config dxMinor dyMinor # lw 0.004 # lc gray)

grid :: CGConfig -> Double -> Double -> Diagram SVG R2
grid CGConfig{axisOptions=AxisOpts{..}} dx dy =
  mconcat $ map xGrid ([xOrig+dx, xOrig+2*dx..xMax] ++ [xOrig-dx, xOrig-2*dx..xMin])
  ++ map yGrid ([yOrig+dy, yOrig+2*dy..yMax] ++ [yOrig-dy, yOrig-2*dy..yMin])
  where
    xGrid x = (x ^& yMin) ~~ (x ^& yMax)
    yGrid y = (xMin ^& y) ~~ (xMax ^& y)

drawCurve :: Int -> CurveOptions -> Curve -> Diagram SVG R2
drawCurve i CurveOpts{..} (BezierJoints (map piPoint -> ps@(_:_:_))) =
  lc (colourFrom curveColor) . dashPatternFrom curveStyle
  . fromLocSegments . (`at` p1) . go $ ps
  where
    p1 = fromMaybe origin $ listToMaybe ps
    go (p1 : c1 : c2 : p2 : ps) =
      bezier3 (c1 .-. p1) (c2 .-. p1) (p2 .-. p1) : go (p2:ps)
    go _ = []
drawCurve _ _ _ = mempty

 
drawTangents :: Int -> CGConfig -> Curve -> Diagram SVG R2
drawTangents i CGConfig{tangentsOptions=TanOpts {..}} (BezierJoints pts) =
  go pts
  where
    go (Through p (normalized -> t) True : pts) =
      position [(p, circle 0.05 # fc tColour)] 
      <> tangentLine (p .-^ tangentLen *^ t) (p .+^ tangentLen *^ t) tColour (dashPatternFrom tangentStyle)
      <> go pts
    go (_:pts) = go pts
    go [] = mempty
    tColour = colourFrom tangentColor
drawTangents _ _ _ = mempty

tangentLine p1 p2 c d = begArrowHead # fc c <> shaft # lc c # d <> endArrowHead # fc c
  where
    v = p2 .-. p1
    shaft     = p1 ~~ p2
    arrowHead = eqTriangle 0.1
    begArrowHead = arrowHead
                   # rotateBy (direction (-v) - 1/4)
                   # translate (p1 .-. origin)
    endArrowHead = arrowHead               
                   # rotateBy (direction v - 1/4)
                   # translate (p2 .-. origin)

colourFrom c = fromMaybe black . readColourName $ c
dashPatternFrom s = case s of
  "solid" -> dashing [] 0
  "dashed" -> dashing [0.3, 0.2] 0
  "dotted" -> dashing [0.05, 0.05] 0 . lw 0.05
  _        -> dashing [] 0
