{-# LANGUAGE OverloadedStrings, ViewPatterns, RecordWildCards, NamedFieldPuns #-}

module Math.TikzGenerator (module Math.CurveGenerator
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
import Diagrams.Prelude (P2, (.-^), (.+^))
import Data.VectorSpace


drawAll :: CGConfig -> Text
drawAll config@CGConfig{axisOptions=AxisOpts{..}, comments, curveInputs=cs} =
  toLazyText $
  "\\begin{tikzpicture}[x=1cm,y=1cm]\n\n"
  <> mWhen comments "% Limite le dessin au repère\n"
  <> "\\clip " <> point (xMin ^& yMin) <> " rectangle " <> point (xMax ^& yMax) <> ";\n\n"
  <> drawGrid config <> drawAxis config
  <> mconcat (zipWith drawNCT [1..] cs)
  <> "\\end{tikzpicture}\n"
  where drawNCT i (createCurve -> curve, curveConfig) =
          drawCurve i config curveConfig curve <> drawTangents i config curve

drawAxis :: CGConfig -> Builder
drawAxis CGConfig{axisOptions=AxisOpts {..}, comments} =
  mWhen comments "% Axes du repère\n"
  <> "\\draw [->] " <> point (xMin ^& yOrig) <> " -- " <> point (xMax ^& yOrig) <> ";\n"
  <> "\\draw [->] " <> point (xOrig ^& yMin) <> " -- " <> point (xOrig ^& yMax) <> ";\n"
  <> "\\foreach \\x in {"
  <> showListF 2 (filter (inRange xMin xMax) $ [xOrig + xTicks, xOrig + 2*xTicks..xMax]
                  ++ [xOrig - xTicks, xOrig - 2*xTicks..xMin]) <> "}\n"
  <> "    \\draw[shift={(\\x," <> showF 3 yOrig <> ")}] (0,0.2) -- (0,-0.2) node[below,fill=white] {$\\x$};\n"
  <> "\\foreach \\y in {"
  <> showListF 2 (filter (inRange yMin yMax) $ [yOrig + yTicks, yOrig + 2*yTicks..yMax]
                  ++ [yOrig - yTicks, yOrig - 2*yTicks..yMin]) <> "}\n"
  <> "    \\draw[shift={(" <> showF 3 xOrig <> ",\\y)}] (0.2,0) -- (-0.2,0) node[left,fill=white] {$\\y$};\n\n"

drawGrid :: CGConfig -> Builder
drawGrid CGConfig{axisOptions=AxisOpts{..}, gridOptions=GridOpts{..}, comments} =
  mWhen minorGrid minor
  <> mWhen majorGrid major
  <> "\n"
  where
    minor =
      mWhen comments ("% Sous grille\n")
      <> "\\draw[xstep=" <> showF 3 dxMinor <> ", ystep=" <> showF 3 dyMinor
      <> ",help lines, ultra thin,shift={" <> point pOrig <> "}] "
      <> point (pMin .-^ vOrig) <> " grid" <> point (pMax .-^ vOrig) <> ";\n"
    major =
      mWhen comments ("% Grille du repère\n")
      <> "\\draw[xstep=" <> showF 3 dxMajor <> ", ystep=" <> showF 3 dyMajor
      <> ",help lines,shift={" <> point pOrig <> "}] " <> point (pMin .-^ vOrig) <> " grid" <> point (pMax .-^ vOrig) <> ";\n"
    pOrig = xOrig ^& yOrig
    vOrig = xOrig ^& yOrig
    pMin = xMin ^& yMin
    pMax = xMax ^& yMax
      
drawCurve :: Int -> CGConfig -> CurveOptions -> Curve -> Builder
drawCurve i CGConfig{comments} CurveOpts{..} (BezierJoints (map piPoint -> p:c:ps)) =
  mWhen comments ("% Courbe n°" <> decimal i <> "\n")
  <> "\\draw[" <> fromString curveColor <> ", " <> fromString curveStyle <> "] "
  <> point p <> " .. controls " <> point c <> go ps
  where
    go [c,p] = " and " <> point c <> " .. " <> point p <> ";\n\n"
    go (lc:p:rc:ps) = " and " <> point lc <> " .. " <> point p <> " .. controls " <> point rc <> go ps
drawCurve _ _ _ _ = mempty
 
drawTangents :: Int -> CGConfig -> Curve -> Builder
drawTangents i CGConfig{tangentsOptions=TanOpts {..}, comments} (BezierJoints pts) = go pts
  where
    go (Through p (normalized -> t) True : pts) =
      mWhen comments ("% Tangente en " <> point p <> " à la courbe n°" <> decimal i <> "\n")
      <> "\\draw [<->, " <> fromString tangentColor <> ", " <> fromString tangentStyle
      <> "] " <> point (p .-^ tangentLen *^ t) <> " -- " <> point (p .+^ tangentLen *^ t) <> ";\n"
      <> "\\fill [" <> fromString tangentColor  <> "] " <> point p <> " circle[radius=1.5pt];\n" <> go pts
    go (_ : pts) = go pts
    go _ = mempty
drawTangents _ _ _ = mempty

