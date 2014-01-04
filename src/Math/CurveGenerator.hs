{-# LANGUAGE TemplateHaskell, ViewPatterns, FlexibleInstances #-}
module Math.CurveGenerator
       (CurveInput(..)
       , CurveOptions(..)
       , GridOptions(..)
       , AxisOptions(..)
       , TangentsOptions(..)
       , CGConfig(..)
       , PointInfo(..)
       , Curve(..)
       , saveConfig
       , loadConfig
       , drawingTangent
       , createCurve) where

import Diagrams.Prelude
import Diagrams.Coordinates

import Data.Maybe
import Data.SafeCopy
import Data.Default
import Data.ByteString (ByteString)
import Data.Serialize

-- All major input types and the configuration that holds them all
data CurveInput = CurvePointsAndT { looseness :: Double, pointsWithT :: [(P2, Maybe Double, Bool)] }
                  | CurveFunction { func :: String, wantTangents :: [Double] }
instance Default CurveInput where
  def = CurvePointsAndT { looseness=0.4, pointsWithT=[] }
instance SafeCopy (Point R2) where
  putCopy (coords -> x :& y) = contain $ safePut x >> safePut y
  getCopy = contain $ curry p2 <$> safeGet <*> safeGet
deriveSafeCopy 1 'base ''CurveInput

data CurveOptions = CurveOpts { curveColor :: String, curveStyle :: String }
instance Default CurveOptions where
  def = CurveOpts { curveColor="black", curveStyle="solid" } -- Allowed curveStyle : solid, dashed, dotted
deriveSafeCopy 1 'base ''CurveOptions

data GridOptions = GridOpts { dxMajor, dyMajor, dxMinor, dyMinor :: Double, majorGrid, minorGrid :: Bool }
instance Default GridOptions where
  def = GridOpts 1 1 0.2 0.2 True False
deriveSafeCopy 1 'base ''GridOptions

data AxisOptions = AxisOpts { xMin, xMax, yMin, yMax, xOrig, yOrig, xTicks, yTicks :: Double }
instance Default AxisOptions where
  def = AxisOpts { xMin=(-8), xMax=8, yMin=(-6), yMax=6, xTicks=1, yTicks=1, xOrig=0, yOrig=0 } 
deriveSafeCopy 1 'base ''AxisOptions

data TangentsOptions = TanOpts { tangentLen :: Double, tangentColor :: String, tangentStyle :: String }
instance Default TangentsOptions where
  def = TanOpts { tangentLen = 2, tangentColor = "black", tangentStyle = "solid" }
deriveSafeCopy 1 'base ''TangentsOptions

data CGConfig = CGConfig { curveInputs :: [(CurveInput, CurveOptions)]
                     , gridOptions :: GridOptions
                     , axisOptions :: AxisOptions
                     , tangentsOptions :: TangentsOptions
                     , comments :: Bool
                     }
instance Default CGConfig where
  def = CGConfig (replicate 10 (def,def)) def def def False
deriveSafeCopy 1 'base ''CGConfig

saveConfig :: CGConfig -> ByteString
saveConfig = runPut . safePut

loadConfig :: ByteString -> Either String CGConfig
loadConfig = runGet safeGet

data PointInfo = Through { piPoint :: P2, tangent :: R2, drawTangent :: Bool } | Control { piPoint :: P2 }

data Curve = BezierJoints [PointInfo]

drawingTangent :: PointInfo -> Bool
drawingTangent (Through _ _ True) = True
drawingTangent _ = False

createCurve :: CurveInput -> Curve 
createCurve (CurvePointsAndT e params@((lextr,t, b):(p2,_,_):ps)) = BezierJoints $ Through lextr dv b : Control clextr : go params
  where
    dv = computeDVector (centralSymAbout lextr p2) lextr p2 t
    clextr = lextr .+^ (e *^ dv)
    go [(pbl,_,_),(rextr,t, b)] = [Control crextr, Through rextr dv b]
      where
        dv = computeDVector pbl rextr (centralSymAbout rextr pbl) t
        crextr = rextr .-^ (e *^ dv)
    go ((p1,_,_):ps@((p2,t,b):(p3,_,_):_)) = Control lcp : Through p2 dv b : Control rcp : go ps
      where
        dv = computeDVector p1 p2 p3 t
        lcp = p2 .-^ (e *^ dv)
        rcp = p2 .+^ (e *^ dv)
createCurve _ = BezierJoints []

computeDVector :: P2 -> P2 -> P2 -> Maybe Double -> R2
computeDVector (coords -> lx :& ly) (coords -> mx :& my) (coords -> rx :& ry) givenT
  | (ly - my)*(ry - my) > 0 && isNothing givenT = x ^& 0
  | otherwise                                  = x ^& y
  where
    t = fromMaybe ((ly-ry)/(lx-rx)) givenT 
    x = min (mx - lx) (rx - mx)
    y = t*x

centralSymAbout c = rotateAbout c (1/2 :: Turn)
