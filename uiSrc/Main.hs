{-# LANGUAGE ScopedTypeVariables, RecordWildCards, NamedFieldPuns, ViewPatterns #-}
module Main (main) where

import Math.TikzGenerator
import qualified Math.SVGGenerator as S
import qualified Math.PSTricksGenerator as P


import Control.Monad (void, replicateM, liftM, liftM2, liftM3, zipWithM, zipWithM_)
import Text.Read (readMaybe)
import Data.Maybe
import Data.Ord
import Data.List
import Data.Monoid
import Data.Default
import qualified Data.ByteString as B

import qualified Data.Traversable as T
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T
import Control.Arrow
import Diagrams.Prelude (p2, P2)
import Diagrams.Coordinates
import Diagrams.TwoD.Size (mkWidth)

-- System stuff
import System.Environment (getArgs)
import System.Directory
import System.FilePath

-- Gui Stuff !!
import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
-- import Paths_curve_generation_gui
import Widgets
{-----------------------------------------------------------------------------
Enabled
------------------------------------------------------------------------------}
main :: IO ()
main = do
  [staticDirToUse] <- getArgs
  
  startGUI defaultConfig { jsPort = Just 51333, jsStatic = Just staticDirToUse } (setup staticDirToUse)

setup :: FilePath -> Window -> UI ()
setup home w = do
    debug "In the looking glass"
    return w # set title "Générateur de courbe point par point"
    UI.addStyleSheet w "style.css"
    UI.addStyleSheet w "widgets.css"

    let autosave =  home </> "autosave.session"
    b <- liftIO $ doesFileExist autosave
    config <- liftIO $ if b
                       then liftM (either (const def) id . loadConfig)
                            $ B.readFile autosave
                       else return def

    (pointss, curveTikz, svgPanel, curvePstricks, axisPanel, gridPanel, tangentPanel, restoreAll) 
    	      <- makeCurves home w config

    restoreAll config

    let curveInput =
          UI.div ## "curves" #+ [
            tabbedPanel 1270 "tabsStuff" (zipWith curveTab [1..] pointss)
            ]
        params = UI.div ## "params" #+ [
          element axisPanel
	  ,element gridPanel
          ,element tangentPanel
          ,UI.div #. "paramsPanel" ## "LaTeXOutputs" #+ [
            collapsiblePanel "tikzCode"
            UI.h3 "code Tikz :"
            [element curveTikz]
            ,collapsiblePanel "pstricksCode"
             UI.h3 "code PsTricks :"
             [element curvePstricks]
            ]
          ]
        svgOutput = element svgPanel

    getBody w #+ [UI.div ## "application" #+ [
                     UI.h1 # set text "Générateur de courbe point à point"
                     , curveInput
                     , params
                     , svgOutput
                     ]
                 ]
    return ()
    
    
makeCurves :: FilePath -> Window -> CGConfig -> UI ( [[((Element,Element),Element,Element)]], Element, Element, Element, Element, Element, Element, CGConfig -> UI () )
makeCurves home w config@CGConfig{curveInputs} = do
    xyss <- replicateM 10
             (replicateM 20 $ liftM3 (,,) (liftM2 (,) coord coord) coord checkTangent)
    xyInss <-
      fmap T.sequenceA $
        zipWithM (\(curveInput,_) xys -> fmap T.sequenceA . sequence $ pointInputs curveInput xys)
          curveInputs xyss

    (axisOptions, axisPanel, restoreAxis) <- makeAxisPanel w config
    (gridOptions, gridPanel, restoreGrid) <- makeGridPanel w config
    (tangentOptions, tangentPanel, restoreTangents) <- makeTangentPanel w config
    
    (svgURI, svgPanel, curveSvg) <- makeSvgPanel w

    let curveIns = map (CurvePointsAndT 0.4 . extractViablePoints) <$> xyInss
        configIn =
          (\curves gOpts aOpts tOpts -> CGConfig (map (id &&& const def) curves) gOpts aOpts tOpts True)
          <$> curveIns <*> gridOptions <*> axisOptions <*> tangentOptions
         
        tikzTotal = T.unpack . drawAll <$> configIn

        svgTotal = S.drawAll (mkWidth 873) <$> configIn 
        
        pstricksTotal = T.unpack . P.drawAll <$> configIn
        restoreCurves CGConfig{curveInputs} =
          zipWithM_ (\(curveInput,_) xys -> restorePoints curveInput xys) curveInputs xyss
        restoreAll config = sequence_ . map ($ config) $ [restoreTangents, restoreGrid, restoreAxis, restoreCurves]


    UI.onEvent (disconnect w)
         (const . liftIO $ B.writeFile (home </> "autosave.session") . saveConfig =<< currentValue configIn)
    

    curveTikz <- UI.textarea #. "tikzOutput" # set UI.rows "10"
                   # set UI.cols "60" # set (attr "readonly") "true"
    element curveTikz # sink value tikzTotal

    element curveSvg # sink html (T.unpack <$> svgTotal)
    onChanges svgTotal (liftIO . T.writeFile (home </> "courbe.svg"))

    curvePstricks <- UI.textarea #. "pstricksOutput" # set UI.rows "10"
                   # set UI.cols "60" # set (attr "readonly") "true"
    element curvePstricks # sink value pstricksTotal

    return (xyss, curveTikz, svgPanel, curvePstricks, axisPanel, gridPanel, tangentPanel, restoreAll)


makeAxisPanel w config@CGConfig{axisOptions=AxisOpts{..}} = do
  es@[exMin, exMax, eyMin, eyMax, exOrig, eyOrig, exTicks, eyTicks] <- replicateM 8 coord
  [ixMin, ixMax, iyMin, iyMax, ixOrig, iyOrig, ixTicks, iyTicks] <-
    zipWithM (\d e -> stepper d $ readDef d <$> UI.valueChange e)
      [xMin, xMax, yMin, yMax, xOrig, yOrig, xTicks, yTicks] es

  let axisOptions =
        axisOptsProtected <$> ixMin <*> ixMax <*> iyMin <*> iyMax <*> ixOrig <*> iyOrig <*> ixTicks <*> iyTicks
      axisOptsProtected xMin xMax yMin yMax xOrig yOrig xTicks yTicks = AxisOpts xMin xMax yMin yMax xOrig yOrig (noZero xTicks) (noZero yTicks)
      restoreAxis CGConfig{axisOptions=AxisOpts{..}} = 
        zipWithM_ (\d e -> element e # set value (show d))
          [xMin, xMax, yMin, yMax, xOrig, yOrig, xTicks, yTicks] es

  axisPanel <- collapsiblePanel "axisPanel"
                 UI.h2 "Paramètres des axes"
                 [UI.p #+ [grid [
                              [header "x Min :", element exMin, header "x Max :", element exMax]
                              ,[header "y Min :", element eyMin, header "y Max :", element eyMax]
                              ,[header "x Origine :", element exOrig, header "y Origine :", element eyOrig]
                              ]]
                  ,UI.p #+ [grid [
                               [header "Graduation sur x tous les :", element exTicks]
                               ,[header "Graduation sur y tous les :", element eyTicks]
                               ]]
                  ]
                 #. "paramsPanel"
  
  return (axisOptions, axisPanel, restoreAxis)

makeGridPanel w config@CGConfig{gridOptions=GridOpts{..}}= do
  es@[exMajor, eyMajor, exMinor, eyMinor, eMajorGrid, eMinorGrid] <-
    zipWithM (#) (replicate 6 UI.input) $ replicate 4 (set UI.class_ "coords") ++ replicate 2 (set UI.type_ "checkbox")
  [ixMajor, iyMajor, ixMinor, iyMinor] <-
    zipWithM (\d e -> stepper d $ noZero . readDef d <$> UI.valueChange e)
      [dxMajor, dyMajor, dxMinor, dyMinor] $ take 4 es
  [iMajorGrid, iMinorGrid] <-
    zipWithM (\d e -> stepper d $ UI.checkedChange e) [majorGrid, minorGrid] $ drop 4 es

  let gridOptions =
        GridOpts <$> ixMajor <*> iyMajor <*> ixMinor <*> iyMinor <*> iMajorGrid <*> iMinorGrid
      restoreGrid CGConfig{gridOptions=GridOpts{..}} = do
        zipWithM_ (\d e -> element e # set value (show d))
          [dxMajor, dyMajor, dxMinor, dyMinor] $ take 4 es
        zipWithM_ (\d e -> element e # set UI.checked d)
          [majorGrid, minorGrid] $ drop 4 es

  gridPanel <- collapsiblePanel "gridPanel"
                 UI.h2 "Paramètres du quadrillage"
                 [UI.p #+ [grid [
                              [header "dx quadrillage :", element exMajor
                              , header "dy quadrillage :", element eyMajor]
                              ,[header "dx sous-quadrillage :", element exMinor
                               , header "dy sous-quadrillage :", element eyMinor]
                              ]
                          ]
                  ,UI.p #+ [grid [
                               [header "Afficher le quadrillage", element eMajorGrid]
                               ,[header "Afficher le sous-quadrillage:", element eMinorGrid]
                               ]]
                  ]
                 #. "paramsPanel"
  
  return (gridOptions, gridPanel, restoreGrid)


makeTangentPanel w config@CGConfig{tangentsOptions=TanOpts{..}} = do
  elen <- coord
  ecolor <- UI.input #. "linecolor"
  estyle <- UI.input #. "linestyle"
  ilen <- stepper tangentLen $  readDef tangentLen <$> UI.valueChange elen
  icolor <- stepper tangentColor $ UI.valueChange ecolor
  istyle <- stepper tangentStyle $ UI.valueChange estyle

  let tangentOptions = TanOpts <$> ilen <*> icolor <*> istyle
      restoreTangents CGConfig{tangentsOptions=TanOpts{..}} = do
        element elen # set value (show tangentLen)
        element ecolor # set value tangentColor
        void $ element estyle # set value tangentStyle

  tangentPanel <- collapsiblePanel "tangentPanel"
                    UI.h2 "Paramètres des tangentes"
                    [grid [
                        [header "Longueur d'une demi-tangente :", element elen]
                        ,[header "Couleur :", element ecolor]
                        ,[header "Type de ligne :", element estyle]
                        ]
                    ]
                    #. "paramsPanel"
  
  return (tangentOptions, tangentPanel, restoreTangents)

makeSvgPanel w = do
  curveSvg <- UI.span  #. "svgOutput"
  -- uriSvg <- loadFile "image/svg+xml" "courbe.svg"
  let uriSvg = "/static/courbe.svg"

  svgPanel <- UI.div ## "svgOutput" #+ [UI.a #. "svgLink" # set UI.href uriSvg  # set UI.target "_blank"
                                        #+ [element curveSvg]
                                     ]
  return (uriSvg, svgPanel, curveSvg)

extractViablePoints :: [((String,String),String,Bool)] -> [(P2 Double, Maybe Double, Bool)]
extractViablePoints = sort . map (\(mp,t,b) -> (p2 (fromJust mp), t, b)) . filter (isJust.fst3) . map handlePoint
  where
    handlePoint ((xstr, ystr),tstr, b) =
      ((,) <$> readMaybe xstr <*> readMaybe ystr, readMaybe tstr, b)


curveTab i points =
  (UI.h4 # set text ("Courbe n°" ++ show i)
   ,[
      UI.div #. "leftPanel" #+ [
               UI.h3 # set text "Tableau des points"
               ,UI.p # set text "Les points peuvent être entrés en désordre."
               ]
      ,UI.div #. "rightPanel" #+ [(grid . transpose $
                                 [header "#", header "x", header "y", header "y'", header "Tracer la tangente ?"]
                                 : zipWith arrangePoints [1..]  points) #. "tableDiv"
                               ]
      ]    
   )

arrangePoints i ((x,y),t,b) =
  [string (show i) #. "vheader", element x, element y, element t, element b]


-- input Points
pointInputs (CurvePointsAndT _ pts) xytbs = go pts xytbs
  where
    go _  [] = []
    go [] (xytb:xytbs) = pointInput xytb : go [] xytbs
    go (ptb:ptbs) (xytb:xytbs) = pointInputted ptb xytb : go ptbs xytbs
pointsInputs _ xytbs = map pointInput xytbs

pointInputted (coords -> x' :& y',t',b') ((x,y),t,b) =
  liftM3 (liftA3 (,,))
    (liftM2 (liftA2 (,))
     (stepper (show x') $ UI.valueChange x)
     (stepper (show y') $ UI.valueChange y)
    )
    (stepper (maybe "" show t') $ UI.valueChange t)
    (stepper b' $ UI.checkedChange b)
pointInput ((x,y),t,b) =
  liftM3 (liftA3 (,,))
    (liftM2 (liftA2 (,))
     (stepper "" $ UI.valueChange x)
     (stepper "" $ UI.valueChange y)
    )
    (stepper "" $ UI.valueChange t)
    (stepper False $ UI.checkedChange b)
-------------------------

-- restoring Points
restorePoints (CurvePointsAndT _ pts) xytbs = go pts xytbs
  where
    go _ [] = return ()
    go [] (xytb:xytbs) = erasePoint xytb >> go [] xytbs
    go (ptb:ptbs) (xytb:xytbs) = restorePoint ptb xytb >> go ptbs xytbs
restorePoints _ xytbs = mapM_ erasePoint xytbs

restorePoint (coords -> x' :& y',t',b') ((x,y),t,b) = do
  element x # set value (show x')
  element y # set value (show y')
  case t' of
    Nothing -> element t # set value ""
    Just jt' -> element t # set value (show jt')
  element b # set UI.checked b'
erasePoint ((x,y),t,b) = do
  mapM_ (set value "" . element) [x,y,t]
  void $ element b # set UI.checked False
-------------------------


readDef def = fromMaybe def . readMaybe

coord = UI.input #. "coords"
checkTangent = UI.input # set UI.type_ "checkbox" #. "coords"
header str = string str #. "header"

fst3 (a,_,_) = a
snd3 (_,b,_) = b
thrd3 (_,_,c) = c
noZero n
  | n >= -0.01 && n <= 0.01 = 1
noZero n = n
