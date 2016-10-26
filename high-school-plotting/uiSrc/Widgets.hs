{-# LANGUAGE QuasiQuotes #-}

module Widgets where

import Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny as UI

import Control.Monad (mapM, zipWithM_)

import qualified NeatInterpolation as N
import qualified Data.Text as T

collapsiblePanel :: String -> UI Element -> String -> [UI Element] -> UI Element
collapsiblePanel ident titleLevel title contents = do
  toggle <- string "+" #. "toggleCollapse"
  collapsingSection <- UI.div #. "collapsible" # set style [("display","none")]
  flipFlop <- UI.accumE True (not <$ UI.click toggle)
  onEvent flipFlop $ \showing ->
    if showing
      then do
        element collapsingSection # set style [("display", "none")]
        element toggle # set text "+"
      else do
        element collapsingSection # set style [("display", "block")]
        element toggle # set text "-"
  UI.div #+ [
    titleLevel #+ [element toggle, string title]
    ,element collapsingSection ## ident #+ contents
    ]

tabbedPanel :: Int -> String -> [(UI Element,[UI Element])] -> UI Element
tabbedPanel width ident tabs = do
  let (titles, contents) = unzip tabs
      tabCount = length tabs
      -- 6px for paddings, 2px for margins, 2px for borders, 2 more for spaces ?
      tabWidth = (width `div` tabCount) - 12
  container <- UI.div #. "tabbedContainer" ## ident
  activeTitles <-
    mapM (\t -> UI.li #. "tab" # set UI.style [("width",show tabWidth  ++ "px")] #+ [t]) titles
  activeContents <-
    mapM (\c -> UI.div #. "tabContent" #+ c) contents

  let changingTab i = do
        element (activeTitles !! i) # set UI.class_ "tab activeTab"
        mapM (set UI.class_ "tab inactiveTab" . element) $ deleteIndex i activeTitles
        element (activeContents !! i) # set style [("display", "block")]
        mapM (set style [("display", "none")] . element) $ deleteIndex i activeContents
        return ()

  zipWithM_ (\i t -> on UI.click t $ \_ -> changingTab i) [0..] activeTitles
  changingTab 0

  element container #+ (
    (UI.olist #. "tabs" #+ map element activeTitles)
      : map element activeContents
    )

deleteIndex :: Int -> [a] -> [a]
deleteIndex _ [] = []
deleteIndex 0 (_:xs) = xs
deleteIndex n xs'@(x:xs)
  | n < 0 = xs'
  | otherwise = x : deleteIndex (n-1) xs

infixl 8 ##
(##) :: UI Element -> String -> UI Element
e ## ident = e # set UI.id_ ident


uploadButton :: UI (Element, (String -> UI a) -> UI ())
uploadButton = do
  fileIn <- UI.input # set UI.id_ "file" # set UI.type_ "file"
  bIn <- stepper "Starting from NULL" $ UI.valueChange fileIn

  let installHandler h = do
        w <- UI.askWindow
        handler <- UI.ffiExport (\s -> runUI w $ do
                                    h s
                                    return ()
                                )
        UI.runFunction (ffi onloadJS handler)
  return (fileIn, installHandler)

onloadJS = T.unpack [N.text|
                    document.querySelector('#file').addEventListener('change', function() {
                      var reader = new FileReader();
                      reader.addEventListener('load', function() {
                                                 %1(reader.result)
                      });
                      reader.readAsBinaryString(this.files[0]);
                   });|]

