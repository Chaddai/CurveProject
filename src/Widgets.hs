module Widgets where

import Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny as UI

import Control.Applicative
import Control.Monad (void, mapM, zipWithM_)

collapsiblePanel :: String -> IO Element -> String -> [IO Element] -> IO Element
collapsiblePanel ident titleLevel title contents = do
  toggle <- string "+" #. "toggleCollapse"
  collapsingSection <- UI.div #. "collapsible" # set style [("display","none")]
  flipFlop <- UI.accumE True (not <$ UI.click toggle)
  void $ register flipFlop $ \showing -> void $ do
    stateToggle <- toggle # get text
    case showing of
      False -> do
        element collapsingSection # set style [("display", "block")]
        element toggle # set text "-"
      True -> do
        element collapsingSection # set style [("display", "none")]
        element toggle # set text "+"
  UI.div #+ [
    titleLevel #+ [element toggle, string title]
    ,element collapsingSection ## ident #+ contents
    ]
  
tabbedPanel :: Int -> String -> [(IO Element,[IO Element])] -> IO Element
tabbedPanel width ident tabs = do
  let (titles, contents) = unzip tabs
      tabCount = length tabs
      -- 6px for paddings, 2px for margins, 2px for borders, 2 more for spaces ?
      tabWidth = ((width `div` tabCount) - 12) 
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

deleteIndex _ [] = []
deleteIndex 0 (x:xs) = xs
deleteIndex n xs'@(x:xs)
  | n < 0 = xs'
  | otherwise = x : deleteIndex (n-1) xs

infixl 8 ##
e ## ident = e # set UI.id_ ident
