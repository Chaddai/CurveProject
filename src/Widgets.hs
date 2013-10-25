module Widgets where

import Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny as UI

import Control.Applicative
import Control.Monad (void)

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
    ,element collapsingSection # set UI.id_ ident #+ contents
    ]
  
