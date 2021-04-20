module Main where

import Graphics.Gloss.Data.Color
import Data.Serialize
import Gui
import Container

main :: IO ()
main = run "Test" (640, 480)
    [ GuiObj $ Container $ Margin
        { marginChildren =
            [ GuiObj $ Container $ HBox
                { hBoxChildren = 
                    [ GuiObj $ Container $ Margin 
                        { marginChildren = []
                        , marginBg = blue
                        , margins = ((5, 5), (5, 5)) }
                    , GuiObj $ Container $ VBox 
                        { vBoxChildren =
                            [ GuiObj $ Container $ Margin
                                { marginChildren = []
                                , marginBg = yellow
                                , margins = ((5, 5), (5, 5)) }
                            , GuiObj $ Container $ Margin
                                { marginChildren = []
                                , marginBg = orange
                                , margins = ((5, 5), (5, 5)) } ]
                        , vBoxBg = red
                        , vSep = 40 } ]
                , hBoxBg = green
                , hSep = 20 } ]
        , marginBg = makeColor 0 0 0 0
        , margins = ((5, 20), (40, 60)) } ]