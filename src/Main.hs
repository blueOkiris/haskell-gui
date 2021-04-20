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
                        , margins = ((5, 5), (5, 5))
                        , margMinSize = (100, 100)
                        , marginBorder = (5, black) }
                    , GuiObj $ Container $ VBox 
                        { vBoxChildren =
                            [ GuiObj $ Container $ Margin
                                { marginChildren = []
                                , marginBg = yellow
                                , margins = ((5, 5), (5, 5))
                                , margMinSize = (100, 100)
                                , marginBorder = (2, magenta) }
                            , GuiObj $ Container $ Margin
                                { marginChildren = []
                                , marginBg = orange
                                , margins = ((5, 5), (5, 5))
                                , margMinSize = (100, 100)
                                , marginBorder = (1, black) } ]
                        , vBoxBg = red
                        , vSep = 40
                        , vBoxMinSize = (100, 100)
                        , vBoxBorder = (1, black) } ]
                , hBoxBg = green
                , hSep = 20
                , hBoxMinSize = (100, 100)
                , hBoxBorder = (1, black) } ]
        , marginBg = makeColor 0 0 0 0
        , margins = ((5, 20), (40, 60)) 
        , margMinSize = (100, 100)
        , marginBorder = (1, black) } ]
