{-# LANGUAGE ExistentialQuantification #-}
module Widget(Widget(..)) where

import Graphics.Gloss.Interface.IO.Game
import Gui

class Widget_ a where
    widgMinSize :: a -> (Int, Int)
    widgUpdate  :: a -> Float -> [GuiObj] -> [GuiObj]
    widgDraw    :: a -> (Int, Int) -> (Int, Int) -> Picture
    widgHandle  :: a -> Event -> [GuiObj] -> [GuiObj]
data Widget = forall a . Widget_ a => Widget a
instance Widget_ Widget where
    widgMinSize (Widget a) = widgMinSize a
    widgUpdate (Widget a) = widgUpdate a
    widgDraw (Widget a) = widgDraw a
    widgHandle (Widget a) = widgHandle a

instance GuiObj_ Widget where
    objUpdate = widgUpdate
    objDraw = widgDraw
    objHandle = widgHandle
    objMinSize = widgMinSize

data Button = Button
    { btnOnClick        :: [GuiObj] -> [GuiObj]
    , btnNormalColor    :: Color
    , btnHoverColor     :: Color
    , btnClickColor     :: Color }
