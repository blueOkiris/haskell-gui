{-# LANGUAGE ExistentialQuantification #-}
module Widget(Widget(..), Button(..), ButtonState(..)) where

import Graphics.Gloss.Interface.IO.Game
import GHC.Float.RealFracMethods
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

data ButtonState = ButtonNormal | ButtonHover | ButtonClick deriving(Eq)
data Button = Button
    { btnOnClick        :: [GuiObj] -> [GuiObj]
    , btnText           :: String
    , btnBorder         :: (Int, Color)
    , btnState          :: ButtonState
    , btnNormalColor    :: Color
    , btnHoverColor     :: Color
    , btnClickColor     :: Color
    , btnMinSize        :: (Int, Int) }
instance Widget_ Button where
    widgMinSize = btnMinSize
    widgUpdate _ _ objs = objs
    widgDraw btn (x, y) (w, h) = do
        let borderRect =
                translate
                (int2Float x + (int2Float w / 2))
                (int2Float y + (int2Float h / 2)) $
                    color (snd $ btnBorder btn) $
                        rectangleSolid (int2Float w) (int2Float h)
        let borderSize = int2Float $ fst $ btnBorder btn
        let fillColor
              | btnState btn == ButtonNormal = btnNormalColor btn
              | btnState btn == ButtonHover = btnHoverColor btn
              | btnState btn == ButtonClick = btnClickColor btn
              | otherwise =  black
        let innerRect =
                translate
                (int2Float x + borderSize + (int2Float w - 2 * borderSize) / 2)
                (int2Float y + borderSize
                        + (int2Float h - 2 * borderSize) / 2) $
                    color fillColor $
                        rectangleSolid 
                            (int2Float w - 2 * borderSize)
                            (int2Float h - 2 * borderSize)
        Pictures [ borderRect, innerRect ]
    widgHandle btn (EventMotion (x, y)) objs =
        objs
