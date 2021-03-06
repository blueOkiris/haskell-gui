{-# LANGUAGE ExistentialQuantification #-}
module Container(Container(..), Margin(..), HBox(..), VBox(..)) where

import Graphics.Gloss.Interface.IO.Game
import GHC.Float.RealFracMethods
import Gui

subForeach :: [a] -> [a] -> (a -> [a] -> [a]) -> [a]
subForeach objs curr transform
    | null objs = curr
    | otherwise = subForeach (tail objs) newCurr transform
    where
        newCurr = transform (head objs) curr

-- Indexed version of map
mapi :: Int -> (a -> Int -> a) -> [a] -> [a]
mapi ind transform list
    | ind >= length list = []
    | otherwise =
        transform (list !! ind) ind : mapi (ind + 1) transform list

class Container_ a where
    contChildren        :: a -> [GuiObj]
    contBg              :: a -> Color
    contBorder          :: a -> (Int, Color)
    contShape           ::
        a -> (Int, Int) -> (Int, Int) -> ((Int, Int), (Int, Int))
    contChildrenShapes  ::
        a -> (Int, Int) -> (Int, Int) -> [((Int, Int), (Int, Int))]
    contMinSize         :: a -> (Int, Int)
data Container = forall a . Container_ a => Container a
instance Container_ Container where
    contChildren (Container a) = contChildren a
    contBg (Container a) = contBg a
    contBorder (Container a) = contBorder a
    contShape (Container a) = contShape a
    contChildrenShapes (Container a) = contChildrenShapes a
    contMinSize (Container a) = contMinSize a

instance GuiObj_ Container where
    objDraw cont pos size = do
        -- Draw a border rectangle where we are and a fill inside it
        let borderColor = snd $ contBorder cont
        let borderSize = int2Float $ fst $ contBorder cont
        let selfShape = contShape cont pos size
        let width = int2Float $ fst $ snd selfShape
        let innerWidth = width - 2 * borderSize
        let height = int2Float $ snd $ snd selfShape
        let innerHeight = height - 2 * borderSize
        let x = int2Float (fst $ fst selfShape) + (width / 2)
        let y = int2Float (snd $ fst selfShape) + (height / 2)
        let innerX =
                int2Float (fst $ fst selfShape) + (innerWidth / 2) + borderSize
        let innerY =
                int2Float (snd $ fst selfShape) + (innerHeight / 2) + borderSize
        let bgColor = contBg cont
        let selfPic = Pictures
                [ translate x (-y) $ color borderColor $
                    rectangleSolid width height
                , translate innerX (-innerY) $
                    color bgColor $
                        rectangleSolid innerWidth innerHeight ]

        -- Call draw with all the children
        let children = contChildren cont
        let childrenPosSizes = contChildrenShapes cont pos size
        let childrenAndSizes = zip children childrenPosSizes
        let drawChild = \(obj, (pos, size)) -> objDraw (obj :: GuiObj) pos size
        let childrenPics = map drawChild childrenAndSizes

        -- Combine all the images
        Pictures $ selfPic : childrenPics

    objUpdate cont delta objs = do
        -- Call children's updates on top-level objs
        let children = contChildren cont
        let updateFunc = (`objUpdate` delta)
        subForeach children objs updateFunc

    objHandle cont event objs = do
        -- Call children's events on top-level objs
        let children = contChildren cont
        let handleFunc = (`objHandle` event)
        subForeach children objs handleFunc
    
    objMinSize = contMinSize

data Margin = Margin
    { marginChildren    :: [GuiObj]
    , marginBg          :: Color
    , marginBorder      :: (Int, Color)
    , margins           :: ((Int, Int), (Int, Int))
    , margMinSize       :: (Int, Int) }
instance Container_ Margin where
    contChildren = marginChildren
    contBg = marginBg
    contBorder = marginBorder
    contShape marg (x, y) (w, h) = do
        let mX = fst $ fst $ margins marg
        let mY = snd $ fst $ margins marg
        let mW = fst $ snd $ margins marg
        let mH = snd $ snd $ margins marg
        ((x + mX, y + mY), (w - mX - mW, h - mY - mH))
    contChildrenShapes marg pos size = do
        map (const $ contShape marg pos size) (marginChildren marg)
    contMinSize = margMinSize

data HBox = HBox
    { hBoxChildren      :: [GuiObj]
    , hBoxBg            :: Color
    , hBoxBorder        :: (Int, Color)
    , hSep              :: Int
    , hBoxMinSize       :: (Int, Int) }
instance Container_ HBox where
    contChildren = hBoxChildren
    contBg = hBoxBg
    contBorder = hBoxBorder
    contShape hBox (x, y) (w, h) = ((x, y), (w, h))
    contChildrenShapes hBox (x, y) (w, h) = do
        let childrenLen = length $ hBoxChildren hBox
        let sizeArr = replicate childrenLen ((0, 0), (0, 0))
        let width = w `div` childrenLen
        let transform = \def ind -> ((x + ind * width, y), (width, h))
        mapi 0 transform sizeArr
    contMinSize = hBoxMinSize

data VBox = VBox
    { vBoxChildren      :: [GuiObj]
    , vBoxBg            :: Color
    , vBoxBorder        :: (Int, Color)
    , vSep              :: Int
    , vBoxMinSize       :: (Int, Int) }
instance Container_ VBox where
    contChildren = vBoxChildren
    contBg = vBoxBg
    contBorder = vBoxBorder
    contShape _ pos size = (pos, size)
    contChildrenShapes vBox (x, y) (w, h) = do
        let childrenLen = length $ vBoxChildren vBox
        let sizeArr = replicate childrenLen ((0, 0), (0, 0))
        let height = h `div` childrenLen
        let transform = \def ind -> ((x, y + ind * height), (w, height))
        mapi 0 transform sizeArr
    contMinSize = vBoxMinSize
