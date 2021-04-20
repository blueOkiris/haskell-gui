{-# LANGUAGE ExistentialQuantification #-}
module Gui(GuiObj_(..), GuiObj(..), run) where
    
import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Interface.Environment
import GHC.Float.RealFracMethods

class GuiObj_ a where
    objDraw     :: a -> (Int, Int) -> (Int, Int) -> Picture
    objUpdate   :: a -> Float -> [GuiObj] -> [GuiObj]
    objHandle   :: a -> Event -> [GuiObj] -> [GuiObj]
    objMinSize  :: a -> (Int, Int)
data GuiObj = forall a . GuiObj_ a => GuiObj a
instance GuiObj_ GuiObj where
    objDraw (GuiObj a) = objDraw a
    objUpdate (GuiObj a) = objUpdate a
    objHandle (GuiObj a) = objHandle a
    objMinSize (GuiObj a) = objMinSize a

foreach :: Int -> [a] -> (a -> [a] -> [a]) -> [a]
foreach index curr transform
    | index >= length curr = curr
    | otherwise = foreach (index + 1) newCurr transform
    where
        newCurr = transform (curr !! index) curr

winDraw :: ((Int, Int), [GuiObj]) -> IO Picture
winDraw (size, objs) = do
    let image = Pictures $ map (\obj -> objDraw obj (0, 0) size) objs
    let shiftX = int2Float $ fst size `div` 2
    let shiftY = int2Float $ snd size `div` 2
    return $ translate (-shiftX) shiftY image

winUpdate :: Float -> ((Int, Int), [GuiObj]) -> IO ((Int, Int), [GuiObj])
winUpdate delta (size, objs) =
    return (size, foreach 0 objs (`objUpdate` delta))

winHandle :: Event -> ((Int, Int), [GuiObj]) -> IO ((Int, Int), [GuiObj])
winHandle (EventResize newSize) (_, objs) =
    return (newSize, objs)
winHandle event (size, objs) =
    return (size, foreach 0 objs (`objHandle` event))

run :: String -> (Int, Int) -> [GuiObj] -> IO ()
run title defSize guiObjs = do
    screenSize <- getScreenSize
    let startX = (fst screenSize `div` 2) - (fst defSize `div` 2)
    let startY = (snd screenSize `div` 2) - (snd defSize `div` 2)
    let win = InWindow title defSize (startX, startY)
    
    let startState = (defSize, guiObjs)
    playIO win black 100 startState winDraw winHandle winUpdate
