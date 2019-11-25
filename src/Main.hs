module Main where

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Juicy

data PointState = PointState
  {
    x :: Float
  , y :: Float
  , angle :: Float
  , img :: Picture
  } deriving(Show, Eq)

-- 周期 s/周
period :: Float
period = 10

-- 半径
radius :: Float
radius = 250

-- 角速度
angVero :: Float
angVero = 2 * pi * 1 / period

initPointState :: IO PointState
initPointState = do
    Just img <- loadJuicy "ichiban.jpg" -- ほんとはパターンマッチしろ
    return $ PointState radius 0 0 img

drawPointState :: PointState -> Picture
drawPointState p = translate (x p) (y p) (scale 0.4 0.4 (img p))

movePoint :: ViewPort -> Float -> PointState -> PointState
movePoint view dt p = PointState (radius * cos k) (radius * sin k) k (img p)
    where
        theta = angle p + angVero * dt -- θ
        k = if theta > 2 * pi then theta - 2 * pi else theta

window :: Display
window = InWindow "Tousoku En Ichivan Shibori" (800, 640) (100, 100)

main :: IO()
main = do
    ps <- initPointState
    simulate window white 100 ps drawPointState movePoint
