module Main where
import Network.MateLight.Simple

import Data.Maybe
import qualified Network.Socket as Sock



move :: (Int, Int) -> String -> (Int, Int) -> (Int, Int)
move (xdim, ydim) "\"s\"" (x, y) = if (y /= 10) then (x, (y + 4)) else (x, y)
move (xdim, ydim) "\"w\"" (x, y) = if (y /= 2) then (x, (y - 4)) else (x, y)
move _ _ x = x

myCar :: (Int, Int) -> [(Int, Int)]
myCar (x, y) = [(x, y + 1), (x, y), (x, y-1), (x + 1, y), (x + 2, y), (x + 3, y + 1), (x + 3, y - 1), (x + 3, y), (x + 4, y)]

toFrame :: (Int, Int) -> (Int, Int) -> [(Int, Int)] -> Int -> ListFrame
toFrame (xdim, ydim) (x', y') xs c = ListFrame $ map (\y -> map (\x -> draw x y xs c) [0 .. xdim - 1]) [0 .. ydim - 1]

draw :: Int -> Int -> [(Int, Int)] -> Int -> Pixel
draw x y xs current | isInMyCar x y xs        = Pixel 0xff 0xc0 0xcb
            | isInStreet x y current  = Pixel 0xff 0xff 0xff
            | otherwise               = Pixel 0 0 0

isInStreet :: Int -> Int -> Int -> Bool
isInStreet x y z | z > 20 && (y == 4 || y == 8) && x /= 4 && x /= 9 && x /= 14 && x /= 19 && x /= 24 && x /= 29 = True
                 | z > 15 && z < 20 && (y == 4 || y == 8) && x /= 3 && x /= 8 && x /= 13 && x /= 18 && x /= 23 && x /= 28 = True
                 | z > 10 && z < 15 && (y == 4 || y == 8) && x /= 2 && x /= 7 && x /= 12 && x /= 17 && x /= 22 && x /= 27 = True
                 | z > 5  && z < 10 && (y == 4 || y == 8) && x /= 1 && x /= 6 && x /= 11 && x /= 16 && x /= 21 && x /= 26 = True
                 | z > 0  && z < 5 && (y == 4 || y == 8) && x /= 0 && x /= 5 && x /= 10 && x /= 15 && x /= 20 && x /= 25 = True
                 | otherwise = False

isInMyCar :: Int -> Int -> [(Int, Int)] -> Bool
isInMyCar a b (x:xs)  | a == fst x && b == snd x  = True
                      | xs == []                 = False
                      | otherwise                = isInMyCar a b xs

lanes :: Int -> Int -> Pixel
lanes x y |y == 4 || y == 8 = Pixel 0xFF 0xFF 0xFF
          | otherwise = Pixel 0 0 0

{-
eventTest :: [Event String] -> (Int, Int) -> (ListFrame, (Int, Int))
eventTest events pixel = (toFrame dim pixel' (myCar pixel') current', pixel')
  where 
      pixel'  = foldl (\acc (Event mod ev) -> if mod == "KEYBOARD" then move dim ev acc else acc) pixel events
-}

newFrame :: [Event String] -> ((Int, Int), Int) -> (ListFrame, ((Int, Int), Int))
newFrame events (playerPosition, tick) = (toFrame dim playerPosition' (myCar playerPosition') tick, (playerPosition', newTick))
                                        where
                                            playerPosition' = foldl (\acc (Event mod ev) -> if mod == "KEYBOARD" then move dim ev acc else acc) playerPosition events
                                            newTick | tick == 0 = 25
                                                    | otherwise = tick - 1

dim :: (Int, Int)
dim = (30, 12)
main :: IO ()
main = Sock.withSocketsDo $ runMate (Config (fromJust $ parseAddress "134.28.70.172") 1337 dim (Just 33000) False []) newFrame ((1, 6), 25)
