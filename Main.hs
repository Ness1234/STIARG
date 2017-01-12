module Main where
import Network.MateLight.Simple

import Data.Maybe
import qualified Network.Socket as Sock



move :: (Int, Int) -> String -> (Int, Int) -> (Int, Int)
move (xdim, ydim) "\"s\"" (x, y) = if (y /= 10) then (x, (y + 4)) else (x, y)
move (xdim, ydim) "\"w\"" (x, y) = if (y /= 2) then (x, (y - 4)) else (x, y)
move _ _ x = x

myCar :: (Int, Int) -> [(Int, Int)]
myCar (x, y) = [(x, y+1), (x, y-1), (x + 1, y), (x + 2, y), (x + 3, y + 1), (x + 3, y - 1), (x + 3, y), (x + 4, y)]

toFrame :: (Int, Int) -> (Int, Int) -> ListFrame
toFrame (xdim, ydim) (x', y') = ListFrame $ map (\y -> map (\x -> if x == x' && y == y' then Pixel 0xff 0xc0 0xcb else Pixel 0 0 0) [0 .. xdim - 1]) [0 .. ydim - 1]

eventTest :: [Event String] -> (Int, Int) -> (ListFrame, (Int, Int))
eventTest events pixel = (toFrame dim pixel', pixel')
  where pixel' = foldl (\acc (Event mod ev) -> if mod == "KEYBOARD" then move dim ev acc else acc) pixel events

dim :: (Int, Int)
dim = (30, 12)
main :: IO ()
main = Sock.withSocketsDo $ runMate (Config (fromJust $ parseAddress "134.28.70.172") 1337 dim (Just 500000) False []) eventTest [(1, 6)]
