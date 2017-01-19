module Main where
import Network.MateLight.Simple
import Data.Maybe
import qualified Network.Socket as Sock
import System.Random

type Player = (Int, Int)
type Enemies = [(Int, Int)]
type Tick = Int

data State = State Player Enemies Tick

move :: (Int, Int) -> String -> (Int, Int) -> (Int, Int)
move (xdim, ydim) "\"s\"" (x, y) = if (y /= 10) then (x, (y + 4)) else (x, y)
move (xdim, ydim) "\"w\"" (x, y) = if (y /= 2) then (x, (y - 4)) else (x, y)
move _ _ x = x

moveEnemy :: (Int, Int) -> (Int, Int)
moveEnemy (x, y) = (x - 1, y)

myCar :: (Int, Int) -> [(Int, Int)]
myCar (x, y) = [(x, y), (x + 1, y), (x + 2, y), (x + 3, y)]

otherCar :: (Int, Int) -> [(Int, Int)]
otherCar (x, y) = [(x, y), (x + 1, y), (x + 2, y), (x + 3, y)]

myTire :: (Int, Int) -> [(Int, Int)]
myTire (x, y) = [(x, y + 1), (x, y - 1), (x + 2, y + 1), (x + 2, y - 1)]

otherTire :: (Int, Int) -> [(Int, Int)]
otherTire (x, y) = [(x + 1, y + 1), (x + 1, y - 1), (x + 3, y + 1), (x + 3, y - 1)]

toFrame :: (Int, Int) -> (Int, Int) -> [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)] -> Int -> ListFrame
toFrame (xdim, ydim) (x', y') xs ys zs as c = ListFrame $ map (\y -> map (\x -> draw x y xs ys zs as c) [0 .. xdim - 1]) [0 .. ydim - 1]

draw :: Int -> Int -> [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)] -> Int -> Pixel
draw x y xs ys zs as current | isInCar x y xs          = Pixel 0xff 0x5a 0
                             | isInCar x y zs          = Pixel 0xff 0 0xff
                             | isInStreet x y current  = Pixel 0xff 0xff 0xff
                             | isInTire x y ys         = Pixel 0 0 0
                             | isInTire x y as         = Pixel 0 0 0
                             | isOnMyGrass y           = Pixel 0x00 0xaa 0x00
                             | otherwise               = Pixel 0x33 0x33 0x33

isInStreet :: Int -> Int -> Int -> Bool
isInStreet x y z | z > 15 &&           (y == 4 || y == 8) && x /= 4 && x /= 5 && x /= 9 && x /= 10 && x /= 14 && x /= 15 && x /= 19 && x /= 20 && x /= 24 && x /= 25 && x /= 29 && x /= 30 = True
                 | z > 11 && z < 16 && (y == 4 || y == 8) && x /= 3 && x /= 4 && x /= 8 && x /=  9 && x /= 13 && x /= 14 && x /= 18 && x /= 19 && x /= 23 && x /= 24 && x /= 28 && x /= 29 = True
                 | z > 7  && z < 12 && (y == 4 || y == 8) && x /= 2 && x /= 3 && x /= 7 && x /=  8 && x /= 12 && x /= 13 && x /= 17 && x /= 18 && x /= 22 && x /= 23 && x /= 27 && x /= 28 = True
                 | z > 3  && z < 8  && (y == 4 || y == 8) && x /= 1 && x /= 2 && x /= 6 && x /=  7 && x /= 11 && x /= 12 && x /= 16 && x /= 17 && x /= 21 && x /= 22 && x /= 26 && x /= 27 = True
                 | z > -1  && z < 4 && (y == 4 || y == 8) && x /= 0 && x /= 1 && x /= 5 && x /=  6 && x /= 10 && x /= 11 && x /= 15 && x /= 16 && x /= 20 && x /= 21 && x /= 25 && x /= 26 = True
                 | otherwise = False

merge :: [a] -> [a] -> [a]
merge xs     []     = xs
merge []     ys     = ys
merge (x:xs) (y:ys) = x : y : merge xs ys

isInTire :: Int -> Int -> [(Int, Int)] -> Bool
isInTire a b (x:xs)   | a == fst x && b == snd x = True
                        | xs == []                 = False
                        | otherwise                = isInTire a b xs
isInTire _ _ [] = False

isOnMyGrass :: Int -> Bool
isOnMyGrass y | y == 1 || y == 11 = True
              | otherwise         = False

isInCar :: Int -> Int -> [(Int, Int)] -> Bool
isInCar a b (x:xs)  | a == fst x && b == snd x  = True
                    | xs == []                  = False
                    | otherwise                 = isInCar a b xs
isInCar _ _ [] = False

withoutConflict :: [(Int, Int)] -> [Int] -> [(Int, Int)]
withoutConflict [] ints = [(30, 2 + (4 * ((ints !! 2) `mod` 3)))]
withoutConflict enemies ints | (isAroundCar enemies (30, (4 * ((ints !! 2) `mod` 3))))  = [(30, 2 + (4 * ((ints !! 2) `mod` 3)))]
                             | otherwise = []

isAroundCar :: [(Int, Int)] -> (Int, Int) -> Bool
isAroundCar enemies (x, y) = endTrue $ map (aroundDat) enemies
                                where
                                    aroundDat (xP, yP) | xP > (x + 4) || xP < (x - 4) = True
                                                       | otherwise    = False
                                    endTrue []                     = True
                                    endTrue (z:zs)     | not z     = False
                                                       | z         = endTrue zs

newFrame :: [Int] -> [Event String] -> State -> (ListFrame, State)
newFrame ints events state@(State playerPosition enemies tick) = (toFrame dim playerPosition' (myCar playerPosition') (myTire playerPosition') (mergeLists (map otherCar enemypositions)) (mergeLists (map otherTire enemypositions)) tick, (State playerPosition' enemypositions newTick))
                                        where
                                            playerPosition'     = foldl (\acc (Event mod ev) -> if mod == "KEYBOARD" then move dim ev acc else acc) playerPosition events
                                            enemyList | ((ints !! 1) `mod` 10) == 1 = enemies ++ (withoutConflict enemies ints)
                                                      | otherwise = enemies  
                                            newTick   | tick == 0 = 20
                                                      | otherwise = tick - 1
                                            enemypositions | ((mod tick 2) == 0) = map moveEnemy enemyList
                                                           | otherwise           = enemyList

mergeLists :: [[a]] -> [a]
mergeLists xxs = foldl (++) [] xxs

dim :: (Int, Int)
dim = (30, 12)
main :: IO ()
main = Sock.withSocketsDo $ runMateRandom (Config (fromJust $ parseAddress "127.0.0.1") 1337 dim (Just 33000) False []) newFrame (State (1, 6) [] 25)
