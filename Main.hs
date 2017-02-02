module Main where
import Network.MateLight.Simple
import Data.Maybe
import qualified Network.Socket as Sock
import System.Random

type Player = (Int, Int)
type Enemies = [(Int, Int)]
type Tick = Int
type Speed = Int
type SpeedTick = Int

data State = State Player Enemies Tick Speed SpeedTick

move :: String -> (Int, Int) -> (Int, Int)                    -- Moves the player 4 pixels (which equals 1 lane) north /south depending on keyboard input.
move "\"s\"" (x, y) = if (y /= 10) then (x, (y + 4)) else (x, y)
move "\"w\"" (x, y) = if (y /= 2) then (x, (y - 4)) else (x, y)
move _ x = x

moveEnemy :: (Int, Int) -> (Int, Int)
moveEnemy (x, y) = (x - 1, y)

myCar :: (Int, Int) -> [(Int, Int)]              -- Sets other coordinates around input to implement the car shape
myCar (x, y) = [(x, y), (x + 1, y), (x + 2, y), (x + 3, y)]

otherCar :: (Int, Int) -> [(Int, Int)]
otherCar (x, y) = [(x, y), (x + 1, y), (x + 2, y), (x + 3, y)]

myTire :: (Int, Int) -> [(Int, Int)]           -- see @myCar
myTire (x, y) = [(x, y + 1), (x, y - 1), (x + 2, y + 1), (x + 2, y - 1)]

otherTire :: (Int, Int) -> [(Int, Int)]
otherTire (x, y) = [(x, y + 1), (x, y - 1), (x + 2, y + 1), (x + 2, y - 1)]

toFrame :: (Int, Int) -> (Int, Int) -> [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)] -> Int -> Int -> ListFrame   -- Takes following : dimension, player position, player tires, enemy position, enemy tires, current tick and the speed and draws every pixel into a list frame 
toFrame (xdim, ydim) (x', y') xs ys zs as c speed = ListFrame $ map (\y -> map (\x -> draw x y xs ys zs as c speed) [0 .. xdim - 1]) [0 .. ydim - 1]

draw :: Int -> Int -> [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)] -> Int -> Int -> Pixel   -- returns pixel values for a pixel, depending the color on type of the list(car, enemy,tire,street,grass)
draw x y xs ys zs as current speed  | showSpeed x y speed     = Pixel 0 0 0xff
                                    | isInCar x y xs          = Pixel 0xff 0x5a 0
                                    | isInCar x y zs          = Pixel 0xff 0 0xff
                                    | isInStreet x y current  = Pixel 0xff 0xff 0xff
                                    | isInTire x y ys         = Pixel 0 0 0
                                    | isInTire x y as         = Pixel 0 0 0
                                    | isOnMyGrass y           = Pixel 0x00 0xaa 0x00
                                    | otherwise               = Pixel 0x33 0x33 0x33

showSpeed :: Int -> Int -> Int -> Bool 
showSpeed x y speed | x <= (30 - speed) && y == 0 = True
                    | otherwise                   = False

isInStreet :: Int -> Int -> Int -> Bool   -- draws the street marks. 
isInStreet x y z | z > 39 &&           (y == 4 || y == 8) && x /= 4 && x /= 5 && x /= 9 && x /= 10 && x /= 14 && x /= 15 && x /= 19 && x /= 20 && x /= 24 && x /= 25 && x /= 29 && x /= 30 = True
                 | z > 29 && z < 40 && (y == 4 || y == 8) && x /= 3 && x /= 4 && x /= 8 && x /=  9 && x /= 13 && x /= 14 && x /= 18 && x /= 19 && x /= 23 && x /= 24 && x /= 28 && x /= 29 = True
                 | z > 19  && z < 30 && (y == 4 || y == 8) && x /= 2 && x /= 3 && x /= 7 && x /=  8 && x /= 12 && x /= 13 && x /= 17 && x /= 18 && x /= 22 && x /= 23 && x /= 27 && x /= 28 = True
                 | z > 9  && z < 20 && (y == 4 || y == 8) && x /= 1 && x /= 2 && x /= 6 && x /=  7 && x /= 11 && x /= 12 && x /= 16 && x /= 17 && x /= 21 && x /= 22 && x /= 26 && x /= 27 = True
                 | z > -1  && z < 10 && (y == 4 || y == 8) && x /= 0 && x /= 1 && x /= 5 && x /=  6 && x /= 10 && x /= 11 && x /= 15 && x /= 16 && x /= 20 && x /= 21 && x /= 25 && x /= 26 = True
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
                                    aroundDat (xP, yP) | xP > (x + 5) || xP < (x - 5) = True
                                                       | otherwise    = False
                                    endTrue []                     = True
                                    endTrue (z:zs)     | not z     = False
                                                       | z         = endTrue zs

enemyHit :: [(Int, Int)] -> (Int, Int) -> Bool              -- detection, if the players front pixel has the same coordinate like one of the enemies back pixels.
enemyHit [] _                                               = False
enemyHit (x:xs) (z, y)  | fst x == (z + 4) && snd x == y    = True
                        | otherwise                         = enemyHit xs (z, y)                                                     

						-- NewFrame checks if the player hit an enemy car, if yes, resets the game, else does following applications : 
						
newFrame :: [Int] -> [Event String] -> State -> (ListFrame, State)
newFrame ints events state@(State playerPosition enemies tick speed speedTick) | (enemyHit enemies playerPosition)  = (toFrame dim playerPosition' (myCar playerPosition') (myTire playerPosition') (mergeLists (map otherCar enemypositions)) (mergeLists (map otherTire enemypositions)) tick speed, (State (1, 6) [] 500 15 0))
                                                               | otherwise                          = (toFrame dim playerPosition' (myCar playerPosition') (myTire playerPosition') (mergeLists (map otherCar enemypositions)) (mergeLists (map otherTire enemypositions)) (tick `mod` 50) speed, (State playerPosition' enemypositions newTick newSpeed newSTick))
                                        where
                                            newSpeed | tick == 500 && speed /= 1 = speed - 1     -- decounts the speed after every 500 ticks
                                                     | otherwise                 = speed
                                            newSTick | speedTick == speed = 0                -- increases the speedticks after every tick, unless speedTick is equal to speed
                                                     | otherwise = speedTick + 1
                                            playerPosition' = foldl (\acc (Event mod ev) -> if mod == "KEYBOARD" then move ev acc else acc) playerPosition events -- moves player pixels, when key is pressed
                                            enemyList | ((ints !! 1) `mod` 10) == 1 = enemies ++ (withoutConflict enemies ints)  --, sometimes adds new enemies to the list
                                                      | otherwise                   = enemies  
                                            newTick | tick == 0 = 500
                                                    | otherwise = tick - 1
                                            enemypositions | speedTick == 0 = map moveEnemy enemyList  -- moves enemies
                                                           | otherwise               = enemyList

mergeLists :: [[a]] -> [a]
mergeLists xxs = foldl (++) [] xxs

dim :: (Int, Int)
dim = (30, 12)
main :: IO ()
main = Sock.withSocketsDo $ runMateRandom (Config (fromJust $ parseAddress "134.28.70.172") 1337 dim (Just 15000) False []) newFrame (State (1, 6) [] 500 15 0)
