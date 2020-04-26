module Main where

import Data.List
import Data.List.Split
import Control.Exception

f :: Num a => a -> a -> a
f a b = a + b

data Player = Player1 | Player2 deriving (Show, Eq)

type Cell = Maybe Player

type Grid = [[Cell]]

type Coord = (Int, Int)

emptyGrid :: [[Cell]]
emptyGrid = [[Nothing, Nothing, Nothing]
        ,[Nothing, Nothing, Nothing]
        ,[Nothing, Nothing, Nothing]]

update :: [a] -> Int -> a -> [a]
update list index _ | index < 0 || index >= length list = list
update list index newVal = take index list ++ [newVal] ++ drop (index + 1) list

enumerate = zip [0 ..]

updateGrid :: [[a]] -> Coord -> a -> [[a]]
updateGrid arr2d c newVal = map replaceLine gridWithIndex
  where
    replaceLine (yLine, line) | yLine == snd c = update line (fst c) newVal
    replaceLine (_, line) = line
    gridWithIndex = enumerate arr2d

myCustomGrid = updateGrid emptyGrid (2, 2) (Just Player1)

main :: IO ()
main = print emptyGrid

get2D :: [[a]] -> Coord -> Maybe a
get2D arr c | x < 0 || y < 0 || x >= size || y >= size = Nothing
  where
      size = length arr
      x = fst c
      y = snd c
get2D arr c = Just (arr !! snd c !! fst c)

playAction :: Grid -> Coord -> Player -> Maybe Grid
playAction grid c _ | get2D grid c == Nothing = Nothing
playAction grid c _ | get2D grid c /= Just Nothing = Nothing
playAction grid c p = Just (updateGrid grid c (Just p))

next Player1 = Player2
next Player2 = Player1

play :: IO (Maybe Player)
play = mapM (\(a,b,c) -> c) $ fn emptyGrid Player1 Nothing
  where
    fn :: Grid -> Player -> Maybe Player -> IO (Grid, Player, Maybe Player)
    fn g p (Just w) = undefined
    fn acc player _ = do
      curCoord <- askCoordUntil
      fn (resultGrid updatedGrid) (next player) (winner updatedGrid)
                          where
                            updatedGrid = playAction acc curCoord player
                            resultGrid (Just g) = g
                            resultGrid Nothing = acc
                            winner (Just g) = Nothing
                            winner Nothing = Just (next player)

ask :: String -> IO String
ask msg = do
  print msg
  getLine

readMaybe :: (Read a) => String -> Maybe a
readMaybe s = case reads s of
              [(x, "")] -> Just x
              _ -> Nothing


maybeToEither :: a -> Maybe b -> Either a b
maybeToEither = (`maybe` Right) . Left

parseCoord :: String -> Either String Coord
parseCoord raw = process split
  where
    process [x, y] = do
      parsedX <- maybeToEither ("given x: " ++ x ++ " is invalid in input " ++ raw) (readMaybe x :: Maybe Int)
      parsedY <- maybeToEither ("given y: " ++ y ++ " is invalid in input " ++ raw) (readMaybe y :: Maybe Int)
      return (parsedX, parsedY)
    process _ = Left ("input is not valid \"x y\": " ++ raw)
    split = splitOn " " raw

askCoord :: IO (Either String Coord)
askCoord = do
  raw <- ask "Give me a coord: \"x y\""
  return (parseCoord raw)

askUntil :: Show e => IO (Either e a) -> IO a
askUntil io = do
  exceptionOrMaybea <- try io
  case exceptionOrMaybea of
    Left (SomeException _) -> askUntil io
    Right (Left e) ->  do
      print e
      askUntil io
    Right (Right v) -> pure v

askCoordUntil :: IO Coord
askCoordUntil = askUntil askCoord
