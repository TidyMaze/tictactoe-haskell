{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Main where

import Data.List
import Data.List.Split
import Control.Exception
import Control.Monad

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

showWinner (Just p) = show p ++ " won. Congratulations!"
showWinner Nothing = "Nobody won, you all suck."

main :: IO ()
main = do
  winner <- play
  let msg = showWinner winner
  putStrLn msg 

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
play = fmap (\(a,b,c) -> c) $ fn emptyGrid Player1 Nothing

fn :: Grid -> Player -> Maybe Player -> IO (Grid, Player, Maybe Player)
fn g p (Just w) = pure (g, p, Just w)
fn acc player _ = do
  putStrLn (showGrid acc)
  curCoord <- askCoordUntil
  let updatedGrid = playAction acc curCoord player
  fn (resultGrid updatedGrid) (next player) (winner updatedGrid)
    where
      resultGrid (Just g) = g
      resultGrid Nothing = acc
      winner (Just g) = checkWinner g
      winner Nothing = Just (next player)

showGrid g = intercalate "\n" (map showLine g)

showLine :: [Cell] -> String
showLine l = unwords (map showCell l)

showCell :: Cell -> String
showCell (Just Player1) = "1"
showCell (Just Player2) = "2"
showCell Nothing = "_"

checkLine :: Grid -> Player -> Int -> Bool
checkLine g p i = all (== Just p) (g !! i)

checkAllLines :: Grid -> Player -> Bool
checkAllLines g p = any (checkLine g p) [0..2]

checkAllColumns :: Grid -> Player -> Bool
checkAllColumns = checkAllLines . transpose

indices :: [a] -> [Int]
indices arr = [0 .. (length arr - 1)]

checkDiag :: Grid -> Player -> Bool
checkDiag g p = all (== Just p) [g !! i !! i | i <- indices g]

checkAntiDiag :: Grid -> Player -> Bool
checkAntiDiag g p = all (== Just p) [g !! i !! ((length g -1) - i) | i <- indices g]

checkWinner' :: Grid -> Player -> Bool
checkWinner' g p = True `elem` [checkAllLines g p, checkAllColumns g p, checkDiag g p, checkAntiDiag g p]

firstJust :: Maybe a -> Maybe a -> Maybe a
firstJust (Just v) _ = Just v
firstJust _ (Just v) = Just v
firstJust _ _ = Nothing

checkWinner :: Grid -> Maybe Player
checkWinner g = firstJust player1Won player2Won
  where
    player1Won = mfilter (checkWinner' g) (Just Player1)
    player2Won = mfilter (checkWinner' g) (Just Player2)

ask :: String -> IO String
ask msg = do
  putStrLn msg
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
