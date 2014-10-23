module Main where

import Control.Monad.State.Lazy
import Data.Char
import Data.Either
import Data.List (intercalate)
import Data.Traversable (mapM)
import System.IO
import System.Random
import System.Environment

data Operator = Plus | Minus

randomRSt :: (RandomGen g, Random a) => a -> a -> State g a
randomRSt min max = state $ randomR (min, max)

plusOrMinus :: State StdGen Operator
plusOrMinus = pickOneRandomly [Plus, Minus]

pickOneRandomly :: [a] -> State StdGen a
pickOneRandomly xs = do
    index <- getIntInRange 0 ((length xs) - 1)
    return $ xs !! index

getIntInRange :: Int -> Int -> State StdGen Int
getIntInRange = randomRSt

getIntInRangeIO :: Int -> Int -> IO Int
getIntInRangeIO min max = randomRIO (min, max)

getRandomFromRangeDiffThanLast :: Int -> Int -> Int -> IO Int
getRandomFromRangeDiffThanLast last min max = do
  pick <- randomRIO (min, max)
  if (pick == last)
    then getRandomFromRangeDiffThanLast last min max
    else return pick
    
pickOneRandomlyDiffThanLast :: Eq a => [a] -> a -> IO a
pickOneRandomlyDiffThanLast possibilities last = do
  index <- getIntInRangeIO 0 ((length possibilities) - 1)
  let pick = possibilities !! index
  if (pick == last)
    then pickOneRandomlyDiffThanLast possibilities last
    else return pick

boo :: String
boo = "\27[31mBoo!\27[0m"

yay :: String
yay = "\27[32mYay!\27[0m"

runRand :: State StdGen a -> IO a
runRand stateComputation = do
  stdGen <- getStdGen
  let (result, newStdGen) = runState stateComputation stdGen
  setStdGen newStdGen
  return result

loop :: [Int] -> Int -> Int -> Int -> Int -> IO ()
loop numbers minuend lastRandom goods bads = do
  subtrahend <- getRandomFromRangeDiffThanLast lastRandom 0 minuend
  putStr $ show minuend ++ " - " ++ show subtrahend ++ " = "
  hFlush stdout
  rawLine <- getLine
  case allDigits rawLine of
    True -> do
      let answer = readInt rawLine
      let loop'' msg = loop' msg minuend subtrahend
      case answer == minuend - subtrahend of
        True -> loop'' yay (goods + 1) bads
        False -> loop'' boo goods (bads + 1)
    False -> do
      loop' boo minuend subtrahend goods (bads + 1)
  where
    loop' msg minuend subtrahend goods bads = do
      putStrLn $ msg ++ " (good: " ++ show goods ++ ", bad: " ++ show bads ++ ")"
      newMinuend <- if (length numbers > 1)
        then pickOneRandomlyDiffThanLast numbers minuend
        else return minuend
      loop numbers newMinuend subtrahend goods bads

allDigits :: String -> Bool
allDigits = all isDigit

readInt :: String -> Int
readInt = read

safeArgToInt :: String -> Either String Int
safeArgToInt arg =
  case allDigits arg of
    True -> Right $ readInt arg
    False -> Left $ "argument '" ++ arg ++ "' isn't a number"

parseArgs :: [String] -> Either [String] [Int]
parseArgs args =
  let parsedArgs = map safeArgToInt args
  in case lefts parsedArgs of
    [] -> Right $ rights parsedArgs
    other -> Left other

usage :: String
usage =
  "USAGE: complementaire x [y [z ...]]\n" ++
  "where x, y, z are numbers (ex complementaire 7 5 2)"

main :: IO ()
main = do
  args <- getArgs
  case length args of
    0 -> putStrLn usage
    _ -> case parseArgs args of
      Left errors -> do
        mapM_ putStrLn $ [usage] ++ ["error:"] ++ errors
      Right numbers -> do
        firstMinuend <- runRand $ pickOneRandomly numbers
        loop numbers firstMinuend 0 0 0
