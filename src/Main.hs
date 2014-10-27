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

data Stats = Stats Int Int

good :: Stats -> Stats
bad :: Stats -> Stats
good (Stats g b) = Stats (g + 1) b
bad  (Stats g b) = Stats g (b + 1)

showStats :: Stats -> String
showStats (Stats goods bads) =
  "(good: " ++ show goods ++ ", bad: " ++ show bads ++ ")"

loop :: [Int] -> Int -> Int -> Stats -> IO ()
loop numbers number lastRandom stats = do
  subtrahend <- getRandomFromRangeDiffThanLast lastRandom 0 number
  putStr $ show number ++ " - " ++ show subtrahend ++ " = "
  hFlush stdout
  rawLine <- getLine
  case allDigits rawLine of
    True -> do
      let answer = readInt rawLine
      let loop'' msg = loop' msg number subtrahend
      case answer == number - subtrahend of
        True -> loop'' yay $ good stats
        False -> loop'' boo $ bad stats
    False -> loop' boo number subtrahend $ bad stats
  where
    loop' msg number subtrahend stats = do
      putStrLn $ msg ++ " " ++ (showStats stats)
      newMinuend <- if (length numbers > 1)
        then pickOneRandomlyDiffThanLast numbers number
        else return number
      loop numbers newMinuend subtrahend stats

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
        loop numbers firstMinuend 0 $ Stats 0 0
