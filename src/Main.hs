module Main (main) where

import Control.Monad.State.Lazy
import Data.Char
import Data.Either (lefts, rights)
import Prelude hiding (last, min, max)
import System.IO
import System.Random (Random, RandomGen, StdGen, randomR, getStdGen, setStdGen)
import System.Environment

data Operator = Plus | Minus

instance Show Operator where
  show Plus = "+"
  show Minus = "-"

data Operation = Operation {
    oLeftOperand :: Int
  , oOperator :: Operator
  , oRightOperand :: Int
  }

data FullOperation = FullOperation {
    foAnswer :: Int
  , foOperation :: Operation
  }

targetNumberOf :: FullOperation -> Int
targetNumberOf (FullOperation answer operation) =
  case oOperator operation of
    Plus -> answer
    Minus -> oLeftOperand operation

instance Show Operation where
  show (Operation left operator right) =
    (show left) ++ " " ++ (show operator) ++ " " ++ (show right) ++ " = "

randomRSt :: (RandomGen g, Random a) => a -> a -> State g a
randomRSt min max = state $ randomR (min, max)

pickOneRandomly :: [a] -> State StdGen a
pickOneRandomly xs = do
    index <- getIntInRange 0 ((length xs) - 1)
    return $ xs !! index

plusOrMinus :: State StdGen Operator
plusOrMinus = pickOneRandomly [Plus, Minus]

getIntInRange :: Int -> Int -> State StdGen Int
getIntInRange = randomRSt

ranOperation :: Int -> Operator -> State StdGen FullOperation
ranOperation number op = do
  otherOperand <- getIntInRange 0 number
  case op of
    Plus -> ranPlus otherOperand number
    Minus -> ranMinus number otherOperand

  where

    ranPlus :: Int -> Int -> State StdGen FullOperation
    ranPlus left num =
      return $ FullOperation num $ Operation left Plus $ num - left

    ranMinus :: Int -> Int -> State StdGen FullOperation
    ranMinus num right = do
      return $ FullOperation (num - right) $ Operation num Minus right

randomOperationDifferent :: [Int] -> Int -> State StdGen FullOperation
randomOperationDifferent numbers lastNumber = do
  newTarget <- case length numbers of
    1 -> return $ head numbers
    _ -> pickRandomDifferent lastNumber numbers
  plusOrMinus >>= ranOperation newTarget

pickRandomDifferent :: Eq a => a -> [a] -> State StdGen a
pickRandomDifferent lastX xs = do
  newX <- pickOneRandomly xs
  case newX == lastX of
    True -> pickRandomDifferent lastX xs
    False -> return newX

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

instance Show Stats where
  show (Stats goods bads) =
    "(good: " ++ show goods ++ ", bad: " ++ show bads ++ ")"

good :: Stats -> Stats
bad  :: Stats -> Stats
good (Stats g b) = Stats (g + 1) b
bad  (Stats g b) = Stats g (b + 1)

loop :: [Int] -> Int -> Stats -> IO ()
loop numbers lastNumber oldStats = do
  fullOperation <- runRand $ randomOperationDifferent numbers lastNumber
  let newTarget = targetNumberOf fullOperation
  let printStats msg stats = putStrLn $ msg ++ " " ++ (show stats)
  let loop' msg stats = printStats msg stats >> loop numbers newTarget stats
  let loopBad = loop' boo $ bad oldStats
  putStrLn $ show $ foOperation fullOperation
  hFlush stdout
  rawLine <- getLine
  case allDigits rawLine of
    Just True -> do
      let readAnswer = readInt rawLine
      case readAnswer == foAnswer fullOperation of
        True -> loop' yay $ good oldStats
        False -> loopBad
    _ -> loopBad

nonEmpty :: [a] -> Maybe [a]
nonEmpty [] = Nothing
nonEmpty xs = Just xs

allDigits :: String -> Maybe Bool
allDigits = fmap (all isDigit) . nonEmpty

readInt :: String -> Int
readInt = read

safeArgToInt :: String -> Either String Int
safeArgToInt arg =
  case allDigits arg of
    Just True -> Right $ readInt arg
    _ -> Left $ "argument '" ++ arg ++ "' isn't a number"

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
      Left errors -> mapM_ putStrLn $ [usage] ++ ["error:"] ++ errors
      Right numbers -> loop numbers 0 $ Stats 0 0
