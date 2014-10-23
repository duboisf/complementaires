import Control.Monad.State.Lazy
import Data.Char
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

boo :: String
boo = "Boo!"

yay :: String
yay = "Yay!"

verdict :: Int -> Int -> String
verdict answer expected =
  if (answer == expected) then yay else boo

runRand :: State StdGen a -> IO a
runRand stateComputation = do
  stdGen <- getStdGen
  let (result, newStdGen) = runState stateComputation stdGen
  setStdGen newStdGen
  return result

loop :: [Int] -> Int -> Int -> IO ()
loop numbers currentNumber lastRandom = do
  comp <- runRand $ getIntInRange 1 (currentNumber - 1)
  putStr $ show currentNumber ++ " - " ++ show comp ++ " = "
  hFlush stdout
  rawLine <- getLine
  case all isDigit rawLine of
    True -> do
      let answer = read rawLine :: Int
      putStrLn $ verdict answer (currentNumber - comp)
    False -> putStrLn boo
  loop numbers currentNumber comp

main :: IO ()
main = do
  args <- getArgs
  let comps = map read args :: [Int]
  first <- runRand $ pickOneRandomly comps
  loop comps first 0
