import System.Random
import System.Environment

printAnswer :: Int -> Int -> IO ()
printAnswer answer expected =
  if (answer == expected)
    then putStrLn "Bravo!"
    else putStrLn "Boo!"

loop :: Int -> IO ()
loop n = do
  comp <- randomRIO (1, n -1) :: IO Int
  putStrLn $ show n ++ " - " ++ show comp ++ ": "
  answer <- fmap read getLine :: IO Int
  printAnswer answer (n - comp)

  loop n
main :: IO ()
main = do
--  stdGen <- Ran.getStdGen
  args <- getArgs
  let complementaire = read (head args) :: Int
  loop complementaire
