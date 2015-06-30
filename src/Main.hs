import GameOfLife
import System.Console.ANSI
import System.Random

main :: IO ()
main = do 
          g <- getStdGen
          let width = 75
          let height = 25
          let numbers = take (width*height) $ (randoms g :: [Bool]) 
          let universe = newUniverse width height numbers
          life universe
