module GameOfLife where
import Control.Parallel
import Control.Concurrent
import System.Console.ANSI
import System.IO
import System.Random
import Data.List.Split
import Data.List

type Pos = (Int, Int)
type Board = [Pos]
data Universe = Universe {
    cells :: Board,
    width :: Int,
    height :: Int
    }


-- write string at a given position
writeat :: Pos -> String -> IO ()
writeat p xs = do setCursorPosition (fst p) (snd p)
                  putStr xs

-- displays universe on a screen
display :: Universe -> IO ()
display u = do setCursorPosition 0 0
               putStr $ strUniverse u

-- generates new universe of cells of given size
newUniverse :: Int -> Int -> [Bool] -> Universe
newUniverse w h positions = Universe {
        cells = newBoard w h positions,
        width = w,
        height = h
    }

-- generates new cells by given positions (array
-- of booleans, starting from left upper corner)
newBoard :: Int -> Int -> [Bool] -> Board
newBoard w h positions = [(x, y) | zx <- zip positions [0..w*h],
                                        (fst zx) == True,
                                        let x = (snd zx) `div` w
                                            y = (snd zx) `mod` w]

-- converts universe to string ready for displaying
-- on a screen
strUniverse :: Universe -> String
strUniverse u = intercalate "\n" $ splitEvery (width u) oneDboard
         where oneDboard = [ch | x <- [0..(height u - 1)],
                                 y <- [0..(width u - 1)],
                                 let ch = if (elem (x,y) (cells u))
                                          then 'O'
                                          else ' ']

-- tells if cell is alive
isAlive :: Board -> Pos -> Bool
isAlive b p = elem p b

-- tell if particular position
-- doesn't contain live cell
isEmpty :: Board -> Pos -> Bool
isEmpty b p = not (isAlive b p)

-- wraps coordinates
wrap :: Int -> Int -> Pos -> Pos
wrap w h (x,y) = (a, b)
                where a = ((x) `mod` h)
                      b = ((y) `mod` w)

-- returns all neightbor positions for
-- given coordinates
neighbs :: Universe -> Pos -> [Pos]
neighbs u (x, y) = map (wrap (width u) (height u)) neightbsMap
    where neightbsMap = [(x-1, y-1), (x, y-1),
                         (x+1, y-1), (x-1, y),
                         (x+1, y ), (x-1,y+1),
                         (x, y+1), (x+1, y+1)]

-- returns number of alive cell in
-- a neightborhood
liveneighbs :: Universe -> Pos -> Int
liveneighbs u pos = length (filter (isAlive (cells u)) (neighbs u pos))

-- computes next generation survivors 
survivors :: Universe -> Universe
survivors u = Universe {
        cells = [p | p <- (cells u), elem (liveneighbs u p) [2,3]],
        width = width u,
        height = height u
    }

-- computes newly born cells
births :: Universe -> Universe
births u = Universe {
        cells = [p | p <- rmdups (concat (map (neighbs u) (cells u))),
                          isEmpty (cells u) p,
                          liveneighbs u p == 3],
        width = width u,
        height = height u
    }

-- removes duplicated elements within an array
rmdups :: Eq a => [a] -> [a]
rmdups [] = []
rmdups (x:xs) = x:rmdups (filter (/= x) xs)

-- computes next generation of cells' universe
nextgen :: Universe -> Universe
nextgen u = Universe {
        cells = (cells survivedCellsUniverse) ++ (cells newlyBornCellsUniverse),
        width = (width u),
        height = (height u)
    } where
        survivedCellsUniverse = survivors u
        newlyBornCellsUniverse = births u

life :: Universe -> IO ()
life u = do clearScreen
            display u
            threadDelay 50000
            threadDelay 50000
            life (nextgen u)

