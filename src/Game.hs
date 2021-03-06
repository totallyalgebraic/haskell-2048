module Game where
import Data.Maybe
import Prelude hiding (Left, Right)
import Data.List
import System.IO
import Text.Printf
import System.Random

data Move = Up | Down | Right | Left deriving (Eq)

-- | A Cell is a singular tile
type Cell = Maybe Int

-- | A row is a list of cells
type Row = [Cell]

-- | A grid is a list of rows
type Grid = [Row]

mergeRight :: Row -> Row
mergeRight ts = merged ++ extra
    where
        merged = multiply (filter (/= Nothing) ts)
        multiply (x:y:xs) | x == y = ((*) <$> Just 2 <*> x) : multiply xs
                          | otherwise = x : multiply (y:xs)
        multiply x = x
        extra = replicate (length ts - length(merged)) Nothing

mergeLeft :: Row -> Row
mergeLeft ts = reverse $ mergeRight $ reverse ts

moveRight :: Grid -> Grid
moveRight board = map mergeRight board

moveLeft :: Grid -> Grid
moveLeft board = map mergeLeft board

moveUp :: Grid -> Grid
moveUp board = transpose (moveRight (transpose board))

moveDown :: Grid -> Grid
moveDown board = transpose (moveRight (transpose board))

choose :: [a] -> IO a
choose xs = do
    i <- randomRIO (0, length xs-1)
    return (xs !! i)

addRandom :: Grid -> IO Grid
addRandom grid = do
    let empty = findEmpty grid
    coordinate <- choose empty
    let new = setValue grid coordinate (Just 2)
    return new

setValue :: Grid -> (Int, Int) -> Maybe Int -> Grid
setValue grid (row, col) val = pre ++ [mid] ++ post
    where 
        pre  = take row grid
        mid  = take col (grid!!row) ++ [val] ++ drop (col + 1) (grid!!row)
        post = drop (row + 1) grid

findEmpty grid = filter (\(row, col) -> (grid!!row)!!col == Nothing) coordinates
    where 
        singleRow n = zip (replicate 4 n) [0..length grid - 1]
        coordinates = concatMap singleRow [0..length grid - 1]

canMoveRight :: Grid -> Bool
canMoveRight grid = do
    let moved = moveRight grid 
    if (moved) == grid 
        then False
    else True

canMoveLeft :: Grid -> Bool
canMoveLeft grid = do
    let moved = moveLeft grid 
    if (moved) == grid 
        then False
    else True

canMoveUp :: Grid -> Bool
canMoveUp grid = do
    let moved = moveUp grid 
    if (moved) == grid 
        then False
    else True

canMoveDown :: Grid -> Bool
canMoveDown grid = do
    let moved = moveDown grid 
    if (moved) == grid 
        then False
    else True

canMove :: Grid -> Bool
canMove grid = foldr (||) False (map ($ grid) [canMoveRight, canMoveLeft, canMoveUp, canMoveDown])


check2048 :: Grid -> Bool
check2048 grid = [] /= filter (== Just 2048) (concat grid)


moves :: [(Char, Move)]
moves = zip "wasd" [Up, Down, Left, Right]


getMove :: IO Move
getMove = do
    input <- getChar
    case lookup input moves of
        Just x  -> return x
        Nothing -> do putStrLn "Use WASD or CHTN as input"
                      getMove






