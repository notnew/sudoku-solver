module Sudoku ()
       where

import Data.Array (Array(..), (!), elems, indices, listArray)
import Data.Ix (Ix(..))
import Data.List(intersperse, (\\))
import Data.Maybe(catMaybes)

data Game a = Game { size :: Int,
                     elements :: Array Position a}

type Position = (Int, Int)
type Puzzle = Game (Maybe Int)
type Solution = Game Int

instance Show a => Show (Game a) where
  show g = "<Game (" ++ s ++ "x" ++ s ++ ")>"
    where s = show $ size g

printGame :: (Show a) => Game a -> String
printGame g = "<Game (" ++ s ++ "x" ++ s ++ ") " ++ contents  ++ ">"
  where s = show $ size g
        contents = show (elems $ elements g)

prettyPrint :: (Show a) => Game a -> String
prettyPrint g  = show g ++ ":\n" ++ rowsString
  where s = show $ size g
        rowsString = concatMap showRow  $ rows (elems $ elements g)
        showRow r = "   " ++ unwords (map show r) ++ "\n"
        rows [] = []
        rows es = let (row, remaining) = splitAt (size g) es
                  in row : rows remaining

makeGame :: Int -> [a] -> Game a
makeGame size elems = Game size $ listArray ((0,0), (size-1,size-1)) elems

getElems :: Game a -> [Position] -> [a]
getElems g ps = map (elements g !) ps

getRow :: Int -> Game a -> [a]
getRow row g = getElems g (range ((row,0), (row, (size g)-1)))

getColumn :: Int -> Game a -> [a]
getColumn col g = getElems g $ range ((0, col), ((size g)-1, col))

getGroup :: Position -> Game a -> [a]
getGroup (x, y) g = getElems g indices
  where -- switch order of x and y, to match ordering of Array
        indices = range ((lowY, lowX), (lowY+2, lowX+2))
        lowX = x*3
        lowY = y*3

getChoices :: Position -> Puzzle -> [Int]
getChoices (col, row) p = (([1..9] \\ rowElems) \\ colElems) \\ groupElems
  where rowElems = catMaybes $ getRow row p
        colElems = catMaybes $ getColumn col p
        groupElems = catMaybes $ getGroup (col `div` 3, row `div` 3) p
