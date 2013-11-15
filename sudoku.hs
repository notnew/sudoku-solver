module Sudoku ()
       where

import Data.Array (Array(..), (!), elems, indices, listArray)
import Data.Ix (Ix(..))
import Data.List(intersperse)

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

