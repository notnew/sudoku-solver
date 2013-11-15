module Sudoku ()
       where

import Data.Array (Array(..))

data Game a = Game { size :: Int,
                     elements :: Array (Int, Int) a}

type Puzzle = Game (Maybe Int)
type Solution = Game Int

