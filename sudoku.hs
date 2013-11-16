module Sudoku ()
       where

import Data.Array ( Array(..), (!), elems, indices, listArray, array, assocs
                  , bounds)
import Data.Function(on)
import Data.Ix (Ix(..))
import Data.List(intersperse, (\\), foldl', sortBy)
import Data.Maybe(catMaybes, isNothing, isJust, listToMaybe, maybeToList)

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

prettyPrint :: Puzzle -> String
prettyPrint g  = show g ++ ":\n" ++ rowsString
  where s = show $ size g
        rowsString = groupByThree "\n" . map showRow  $ rows (elems $ elements g)
        showRow r = "   " ++ groupByThree " " (map showElem r) ++ "\n"
        showElem Nothing = "[ ]"
        showElem x = show $ maybeToList x
        rows [] = []
        rows xs = let (row, remaining) = splitAt (size g) xs
                  in row : rows remaining
        -- groupByThree is like concat, but first adds a separator between
        -- every 3 sublists
        groupByThree :: [a] -> [[a]] -> [a]
        groupByThree separator = concat . intersperseN 3 separator

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
-- switch order of row and col, to match ordering of Array
getChoices (row, col) p = (([1..9] \\ rowElems) \\ colElems) \\ groupElems
  where rowElems = catMaybes $ getRow row p
        colElems = catMaybes $ getColumn col p
        groupElems = catMaybes $ getGroup (col `div` 3, row `div` 3) p

solve :: Puzzle -> [Puzzle]
solve p = if unsolved
          then solveOne p >>= solve
          else [p]
  where unsolved = isJust . arrayFind  isNothing $ elements p

solve' :: Puzzle -> [Puzzle]
solve' p = expandChoices nextUnknown
  where nextUnknown = arrayFind  isNothing $ elements p
        expandChoices (Just pos) = do
          c <- getChoices pos p
          let newGame = updateGame p pos $ Just c
          solve' newGame
        expandChoices _ = [p]

-- choose a single unkown box and expand its possibilities
solveOne :: Puzzle -> [Puzzle]
solveOne p = expandChoices nextUnknown
  where unKnowns = arrayFindAll  isNothing $ elements p
        nextUnknown = listToMaybe $ sortBy mostCertain unKnowns
        mostCertain = compare `on` (length . flip getChoices p)
        expandChoices (Just pos) =
          map (updateGame p pos . Just) $ getChoices pos p
        expandChoices _ = [p]

-- get a new game with the same values as a given old game, but updating a
-- given position with a given new value
updateGame :: Game a -> Position -> a -> Game a
updateGame g pos newVal = g { elements = newElements }
  where newElements = array range (oldElements ++ [(pos, newVal)])
        oldElements = assocs $ elements g
        range = bounds $ elements g

-- finds indexes of an array, at which the values satisfy a predicate
arrayFindAll :: Ix i => (a -> Bool) -> Array i a -> [i]
arrayFindAll p a = foldl' step [] (indices a)
  where step as i = if p (a ! i) then as ++ [i] else as

arrayFind :: Ix i => (a -> Bool) -> Array i a -> Maybe i
arrayFind p a = listToMaybe $ arrayFindAll p a

-- like intersperse but only insert separator between every n elements
intersperseN :: Int -> a -> [a] -> [a]
intersperseN n sep = intersperse' 0
  where intersperse' _ [] = []
        intersperse' i (x:xs)
          | i == n = sep : intersperse' 0 (x:xs)
          | otherwise = x: intersperse' (i+1) xs

puzzle :: Puzzle
puzzle = makeGame 9 $ map listToMaybe [
  [5],[3],[ ],  [ ],[7],[ ],  [ ],[ ],[ ],
  [6],[ ],[ ],  [1],[9],[5],  [ ],[ ],[ ],
  [ ],[9],[8],  [ ],[ ],[ ],  [ ],[6],[ ],

  [8],[ ],[ ],  [ ],[6],[ ],  [ ],[ ],[3],
  [4],[ ],[ ],  [8],[ ],[3],  [ ],[ ],[1],
  [7],[ ],[ ],  [ ],[2],[ ],  [ ],[ ],[6],

  [ ],[6],[ ],  [ ],[ ],[ ],  [2],[8],[ ],
  [ ],[ ],[ ],  [4],[1],[9],  [ ],[ ],[5],
  [ ],[ ],[ ],  [ ],[8],[ ],  [ ],[7],[9]]
