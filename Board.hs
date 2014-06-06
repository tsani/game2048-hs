module Board ( makeBoard, boardSize, shiftBoard, boardTranspose,
               putValue, put2, put4, addBlockIO, 
               freeSquares, possibleMoves, possibleBoards, possibleMoveResults, possibleMoveResults', chooseFrom, didWin,
               boardToString, boardFromLists,
               Vec2i(..), Direction(..), Board, BoardChance ) where

import Data.List
import System.Random

type Length = Int

type Tile = Int
type Row = [Tile]
type Board = [Row]

type BoardChance = (Board, Double)

data Direction = DirUp | DirDown | DirLeft | DirRight deriving (Eq, Show, Enum)

data Vec2i = Vec2i Int Int deriving (Eq, Show)

didWin :: Board -> Bool
didWin = any (any (>=2048))

chooseFrom :: [a] -> StdGen -> (StdGen, a)
chooseFrom xs rng = let (i, rng') = randomR (0, length xs - 1) rng
                    in (rng', xs !! i)

addBlockIO :: Board -> IO Board
addBlockIO b = do 
             rng <- newStdGen
             let (rng', sq) = chooseFrom (freeSquares b) rng
             let prob = fst $ random rng' :: Double
             let putf = if prob <= 0.1 then put4 else put2
             return $ putf sq b

boardToString :: Board -> String
boardToString b = unlines $ map (\r -> concatMap (\i -> show i ++ " ") r) b

boardFromLists :: [[Int]] -> Board
boardFromLists = id -- now that we're using lists...

-- |Write a value into the "Board".
putValue :: Int -> Vec2i -> Board -> Board
putValue v (Vec2i rx cx) b = iRows ++ (iCols ++ v:fCols) : fRows
    where (iRows, r':fRows) = splitAt rx b
          (iCols, _:fCols) = splitAt cx r'

-- |Shorthand for writing the value 2.
put2 = putValue 2

-- |Shorthand for writing the value 4.
put4 = putValue 4

prob2 = 0.9
prob4 = 0.1

-- |Create a square board initialized with zeroes. 
makeBoard :: Length -> Board
makeBoard l = replicate l $ replicate l 0

-- |A 2-vector (rows,cols) representing the size of a Board.
boardSize :: Board -> Vec2i
boardSize b = Vec2i (length b) (length $ head b)

-- |Find all the zero tiles on the Board.
freeSquares :: Board -> [Vec2i]
freeSquares b = foldl' (\sqs (rowi, row) -> foldl' (\sqs (coli, v) -> if v == 0 then (Vec2i rowi coli):sqs else sqs) sqs row) [] b'
    where b' = enumerate $ map enumerate $ b
          enumerate = zip [0..]

pullLeft :: [Int] -> [Int]
pullLeft [] = []
pullLeft ns
    | isEmpty ns = ns
    | (null . tail) ns = ns
    | this == 0 = pullLeft $ nexts ++ [0]
    | next == 0 && (not . isEmpty) nexts = pullLeft $ this : pullLeft nexts
    | this == next = pullLeft $ (this * 2):0:tail nexts
    | otherwise = this : pullLeft nexts
    where isEmpty = all (== 0)
          this : nexts@(next : _) = ns

pullRight = reverse . pullLeft . reverse

possibleMoves :: Board -> [(Direction, Board)]
possibleMoves b = map (\(d, Just b) -> (d, b)) $ 
                    filter (\(d, b) -> case b of
                                         Just _ -> True
                                         Nothing -> False
                           ) $ map (\d -> (d, shiftBoard d b)) $ enumFrom DirUp

-- |Yields a list of all the possible boards that can be made by placing a 2 or 4 on an empty tile 
-- alongside the associated probability of that board being picked.
possibleBoards :: Board -> [BoardChance]
possibleBoards b = concatMap (\sq -> [(put2 sq b, prob2/n),(put4 sq b, prob4/n)]) fsqs
    where fsqs = freeSquares b
          n = fromIntegral $ length fsqs

possibleMoveResults :: Board -> [(Direction, Board, [(Board, Double)])]
possibleMoveResults bi = map (\(d, b) -> (d, b, map (fmap (/n)) (possibleBoards b))) pmb
    where pmb = possibleMoves bi
          n = fromIntegral $ length pmb

possibleMoveResults' :: Board -> [(Board, [(Board, Double)])]
possibleMoveResults' = map (\(_, b, bps) -> (b, bps)) . possibleMoveResults

boardTranspose :: Board -> Board
boardTranspose = transpose

shiftBoard :: Direction -> Board -> Maybe Board
shiftBoard d b = shiftf d' b
    where verticalShift d = fmap boardTranspose . shiftBoard d . boardTranspose
          d' = if d == DirUp then DirLeft else if d == DirDown then DirRight else d

          shiftf :: Direction -> Board -> Maybe Board
          shiftf d' b = if d == DirLeft || d == DirRight 
                        then let b' = map (pull d') b in if b' == b then Nothing else Just b'
                        else verticalShift d' b
          pull DirRight = pullRight
          pull DirLeft = pullLeft
