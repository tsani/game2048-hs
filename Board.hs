module Board ( shiftBoard, putValue, put2, put4, makeBoard, freeSquares, boardToString, possibleMoves, Vec2i(..), Direction(..), Board ) where

import Prelude hiding ( head, init, tail, last )

import qualified Data.Vector as V
import qualified Data.List as L
import Control.Applicative
import Data.Vector ( (//), update, head, init, tail, last )

type Length = Int

-- |The first index is the row and the second is the column.
type Board = V.Vector (V.Vector Int)

data Direction = DirUp | DirDown | DirLeft | DirRight deriving (Eq, Show, Enum)

data Vec2i = Vec2i Int Int deriving (Eq, Show)

boardToString :: Board -> String
boardToString b = unlines $ map (\row -> concatMap (\i -> show i ++ " ") $ V.toList row) $ V.toList b

putValue :: Int -> Vec2i -> Board -> Board
putValue v (Vec2i row col) b = b // [(row, rowVec // [(col, v)])]
    where rowVec = b V.! row

put2 = putValue 2
put4 = putValue 4

makeBoard :: Length -> Board
makeBoard l = V.replicate l $ V.replicate l 0

freeSquares :: Board -> [Vec2i]
freeSquares b = V.ifoldl' (\sqs rowi row -> V.ifoldl' (\sqs coli v -> if v == 0 then (Vec2i rowi coli):sqs else sqs) sqs row) [] b

-- |Starts at the end of the Vector and pulls elements to the right.
pullRight :: V.Vector Int -> V.Vector Int
pullRight v
    | V.all (==0) v             = v -- empty row !
    | (V.null . init) v         = v -- nothing left to pull !
    | last v == 0               = pullRight $ v // ((0,0) : zip [1..] (V.toList $ V.init v))
    | last v == (last . init) v = (pullRight $ V.init $ v // [(V.length v - 2, 0)]) `V.snoc` (V.last v * 2)
    | otherwise                 = (pullRight $ V.init v) `V.snoc` (V.last v)

pullLeft = V.reverse . pullRight . V.reverse

-- |Shift all blocks on the given "Board" in the specified "Direction".
-- If the shift produced no movement, then "Nothing" is yielded.
--shiftBoard :: Direction -> Board -> Board
--shiftBoard d = shiftf d'
--    where boardTranspose = V.fromList . map V.fromList . L.transpose . V.toList . V.map V.toList
--          verticalShift d = boardTranspose . shiftBoard d . boardTranspose
--          d' = if d == DirUp then DirLeft else if d == DirDown then DirRight else d
--          shiftf = if d == DirLeft || d == DirRight then (V.map . pull) else verticalShift
--          pull DirRight = pullRight
--          pull DirLeft = pullLeft

possibleMoves :: Board -> [(Direction, Board)]
possibleMoves b = map (\(d, Just b) -> (d, b)) $ 
                    filter (\(d, b) -> case b of
                                         Just _ -> True
                                         Nothing -> False
                           ) $ map (\d -> (d, shiftBoard d b)) $ enumFrom DirUp

shiftBoard :: Direction -> Board -> Maybe Board
shiftBoard d b = shiftf d' b
    where boardTranspose = V.fromList . map V.fromList . L.transpose . V.toList . V.map V.toList
          verticalShift d = fmap boardTranspose . shiftBoard d . boardTranspose
          d' = if d == DirUp then DirLeft else if d == DirDown then DirRight else d

          shiftf :: Direction -> Board -> Maybe Board
          shiftf d' b = if d == DirLeft || d == DirRight 
                        then let b' = V.map (pull d') b in if b' == b then Nothing else Just b'
                        else verticalShift d' b
          pull DirRight = pullRight
          pull DirLeft = pullLeft
