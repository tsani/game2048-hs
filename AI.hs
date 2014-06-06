module AI where

{-# LANGUAGE ScopedTypeVariables #-}

import Board

import Data.List
import Data.Function ( on )

import Control.Monad ( forever, when )
import System.IO
import System.Exit

import Debug.Trace

data Hole = Hole

-- |Multiplier triple used for the heuristics.
type HMults = (Double, Double, Double)

type Score = Double
type ScoredBoard = (Board, Score)

deathPenalty = (-20000)

-- | Calculate the multiplier associated with whether the given point is on an edge or not.
-- This multiplier is k^n if n is nonzero where n is the number of edges the tile at the given point is touching.
edgeMult :: Double -> Board -> Vec2i -> Double
edgeMult k b (Vec2i r c) = if n > 0 then k ** n else 0
    where (Vec2i rows cols) = boardSize b
          n = (if r == 0 || r == rows - 1 then 1 else 0) + (if c == 0 || c == cols - 1 then 1 else 0)

smoothness :: Double -> Board -> Double
smoothness k b = foldl' ac 0 b + foldl' ac 0 (boardTranspose b)
    where ac s r = s + rowSmoothness r
          rowSmoothness r | (null . tail) r                   = fromIntegral . head $ r
                          | head r `div` 2 == (head . tail) r = sqrt k * fromIntegral (head r) + rowSmoothness (tail r)
                          | head r         == (head . tail) r = (k^(2 :: Int)) * fromIntegral (head r) + rowSmoothness (tail r)
                          | otherwise                         = k * (fromIntegral . negate) (abs $ head r - (head . tail) r)  + rowSmoothness (tail r)

mergeMult :: Double -> Board -> Double
mergeMult k b = k * (fromIntegral . length) (freeSquares b)

scoreBoard :: HMults -> Board -> Double
scoreBoard (ek,mk,sk) b = let r = edgeScore + mergeScore + smoothScore in {-trace (show r)-} r
    where edgeScore = 0 --V.ifoldl' (\s r row -> s + V.ifoldl' (\s c v -> s + fromIntegral v * edgeMult ek b (Vec2i r c)) s row) 0 b
          mergeScore = mergeMult mk b
          smoothScore = smoothness sk b

bestMoveN :: HMults -> Int -> Board -> Maybe (Direction, ScoredBoard)
bestMoveN h n b
    | null scoredMoves   = Nothing
    | otherwise          = Just $ maximumBy (compare `on` (snd . snd)) scoredMoves
    where scoredMoves = map (\(d, b) -> (d, (b, evalMoveN h n b))) $ possibleMoves b 

-- |Takes a board after a move has been applied.
evalMoveN :: HMults -> Int -> Board -> Double
evalMoveN _ 0 _ = 0
evalMoveN h n b = sum scores
    where bs = possibleBoards b :: [BoardChance] -- (Board, Probability)
          evalNext b' = case bestMoveN h (n-1) b' of Just (_, (_, s)) -> s ; Nothing -> deathPenalty
          scores = map (\(b', prob) -> prob * (scoreBoard h b' + evalNext b')) bs :: [Double] -- (Board, Score)
          
game' :: HMults -> Board -> IO Board
game' p b = do putStrLn "---" 
               putStr (boardToString b)
               case bestMoveN p 3 b of
                   Just (d, (b', s)) -> print d >> putStrLn (boardToString b') >> addBlockIO b' >>= game' p 
                   Nothing -> return b 

successRateFor :: (Double, Double, Double) -> Int -> Int -> IO Double
successRateFor p bn n = winningGames p bn n >>= return . flip ((/) `on` fromIntegral) bn

winningGames :: (Double, Double, Double) -> Int -> Int -> IO Int
winningGames p bn n = do finishedBoards <- (sequence $ replicate bn $ addBlockIO $ makeBoard n) >>= mapM (game' p)
                         return $ length $ filter didWin finishedBoards

play' :: (Double, Double, Double) -> Int -> IO ()
play' p n = forever $ do b <- (addBlockIO (makeBoard n) >>= game' p)
                         putStrLn $ boardToString b
                         when (didWin b) (exitWith ExitSuccess)
                         hFlush stdout


