module Main where

import Board
import System.Random
import System.IO
import qualified Data.Map as M

directions = M.fromList $ zip ['w','s','a','d'] (enumFrom DirUp)

chooseFrom :: [a] -> StdGen -> (StdGen, a)
chooseFrom xs rng = let (i, rng') = randomR (0, length xs - 1) rng
                    in (rng', xs !! i)

addBlock :: Board -> IO Board
addBlock b = do 
             rng <- newStdGen
             let (rng', sq) = chooseFrom (freeSquares b) rng
             let prob = fst $ random rng' :: Double
             let putf = if prob <= 0.1 then put4 else put2
             return $ putf sq b

main = do
       hSetEcho stdout False
       addBlock (makeBoard 4) >>= game

game :: Board -> IO Board
game b = do putStrLn $ boardToString b
            let ms = possibleMoves b
            if null ms 
            then putStrLn "Game over!" >> return b
            else do dirC <- getChar
                    case M.lookup dirC directions of
                        Just dir -> 
                            case shiftBoard dir b of
                                Just b' -> addBlock b' >>= game
                                Nothing -> putStrLn "Invalid move!" >> game b
                        Nothing -> putStrLn "Invalid direction!" >> game b
               
         


