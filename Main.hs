module Main where

import Board
import AI
import System.Random
import System.IO
import qualified Data.Map as M
import System.Environment ( getArgs )

directions = M.fromList $ zip ['w','s','a','d'] (enumFrom DirUp)

--main = do [ekS, mkS, skS, bnS, nS] <- getArgs
--          let [ek, mk, sk] = map read [ekS, mkS, skS]
--          let [bn, n] = map read [bnS, nS]
--          sequence_ $ [test e m s bn n | e <- [0,0.5..ek], m <- [0,0.5..mk], s <- [0,0.5..sk]]
          
          
test ek mk sk bn n = do s <- successRateFor (ek, mk, sk) bn n
                        print (ek, mk, sk, s)
                        hFlush stdout

--main = do
--       addBlockIO (makeBoard 4) >>= game' (0, 5, 5)
--

main = hSetEcho stdout False >> (addBlockIO (makeBoard 4) >>= game' (1, 2, 5)) >> return ()

game :: Board -> IO Board
game b = do putStrLn "---"
            putStrLn $ boardToString b
            let ms = possibleMoves b
            if null ms 
            then putStrLn "Game over!" >> return b
            else do sequence_ $ map (putStrLn . boardToString . snd) ms
                    dirC <- getChar
                    case M.lookup dirC directions of
                        Just dir -> 
                            case shiftBoard dir b of
                                Just b' -> addBlockIO b' >>= game
                                Nothing -> putStrLn "Invalid move!" >> game b
                        Nothing -> putStrLn "Invalid direction!" >> game b
               
         


