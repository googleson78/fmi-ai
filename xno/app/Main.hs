module Main where

import Xno
import Control.Monad (when)
import qualified Data.Vector as Vec

main :: IO ()
main = do
  putStrLn "Board size:"
  n <- readLn
  putStrLn "The human will play as:"
  human <- readLn
  loop human X $ start n

loop :: Spot -> Spot -> Board Spot ->  IO ()
loop human currentSign currentBoard = do
  when (human == currentSign) $ do
    putStrLn "Current board:"
    pretty currentBoard

  case winner currentBoard of
    Draw -> putStrLn "Draw yo"
    Winner x -> putStrLn $ "A winrar is " ++ show x
    InProgress -> do
      nextBoard <- playHumanOrAI human currentSign currentBoard

      loop human (opp currentSign) nextBoard

playHumanOrAI :: Spot -> Spot -> Board Spot -> IO (Board Spot)
playHumanOrAI human currentSign currentBoard
  | human == currentSign = do
    (x, y) <- readLn
    pure $ place currentSign currentBoard (x, y)
  | otherwise = pure $ fst $ play currentSign currentBoard

start :: Int -> Board a
start n = Board $ Vec.replicate n $ Vec.replicate n Nothing
