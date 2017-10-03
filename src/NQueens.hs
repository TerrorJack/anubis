{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE ScopedTypeVariables #-}

module NQueens
  ( nqueens
  ) where

import Data.SBV

nqueens :: Int -> IO AllSatResult
nqueens n =
  allSat $ do
    cols :: [SInteger] <- mkFreeVars n
    pure $
      bAll (`inRange` (0, fromIntegral $ n - 1)) cols &&&
      bAll
        (\op ->
           distinct
             [ row `op` col
             | (row, col) <-
                 [ (row, col)
                 | row <- map fromIntegral [0 .. n - 1]
                 | col <- cols
                 ]
             ])
        [(+), (-), curry snd]
