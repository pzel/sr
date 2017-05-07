module Main where

import Control.Concurrent (threadDelay)
import Data.Char (chr)
import Lib
import System.CPUTime (getCPUTime)
import System.IO

main :: IO ()
main = do
  setupTerm
  loop
 where
   setupTerm = hSetBuffering stdin NoBuffering >>
               hSetEcho stdin False >> hideCursor
   loop = do
         clearTerm
         t <- getCPUTime
         putStrLn (show t)
         tick
         loop

clearTerm, hideCursor, homeCursor, showCursor :: IO ()
clearTerm = putStrLn (chr 27 : "[2J") >> homeCursor
homeCursor = putStr (chr 27 : "[H")
hideCursor = putStr (chr 27 : "[?25l")
showCursor = putStr (chr 27 : "[?25h")

tick :: IO ()
tick = threadDelay $ 100
