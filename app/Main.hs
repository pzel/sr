module Main where
--import Control.Concurrent (threadDelay)
import Data.Char (chr)
import Lib
import System.CPUTime (getCPUTime)
import System.IO

main :: IO ()
main = setupTerm >> gameLoop freshGame

gameLoop :: Game -> IO ()
gameLoop g = do
  clearTerm
  putStr (draw g)
  hFlush stdout
  i <- waitForInput
  gameLoop (update g i)

clearTerm, hideCursor, homeCursor, showCursor, setupTerm :: IO ()
clearTerm = putStrLn (chr 27 : "[2J") >> homeCursor
homeCursor = putStr (chr 27 : "[H")
hideCursor = putStr (chr 27 : "[?25l")
showCursor = putStr (chr 27 : "[?25h")
setupTerm = do
  hSetBuffering stdin NoBuffering
  hSetEcho stdin False
  hideCursor

waitForInput :: IO InputEvent
waitForInput = do
  userInput <- getChar
  case userInput of
    'l' -> return CmdMoveRight
    'h' -> return CmdMoveLeft
    _ -> waitForInput
