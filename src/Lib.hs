module Lib
    ( Game(..)
    , InputEvent(..)
    , draw
    , update
    ) where

data Game = Game Integer deriving (Eq,Ord,Show)
data InputEvent = MoveLeft | MoveRight

draw (Game i) = show i
update (Game i) MoveRight = Game (i+1)
update (Game i) MoveLeft = Game (i-1)
