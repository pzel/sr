
{-# LANGUAGE FlexibleInstances #-}
module Lib
    ( Game(..)
    , InputEvent(..)
    , draw
    , freshGame
    , update
    ) where

data Dir = FacingLeft | FacingRight deriving (Eq,Ord,Show)
data Game = Game Int Dir deriving (Eq,Ord,Show)
data InputEvent = MoveLeft | MoveRight

type Glyph = Char

class Oriented a where
  facing :: Dir -> a -> Glyph

instance Oriented (Glyph,Glyph) where
  facing FacingRight (_,r) = r
  facing FacingLeft (l,_) = l

instance Oriented [Glyph] where
  facing FacingRight = head . tail
  facing FacingLeft = head

screenWidth = 90
solid = '█'
dotted = '▒'
light = '░'

gunG = "¬⌐"
smgG = "┭┮"
knifeG = "--"
playerG = "@@"

freshGame :: Game
freshGame = Game 10 FacingRight

draw :: Game -> String
draw (Game i d) = replicate (i-1) ' ' ++ drawPlayer d ++ "\n"
  where

drawPlayer :: Dir -> [Glyph]
drawPlayer d =
  let (player,weapon) = (facing d playerG, facing d smgG)
  in  if d == FacingRight
      then [' ', player, weapon]
      else [weapon, player]

update :: Game -> InputEvent -> Game
update (Game i FacingRight) MoveRight = Game (i+1) FacingRight
update (Game i FacingLeft) MoveLeft = Game (i-1) FacingLeft
update (Game i _) MoveRight = Game (i) FacingRight
update (Game i _) MoveLeft = Game (i) FacingLeft
