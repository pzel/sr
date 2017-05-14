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
data InputEvent = CmdMoveLeft | CmdMoveRight
data GameEvent = MoveRight | MoveLeft | TurnRight | TurnLeft

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
freshGame = Game 1 FacingRight

draw :: Game -> String
draw g@(Game i d) =
  replicate (i-1) '_' ++ drawPlayer d ++
  replicate (screenWidth - (i+1)) '_' ++ (showMsgs g)

showMsgs g = '\n' : show g

drawPlayer :: Dir -> [Glyph]
drawPlayer d =
  let (player,weapon) = (facing d playerG, facing d smgG)
  in  if d == FacingRight
      then ['_', player, weapon]
      else [weapon, player, '_']

update :: Game -> InputEvent -> Game
update g ie = update' g (generateEvents g ie)

update' :: Game -> [GameEvent] -> Game
update' = foldl (\g ev -> applyEvent g ev)

generateEvents :: Game -> InputEvent -> [GameEvent]
generateEvents (Game i FacingRight) CmdMoveRight = [MoveRight]
generateEvents (Game i FacingLeft) CmdMoveLeft = [MoveLeft]
generateEvents (Game i _) CmdMoveRight = [TurnRight]
generateEvents (Game i _) CmdMoveLeft = [TurnLeft]

applyEvent :: Game -> GameEvent -> Game
applyEvent (Game i _) TurnRight = Game i FacingRight
applyEvent (Game i _) TurnLeft = Game i FacingLeft
applyEvent g@(Game i f) MoveRight =
  if i >= (screenWidth-1) then g else (Game (i+1) f)
applyEvent g@(Game i f) MoveLeft =
  if i <= 1 then g else (Game (i-1) f)
