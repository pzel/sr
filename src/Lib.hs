{-# LANGUAGE FlexibleInstances, RecordWildCards #-}
module Lib
    ( Game(..)
    , InputEvent(..)
    , draw
    , freshGame
    , update
    ) where

data Dir = FacingLeft | FacingRight deriving (Eq,Ord,Show)
data InputEvent = CmdMoveLeft | CmdMoveRight | CmdQuit| CmdSwitchWeapon
data GameEvent = MoveRight | MoveLeft | TurnRight | TurnLeft | NextWeapon
data Game = Game {
    gamePlayerLocation :: Int
  , gamePlayerDir :: Dir
  , gamePlayerWeapon :: Weapon
  , gameCurrentSegment :: Segment
  , gameWorld :: [Segment]
  } deriving (Eq,Show)

data Segment = Segment [Cell] deriving (Eq,Show)
data Cell = Empty | Door deriving (Eq,Show)

type Glyph = Char

class Oriented a where
  facing :: Dir -> a -> Glyph

instance Oriented [Glyph] where
  facing FacingRight = head . tail
  facing FacingLeft = head

instance Oriented Weapon where
  facing FacingLeft Pistol = '¬'
  facing FacingRight Pistol = '⌐'
  facing FacingLeft SMG = '┭'
  facing FacingRight SMG = '┮'
  facing _ Knife = '-'

class HasGlyph a where
  glyphOf :: a -> Glyph

data Weapon = Knife | Pistol | SMG deriving (Eq, Show)

instance HasGlyph Cell where
  glyphOf Empty = '_'
  glyphOf Door = 'A'

segmentLength :: Segment -> Int
segmentLength (Segment l) = length l

drawCell Empty = '_'
drawCell Door = 'A'

solid = '█'
dotted = '▒'
light = '░'
playerG = "@@"

freshGame :: Game
freshGame = Game {
    gamePlayerLocation = 1
  , gamePlayerWeapon = SMG
  , gamePlayerDir = FacingRight
  , gameCurrentSegment = Segment $ replicate 50 Empty
  , gameWorld = []
  }

draw :: Game -> String
draw g@Game{..} =
  replicate (gamePlayerLocation - 1) '_' ++
  (drawPlayer gamePlayerDir gamePlayerWeapon) ++
  replicate ((segmentLength gameCurrentSegment) - (gamePlayerLocation+1)) '_' -- ++ (showMsgs g)

showMsgs g = '\n' : show g

drawPlayer :: Dir -> Weapon -> [Glyph]
drawPlayer d w =
  let (player,weapon) = (facing d playerG, facing d w)
  in  if d == FacingRight
      then ['_', player, weapon]
      else [weapon, player, '_']

update :: Game -> InputEvent -> Game
update g ie = update' g (generateEvents g ie)

update' :: Game -> [GameEvent] -> Game
update' = foldl (\g ev -> applyEvent g ev)

generateEvents :: Game -> InputEvent -> [GameEvent]
generateEvents Game{..} cmd =
  case (gamePlayerDir, cmd) of
    (FacingLeft, CmdMoveLeft) -> return MoveLeft
    (FacingRight, CmdMoveRight) -> return MoveRight
    (_, CmdMoveLeft) -> return TurnLeft
    (_, CmdMoveRight) -> return TurnRight
    (_, CmdQuit) -> error "QUIT"
    (_, CmdSwitchWeapon) -> return NextWeapon

applyEvent :: Game -> GameEvent -> Game
applyEvent g@Game{..} TurnLeft = g{gamePlayerDir=FacingLeft}
applyEvent g@Game{..} TurnRight = g{gamePlayerDir=FacingRight}
applyEvent g@Game{..} MoveLeft =
  let l = gamePlayerLocation
  in if gamePlayerLocation <= 1
     then g
     else g{gamePlayerLocation=l-1}
applyEvent g@Game{..} MoveRight =
  let l = gamePlayerLocation
      len = segmentLength gameCurrentSegment
  in if l >= (len-1) then g else g{gamePlayerLocation=l+1}
applyEvent g@Game{..} NextWeapon =
  case gamePlayerWeapon of
    Knife -> g{gamePlayerWeapon=Pistol}
    Pistol -> g{gamePlayerWeapon=SMG}
    SMG -> g{gamePlayerWeapon=Knife}
