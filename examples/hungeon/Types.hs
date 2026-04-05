{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# LANGUAGE DeriveGeneric #-}

module Types where

import Cadence.Systems
import Cadence.Types
import Apecs
import Linear
import qualified Data.Set as Set
import qualified Data.Map as Map
import GHC.Generics (Generic)
import Data.Aeson
import qualified Data.Vector as V 

data Settings = Settings {
    fullscreen :: Bool
} deriving (Generic, Show)
instance ToJSON Settings where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON Settings where
    parseJSON = genericParseJSON defaultOptions
instance Semigroup Settings where
    (Settings f1) <> (Settings f2) = Settings f2
instance Monoid Settings where
    mempty = Settings False
instance Component Settings where type Storage Settings = Global Settings

newtype Viewport = Viewport (Int, Int) deriving Show
instance Semigroup Viewport where
    (Viewport (w1, h1)) <> (Viewport (w2, h2)) = Viewport (w1 + w2, h1 + h2)
instance Monoid Viewport where
    mempty = Viewport (1280, 720)
instance Component Viewport where type Storage Viewport = Global Viewport

data UIState = CombatAttackSelectUI | CombatMagicSelectUI deriving (Show, Eq)
instance Semigroup UIState where
    (<>) :: UIState -> UIState -> UIState
    _ <> u2 = u2
instance Monoid UIState where
    mempty :: UIState
    mempty = CombatAttackSelectUI
instance Component UIState where type Storage UIState = Global UIState

data GameState = MenuState | DungeonState | PauseState | CombatState | SettingsState deriving (Show, Eq)
instance Semigroup GameState where
    (<>) :: GameState -> GameState -> GameState
    _ <> gs2 = gs2
instance Monoid GameState where
    mempty :: GameState
    mempty = MenuState
instance Component GameState where type Storage GameState = Global GameState

-- Input key components and types
data GameKey = GkUp 
            | GkDown 
            | GkLeft 
            | GkRight
            | GkEsc
            | GkE
            | GkSpace
            | GkQ
            | GkF
            | GkLMB
            deriving (Show, Eq, Ord)
newtype KeyBindings rawKey = KeyBindings (Map.Map rawKey GameKey)

newtype KeysPressed = KeysPressed (Set.Set GameKey)
instance Semigroup KeysPressed where
    (KeysPressed ks1) <> (KeysPressed ks2) = KeysPressed (ks1 <> ks2)
instance Monoid KeysPressed where
    mempty = KeysPressed Set.empty
instance Component KeysPressed where type Storage KeysPressed = Global KeysPressed

newtype Velocity = Velocity (V2 Float) deriving (Show)
instance Component Velocity where type Storage Velocity = Map Velocity

newtype MoveDirection = MoveDirection (Set.Set Direction) deriving (Show)
instance Component MoveDirection where type Storage MoveDirection = Map MoveDirection

data Direction = UpDir | DownDir | LeftDir | RightDir deriving (Show, Eq, Ord)
instance Enum Direction where
    fromEnum :: Direction -> Int
    fromEnum UpDir = 1
    fromEnum RightDir = 2
    fromEnum DownDir = 3
    fromEnum LeftDir = 4
    toEnum :: Int -> Direction
    toEnum 1 = UpDir
    toEnum 2 = RightDir
    toEnum 3 = DownDir
    toEnum 4 = LeftDir
    toEnum _ = error "Invalid enum value for Direction"

-- Visible components
data Player = Player deriving Show
instance Component Player where type Storage Player = Unique Player

data EnemyType = Reaper | Vampire | Skeleton | GoldenReaper deriving (Show, Enum)

data Enemy = Enemy { enemyType :: EnemyType } deriving Show
instance Component Enemy where type Storage Enemy = Map Enemy

data Floor = Floor deriving Show
instance Component Floor where type Storage Floor = Map Floor

data Wall = Wall deriving Show
instance Component Wall where type Storage Wall = Map Wall

data Tile = Tile deriving Show
instance Component Tile where type Storage Tile = Map Tile

data Ladder = Ladder deriving Show
instance Component Ladder where type Storage Ladder = Map Ladder

-- BoundaryBox (width, height) (offsetX, offsetY) from centre
data BoundaryBox = BoundaryBox (Int, Int) (Int, Int) deriving (Show)
instance Component BoundaryBox where type Storage BoundaryBox = Map BoundaryBox

-- Dungeon components
data RoomType = StartRoom | NormalRoom | BossRoom | HubRoom | LadderRoom | HeartRoom deriving (Show, Eq)

data GameRoom = GameRoom { roomType :: RoomType,
                           roomLayout :: [[Char]],
                           exits :: [Direction]
                         } deriving (Show)
instance Component GameRoom where type Storage GameRoom = Map GameRoom

newtype Health = Health Int deriving (Show, Num)
instance Component Health where type Storage Health = Map Health

data MapError = MapError deriving (Show)
instance Component MapError where type Storage MapError = Unique MapError 

data Heart = Heart deriving (Show)
instance Component Heart where type Storage Heart = Map Heart

data Item = Item deriving (Show)
instance Component Item where type Storage Item = Map Item

-- Combat components
newtype CombatEnemy = CombatEnemy Entity deriving (Show)
instance Component CombatEnemy where type Storage CombatEnemy = Unique CombatEnemy

data CombatPlayer = CombatPlayer deriving (Show)
instance Component CombatPlayer where type Storage CombatPlayer = Unique CombatPlayer

data CombatAttackParticle = CombatAttackParticle Entity deriving (Show)
instance Component CombatAttackParticle where type Storage CombatAttackParticle = Unique CombatAttackParticle

data CombatTurn = CombatTurn TurnState deriving (Show, Eq)
instance Semigroup CombatTurn where
    (<>) :: CombatTurn -> CombatTurn -> CombatTurn
    _ <> t2 = t2
instance Monoid CombatTurn where
    mempty :: CombatTurn
    mempty = CombatTurn PlayerTurn
instance Component CombatTurn where type Storage CombatTurn = Global CombatTurn

data TurnState = PlayerTurn | EnemyTurn | PlayerAttacking | EnemyAttacking | PlayerWin | EnemyWin deriving (Show, Eq)

data CombatTile = CombatTile deriving (Show)
instance Component CombatTile where type Storage CombatTile = Map CombatTile

data CombatWall = CombatWall deriving (Show)
instance Component CombatWall where type Storage CombatWall = Map CombatWall

newtype ShieldCooldown = ShieldCooldown Float deriving (Show, Num)
instance Semigroup ShieldCooldown where
    (<>) :: ShieldCooldown -> ShieldCooldown -> ShieldCooldown
    (ShieldCooldown sc1) <> (ShieldCooldown sc2) = ShieldCooldown (sc1 + sc2) 
instance Monoid ShieldCooldown where
    mempty :: ShieldCooldown
    mempty = ShieldCooldown 0
instance Component ShieldCooldown where type Storage ShieldCooldown = Global ShieldCooldown


data TransitionEvent = ToCombat | ToDungeon | ToNextLevel | StartDungeon | ToMenu | ToSettings deriving (Show, Eq)

-- Transition Components
data Transition = Transition {
    trProgress :: Float, -- 0 to 1
    trAngle :: Float,    -- angle in radians
    trSpeed :: Float,
    trCoverEventFired :: Bool,
    trEvent :: TransitionEvent
} deriving (Show)
instance Component Transition where type Storage Transition = Unique Transition

newtype Particle = Particle Position deriving (Show)
instance Component Particle where type Storage Particle = Map Particle

newtype MousePosition = MousePosition (V2 Float) deriving Show
instance Semigroup MousePosition where
    (MousePosition pos1) <> (MousePosition pos2) = MousePosition (pos1 + pos2)
instance Monoid MousePosition where
    mempty = MousePosition (V2 0 0)
instance Component MousePosition where type Storage MousePosition = Global MousePosition

-- UI Components
data ButtonAction = StartGameButton | FullscreenButton | SettingsButton | WindowedButton | BackToTitleButton deriving (Show, Eq)

newtype Button = Button ButtonAction
instance Component Button where type Storage Button = Map Button

data ButtonGroup = ButtonGroup (V.Vector Entity) Entity deriving Show
instance Component ButtonGroup where type Storage ButtonGroup = Map ButtonGroup

newtype TextLabel = TextLabel String deriving Show
instance Component TextLabel where type Storage TextLabel = Map TextLabel

data FloatingText = FloatingText {
    currLifetime :: Float,
    lifetime :: Float
} deriving Show
instance Component FloatingText where type Storage FloatingText = Map FloatingText

data MainMenuUIElement = MainMenuUIElement deriving Show
instance Component MainMenuUIElement where type Storage MainMenuUIElement = Map MainMenuUIElement

data SettingsUIElement = SettingsUIElement deriving Show
instance Component SettingsUIElement where type Storage SettingsUIElement = Map SettingsUIElement

data TitleScreen = TitleScreen deriving Show
instance Component TitleScreen where type Storage TitleScreen = Unique TitleScreen

data CombatUI = CombatUI deriving Show
instance Component CombatUI where type Storage CombatUI = Unique CombatUI

makeWorld' [ ''Settings,
             ''Viewport,
             ''UIState,
             ''GameState,
             ''KeysPressed,
             ''Velocity,
             ''MoveDirection,
             ''Player,
             ''Enemy,
             ''Floor,
             ''Wall,
             ''Tile,
             ''Ladder,
             ''BoundaryBox,
             ''GameRoom,
             ''Health,
             ''MapError,
             ''Heart,
             ''Item,
             ''CombatEnemy,
             ''CombatPlayer,
             ''CombatAttackParticle,
             ''CombatTurn,
             ''CombatTile,
             ''CombatWall,
             ''ShieldCooldown,
             ''Transition,
             ''Particle,
             ''MousePosition,
             ''Button,
             ''ButtonGroup,
             ''TextLabel,
             ''FloatingText,
             ''MainMenuUIElement,
             ''SettingsUIElement,
             ''TitleScreen,
             ''CombatUI
           ]

type System' a = System World a
type Kinetic = (Position, Velocity)