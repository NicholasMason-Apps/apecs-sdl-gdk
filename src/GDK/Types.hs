{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# LANGUAGE InstanceSigs #-}

module GDK.Types (Config(..)
                 , Renderable(..)
                 , FPS
                 , Position(..)
                 , Time(..)
                 , Renderer(..)
                 , Window(..)
                 , TargetFPS(..)
                 , RenPoint(..)
                 , RenRectangle(..)
                 , RenLine(..)
                 , Camera(..)
                 , defaultConfig) where

import qualified SDL
import Apecs
import Data.Word (Word8)
import GDK.Texture (RenTexture)
import GDK.Font (RenText)
import GHC.TypeLits (Nat)
import Linear

type FPS = Int

data TargetFPS = VSync
               | Unlimited
               | Limited Nat

-- | Configuration settings for the game upon initialisation
data Config = Config
    {
        windowTitle :: String, -- ^ Title of the game window
        windowDimensions :: (Int, Int), -- ^ Width and height of the game window in pixels
        backgroundColor :: V4 Word8, -- ^ Background color of the game window as an RGBA value
        targetFPS :: TargetFPS, -- ^ Desired FPS for the game loop
        showFPS :: Maybe String -- ^ Whether to display the current FPS on the screen, and if so, the font to use
    }
instance Semigroup Config where
    _ <> c2 = c2
instance Monoid Config where
    mempty = defaultConfig
instance Component Config where type Storage Config = Global Config 

defaultConfig :: Config
defaultConfig = Config
    { windowTitle = "GDK Game"
    , windowDimensions = (800, 600)
    , backgroundColor = V4 255 255 255 255
    , targetFPS = VSync
    , showFPS = Just "Roboto-Regular"
    }

-- data Render r = Render r

-- class Renderable r w where
--     render :: SDL.Renderer -> r -> System w ()
-- instance Renderable r w => Component (Render r) where type Storage (Render r) = Map (Render r)


-- class Component a => Renderable a where
--     render :: forall w. (Has w IO TextureMap) => SDL.Renderer -> Position -> a -> System w ()

-- data RenderableTexture = RenderableTexture RenTexture
-- instance Renderable RenderableTexture where
--     render r pos (RenderableTexture t) = do
--         TextureMap tm <- get global
--         return ()

-- | Represents a point to be rendered
data RenPoint = RenPoint
    { pointColour :: V4 Word8
    , pointLayer :: Int
    , pointVisible :: Bool
    } deriving (Show, Eq)

-- | Represents a line to be rendered
data RenLine = RenLine
    { lineColour :: V4 Word8
    , lineLayer :: Int
    , lineX :: Float -- ^ X coordinate of the line's ending point
    , lineY :: Float -- ^ Y coordinate of the line's ending point
    , lineVisible :: Bool
    } deriving (Show, Eq)

-- | Represents a rectangle to be rendered
data RenRectangle = RenRectangle
    { rectSize :: V2 Float -- ^ Width and height of the rectangle
    , rectColour :: V4 Word8
    , rectLayer :: Int
    , rectVisible :: Bool
    } deriving (Show, Eq)

-- | Represents an entity that can be rendered
data Renderable = Texture RenTexture
                | Text RenText
                | Point RenPoint
                | Line RenLine
                | Rectangle RenRectangle
                | FilledRectangle RenRectangle
                deriving (Eq, Show)
instance Component Renderable where type Storage Renderable = Map Renderable

newtype Position = Position (V2 Float) deriving (Show, Eq)
instance Component Position where type Storage Position = Map Position

newtype Camera = Camera { camFunc :: V2 Float -> V2 Float }
instance Semigroup Camera where
    (Camera f1) <> (Camera f2) = Camera $ \pos -> f1 (f2 pos)
instance Monoid Camera where
    mempty = Camera id
instance Component Camera where type Storage Camera = Global Camera

newtype Time = Time Float deriving (Show, Eq, Num)
instance Semigroup Time where
    (Time t1) <> (Time t2) = Time (t1 + t2)
instance Monoid Time where
    mempty = Time 0
instance Component Time where type Storage Time = Global Time

newtype Renderer = Renderer (Maybe SDL.Renderer)
instance Semigroup Renderer where
    (Renderer r1) <> (Renderer _) = Renderer r1
instance Monoid Renderer where
    mempty = Renderer Nothing
instance Component Renderer where type Storage Renderer = Global Renderer

newtype Window = Window (Maybe SDL.Window)
instance Semigroup Window where
    (<>) :: Window -> Window -> Window
    (Window w1) <> (Window _) = Window w1
instance Monoid Window where
    mempty = Window Nothing
instance Component Window where type Storage Window = Global Window