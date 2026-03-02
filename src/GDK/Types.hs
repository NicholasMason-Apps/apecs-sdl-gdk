{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module GDK.Types (Config(..)
                 , Renderable(..)
                 , FPS
                 , Position(..)
                 , Time(..)) where

import qualified SDL
import Apecs
import Data.Word (Word8)
import GDK.Texture (RenTexture)
import GDK.Font (RenText)

type FPS = Int

-- | Configuration settings for the game upon initialisation
data Config = Config
    {
        windowTitle :: String, -- ^ Title of the game window
        windowDimensions :: (Int, Int), -- ^ Width and height of the game window in pixels
        backgroundColor :: SDL.V4 Word8, -- ^ Background color of the game window as an RGBA value
        targetFPS :: FPS -- ^ Desired FPS for the game loop
    }

-- class Component a => Renderable a where
--     render :: SDL.Renderer -> Position -> a -> IO ()

-- | Represents an entity that can be rendered
data Renderable = RenderableTexture RenTexture
                | RenderableText RenText
                deriving (Eq, Show)

newtype Position = Position (SDL.V2 Float)
instance Component Position where type Storage Position = Map Position

newtype Time = Time Float deriving (Show, Eq, Num)
instance Semigroup Time where
    (Time t1) <> (Time t2) = Time (t1 + t2)
instance Monoid Time where
    mempty = Time 0
instance Component Time where type Storage Time = Global Time
