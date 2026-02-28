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

module GDK.Types where

import qualified SDL
import Apecs
import qualified SDL.Font as TTF
import qualified Data.Map as Map
import qualified Data.Vector as V
import Language.Haskell.TH.Syntax
import Data.Word (Word8)

type FPS = Int

-- | Configuration settings for the game upon initialisation
data Config = Config
    {
        windowTitle :: String, -- ^ Title of the game window
        windowDimensions :: (Int, Int), -- ^ Width and height of the game window in pixels
        backgroundColor :: SDL.V4 Word8, -- ^ Background color of the game window as an RGBA value
        targetFPS :: FPS -- ^ Desired FPS for the game loop
    }

-- | Stores the SDL textures loaded in the game, mapped by their identifiers
newtype TextureMap = TextureMap (Map.Map String SDL.Texture)
instance Component TextureMap where type Storage TextureMap = Map TextureMap

-- | Stores the SDL fonts loaded in the game, mapped by their identifiers
newtype FontMap = FontMap (Map.Map String TTF.Font)
instance Component FontMap where type Storage FontMap = Map FontMap

data Animation = Animation
    { frameCount :: Int
    -- ^ Total number of frames in the animation
    , frameDuration :: Float 
    -- ^ Duration of each frame in seconds
    , currentFrame :: Int   
    -- ^ Current frame index
    , nextAnimation :: String 
    -- ^ Identifier of the next animation texture to transition to after this one finishes
    } deriving (Show, Eq)

-- | Represents an entity that can be rendered, containing a reference to its texture and optional animation data
data Renderable = Renderable
    { textureRef :: String 
    -- ^ Identifier for the texture to render
    , animation :: Maybe Animation 
    -- ^ Optional animation data for the renderable entity
    } deriving (Eq, Show)

{-|
A queue of renderable entities, organised as a First-in-First-out (FIFO) structure where the first vector is drawn on the first layer, and each inner vector represents a batch of renderables to be drawn together

Instead of using Apecs 'makeWorld' directly to create a world record, we wrap it such that additional components for GDK can be included in the world.
-}
newtype RenderQueue = RenderQueue (V.Vector (V.Vector Renderable))
instance Semigroup RenderQueue where
    (RenderQueue q1) <> (RenderQueue q2) = RenderQueue (q1 V.++ q2)
instance Monoid RenderQueue where
    mempty = RenderQueue V.empty
instance Component RenderQueue where type Storage RenderQueue = Global RenderQueue

makeWorld' :: [Name] -> Q [Dec]
makeWorld' cTypes = makeWorld "World" (cTypes ++ [''TextureMap, ''FontMap, ''RenderQueue])