{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

module GDK.Texture (TextureData(..), Animation(..), TextureMap(..), loadTexture) where

import qualified SDL.Image as IMG
import qualified SDL
import Apecs
import qualified Data.Map as Map
import Control.Monad.IO.Class (MonadIO)

{-|
Represents a texture in the game, which may be static or animated.
Note that animations must be stored as a horizontal [sprite sheet](https://www.aseprite.org/docs/sprite-sheet/).
-}
data TextureData = TextureData
    { texture :: SDL.Texture
    , animation :: Maybe Animation
    } deriving (Eq)

data Animation = Animation
    { frameCount :: Int
    -- ^ Total number of frames in the animation
    , frameSpeed :: Float
    -- ^ Duration of each frame in seconds
    , next :: String
    -- ^ Identifier of the next texture to transition to after this one finishes
    } deriving (Show, Eq)

-- | Stores the SDL textures loaded in the game, mapped by their identifiers
newtype TextureMap = TextureMap (Map.Map String TextureData)
instance Semigroup TextureMap where
    (TextureMap m1) <> (TextureMap m2) = TextureMap (Map.union m1 m2)
instance Monoid TextureMap where
    mempty = TextureMap Map.empty
instance Component TextureMap where type Storage TextureMap = Global TextureMap

-- | Loads a texture into the TextureMap with an associated identifier and optional animation data
loadTexture :: forall w m. (Has w m TextureMap, MonadIO m) 
            => SDL.Renderer -- ^ SDL renderer context 
            -> FilePath
            -> String -- ^ Texture identifier
            -> Maybe Animation
            -> SystemT w m ()
loadTexture r path ident anim = do
    tex <- liftIO $ IMG.loadTexture r path
    modify global $ \(TextureMap m) -> TextureMap (Map.insert ident (TextureData tex anim) m)