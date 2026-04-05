{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

module Cadence.Font (FontMap(..), RenText(..), loadFont) where

import qualified SDL.Font as TTF
import Apecs
import qualified Data.Map as Map
import Control.Monad.IO.Class (MonadIO)

-- | Stores the SDL fonts loaded in the game, mapped by their identifiers
newtype FontMap = FontMap (Map.Map String TTF.Font)
instance Semigroup FontMap where
    (FontMap m1) <> (FontMap m2) = FontMap (Map.union m1 m2)
instance Monoid FontMap where
    mempty = FontMap Map.empty
instance Component FontMap where type Storage FontMap = Global FontMap

data RenText = RenText
    { fontRef :: String
    , displayText :: String
    } deriving (Show, Eq)

-- | Load a font into the 'FontMap'
loadFont :: forall w m. (Has w m FontMap, MonadIO m) 
         => FilePath
         -> String -- ^ Font identifier
         -> Int -- ^ Font size in points
         -> SystemT w m ()
loadFont path ident size = do
    font <- liftIO $ TTF.load path size
    modify global $ \(FontMap m) -> FontMap (Map.insert ident font m)