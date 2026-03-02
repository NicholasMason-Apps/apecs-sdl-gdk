{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

module GDK.Font (FontMap(..), RenText(..), loadFont) where

import qualified SDL.Font as TTF
import Apecs
import qualified Data.Map as Map
import Control.Monad.IO.Class (MonadIO)

-- | Stores the SDL fonts loaded in the game, mapped by their identifiers
newtype FontMap = FontMap (Map.Map String TTF.Font)
instance Component FontMap where type Storage FontMap = Map FontMap

data RenText = RenText
    { fontRef :: String
    , text :: String
    , colour :: TTF.Color
    , layer :: Int
    } deriving (Show, Eq)

loadFont :: forall w m. (Has w m FontMap, MonadIO m) 
         => FilePath
         -> String -- ^ Font identifier
         -> Int -- ^ Font size in points
         -> SystemT w m ()
loadFont path ident size = do
    font <- liftIO $ TTF.load path size
    modify global $ \(FontMap m) -> FontMap (Map.insert ident font m)