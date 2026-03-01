{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

module GDK.Systems (initialise, run, defaultConfig, makeWorld') where

import Apecs
import GDK.Types
import GDK.Texture
import GDK.Font (FontMap(..))
import qualified SDL
import qualified SDL.Font as TTF
import qualified SDL.Image as IMG
import qualified Data.Text as T
import Language.Haskell.TH.Syntax
import Control.Monad (unless)
import qualified SDL.Raw
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Control.Monad.IO.Class (MonadIO)
import GDK.Draw (RenderLayers)
import System.Exit (exitSuccess)

initialise :: Config -- ^ Game config
           -> IO (SDL.Window, SDL.Renderer) -- ^ Returns the created window and renderer contexts
initialise config = do
    SDL.initialize [SDL.InitVideo]
    TTF.initialize
    IMG.initialize []
    let (w,h) = windowDimensions config
        title = windowTitle config
        windowConfig = SDL.defaultWindow { SDL.windowInitialSize = SDL.V2 (fromIntegral w) (fromIntegral h),
                                           SDL.windowMode = SDL.Windowed,
                                           SDL.windowResizable = False }
    window <- SDL.createWindow (T.pack title) windowConfig

    let rendererConfig = SDL.defaultRenderer { SDL.rendererType = SDL.AcceleratedVSyncRenderer,
                                               SDL.rendererTargetTexture = True }
    renderer <- SDL.createRenderer window (-1) rendererConfig

    return (window, renderer)

run :: forall w. 
     (Has w IO Time
     , Has w IO TextureMap
     , Has w IO Position
     , Get w IO Renderable
     , Set w IO Renderable
     , Members w IO Renderable)
     => w -- ^ Initial world state
     -> SDL.Renderer -- ^ SDL renderer context
     -> SDL.Window -- ^ SDL window context
     -> Config -- ^ Game config
     -> (Float -> System w ()) -- ^ World step function
     -> ([SDL.EventPayload] -> System w ()) -- ^ Event handler
     -> (SDL.Renderer -> FPS -> System w ()) -- ^ Draw function, receives the renderer and current FPS
     -> IO ()
run w r window c step eventHandler draw = do
    SDL.showWindow window
    let loop prevTicks prevPerf tickAcc fpsAcc = do
            ticks <- SDL.ticks
            perf <- SDL.Raw.getPerformanceCounter
            freq <- SDL.Raw.getPerformanceFrequency
            payload <- map SDL.eventPayload <$> SDL.pollEvents
            let quit = SDL.QuitEvent `elem` payload
                dt = ticks - prevTicks
                tickAcc' = tickAcc + dt
                avgFps = 1000.0 / (fromIntegral tickAcc' / fromIntegral fpsAcc)
                elapsed = fromIntegral (perf - prevPerf) / fromIntegral freq * 1000
            runSystem (eventHandler payload) w
            runSystem (do
                stepAnimations $ fromIntegral dt / 1000
                step $ fromIntegral dt / 1000) w
            SDL.rendererDrawColor r SDL.$= backgroundColor c
            SDL.clear r
            runSystem (draw r (round avgFps)) w
            SDL.present r
            SDL.delay $ floor ((1000 / fromIntegral (targetFPS c)) - elapsed)
            unless quit $ loop ticks perf tickAcc' (fpsAcc + 1)
    loop 0 0 0 0
    SDL.destroyRenderer r
    SDL.destroyWindow window
    TTF.quit
    IMG.quit
    SDL.quit
    exitSuccess

defaultConfig :: Config
defaultConfig = Config
    { windowTitle = "GDK Game"
    , windowDimensions = (800, 600)
    , backgroundColor = SDL.V4 0 0 0 255
    , targetFPS = 60
    }

makeWorld' :: [Name] -> Q [Dec]
makeWorld' cTypes = makeWorld "World" (cTypes ++ [''TextureMap, ''FontMap, ''Position, ''Time])

stepAnimations :: forall w. 
                (Has w IO Time
                , Has w IO TextureMap
                , Get w IO Renderable
                , Set w IO Renderable
                , Members w IO Renderable)
                => Float 
                -> System w ()
stepAnimations dt = cmapM $ \r -> do
    Time t <- get global
    TextureMap m <- get global
    let tex = textureRef r `Map.lookup` m
    case tex of
        Nothing -> return r
        Just tex' -> case animation tex' of
                Just a -> do
                    let trigger = floor (t / frameSpeed a) /= floor ((t + dt) / frameSpeed a)
                    if trigger then do
                        let frame = fromMaybe 0 (animationFrame r)
                            newFrame = (frame + 1) `mod` frameCount a
                        if newFrame == 0 then
                            return r { textureRef = next a, animationFrame = Just 0 }
                        else
                            return r { animationFrame = Just newFrame }
                    else return r
                Nothing -> return r
