{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

module GDK.Systems (initialise, run, makeWorld', stepAnimations) where

import Apecs
import GDK.Types
import GDK.Texture
import GDK.Font (FontMap(..), loadFont)
import qualified SDL
import qualified SDL.Font as TTF
import qualified SDL.Image as IMG
import qualified Data.Text as T
import Language.Haskell.TH.Syntax
import Control.Monad (unless)
import qualified SDL.Raw
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import System.Exit (exitSuccess)
import Linear

-- | Initialise the SDL window and renderer
initialise :: forall w.
            (Set w IO Renderer
            , Set w IO Window
            , Set w IO Config
            , Set w IO FontMap)
           => w
           -> Config -- ^ Game config
           -> IO (SDL.Window, SDL.Renderer) -- ^ Returns the created window and renderer contexts
initialise world config = do
    SDL.initialize [SDL.InitVideo]
    TTF.initialize
    IMG.initialize []
    let (w,h) = windowDimensions config
        title = windowTitle config
        windowConfig = SDL.defaultWindow { SDL.windowInitialSize = V2 (fromIntegral w) (fromIntegral h),
                                           SDL.windowMode = SDL.Windowed,
                                           SDL.windowResizable = False }
    window <- SDL.createWindow (T.pack title) windowConfig
    runWith world (set global $ Window $ Just window)
    runWith world (set global config)

    let rendererType = case targetFPS config of
            VSync -> SDL.AcceleratedVSyncRenderer
            _ -> SDL.AcceleratedRenderer
        rendererConfig = SDL.defaultRenderer { SDL.rendererType = rendererType,
                                               SDL.rendererTargetTexture = True }
    renderer <- SDL.createRenderer window (-1) rendererConfig
    runWith world (set global $ Renderer $ Just renderer)
    runWith world (loadFont "resources/Roboto-Regular.ttf" "Roboto-Regular" 24)

    return (window, renderer)

-- | Main game loop
run :: forall w.
     (Has w IO Time
     , Has w IO TextureMap
     , Get w IO Renderable
     , Get w IO Config)
     => w -- ^ World state
     -> SDL.Renderer -- ^ SDL renderer context
     -> SDL.Window -- ^ SDL window context
     -> (Float -> System w ()) -- ^ World step function
     -> ([SDL.EventPayload] -> System w ()) -- ^ Event handler
     -> (SDL.Renderer -> FPS -> System w ()) -- ^ Draw function, receives the renderer and current FPS
     -> IO ()
run w r window step eventHandler draw = do
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
                let dt' = fromIntegral dt / 1000
                modify global $ \(Time t) -> Time (t + dt')
                stepAnimations dt'
                step dt') w
            runSystem (do
                c <- get global
                liftIO $ SDL.rendererDrawColor r SDL.$= backgroundColor c) w
            SDL.clear r
            runSystem (draw r (round avgFps)) w
            SDL.present r
            runSystem (do
                c <- get global
                case targetFPS c of
                    Limited fps -> let
                            frameTime = 1000 / fromIntegral fps
                            delayTime = max 0 (frameTime - elapsed)
                        in SDL.delay $ floor delayTime
                    _ -> return ()) w
            unless quit $ loop ticks perf tickAcc' (fpsAcc + 1)
    loop 0 0 0 0
    SDL.destroyRenderer r
    SDL.destroyWindow window
    TTF.quit
    IMG.quit
    SDL.quit
    exitSuccess

makeWorld' :: [Name] -> Q [Dec]
makeWorld' cTypes = makeWorld "World" (cTypes ++ [''TextureMap, ''FontMap, ''Position, ''Time, ''Renderable, ''Renderer, ''Window, ''Camera, ''Config])

stepAnimations :: forall w.
                (Has w IO Time
                , Has w IO TextureMap
                , Get w IO Renderable
                , Set w IO Renderable
                , Members w IO Renderable)
                => Float
                -> System w ()
stepAnimations dt = cmapM $ \r -> do
    case r of
        Texture t -> do
            Time t' <- get global
            TextureMap m <- get global
            let tex = textureRef t `Map.lookup` m
            case tex of
                Nothing -> return r
                Just tex' -> case animation tex' of
                        Just a -> do
                            let trigger = floor (t' / frameSpeed a) /= floor ((t' + dt) / frameSpeed a)
                            if trigger then do
                                let frame = fromMaybe 0 (animationFrame t)
                                    newFrame = (frame + 1) `mod` frameCount a
                                if newFrame == 0 && next a /= "" then
                                    return $ Texture t { textureRef = next a, animationFrame = Just 0 }
                                else if newFrame == 0 then
                                    return $ Texture t { animationFrame = Just frame }
                                else
                                    return $ Texture t { animationFrame = Just newFrame }
                            else return r
                        Nothing -> return r
        _ -> return r
