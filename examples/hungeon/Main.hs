module Main (main) where

import Apecs
import Types
import Systems
import Linear
import qualified SDL
import Control.Monad (when)
import Input
import Cadence

main :: IO ()
main = do
    w <- initWorld
    (window, renderer) <- initialise w (defaultConfig { windowDimensions = (1280, 720), windowTitle = "Hungeon", backgroundColor = V4 37 19 26 255, targetFPS = Limited 60, showFPS = Just "Roboto-Regular" })
    runWith w (initialize renderer)
    runSystem (do
        settings <- get global :: System' Settings
        when (fullscreen settings) $ do
            liftIO $ SDL.setWindowMode window SDL.FullscreenDesktop
            viewport <- SDL.get (SDL.rendererViewport renderer)
            let (w',h') = case viewport of
                    Just (SDL.Rectangle _ (SDL.V2 w h)) -> (w, h)
                    Nothing -> (1280, 720)
            (SDL.$=) (SDL.rendererScale renderer) (V2 (fromIntegral w' / 1280) (fromIntegral h' / 720))
            modify global $ \(Viewport _) -> Viewport (fromIntegral w', fromIntegral h')
        ) w
    run w renderer window step handlePayload draw