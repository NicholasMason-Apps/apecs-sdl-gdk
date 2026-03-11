{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

module GDK.Draw (draw, 
                drawTexture, 
                drawText,
                drawLine,
                drawPoint,
                drawRect,
                drawFilledRect) where

import qualified SDL
import qualified SDL.Font as TTF
import Apecs
import GDK.Types
import GDK.Texture
import GDK.Font (RenText(..), FontMap(..))
import Control.Monad.IO.Class (MonadIO)
import qualified Data.Vector as V
import qualified Data.Text as T
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Control.Monad (when)
import Linear

{-|
Draw all 'Renderable' entities onto their appropriate layer.
This function is the typical draw function to pass to 'run', however you are free to implement your own
For 'Texture' and 'Text', they need to also have a 'Position' component.
It is important to note that elements are drawn with top-left origin, and the x and y axes increase towards the right and down respectively.
-}
draw :: forall w.
      (Has w IO TextureMap
      , Get w IO Window
      , Get w IO Renderable
      , Get w IO FontMap
      , Get w IO Camera
      , Get w IO Position
      , Get w IO Config)
      => SDL.Renderer
      -> FPS
      -> System w ()
draw renderer fps = do
    Window win <- get global
    c <- get global
    let window = fromMaybe (error "Window not initialised") win
    size <- SDL.get $ SDL.windowSize window
    Camera cam <- get global
    let 
        isInView :: Position -> (Float,Float) -> Bool
        isInView (Position (V2 x y)) (w,h) = let
                (V2 vw vh) = size
                (V2 cx cy) = cam
                leftInView = x <= fromIntegral cx + fromIntegral vw
                rightInView = x + w >= fromIntegral cx
                topInView = y <= fromIntegral cy + fromIntegral vh
                bottomInView = y + h >= fromIntegral cy
            in
                leftInView && rightInView && topInView && bottomInView   
    maxLayer <- cfold (\acc r -> case r of
        Texture t -> max acc (textureLayer t)
        Text t -> max acc (textLayer t)
        Point p -> max acc (pointLayer p)
        Line l -> max acc (lineLayer l)
        Rectangle r -> max acc (rectLayer r)
        FilledRectangle r -> max acc (rectLayer r)
        ) 0
    layers <- V.replicateM maxLayer (SDL.createTexture renderer SDL.RGBA8888 SDL.TextureAccessTarget size)
    TextureMap tm <- get global
    FontMap fm <- get global
    cmapM_ $ \(r, pos) -> case r of
        Texture t -> case Map.lookup (textureRef t) tm of
            Just td -> do
                info <- liftIO $ SDL.queryTexture (texture td)
                let layerTex = layers V.! textureLayer t
                    w = fromIntegral $ SDL.textureWidth info
                    h = fromIntegral $ SDL.textureHeight info
                when (isInView pos (w,h)) $ do
                    SDL.rendererRenderTarget renderer SDL.$= Just layerTex
                    drawTexture renderer td pos (animationFrame t)
            Nothing -> return () -- Texture not found, skip drawing
        Text t -> case Map.lookup (fontRef t) fm of
            Just font -> do
                let layerTex = layers V.! textLayer t
                (w,h) <- liftIO $ TTF.size font (T.pack $ displayText t)
                when (isInView pos (fromIntegral w, fromIntegral h)) $ do
                    SDL.rendererRenderTarget renderer SDL.$= Just layerTex
                    drawText renderer t font pos
            Nothing -> return () -- Font not found, skip drawing
        Point p -> do
            let layerTex = layers V.! pointLayer p
            when (isInView pos (0,0)) $ do
                SDL.rendererRenderTarget renderer SDL.$= Just layerTex
                drawPoint renderer p pos
        Line l -> do
            let layerTex = layers V.! lineLayer l
            when (isInView pos (lineX l, lineY l)) $ do
                SDL.rendererRenderTarget renderer SDL.$= Just layerTex
                drawLine renderer l pos
        Rectangle r' -> do
            let layerTex = layers V.! rectLayer r'
                (V2 w h) = rectSize r'
            when (isInView pos (w,h)) $ do
                SDL.rendererRenderTarget renderer SDL.$= Just layerTex
                drawRect renderer r' pos
        FilledRectangle r' -> do
            let layerTex = layers V.! rectLayer r'
                (V2 w h) = rectSize r'
            when (isInView pos (w,h)) $ do
                SDL.rendererRenderTarget renderer SDL.$= Just layerTex
                drawFilledRect renderer r' pos
    case showFPS c of
        Just ref -> case Map.lookup ref fm of
            Just font -> do
                let layerTex = layers V.! maxLayer
                SDL.rendererRenderTarget renderer SDL.$= Just layerTex
                drawText renderer (RenText ref (show fps ++ " FPS") (SDL.V4 255 255 255 255) (maxLayer + 1)) font (Position $ SDL.V2 10 10)
            Nothing -> return () -- Font not found, skip drawing FPS
        Nothing -> return () -- Not showing FPS, skip drawing it
        
    SDL.rendererRenderTarget renderer SDL.$= Nothing
    V.mapM_ (\t -> do
        SDL.copy renderer t Nothing Nothing
        SDL.destroyTexture t) layers

drawLine :: SDL.Renderer -> RenLine -> Position -> System w ()
drawLine r l (Position pos) = do
    SDL.rendererDrawColor r SDL.$= lineColour l
    SDL.drawLine r (SDL.P $ round <$> pos) (SDL.P $ round <$> V2 (lineX l) (lineY l))

drawPoint :: SDL.Renderer -> RenPoint -> Position -> System w ()
drawPoint r p (Position pos) = do
    SDL.rendererDrawColor r SDL.$= pointColour p
    SDL.drawPoint r (SDL.P $ round <$> pos)

drawRect :: SDL.Renderer -> RenRectangle -> Position -> System w ()
drawRect r rect (Position pos) = do
    SDL.rendererDrawColor r SDL.$= rectColour rect
    SDL.drawRect r $ Just (SDL.Rectangle (SDL.P (round <$> pos)) (round <$> rectSize rect))


drawFilledRect :: SDL.Renderer -> RenRectangle -> Position -> System w ()
drawFilledRect r rect (Position pos) = do
    SDL.rendererDrawColor r SDL.$= rectColour rect
    SDL.fillRect r $ Just (SDL.Rectangle (SDL.P (round <$> pos)) (round <$> rectSize rect))

{-|
Draw a 'Texture' given its 'TextureData' and 'Position'
If either the 'TextureData' does not have an 'Animation' or no frame index is provided, the texture will be drawn as if it was static.
-}
drawTexture :: SDL.Renderer -> TextureData -> Position -> Maybe Int -> System w ()
drawTexture r (TextureData t (Just a)) (Position pos) (Just n) = do
    info <- liftIO $ SDL.queryTexture t
    let w = fromIntegral $ SDL.textureWidth info
        h = fromIntegral $ SDL.textureHeight info
        fw = w `div` fromIntegral (frameCount a)
        srcRect = SDL.Rectangle (SDL.P (V2 (fromIntegral n * fw) 0)) (V2 fw h)
        dstRect = SDL.Rectangle (SDL.P (round <$> pos)) (V2 fw h)
    liftIO $ SDL.copy r t (Just srcRect) (Just dstRect)
drawTexture r (TextureData t _) (Position pos) _ = do
    info <- liftIO $ SDL.queryTexture t
    let w = fromIntegral $ SDL.textureWidth info
        h = fromIntegral $ SDL.textureHeight info
        pos' = SDL.Rectangle (SDL.P (round <$> pos)) (V2 w h)
    liftIO $ SDL.copy r t Nothing (Just pos')

-- | Draw text given its 'RenText', 'Font' and 'Position'
drawText :: SDL.Renderer -> RenText -> TTF.Font -> Position -> System w ()
drawText r t font (Position pos) = do
    (tex, size) <- generateSolidText r font (textColour t) (displayText t)
    SDL.copy r tex Nothing (Just $ SDL.Rectangle (SDL.P (round <$> pos)) (fromIntegral <$> size))
    SDL.destroyTexture tex

generateSolidText :: MonadIO m => SDL.Renderer -> TTF.Font -> TTF.Color -> String -> m (SDL.Texture, SDL.V2 Int)
generateSolidText r font = generateText r font (TTF.solid font)

generateText :: MonadIO m => SDL.Renderer -> TTF.Font -> (TTF.Color -> T.Text -> m SDL.Surface) -> TTF.Color -> String -> m (SDL.Texture, SDL.V2 Int)
generateText r font f col str = do
    let t = T.pack str
    surface <- f col t
    tex <- liftIO $ SDL.createTextureFromSurface r surface
    SDL.freeSurface surface
    (w,h) <- liftIO $ TTF.size font t
    return (tex, V2 w h)
