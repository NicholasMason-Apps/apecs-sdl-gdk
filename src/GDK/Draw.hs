{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}


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
import GDK.Systems (getMaybe)
import Control.Monad.IO.Class (MonadIO)
import qualified Data.Vector.Mutable as MV
import qualified Data.Text as T
import qualified Data.Map as Map
import Linear
import Data.Word (Word8)
import Control.Monad (join, when)
import Data.Foldable (forM_)
import Data.Maybe (isNothing, fromJust)

{-|
Draw all 'Renderable' entities onto their appropriate layer.
This function is the typical draw function to pass to 'run', however you are free to implement your own
Every 'Renderable' Entity must have a 'Position' component. If a 'Renderable' Entity is set to not be visible, it is ignored
It is important to note that elements are drawn with top-left origin, and the x and y axes increase towards the right and down respectively.
-}
draw :: forall w.
      (Has w IO TextureMap
      , Get w IO Renderable
      , Get w IO FontMap
      , Get w IO Camera
      , Get w IO Position
      , Get w IO Config
      , Get w IO Layer
      , Has w IO IsVisible
      , Get w IO Colour)
      => SDL.Renderer
      -> FPS
      -> System w ()
draw renderer fps = do
    c <- get global
    Camera cam <- get global
    let (vw, vh) = windowDimensions c
        isInView :: V2 Float -> (Float,Float) -> Bool
        isInView pos (w,h) =
            let
                (V2 x y) = cam pos
                leftInView = x <= fromIntegral vw
                rightInView = x + w >= 0
                topInView = y <= fromIntegral vh
                bottomInView = y + h >= 0
            in
                leftInView && rightInView && topInView && bottomInView
    maxLayer <- cfold (\acc (_ :: Renderable, Position _, Layer n) -> max acc n) 0
    layerBuffers <- liftIO $ MV.replicateM (maxLayer + 1) (MV.new 1024) -- Preallocate mutable vectors for each layer
    layerCounts <- liftIO $ MV.replicate (maxLayer + 1) 0 -- Track the number of entities in each layer
    TextureMap tm <- get global
    FontMap fm <- get global
    -- Take an action to draw an entity on a given layer, and store it in the appropriate layer buffer
    let updateLayer :: Int -> System w () -> System w ()
        updateLayer i command = do
            buf <- liftIO $ MV.read layerBuffers i
            i' <- liftIO $ MV.read layerCounts i
            buf' <- if i' == MV.length buf then do
                    newBuf <- liftIO $ MV.grow buf (MV.length buf) -- Double the buffer size
                    liftIO $ MV.write layerBuffers i newBuf
                    return newBuf
                else return buf
            liftIO $ MV.write buf' i' command
            liftIO $ MV.write layerCounts i (i' + 1)
    -- Iterate through all Renderable entities, generate their draw commands and store them in the appropriate layer buffer
    cmapM_ $ \(r, Position pos, Layer n, e) -> case r of
        -- For each Renderable type, we do the following:
        -- 1. Check if the entity is visible by using a Proxy on the IsVisible component, if not we skip it
        -- 2. For Textures and Text, we check if their referenced resource exists in the TextureMap/FontMap, if not we skip it
        -- 3. Check if the entity is in view of the camera, if not we skip it
        -- 4. If the entity has a Colour component, we use that colour to draw it, otherwise we default to black
        -- 5. Add the drawing action to the appropriate layer buffer
        Texture t -> getMaybe e (Proxy @IsVisible) >>= \res -> when (isNothing res || (let IsVisible visible = fromJust res in visible)) $ case Map.lookup (textureRef t) tm of
            Just td -> do
                info <- liftIO $ SDL.queryTexture (texture td)
                IsVisible b <- get e
                let w = fromIntegral $ SDL.textureWidth info
                    h = fromIntegral $ SDL.textureHeight info
                when (isInView pos (w,h) && b) $ updateLayer n (drawTexture renderer td (cam pos) (animationFrame t))
            Nothing -> return () -- Texture not found, skip drawing
        Text t -> getMaybe e (Proxy @IsVisible) >>= \res -> when (isNothing res || (let IsVisible visible = fromJust res in visible)) $ case Map.lookup (fontRef t) fm of
            Just font -> do
                IsVisible b <- get e
                (w,h) <- liftIO $ TTF.size font (T.pack $ displayText t)
                when (isInView pos (fromIntegral w, fromIntegral h) && b) $ exists e (Proxy @Colour) >>= \c' -> if c' then
                    get e >>= \(Colour col) -> updateLayer n (drawText renderer t font (cam pos) col)
                else
                    updateLayer n (drawText renderer t font (cam pos) (V4 0 0 0 255))
            Nothing -> return () -- Font not found, skip drawing
        Point p -> getMaybe e (Proxy @IsVisible) >>= \res -> when (isNothing res || (let IsVisible visible = fromJust res in visible)) $ do
            IsVisible b <- get e
            when (isInView pos (0,0) && b) $ exists e (Proxy @Colour) >>= \c' -> if c' then
                get e >>= \(Colour col) -> updateLayer n (drawPoint renderer p (cam pos) col)
            else
                updateLayer n (drawPoint renderer p (cam pos) (V4 0 0 0 255))
        Line l -> getMaybe e (Proxy @IsVisible) >>= \res -> when (isNothing res || (let IsVisible visible = fromJust res in visible)) $ do
            IsVisible b <- get e
            when (isInView pos (lineX l, lineY l) && b) $ exists e (Proxy @Colour) >>= \c' -> if c' then
                get e >>= \(Colour col) -> updateLayer n (drawLine renderer l (cam pos) col)
            else
                updateLayer n (drawLine renderer l (cam pos) (V4 0 0 0 255))
        Rectangle r' -> getMaybe e (Proxy @IsVisible) >>= \res -> when (isNothing res || (let IsVisible visible = fromJust res in visible)) $ do
            IsVisible b <- get e
            let (V2 w h) = rectSize r'
            when (isInView pos (w,h) && b) $ exists e (Proxy @Colour) >>= \c' -> if c' then
                get e >>= \(Colour col) -> updateLayer n (drawRect renderer r' (cam pos) col)
            else
                updateLayer n (drawRect renderer r' (cam pos) (V4 0 0 0 255))
        FilledRectangle r' -> getMaybe e (Proxy @IsVisible) >>= \res -> when (isNothing res || (let IsVisible visible = fromJust res in visible)) $ do
            IsVisible b <- get e
            let (V2 w h) = rectSize r'
            when (isInView pos (w,h) && b) $ exists e (Proxy @Colour) >>= \c' -> if c' then
                get e >>= \(Colour col) -> updateLayer n (drawFilledRect renderer r' (cam pos) col)
            else
                updateLayer n (drawFilledRect renderer r' (cam pos) (V4 0 0 0 255))
    case showFPS c of
        Just ref -> case Map.lookup ref fm of
            Just font -> updateLayer maxLayer (drawText renderer (RenText ref (show fps ++ " FPS")) font (V2 10 10) (V4 0 255 0 255))
            Nothing -> return () -- Font not found, skip drawing FPS
        Nothing -> return () -- Not showing FPS, skip drawing it
    -- Iterate through each layer buffer and execute the drawing commands in order
    forM_ [0..maxLayer] $ \i -> do
        buf <- liftIO $ MV.read layerBuffers i
        count <- liftIO $ MV.read layerCounts i
        forM_ [0..(count - 1)] $ \j -> do
            join $ liftIO $ MV.read buf j -- Execute the drawing command

-- draw renderer fps = do
--     c <- get global
--     (Camera cam) <- get global
--     let (ww,wh) = windowDimensions c
--         size = V2 (fromIntegral ww) (fromIntegral wh)
--         isInView :: V2 Float -> (Float,Float) -> Bool
--         isInView (V2 x y) (w,h) =
--             let
--                 (V2 vw vh) = size
--                 leftInView = x <= fromIntegral vw
--                 rightInView = x + w >= 0
--                 topInView = y <= fromIntegral vh
--                 bottomInView = y + h >= 0
--             in
--                 leftInView && rightInView && topInView && bottomInView
--     maxLayer <- cfold (\acc r -> case r of
--         Texture t -> max acc (if textureVisible t then textureLayer t else acc)
--         Text t -> max acc (if textVisible t then textLayer t else acc)
--         Point p -> max acc (if pointVisible p then pointLayer p else acc)
--         Line l -> max acc (if lineVisible l then lineLayer l else acc)
--         Rectangle r' -> max acc (if rectVisible r' then rectLayer r' else acc)
--         FilledRectangle r' -> max acc (if rectVisible r' then rectLayer r' else acc)
--         ) 0
--     layers <- V.replicateM (maxLayer + 1) (do
--         tex <- SDL.createTexture renderer SDL.RGBA8888 SDL.TextureAccessTarget size
--         SDL.textureBlendMode tex SDL.$= SDL.BlendAlphaBlend
--         SDL.rendererRenderTarget renderer SDL.$= Just tex
--         SDL.rendererDrawColor renderer SDL.$= SDL.V4 0 0 0 0
--         SDL.clear renderer
--         return tex)
--     TextureMap tm <- get global
--     FontMap fm <- get global
--     cmapM_ $ \(r, Position pos) -> case r of
--         Texture t -> when (textureVisible t) $ case Map.lookup (textureRef t) tm of
--             Just td -> do
--                 info <- liftIO $ SDL.queryTexture (texture td)
--                 let layerTex = layers V.! textureLayer t
--                     w = fromIntegral $ SDL.textureWidth info
--                     h = fromIntegral $ SDL.textureHeight info
--                 when (isInView (cam pos) (w,h)) $ do
--                     SDL.rendererRenderTarget renderer SDL.$= Just layerTex
--                     drawTexture renderer td (cam pos) (animationFrame t)
--             Nothing -> return () -- Texture not found, skip drawing
--         Text t -> when (textVisible t) $ case Map.lookup (fontRef t) fm of
--             Just font -> do
--                 let layerTex = layers V.! textLayer t
--                 (w,h) <- liftIO $ TTF.size font (T.pack $ displayText t)
--                 when (isInView (cam pos) (fromIntegral w, fromIntegral h)) $ do
--                     SDL.rendererRenderTarget renderer SDL.$= Just layerTex
--                     drawText renderer t font (cam pos)
--             Nothing -> return () -- Font not found, skip drawing
--         Point p -> when (pointVisible p) $ do
--             let layerTex = layers V.! pointLayer p
--             when (isInView (cam pos) (0,0)) $ do
--                 SDL.rendererRenderTarget renderer SDL.$= Just layerTex
--                 drawPoint renderer p (cam pos)
--         Line l -> when (lineVisible l) $ do
--             let layerTex = layers V.! lineLayer l
--             when (isInView (cam pos) (lineX l, lineY l)) $ do
--                 SDL.rendererRenderTarget renderer SDL.$= Just layerTex
--                 drawLine renderer l (cam pos)
--         Rectangle r' -> when (rectVisible r') $ do
--             let layerTex = layers V.! rectLayer r'
--                 (V2 w h) = rectSize r'
--             when (isInView (cam pos) (w,h)) $ do
--                 SDL.rendererRenderTarget renderer SDL.$= Just layerTex
--                 drawRect renderer r' (cam pos)
--         FilledRectangle r' -> when (rectVisible r') $ do
--             let layerTex = layers V.! rectLayer r'
--                 (V2 w h) = rectSize r'
--             when (isInView (cam pos) (w,h)) $ do
--                 SDL.rendererRenderTarget renderer SDL.$= Just layerTex
--                 drawFilledRect renderer r' (cam pos)
--     case showFPS c of
--         Just ref -> case Map.lookup ref fm of
--             Just font -> do
--                 let layerTex = layers V.! maxLayer
--                 SDL.rendererRenderTarget renderer SDL.$= Just layerTex
--                 drawText renderer (RenText ref (show fps ++ " FPS") (SDL.V4 255 255 255 255) (maxLayer + 1) True) font (SDL.V2 10 10)
--             Nothing -> return () -- Font not found, skip drawing FPS
--         Nothing -> return () -- Not showing FPS, skip drawing it

--     SDL.rendererRenderTarget renderer SDL.$= Nothing
--     V.mapM_ (\t -> do
--         SDL.copy renderer t Nothing Nothing
--         SDL.destroyTexture t) layers

drawLine :: SDL.Renderer -> RenLine -> V2 Float -> V4 Word8 -> System w ()
drawLine r l pos col = do
    SDL.rendererDrawColor r SDL.$= col
    SDL.drawLine r (SDL.P $ floor <$> pos) (SDL.P $ floor <$> (pos + V2 (lineX l) (lineY l)))

drawPoint :: SDL.Renderer -> RenPoint -> V2 Float -> V4 Word8 -> System w ()
drawPoint r p pos col = do
    SDL.rendererDrawColor r SDL.$= col
    SDL.drawPoint r (SDL.P $ floor <$> pos)

drawRect :: SDL.Renderer -> RenRectangle -> V2 Float -> V4 Word8 -> System w ()
drawRect r rect pos col = do
    SDL.rendererDrawColor r SDL.$= col
    SDL.drawRect r $ Just (SDL.Rectangle (SDL.P (floor <$> pos)) (floor <$> rectSize rect))


drawFilledRect :: SDL.Renderer -> RenRectangle -> V2 Float -> V4 Word8 -> System w ()
drawFilledRect r rect pos col = do
    SDL.rendererDrawColor r SDL.$= col
    SDL.fillRect r $ Just (SDL.Rectangle (SDL.P (floor <$> pos)) (floor <$> rectSize rect))

{-|
Draw a 'Texture' given its 'TextureData' and 'Position'
If either the 'TextureData' does not have an 'Animation' or no frame index is provided, the texture will be drawn as if it was static.
-}
drawTexture :: SDL.Renderer -> TextureData -> V2 Float -> Maybe Int -> System w ()
drawTexture r (TextureData t (Just a)) pos (Just n) = do
    info <- liftIO $ SDL.queryTexture t
    let w = SDL.textureWidth info
        h = SDL.textureHeight info
        fw = w `div` fromIntegral (frameCount a)
        srcRect = SDL.Rectangle (SDL.P (V2 (fromIntegral n * fw) 0)) (V2 fw h)
        dstRect = SDL.Rectangle (SDL.P (floor <$> pos)) (V2 fw h)
    liftIO $ SDL.copy r t (Just srcRect) (Just dstRect)
drawTexture r (TextureData t _) pos _ = do
    info <- liftIO $ SDL.queryTexture t
    let w = SDL.textureWidth info
        h = SDL.textureHeight info
        pos' = SDL.Rectangle (SDL.P (floor <$> pos)) (V2 w h)
    liftIO $ SDL.copy r t Nothing (Just pos')

-- | Draw text given its 'RenText', 'Font' and 'Position'
drawText :: SDL.Renderer -> RenText -> TTF.Font -> V2 Float -> V4 Word8 -> System w ()
drawText r t font pos col = do
    (tex, size) <- generateSolidText r font col (displayText t)
    SDL.copy r tex Nothing (Just $ SDL.Rectangle (SDL.P (floor <$> pos)) (fromIntegral <$> size))
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
