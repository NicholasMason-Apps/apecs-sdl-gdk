= Introduction

This document breaks down a very small game written using `apecs-sdl-gdk`. We will be making noughts-and-crosses (or tic-tac-toe depending on where you are from). The main focus of this document will be:

1. To highlight how to use `apecs-sdl-gdk`
2. (Briefly) introduce how to use Apecs
3. Explain what some of the `apecs-sdl-gdk` functions do under-the-hood
4. Highlight some useful tips for game development using Apecs and Haskell from my experience

Whilst you can run the game by cloning this repository and running `stack run noughts-and-crosses`, I **strongly** recommend you write code yourself as you follow along, as it will help a lot to solidfy the basic concepts. Additionally, Apecs itself has a [tutorial](https://github.com/jonascarpay/apecs/blob/master/examples/Shmup.md), which whilst it does not use `apecs-sdl-gdk`, I also highly recommend walking through it as well for a more comprehensive overview of how to use Apecs specifically.

= Types and Top-Level Declarations

With all that said, let's start writing some code! 

== Language Extensions and Imports

To start with, we will need some language extensions since Apecs makes use of a lot of extensions for its inner workings.

> {-# LANGUAGE DataKinds                  #-}
> {-# LANGUAGE FlexibleContexts           #-}
> {-# LANGUAGE FlexibleInstances          #-}
> {-# LANGUAGE MultiParamTypeClasses      #-}
> {-# LANGUAGE ScopedTypeVariables        #-}
> {-# LANGUAGE TemplateHaskell            #-}
> {-# LANGUAGE TypeApplications           #-}
> {-# LANGUAGE TypeFamilies               #-}
> {-# LANGUAGE GeneralizedNewtypeDeriving #-}

Next, we need some imports. To work with Apecs and create some game logic, we simply import Apecs directly

> import Apecs

Following this, we import `apecs-sdl-gdk`. The only thing to note here is that, since we will also reuse the `initialise` function name later on, we also import `GDK.Systems` qualified.

> import GDK.Systems hiding (initialise)
> import qualified GDK.Systems as GDK
> import GDK.Types
> import GDK.Draw

Next we want to import `linear` for working with multi-dimensional vectors.

> import Linear

Finally, we will import some other utilities to help us later:

> import qualified Data.Set as Set
> import qualified SDL
> import Control.Monad (when, void)
> import System.Random
> import Data.Maybe (isJust)
> import Data.Foldable (foldl')

== Creating Types and Components

Now with the imports complete, we can define some Components needed for our game logic! If you are not familiar with the notion of an ECS, I would recommend reading the [Apecs paper](https://github.com/jonascarpay/apecs/blob/master/apecs/prepub.pdf) (as well as the other resources linked in the `README.md`) to get an understanding of how it works. In short, an ECS works by:

- Having you create Entities which inhabit the game world
- Tying to Entities a composition of components, used to represent properties about that Entity
- Writing Systems to manipulate all entities with a specific subset of components


For example:

- Suppose we have three entities
- One entity holds `Player` and `Position` components, and the other two hold `Enemy` and `Position` components
- We could then write a System which updates **all** Entities which have a `Position` component to move them
- We could then write another which updates only the Entity with the `Player` component to decrement some internal health.

In Apecs, Components come in the form of Haskell types being instances of the Component class, and each have their own store specified. A store is simply the data structure used for that component. There are four different types of Stores:
- `Map` - this is the most common one you will use. It allows for each entity to potentially have an instance of this component. 
- `Global` - allows data to be stored irrespective to any entity. Specifically, `Global` components are shared across *all* Entities, meaning *any* Entity can be used to access it. 
- `Unique` - allows at most one entity to hold the component
- `Cache` - wraps around another store to allow for O(1) reads and writes

For the purpose of noughts-and-crosses, we will need to store the following:
- The current board state - a 3x3 list of the board. For each entry within that list, we will use a Sum type for pattern matching
- The state of the game - a simple Sum type to represent who's Turn it is within the game
- Mouse inputs from the player - using a `Set` for mouse buttons and a 2D vector for the mouse's position in the window
- A countdown timer - a simple floating-point timer which we will decrement each frame until it reaches 0, and then do something
- The noughts or crosses to draw on the screen - more on this below

For the first three, all of them will be stored as a `Global` component, since all of these we want to be able to access irrespective of any specific Entity.

For the countdown timer, we will store it using `Unique`, since for our example we only ever want one timer to be active at any time.

The code for the first four are shown below

> data Turn = PlayerTurn | AITurn | Win | Reset | CheckWinPlayer | CheckWinAI deriving Show
> instance Semigroup Turn where
>     _ <> t2 = t2
> instance Monoid Turn where
>     mempty = PlayerTurn
> instance Component Turn where type Storage Turn = Global Turn
> 
> data BoardEntry = Nought | Cross | Empty deriving (Show, Eq)
> instance Semigroup BoardEntry where
>     Empty <> b = b
>     b <> Empty = b
>     b <> _ = b
> 
> newtype Board = Board [[BoardEntry]] deriving Show
> instance Semigroup Board where
>     b1 <> b2 = b1 <> b2
> instance Monoid Board where
>     mempty = Board $ [
>             [Empty, Empty, Empty],
>             [Empty, Empty, Empty],
>             [Empty, Empty, Empty]
>         ]
> instance Component Board where type Storage Board = Global Board
> 
> newtype MousePosition = MousePosition (V2 Float)
> instance Semigroup MousePosition where
>     (MousePosition m1) <> (MousePosition m2) = MousePosition (m1 + m2)
> instance Monoid MousePosition where
>     mempty = MousePosition (V2 0 0) 
> instance Component MousePosition where type Storage MousePosition = Global MousePosition
> 
> newtype MouseButtons = MouseButtons (Set.Set SDL.MouseButton)
> instance Semigroup MouseButtons where
>     (MouseButtons mbs1) <> (MouseButtons mbs2) = MouseButtons (mbs1 `Set.union` mbs2)
> instance Monoid MouseButtons where
>     mempty = MouseButtons Set.empty
> instance Component MouseButtons where type Storage MouseButtons = Global MouseButtons
> 
> newtype Countdown = Countdown Float deriving (Show, Eq, Num)
> instance Component Countdown where type Storage Countdown = Unique Countdown 

For the storing of the things to draw, we will make use of the `Renderable`, `Position`, `Colour`, and `Layer` components exposed by `apces-sdl-gdk`. 

== Explaining a subset of `apecs-sdl-gdk`

A subset of `Renderable`, and `Position`, `Layer` and `Colour` are shown below.

< data RenTexture = RenTexture
<     { textureRef :: String
<     -- ^ Identifier for the texture to render
<     , animationFrame :: Maybe Int
<     -- ^ Frame index for animations, if applicable
<     } deriving (Eq, Show)
< 
< -- | Component used to tag a single Entity with data for it to be rendered
< data Renderable = Texture RenTexture
<                 | Text RenText
<                 | Point RenPoint
<                 | Line RenLine
<                 | Rectangle RenRectangle
<                 | FilledRectangle RenRectangle
<                 deriving (Eq, Show)
< instance Component Renderable where type Storage Renderable = Map Renderable
< 
< newtype Position = Position (V2 Float) deriving (Show, Eq)
< instance Component Position where type Storage Position = Map Position
< 
< -- | Associate a colour with an entity for rendering. If no colour is supplied, it will default to black
< newtype Colour = Colour (V4 Word8) deriving (Show, Eq)
< instance Component Colour where type Storage Colour = Map Colour
< 
< -- | Layer Component for draw ordering, higher layers are drawn on top of lower layers. Indexing starts at 0
< newtype Layer = Layer Int deriving (Show, Eq, Ord)
< instance Component Layer where type Storage Layer = Map Layer

`Renderable` is used to tag a single Entity with data for it to be drawn, using a Sum type to pattern match on what is to be drawn. Each constructor of `Renderable` then has a Record to store data about it. The Records have many similarities, but also some naunces:

- Only `RenTexture` stores a `Maybe Int` to represent which frame of an animation the texture is on
- Both `RenTexture` and `RenText` store a reference String, which is used as a key for accessing the `TextureMap` and `FontMap` respectively. This reduces memory usage as we abide by the flyweight pattern

`Position` simply represents the position of the Entity with 2D space. An important thing to note is that in SDL, all things are drawn with the top-left being the point of origin, and the x and y axes increase to the right and down respectively.

`Layer` represents the layer that the Entity will be drawn on. As the comment says, higher layers are drawn on lower layers.

`Colour` simply attaches a colour to Entities.

It is important to note that `Renderable`, `Position`, and `Layer` are required for Entities to be drawn. `IsVisible` and `Colour` are optional

- If `IsVisible` is not attached to the Entity, it will default to assuming the Entity is visible.
    - The purpose of `IsVisible` is to allow control over what is drawn more explicitly, e.g. with different game scenes#
- If `Colour` is not supplied, the colour will default to black
    - Note `Colour` is not needed for Entities whose `Renderable` component is a `Texture`

For loading textures or fonts into their respective maps, `apecs-sdl-gdk` exposes the following two functions:

< -- | Load a font into the 'FontMap'
< loadFont :: (..) => FilePath -> String -> Int -> SystemT w m ()
< loadFont path ident size = ...
< 
< -- | Loads a texture into the 'TextureMap' with an associated identifier and optional animation data
< loadTexture :: (..) => SDL.Renderer -> FilePath -> String -> Maybe Animation -> SystemT w m ()
< loadTexture r path ident anim = ...

The actual drawing of everything can either be done yourself by writing your own code, or using `draw` exposed by `apecs-sdl-gdk`. `draw` handles the entire rendering pipeline for you by making use of the `Renderable`, `Position`, `Layer`, and `Colour` components and allows you to simply write game logic.

Alongside `Renderable`, `Position`, `Colour`, and `Layer`, `apecs-sdl-gdk` also exposes some other important components. The first of which is `Config`

< -- | Configuration settings for the game upon initialisation
< data Config = Config
<     {
<         windowTitle :: String, -- ^ Title of the game window
<         windowDimensions :: (Int, Int), -- ^ Width and height of the game window in pixels
<         backgroundColor :: V4 Word8, -- ^ Background color of the game window as an RGBA value
<         targetFPS :: TargetFPS, -- ^ Desired FPS for the game loop
<         showFPS :: Maybe String -- ^ Whether to display the current FPS on the screen, and if so, the font to use
<     }
< instance Semigroup Config where
<     _ <> c2 = c2
< instance Monoid Config where
<     mempty = defaultConfig
< instance Component Config where type Storage Config = Global Config 

`Config` is used to configure the game window, and is passed during initialisation, and then stored globally such that we can access it in the future if needed.

A default config is also exposed to get code running quicker:

< defaultConfig :: Config
< defaultConfig = Config
<     { windowTitle = "GDK Game"
<     , windowDimensions = (800, 600)
<     , backgroundColor = V4 255 255 255 255
<     , targetFPS = VSync
<     , showFPS = Just "Roboto-Regular"
<     }

Following `Config`, there are these other Components:

< newtype Camera = Camera { camFunc :: V2 Float -> V2 Float }
< instance Semigroup Camera where
<     (Camera f1) <> (Camera f2) = Camera $ \pos -> f1 (f2 pos)
< instance Monoid Camera where
<     mempty = Camera id
< instance Component Camera where type Storage Camera = Global Camera
< 
< newtype Time = Time Float deriving (Show, Eq, Num)
< instance Semigroup Time where
<     (Time t1) <> (Time t2) = Time (t1 + t2)
< instance Monoid Time where
<     mempty = Time 0
< instance Component Time where type Storage Time = Global Time
< 
< newtype Renderer = Renderer (Maybe SDL.Renderer)
< instance Semigroup Renderer where
<     (Renderer r1) <> (Renderer r2) = Renderer r1
< instance Monoid Renderer where
<     mempty = Renderer Nothing
< instance Component Renderer where type Storage Renderer = Global Renderer
< 
< newtype Window = Window (Maybe SDL.Window)
< instance Semigroup Window where
<     (Window w1) <> (Window w2) = Window w1
< instance Monoid Window where
<     mempty = Window Nothing
< instance Component Window where type Storage Window = Global Window

- `Camera` - stores a function that is applied during the `draw` function to offset all `Renderable` Entities. An example use case for this would be keeping a Player always in the centre of the game window.
- `Time` - stores the total accumulated time of the game
- `Renderer` - stores the SDL rendering context if you want to adjust the renderer
- `Window` - stores the SDL window context if you want to adjust the window

== Creating our Game World

With all of our Components defined, we need to create our Game World type. In Apecs, this is done by writing `makeWorld "World" [..]`, however `apecs-sdl-gdk` exposes `makeWorld'`, which alongside your own Components, also initialises the game world to include the `apecs-sdl-gdk` specific components. Thus, we need to write the following:

> makeWorld' [''Turn, ''Board, ''MousePosition, ''MouseButtons, ''Countdown]

What this is doing is utilising [Template Haskell](https://wiki.haskell.org/Template_Haskell) to create a `World` data type at compile-time which includes all the necessary data class instances to define our game world within Apecs. If you want to see what this compiles to, you can do so [here](https://hackage.haskell.org/package/apecs-0.9.6/docs/Apecs.html#v:makeWorld).

Finally, I like to define a type synonym around our World to make our lives ever so slightly easier.

> type System' a = System World a

= Game Logic Implementation

Now with our World created, we can now start implementing our noughts-and-crosses game logic. We will first start by defining our Game's configuration.

> config :: Config
> config = defaultConfig { windowTitle = "Noughts and Crosses"
>                        , windowDimensions = (600,600)
>                        , showFPS = Nothing }

The only notable configuration here is the window dimensions. For the sake of this toy example, we will make the entire window the noughts and crosses board, which means our mouse position will always be in one of the 9 sections of the board.

Now we will start writing our first system!

> initialise :: System' ()
> initialise = do
>     let rly = RenLine { lineX = 0, lineY = 600 }
>         rlx = RenLine { lineY = 0, lineX = 600}
>     line1 <- newEntity (Line rly, Position (V2 200 0), Colour (V4 0 0 0 255), Layer 0, IsVisible True)
>     line2 <- newEntity (Line rly, Position (V2 400 0), Colour (V4 0 0 0 255), Layer 0, IsVisible True)
>     line3 <- newEntity (Line rlx, Position (V2 0 200), Colour (V4 0 0 0 255), Layer 0, IsVisible True)
>     void $ newEntity (Line rlx, Position (V2 0 400), Colour (V4 0 0 0 255), Layer 0, IsVisible True)

`initialise`, as the name suggests, initialises what we need for our game world. Due to each `Global` component having a `Monoid` instance, their initial value is taken care of for us. Therefore, all we need to do is set up the visual game board by creating lines to be rendered. This is done by:

- Using the `Renderable` and `Position` components, we create two vertical and horizontal lines.
- We also tag each Entity with a Colour component to set it black, make them visible, and render them on the 0th layer
- Inside `RenLine`, `lineX` and `lineY` represent the x and y offset of the line's endpoint from its starting position respectively.
- We use `newEntity` to create a new Entity within our game world with the components specified.

In Apecs, Entities are simply integers. In many cases you will not need to interact with Entity values directly, but it is important to know.

Now let's define our main entry point into the game logic, our `step` function which will be called every frame to update the game world

> step :: Float -> System' ()
> step dt = do
>     t <- get global
>     case t of
>         PlayerTurn -> stepPlayer dt
>         AITurn -> stepAI dt
>         CheckWinPlayer -> stepCheckWin dt AITurn
>         CheckWinAI -> stepCheckWin dt PlayerTurn
>         Win -> stepWin dt
>         Reset -> stepReset dt

Whilst `step` does not have much code, the use of `get global` introduces a lot of new Apecs concepts:

- `get` is one of the ways to get Component data from an Apecs Store given an Entity
- `get` infers from the type signature which Component you are wanting to access, and using that determines what Store to access
    - Because of this, if GHC cannot infer the type, you will need to add a type constraint, such as `t <- get global :: System' Turn`
- In our case, we want to access `Turn` which is stored in `Global`
- As mentioned earlier, all `Global` components are accessible using any Entity, therefore we use the alias `global` exposed by Apecs for nice readability
    - Under the hood, `global` is defined as -1

After getting our Turn component, we then pattern match on its value to step accordingly. This allows GHC to infer the type of `t`, resulting in us not needing a type constraint.

Before getitng to our turn-specific step functions, I want to introduce a helper to reduce the amount of code we will need to write.

> updateBoard :: Board -> Int -> Int -> BoardEntry -> System' ()
> updateBoard (Board b) r c t = do
>     let (l1,r1) = splitAt c b
>         (l2,r2) = splitAt r (head r1)
>         r2' = t : tail r2
>         r2'' = l2 ++ r2'
>         r1' = r2'' : tail r1
>         b' = l1 ++ r1'
>         x = 200 * fromIntegral r
>         y = 200 * fromIntegral c
>     case t of
>         Nought -> do
>            _ <- newEntity (Rectangle RenRectangle { rectSize = V2 100 100 }, Position (V2 (x+50) (y+50)), Colour (V4 0 0 0 255), Layer 1, IsVisible True)
>            modify global $ \(_ :: Turn) -> CheckWinPlayer
>         Cross -> do
>             _ <-newEntity (Line RenLine { lineX = 100, lineY = 100 }, Position (V2 (x+50) (y+50)), Colour (V4 0 0 0 255), Layer 1, IsVisible True)
>             _ <- newEntity (Line RenLine { lineX = -100, lineY = 100 }, Position (V2 (x + 150) (y+50)), Colour (V4 0 0 0 255), Layer 1, IsVisible True)
>             modify global $ \(_ :: Turn) -> CheckWinAI
>         Empty -> return ()
>     modify global $ \(Board _) -> Board b'

`updateBoard` takes our current game board, a row and column index, and the tile we want to replace the existing one with, and replaces that tile whilst also creating new Entities with `Renderable` and `Position` components to make them visible. It also progresses our game state so that we check for a win afterwards.

The new concept in `updateBoard` is `modify`. What `modify` does is:

- Takes a single Entity to update a single Component for
- Applies a function to the original Component and stores the result

In our case, we apply `modify` to `global` to simply progress our `Turn` state.

Now let's implement our turn-specific step functions. We will go top-down in our implementations.

> stepPlayer :: Float -> System' ()
> stepPlayer dt = do
>     MousePosition (V2 mx my) <- get global
>     MouseButtons mbs <- get global
>     (Board b) <- get global
>     let r = floor (mx / 200)
>         c = floor (my / 200)
>         t = (b !! c) !! r
>     case t of
>         Empty -> when (SDL.ButtonLeft `Set.member` mbs) $ updateBoard (Board b) r c Nought
>         _ -> return ()

`stepPlayer` does the following:

- Gets our globally stored mouse position and currently pressed mouse buttons
- Accesses the current tile being hovered over by the mouse
- If the tile is empty and the left mouse button is currently pressed, then updates the board and switches the game's state to check the board for a potential win

> stepAI :: Float -> System' ()
> stepAI dt = do
>     (Board b) <- get global
>     let getTile = do
>             n <- randomRIO (0,8) :: IO Int
>             let r = n `div` 3
>                 c = n `mod` 3
>                 r' = fromIntegral r
>                 c' = fromIntegral c
>                 t = (b !! c') !! r'
>             case t of
>                 Empty -> return (r',c')
>                 _ -> getTile
>     (r,c) <- liftIO getTile
>     updateBoard (Board b) r c Cross

`stepAI` does the same as `stepPlayer`, except it uses a random integer instead of the mouse position for tile placement.

> stepCheckWin :: Float -> Turn -> System' ()
> stepCheckWin dt t = do
>     (Board b) <- get global
>     let lines = [(0,1,2), (3,4,5), (6,7,8), (0,3,6), (1,4,7), (2,5,8), (0,4,8), (2,4,6)]
>         checkLine (Board b) (n1,n2,n3) 
>             | t1 == Nought && t2 == Nought && t3 == Nought = Just (n1,n2,n3)
>             | t1 == Cross && t2 == Cross && t3 == Cross = Just (n1,n2,n3)
>             | otherwise = Nothing
>             where
>                 r1 = n1 `div` 3
>                 c1 = n1 `mod` 3
>                 r2 = n2 `div` 3
>                 c2 = n2 `mod` 3
>                 r3 = n3 `div` 3
>                 c3 = n3 `mod` 3
>                 t1 = (b !! c1) !! r1
>                 t2 = (b !! c2) !! r2
>                 t3 = (b !! c3) !! r3
>         isFull = Empty `notElem` concat b
>         line = foldl' (\acc l -> if isJust acc then acc else checkLine (Board b) l) Nothing lines
>     case line of
>         Just (n1,_,n3) -> do
>             let r1 = n1 `div` 3
>                 c1 = n1 `mod` 3
>                 r3 = n3 `div` 3
>                 c3 = n3 `mod` 3
>                 c1' = fromIntegral c1
>                 c3' = fromIntegral c3
>                 r1' = fromIntegral r1
>                 r3' = fromIntegral r3
>                 rl = RenLine { lineX = 0
>                              , lineY = 0 }
>             if c1' == c3' then
>                 void $ newEntity (Line rl { lineX = 600
>                                           , lineY = 0 }, Position (V2 0 (200 * c1' + 100))
>                                           , Layer 2
>                                           , Colour (V4 255 0 0 255)
>                                           , IsVisible True)
>             else if r1' == r3' then
>                 void $ newEntity (Line rl { lineX = 0
>                                           , lineY = 600 }, Position (V2 (200 * r1' + 100) 0)
>                                           , Layer 2
>                                           , Colour (V4 255 0 0 255)
>                                           , IsVisible True)
>             else if (c1' > c3') then
>                 void $ newEntity (Line rl { lineX = -600
>                                           , lineY = 600 }, Position (V2 600 0)
>                                           , Layer 2
>                                           , Colour (V4 255 0 0 255)
>                                           , IsVisible True)
>             else
>                 void $ newEntity (Line rl { lineX = 600
>                                           , lineY = 600 }, Position(V2 0 0)
>                                           , Layer 2
>                                           , Colour (V4 255 0 0 255)
>                                           , IsVisible True)
>             _ <- newEntity (Countdown 3.0)
>             modify global $ \(_ :: Turn) -> Win
>         Nothing -> if isFull then do
>                 _ <- newEntity (Countdown 3.0)
>                 modify global $ \(_ :: Turn) -> Win
>             else
>                 modify global $ \(_ :: Turn) -> t

`stepCheckWin` does the following:

- Checks all possible locations for a complete line
- If a complete line is found, it creates a new line Entity to highlight the line along starting a countdown timer of 3 seconds
    - For wins which are straight lines, we offset them to be in the centre of the square
- If the board is full and no one has a line (i.e. we reach a tie), then we just progress to the `Win` state to simulate the cooldown before wiping the board

> stepWin :: Float -> System' ()
> stepWin dt = cmapM $ \(Countdown t) -> if t < 0
>     then do
>         modify global $ \(_ :: Turn) -> Reset
>         return $ Right $ Not @Countdown
>     else return $ Left $ Countdown (t-dt) 

What `stepWin` does is very simple; it counts down our timer and if it reaches 0, it changes our game state to reset the board. However, it introduces a lot of new Apecs concepts, and so we will focus a lot on it.

`cmap`, and its monadic variant `cmapM`, is Apecs' form of `map`. Its functionality is dependent on the type of the function you provide it. For example:

- Suppose we provide `cmap` a function of type `(Componment1,Component2) -> Component1`
- `cmap` iterates over all entities that has the component on the left-hand side, and then writes to the component on the right-hand side
- In the example, we have a tuple on the left-hand side. In Apecs tuples of Components are considered a Component
    - This is an example of how Components can be composed together
- Tuples in Apecs work by mapping over all entities which have the first component, and then checks if those entities also has the subsequent components in the tuple.
- For our case, this means Apecs will get all Entities which have `Component1`, and then also check if they have `Component2` as well.
- Then, by the right-hand side, Apecs will update `Component1` for that Entity

When composing components together, for performance concerns it is important to include the most uniquely identifying component as the first component. For example, suppose we have a `Player`, `Enemy`, and `Position` component. In our game, we have only one player, but multiple enemies, and all of them have a `Position` component. If we wrote a cmap as follows:

< cmap $ \(Position _, Player) -> ...

That would be less efficient than

< cmap $ \(Player, Position _) -> ...

Since Apecs would be checking all Entities with a Position if they were a player, rather than checking just a single entity with Player if it had a Position component.

For the case of `stepWin`, we actually use `cmapM`. Like with `map` and its monadic variant `mapM`, `cmapM` is Apecs' monadic variant of `cmap`, allowing for a monadic function to be passed. For our case, this monadic function allows us to do two operations rather than one.

Now let's actually focus on the type of `stepWin`'s `cmapM`, since it also is something of interest:

- The type of our function is `Countdown -> System' (Either Countdown (Not Countdown))`
- If we remove the `System'` Monad from the type, like how it would be for `cmap`, we would get the following `Countdown -> Either Countdown (Not Countdown)`
- Our type specifies that we get an Entity which has a `Countdown` component, and then return `Either Countdown (Not Countdown)`

So we know what our type is, and know what the left-hand side does, but what about the right, i.e. what does `Either Countdown (Not Countdown)` do for Apecs?

- In Apecs, `Either a b` is used to represent two possible outcomes, captured by our `Left a` and `Right b` respectively
- For our case, we either write to `Countdown` in the case of our `Left`, or we have `Not Countdown` for our `Right`
- `Not :: Not c` is used to delete something, meaning in our `Right` case we delete the `Countdown` component from the Entity.
    - Note - there are other ways to delete components, such as with `Maybe`, or by using `destroy`. We will showcase these later.

It is important to note that, when destroying an Entity, make sure to specify **all** the Entity's components. Failing to do so will mean you have components floating around in memory, which not only causes a memory leak but may affect logic!

> stepReset :: Float -> System' ()
> stepReset dt = do
>     cmap $ \(_ :: Renderable, Position _) -> (Nothing :: Maybe (Renderable, Position))
>     initialise
>     modify global $ \(_ :: Board) -> mempty :: Board
>     modify global $ \(_ :: Turn) -> PlayerTurn

`stepReset` simply deletes all entities with a `Renderable` component along with their `Position` component, and resets the board by using its `mempty`. Since this removes every Renderable entity, we then re-initialise the grid lines, and then give the player the turn to play.

The way we delete components in `stepReset` is using `Maybe`. Like with `Either`, `Maybe c = Just c | Nothing` represents optionality, but is a bit more explicit in what it represents

- `Just c` represents writing the component `c`
- `Nothing` represents deleting the component `c`

For our case, this method is not ideal. To ensure we delete all the components captured by `c`, we have to also read `Position`. This is unnecessary since we just want to delete it. Therefore, a better alternative would have been to use `Either` or `destroy`. We have already seen `Either`, so I will only show `destroy` below

< cmapM_ $ \(_ :: Renderable, e) -> destroy e (Proxy @(Renderable, Position))

As you have probably noticed, all our step functions take in a `Float` named `dt`, but never actually use it. Whilst in practice you would simply adjust their type to never take it as an input (which is the right thing to do!), I wanted to reinforce the idea of passing around `dt`.

When running a game with `apecs-sdl-gdk`, `dt` is given to your top-level step function, and represents the amount of time elapsed between the previous frame and the current frame in milliseconds. Therefore, in any case where you need to have frame specific timings, you will need to make use of the input `dt`.

Now that we have written all of the systems needed for our game logic, we have two things left to do:

1. Write an event handler
2. Write our `main :: IO ()` function

In `apecs-sdl-gdk`, events are polled for you, and then passed onto your event handler as `[SDL.EventPayload]`. If you want to see all the possible events that are polled and sent for handling, please refer to the [SDL Documentation](https://hackage.haskell.org/package/sdl2-2.5.5.0/docs/SDL-Event.html#t:EventPayload). SDL's `EventPayload` makes use of Sum types and records for capturing all possible event payload, and so we will use top-level pattern matching for our event handlers

> handlePayload :: [SDL.EventPayload] -> System' ()
> handlePayload = mapM_ handleEvent
> 
> handleEvent :: SDL.EventPayload -> System' ()
> handleEvent (SDL.MouseMotionEvent ev) = handleMouseMotionEvent ev
> handleEvent (SDL.MouseButtonEvent ev) = handleMouseButtonEvent ev
> handleEvent _ = return ()
> 
> handleMouseMotionEvent :: SDL.MouseMotionEventData -> System' ()
> handleMouseMotionEvent ev = let
>         (SDL.P pos) = SDL.mouseMotionEventPos ev
>     in
>         modify global $ \(MousePosition _ ) -> MousePosition $ fromIntegral <$> pos
> 
> handleMouseButtonEvent :: SDL.MouseButtonEventData -> System' ()
> handleMouseButtonEvent ev
>     | SDL.mouseButtonEventMotion ev == SDL.Pressed = modify global $ \(MouseButtons mbs) -> MouseButtons (Set.insert (SDL.mouseButtonEventButton ev) mbs)
>     | SDL.mouseButtonEventMotion ev == SDL.Released = modify global $ \(MouseButtons mbs) -> MouseButtons (Set.delete (SDL.mouseButtonEventButton ev) mbs)
>     | otherwise = return ()

Our `main :: IO ()` function is the entrypoint into our program, and is used to start our game. As mentioned, `apecs-sdl-gdk` exposes 3 main functions to help with this:

- `initialise` is used to create the SDL window and renderer context, and needs your Apecs `World`
- `run` is the main game loop, which takes the entry point for stepping your game world, your event handler, and a draw function
- `draw` which is the default draw function, and handles the entire rendering pipeline for you by using all entities which have `Renderable`, `Position` and `Layer`, and optionally `Colour` and `IsVisible` components

> main :: IO ()
> main = do
>     w <- initWorld
>     runWith w initialise
>     (window, renderer) <- GDK.initialise w config
>     run w renderer window step handlePayload draw

There are two new Apecs concepts here:

- `initWorld` - this function is generated by `makeWorld'`, and is used to get your World state in the form of `w`
- `runWith` - this runs a given System inside your world state

And just like that, our game is complete!

= Tips, Tricks, and Advice for Developing Games in Haskell

With our noughts-and-crosses game complete, I now want to just highlight and collate together some tips for developing games, especially with Apecs, SDL, and `apecs-sdl-gdk`

- Always try write game logic as separated as possible. Not only is this good software practice in general, but for Haskell specifically, it is one of the best practices you can follow as it will allow for more reusable, idiomatic code through the use of e.g. higher order functions.
- When working with Apecs, since you are always inside a `System' a` Monad, it is very tempting to make use of functions like `cmapM`, `cfoldM`, etc. However, if you are able to write a Component logic which does not need to be inside a Monadic context, then you should not be inside one, as being inside one means you have the possiblity of introducing side effects.
- Interactable user interfaces are inherently stateful things. Therefore, when attempting to build one I recommend decomposing each UI element by exploiting Haskell's type system, and making use of functions being first-class citizens.
- SDL allows for a lot of changes to the renderer or window to be done pragmatically. Therefore, you are able to do pretty much anything you desire. However, the `sdl2` library binds onto the underlying C API for SDL2, and therefore has quite a bit of boilerplate and is very stateful
- When wanting to handle multiple different events, such as keyboard input, mouse movement, mouse buttons, etc. it is often quite useful having Globally stored data structures for storing the currently pressed buttons, like we did for `MouseButtons`, as it allows you to access them from anywhere within your game logic.