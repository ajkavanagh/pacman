{-# LANGUAGE OverloadedStrings #-}
module UI where

import Control.Monad (forever, void, when, unless, forM_)
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent (threadDelay, forkIO)
import Data.Maybe (fromMaybe)

import Data.Vector (Vector, (!), fromList)
--import qualified Data.Vector as V
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import qualified Data.DList as DL
import Data.Sort (sortOn)

import Control.Monad.Trans.Writer.Strict (runWriter)

import UI.NCurses
  ( Window, Update, Curses, Event(..)
  , updateWindow, defaultWindow, newWindow, closeWindow
  , runCurses, render
  , clear
  , moveCursor
  , drawBorder, drawString
  , Attribute(..), setAttributes
  , Color(..), ColorID, setColor, newColorID, supportsColor
  , getEvent
  , setCursorMode, CursorMode(..)
  )

import Pacman

import BChan (BChan, newBChan, writeBChan, readBChan)
import Linear.V2 (V2(..))
import Lens.Micro ((^.))

-- Types

-- | Ticks mark passing of time
data Tick = Tick deriving (Show, Eq)
oneSixtyth = 1000000 `quot` round framesPerSecond

-- | Things we draw
data Cell = PacmanW PacmanData
          | GhostW GhostData Bool
          | PowerupW
          | PillW
          | WallW Char
          | SpaceW
  deriving (Eq, Show)

-- These are the windows that make up the App.
data AppWindow = ScoreWindow
               | GridWindow
               deriving (Eq, Ord, Show)

-- The WindowMap stores a lookup of AppWindow to actual ncurses window
type WindowMap = Map AppWindow Window

-- This is a reader type (effectively) to pass around drawing functions.
-- TODO: we might be able to use a ReaderT to wrap Curses a so that we don't
-- have to pass it around in every function.
data Display = Display
    { windows :: WindowMap
    , attrs   :: Attrs
    }


-- | Main thing -- set up a channel for the ticks, init a game state and then
-- run the ncursesMain function.
main :: IO ()
main = do
    chan <- newBChan 10
    forkIO $ forever $ do
        writeBChan chan Tick
        threadDelay oneSixtyth  -- ticks are 1/60 of a second
    -- now initialse the game and call the ncurses loop
    let g = initGame
    ncursesMain chan g

--
-- | ncurses main - set up the windows, draw the initial display and then loop
-- until the user quits.
ncursesMain :: BChan a -> Game -> IO ()
ncursesMain chan initG = runCurses $ do
    lastCursorMode <- setCursorMode CursorInvisible
    let (mh, mw) = mazeHW (initG ^. maze)
    theAttrs <- makeAttrs
    sw <- newWindow 5 20 0 0
    gw <- newWindow (fromIntegral (mh+2)) (fromIntegral (mw+2)) 0 30
    let display =
            Display { windows = M.fromList [(ScoreWindow,sw), (GridWindow,gw)]
                    , attrs   = theAttrs }
    drawEverything initG display
    render
    let loop g = do
        g' <- handleTick g display
        -- wait for the Tick event - blocks until the event is received
        liftIO $ readBChan chan
        -- get any keyboard event -- but don't wait for it
        ev <- getEvent sw (Just 0)
        case ev of
            Nothing -> loop g'
            Just ev' -> do
                g'' <- handleEvent g' display ev'
                unless (isQuit ev') (loop g'')
    -- run the loop until the user quits
    loop initG
    -- clean up
    closeWindow sw
    closeWindow gw
    void $ setCursorMode lastCursorMode


-- | Handle an ncurses event (probably keyboard) and update the game state and
-- maybe redraw an window or two.
handleEvent :: Game -> Display -> Event -> Curses Game
handleEvent g _ _ = return g


-- | handle the tick -- i.e. update the game state and maybe do a redraw
handleTick :: Game -> Display -> Curses Game
handleTick = runAction tickAction


-- | run a (Game -> DrawList Game) function, which may change the state and
-- return a list of things to draw (from changing that state).  Draw those items
-- and then return (in the Curses monad) the modified game state.
-- The action function returns the DrawList Game monad which is a Writer monad
-- allowing the action function to queue up DrawListItem draws.
runAction :: (Game -> DrawList Game) -> Game -> Display -> Curses Game
runAction action g display = do
    let (g', drawList) = runWriter (action g)
    if null drawList
      then return g'
      else do
        doDrawList drawList g' display
        return g'


-- | Execute the DrawListItems inside the Curses () monad.
-- This takes the drawlist, prioritises it, and then performs the draws.
doDrawList :: DrawListItems -> Game -> Display -> Curses ()
doDrawList ds g display = do
    let priorityDs = prioritiseDrawList ds
    forM_ priorityDs $ doDrawListItem g display
    render


-- | prioritise and clean the DrawList  -- basically sort the items in the
-- drawlist according to their `drawPriority` and if the fist element is
-- DrawEverything then just return that item.  It would be nice to do culling,
-- but the work probably doesn't warranty the saving in drawing to a virtual
-- buffer a couple of times.
-- TODO actually make it work;
-- 1. change it to cells and score, other bits, only
-- 2. prioritise the cells (sort) and clean out the overlapping ones.
-- 3. If it's everything, just return the single item (no need to do the other
-- calls).
prioritiseDrawList :: DrawListItems -> [DrawListItem]
prioritiseDrawList dli = sortOn drawPriority $ DL.toList dli


-- | Draw a single DrawListItem
-- Updates in a Curses () monad context.
doDrawListItem :: Game -> Display -> DrawListItem -> Curses ()
doDrawListItem g display dli =
    case dli of
        DrawPacman -> drawPacman g display
        DrawGhost gp -> drawGhost g display gp
        DrawScore -> renderScoreOnWindow (windowFor ScoreWindow display)
                                         (g ^. score)
                                         (attrs display)
        DrawLevel -> return ()
        DrawLives -> return ()
        DrawBackground hw -> drawBackGroundAt g display hw
        DrawEverything -> drawEverything g display


windowFor :: AppWindow -> Display -> Window
windowFor aw d = windows d M.! aw


-- TODO: what's the type for this function????
isQuit c = c == EventCharacter 'q' || c == EventCharacter 'Q'


drawBorderHelper :: Window -> Curses ()
drawBorderHelper w =
    updateWindow w $ drawBorder Nothing Nothing Nothing Nothing
                                Nothing Nothing Nothing Nothing


drawEverything :: Game -> Display -> Curses ()
drawEverything g display = do
    let gw = windowFor GridWindow display
        sw = windowFor ScoreWindow display
        as = attrs display
    updateWindow gw clear
    updateWindow sw clear
    drawBorderHelper sw
    drawBorderHelper gw
    renderScoreOnWindow sw (g ^. score) as
    renderAllGameOnWindow gw g as


renderScoreOnWindow :: Window -> Int -> Attrs -> Curses ()
renderScoreOnWindow w s as =
    updateWindow w $ do
        moveCursor 1 1
        setAttrUsing ScoreAttr as
        drawString ("Score: " ++ show s)
        moveCursor 0 0


renderAllGameOnWindow :: Window -> Game -> Attrs -> Curses ()
renderAllGameOnWindow w g as =
    updateWindow w $ do
        drawFullMazeUpdate g as
        drawGhostsUpdate g as
        drawPacmanUpdate g as
        clearAttrUpdate as
        moveCursor 0 0


drawFullMazeUpdate :: Game -> Attrs -> Update ()
drawFullMazeUpdate g as =
    forM_ [0..height-1] $ \h ->
        forM_ [0..width-1] $ \w -> do
            moveCursor (fromIntegral (h+1)) (fromIntegral (w+1))
            drawCoord (V2 h w)
  where
    height    = g ^. mazeHeight
    width     = g ^. mazeWidth
    drawCoord = drawCell as . cellAt g


drawBackGroundAt :: Game -> Display -> Coord -> Curses ()
drawBackGroundAt g d hw@(V2 h w) =
    updateWindow (windowFor GridWindow d) $ do
        moveCursor (fromIntegral (h+1)) (fromIntegral (w+1))
        (drawCell (attrs d) . cellAt g) hw


drawGhostsUpdate :: Game -> Attrs -> Update ()
drawGhostsUpdate g as = forM_ (g ^. ghosts) (drawGhostUpdate g as)


drawGhost :: Game -> Display -> GhostPersonality -> Curses ()
drawGhost g d gp =
    updateWindow (windowFor GridWindow d)
                 $ drawGhostUpdate g (attrs d) (nameToGhostData gp g)


drawGhostUpdate :: Game -> Attrs -> GhostData -> Update ()
drawGhostUpdate g as gd =
    unless dead $ do
        moveCursor (fromIntegral (h+1)) (fromIntegral (w+1))
        setAttrUsing ghAttr as
        drawString "M"
  where
    dead = gd ^. ghostState == GhostDead
    (V2 h w) = gd ^. ghostAt
    fleeing  = ghostsAreFleeing g
    ghAttr = if fleeing
               then GhostFleeAttr
               else case gd ^. name of
        Shadow  -> GhostShadowAttr
        Bashful -> GhostBashfulAttr
        Speedy  -> GhostSpeedyAttr
        Pokey   -> GhostPokeyAttr


-- | Draw Pac-man on the GridWindow
drawPacman :: Game -> Display -> Curses ()
drawPacman g d =
    updateWindow (windowFor GridWindow d) $ drawPacmanUpdate g (attrs d)


drawPacmanUpdate :: Game -> Attrs -> Update ()
drawPacmanUpdate g as = do
    moveCursor (fromIntegral (h+1)) (fromIntegral (w+1))
    setAttrUsing PacmanAttr as
    drawString st
  where
    p = g ^. pacman
    (V2 h w) = p ^. pacAt
    st = if p ^. dying
      then if _t >= length pacmanDiesChars
             then " "
             else [pacmanDiesChars !! _t]
      else  let s = fromMaybe "" $ lookup (p ^. pacDir) pacmanChars
                i = _t  `rem` length s
             in [s !! i]
    _t = p ^. pacAnimate

-- Handling events
{-
cacheHelper :: Game -> (Game -> Game) -> Name -> EventM Name (Next Game)
cacheHelper g f n = do
    let g' = f g
    when (g'^.doInvalidateCache) $ invalidateCacheEntry n
    continue g'

handleEvent :: Game -> BrickEvent Name Tick -> EventM Name (Next Game)
handleEvent g (AppEvent TickPacman)                 = cacheHelper g stepPacman GridR
handleEvent g (AppEvent TickGhostNormal)            = cacheHelper g stepGhostsNormal GridR
handleEvent g (AppEvent TickGhostSpeedup1)          = cacheHelper g stepGhostsSpeedup1 GridR
handleEvent g (AppEvent TickGhostSpeedup2)          = cacheHelper g stepGhostsSpeedup2 GridR
handleEvent g (AppEvent TickGhostFlee)              = cacheHelper g stepGhostsFlee GridR
handleEvent g (VtyEvent (V.EvKey V.KUp []))         = continue $ turn North g
handleEvent g (VtyEvent (V.EvKey V.KDown []))       = continue $ turn South g
handleEvent g (VtyEvent (V.EvKey V.KRight []))      = continue $ turn East g
handleEvent g (VtyEvent (V.EvKey V.KLeft []))       = continue $ turn West g
handleEvent g (VtyEvent (V.EvKey (V.KChar 'k') [])) = continue $ turn North g
handleEvent g (VtyEvent (V.EvKey (V.KChar 'j') [])) = continue $ turn South g
handleEvent g (VtyEvent (V.EvKey (V.KChar 'l') [])) = continue $ turn East g
handleEvent g (VtyEvent (V.EvKey (V.KChar 'h') [])) = continue $ turn West g
handleEvent g (VtyEvent (V.EvKey (V.KChar 'r') [])) = liftIO initGameIO >>= continue
handleEvent g (VtyEvent (V.EvKey (V.KChar 'p') [])) = continue $ pause g
handleEvent g (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt g
handleEvent g (VtyEvent (V.EvKey V.KEsc []))        = halt g
handleEvent g _                                     = continue g
-}


{-
drawDebugPacman :: Game -> Widget Name
drawDebugPacman g = withBorderStyle BS.unicodeBold
  $ B.borderWithLabel (str "Pacman debug")
  $ vBox [ hBox [str "pacAt:      ", str $ show (p ^. pacAt)]
         , hBox [str "pacDir:     ", str $ show (p ^. pacDir)]
         , hBox [str "pacNextDir: ", str $ show (p ^. pacNextDir)]
         , hBox [str "dying:      ", str $ show (p ^. dying)]
         ]
  where p = g ^. pacman
-}

{-
drawDebugGame :: Game -> Widget Name
drawDebugGame g = withBorderStyle BS.unicodeBold
  $ B.borderWithLabel (str "Game debug")
  $ vBox [ hBox [str "pillsLeft: ", str $ show (g ^. pillsLeft)]
         , hBox [str "paused:    ", str $ show (g ^. paused)]
         , hBox [str "gameTick:  ", str $ show (g ^. gameTick)]
         , hBox [str "nextMove:  ", str $ show (g ^. nextMove)]
         , hBox [str "ghostMode: ", str $ show (g ^. ghostMode)]
         , hBox [str "gameover:  ", str $ show (g ^. gameover)]
         , hBox [str "state:     ", str $ show (g ^. state)]
         , hBox [str "score:     ", str $ show (g ^. score)]
         ]
-}

{-
drawDebugGhost :: Game -> GhostPersonality -> Widget Name
drawDebugGhost g gp = withBorderStyle BS.unicodeBold
  $ B.borderWithLabel (str ("Ghost " ++ show gp))
  $ vBox [ hBox [str "ghostAt:    ", str $ show (gd ^. ghostAt)]
         , hBox [str "ghostDir:   ", str $ show (gd ^. ghostDir)]
         , hBox [str "ghostState: ", str $ show (gd ^. ghostState)]
         ]
  where gd = head $ filter ((==gp).(^. name)) $ g ^. ghosts

drawGameOverOrPaused :: Game -> Widget Name
drawGameOverOrPaused g = if not (g ^. paused)
                           then drawGameOver (g ^. gameover)
                           else drawPaused
-}

{-
drawGameOver :: Bool -> Widget Name
drawGameOver True  = withAttr gameOverAttr $ C.hCenter $ str "GAME OVER"
drawGameOver False = emptyWidget

gameOverAttr :: AttrName
gameOverAttr = "gameOver"
-}

{-
drawPaused :: Widget Name
drawPaused = withAttr gameOverAttr $ C.hCenter $ str "Paused"
-}

-- we return a Cell which represents what we want to draw, which might well just
-- be the character in the maze
cellAt g hw
  -- | hw == _pacman ^. pacAt = PacmanW _pacman
  -- | not (null ghs)         = GhostW (head ghs) fleeing
  | c == pillChar          = PillW
  | c == powerupChar       = PowerupW
  | c `elem` wallChars     = WallW c
  | otherwise              = SpaceW
  --where ghs      = filter ((==hw).(^. ghostAt)) $ g ^. ghosts
        --_pacman  = g ^. pacman
  where c        = mazeCharAt g hw
        --fleeing  = ghostsAreFleeing g


drawCell :: Attrs -> Cell -> Update ()
drawCell as cell = do
    setAttrUsing attr as
    drawString [ch]
    clearAttrUpdate as
  where
    (attr, ch) = case cell of
        SpaceW -> (EmptyAttr, ' ')
        PillW  -> (PillAttr,  pillChar)
        PowerupW -> (PowerupAttr, powerupChar)
        (WallW c) -> (WallAttr, c)

{-
drawCell :: Cell -> Widget Name
drawCell SpaceW            = withAttr emptyAttr $ str " "
drawCell PillW             = withAttr pillAttr $ str [pillChar]
drawCell PowerupW          = withAttr powerupAttr $ str [powerupChar]
drawCell (WallW c)         = withAttr wallAttr $ str [c]
drawCell (GhostW gh f)
  | dead      = withAttr emptyAttr $ str " "
  | f         = withAttr ghostFleeAttr $ str [ghostChar]
  | otherwise = withAttr ghAttr $ str [ghostChar]
  where
    dead = gh ^. ghostState == GhostDead
    ghAttr = case gh ^. name of
        Shadow  -> ghostShadowAttr
        Bashful -> ghostBashfulAttr
        Speedy  -> ghostSpeedyAttr
        Pokey   -> ghostPokeyAttr
-}

-- note this relies on pacTick being reset in game when pacman dies
{-
drawCell (PacmanW p) = withAttr pacmanAttr $
    if p ^. dying
      then if _t >= length pacmanDiesChars
             then str " "
             else str [pacmanDiesChars !! _t]
      else  let s = fromMaybe "" $ lookup (p ^. pacDir) pacmanChars
                i = _t  `rem` length s
             in str [s !! i]
  where _t = p ^. pacTick
-}


data Attr
  = EmptyAttr
  | PillAttr
  | PowerupAttr
  | WallAttr
  | PacmanAttr
  | GhostFleeAttr
  | GhostShadowAttr
  | GhostBashfulAttr
  | GhostSpeedyAttr
  | GhostPokeyAttr
  | ScoreAttr
  deriving (Show, Eq, Enum)

type Attrs = Vector (ColorID, [Attribute])


setAttrUsing :: Attr -> Attrs -> Update ()
setAttrUsing a as = do
    let (cID, theAttrs) = as ! fromEnum a
    setColor cID
    setAttributes theAttrs


clearAttrUpdate :: Attrs -> Update ()
clearAttrUpdate = setAttrUsing EmptyAttr


makeAttrs :: Curses Attrs
makeAttrs = do
    whiteID <- newColorID ColorWhite ColorDefault 1
    yellowID <- newColorID ColorYellow ColorDefault 2
    blueID <- newColorID ColorBlue ColorDefault 3
    redID <- newColorID ColorRed ColorDefault 4
    cyanID <- newColorID ColorCyan ColorDefault 5
    magentaID <- newColorID ColorMagenta ColorDefault 6
    greenID <- newColorID ColorGreen ColorDefault 7
    let emptyAttr = (whiteID, [])
        pillAttr = (whiteID, [])
        powerupAttr = (whiteID, [AttributeBold])
        wallAttr = (whiteID, [AttributeDim])
        pacmanAttr = (yellowID, [AttributeBold])
        ghostFleeAttr = (blueID, [])
        ghostShadowAttr = (redID, [AttributeBold])
        ghostBashfulAttr = (cyanID, [AttributeBold])
        ghostSpeedyAttr = (magentaID, [AttributeBold])
        ghostPokeyAttr = (greenID, [AttributeBold])
        scoreAttr = (whiteID, [AttributeBold])
    return $ fromList [ emptyAttr
                      , pillAttr
                      , powerupAttr
                      , wallAttr
                      , pacmanAttr
                      , ghostFleeAttr
                      , ghostShadowAttr
                      , ghostBashfulAttr
                      , ghostSpeedyAttr
                      , ghostPokeyAttr
                      , scoreAttr
                      ]
