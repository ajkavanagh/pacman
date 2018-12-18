{-# LANGUAGE OverloadedStrings #-}
module UI where

import Control.Monad (forever, void, when, unless, forM_)
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent (threadDelay, forkIO)
import Data.Maybe (fromMaybe)

import Data.Vector (Vector, (!), fromList)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import qualified Data.DList as DL
import Data.Sort (sortOn)
import Data.List (nub)

import Control.Monad.Trans.Writer.Strict (runWriter)

import UI.NCurses
  ( Window, Update, Curses, Event(..), Key(..)
  , updateWindow, defaultWindow, newWindow, closeWindow
  , runCurses, render
  , clear
  , moveCursor
  , drawBorder, drawString
  , Attribute(..), setAttributes
  , Color(..), ColorID, setColor, newColorID, supportsColor
  , getEvent, setEcho, setKeypad
  , setCursorMode, CursorMode(..)
  )

import Pacman

import BChan (BChan, newBChan, writeBChan, readBChan)
import Linear.V2 (V2(..))
import Lens.Micro ((^.), ix)


showDebug = True
showPacmanDebug = False
showGameDebug = True
showGhostDebug = True

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
  deriving (Show)

-- These are the windows that make up the App.
data AppWindow = ScoreWindow
               | GridWindow
               | DebugWindow
               | InfoWindow
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
    let g = initGame 1
    ncursesMain chan g

--
-- | ncurses main - set up the windows, draw the initial display and then loop
-- until the user quits.
ncursesMain :: BChan a -> Game -> IO ()
ncursesMain chan initG = runCurses $ do
    lastCursorMode <- setCursorMode CursorInvisible
    setEcho False
    let (mh, mw) = mazeHW (initG ^. maze)
        h = fromIntegral (mh+2)
        w = fromIntegral (mw+2)
    theAttrs <- makeAttrs
    sw <- newWindow 5 20 0 0
    setKeypad sw True
    gw <- newWindow h w 0 30
    iw <- newWindow 3 w (h+1) 30
    pwdebug <- newWindow 15 30 10 0
    updateWindow pwdebug clear
    let display =
            Display { windows = M.fromList
                                    [ (ScoreWindow, sw)
                                    , (GridWindow,  gw)
                                    , (DebugWindow, pwdebug)
                                    , (InfoWindow, iw)
                                    ]
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
                         g'' <- handleEvent ev' g' display
                         unless (isQuit ev') (loop g'')
    -- run the loop until the user quits
    loop initG
    -- clean up
    closeWindow sw
    closeWindow gw
    void $ setCursorMode lastCursorMode


-- | Handle an ncurses event (probably keyboard) and update the game state and
-- maybe redraw an window or two.
handleEvent :: Event -> Game -> Display -> Curses Game
handleEvent ev g d = case mapEvent ev of
    Just f -> runAction f g d
    Nothing -> return g


mapEvent :: Event -> Maybe (Game -> DrawList Game)
mapEvent (EventSpecialKey KeyUpArrow)    = Just $ turnAction North
mapEvent ev | isEventChars "kK" ev       = Just $ turnAction North
mapEvent (EventSpecialKey KeyRightArrow) = Just $ turnAction East
mapEvent ev | isEventChars "lL" ev       = Just $ turnAction East
mapEvent (EventSpecialKey KeyDownArrow)  = Just $ turnAction South
mapEvent ev | isEventChars "jJ" ev       = Just $ turnAction South
mapEvent (EventSpecialKey KeyLeftArrow)  = Just $ turnAction West
mapEvent ev | isEventChars "hH" ev       = Just $ turnAction West
mapEvent ev | isEventChars "dD" ev       = Just   debugDieAction
-- TODO: need to access the random number generator here!
mapEvent ev | isEventChars "sS" ev       = Just $ startAction (initGame 1)
mapEvent ev | isEventChars "pP" ev       = Just   pauseAction
mapEvent _ = Nothing

--
-- Handling events
{-
handleEvent g (VtyEvent (V.EvKey (V.KChar 'r') [])) = liftIO initGameIO >>= continue
handleEvent g (VtyEvent (V.EvKey (V.KChar 'p') [])) = continue $ pause g
handleEvent g (VtyEvent (V.EvKey V.KEsc []))        = halt g
handleEvent g _                                     = continue g
-}



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
    when showDebug $ drawDebugData g display
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


-- | prioritise and clean the DrawList
-- This takes the `drawGridAt` calls and ensures that there is only one of them.
-- We unique the list using a Data.Set (this reduces multiple DrawGridAt coord
-- instructions to a single one) and then sort it using drawPriority.  If the
-- first element in the list is `DrawEverything` then we only need to return
-- that item, otherwise we return the list.
prioritiseDrawList :: DrawListItems -> [DrawListItem]
prioritiseDrawList dli = case ds of
    (x:xs) | x == DrawEverything -> [x]
    _                            -> ds
  where
      ds = sortOn drawPriority $ nub $ DL.toList dli



-- | Draw a single DrawListItem
-- Updates in a Curses () monad context.
doDrawListItem :: Game -> Display -> DrawListItem -> Curses ()
doDrawListItem g display dli = do
    let sw = windowFor ScoreWindow display
        as = attrs display
    case dli of
        DrawScore -> renderScoreOnWindow sw (g ^. score) as
        DrawLevel -> renderLivesOnWindow sw (g ^. livesLeft) as
        DrawLives -> renderLevelOnWindow sw (g ^. gameLevel) as
        DrawGridAt hw -> drawGridAt g display hw
        DrawEverything -> drawEverything g display


windowFor :: AppWindow -> Display -> Window
windowFor aw d = windows d M.! aw


isQuit :: Event -> Bool
isQuit = isEventChars "qQ"


isEventChars :: String -> Event -> Bool
isEventChars s (EventCharacter c) = c `elem` s
isEventChars _ _ = False


drawBorderHelper :: Window -> Curses ()
drawBorderHelper w =
    updateWindow w $ drawBorder Nothing Nothing Nothing Nothing
                                Nothing Nothing Nothing Nothing


drawEverything :: Game -> Display -> Curses ()
drawEverything g display = do
    let gw = windowFor GridWindow display
        sw = windowFor ScoreWindow display
        iw = windowFor InfoWindow display
        as = attrs display
    updateWindow gw clear
    updateWindow sw clear
    updateWindow iw clear
    drawBorderHelper sw
    drawBorderHelper gw
    renderScoreOnWindow sw (g ^. score) as
    renderLivesOnWindow sw (g ^. livesLeft) as
    renderLevelOnWindow sw (g ^. gameLevel) as
    renderAllGameOnWindow gw g as
    renderInfoWindow iw g as


renderScoreOnWindow :: Window -> Int -> Attrs -> Curses ()
renderScoreOnWindow w s =
    commonRenderStats w ("Score: " ++ show s) 1 ScoreAttr


renderLivesOnWindow :: Window -> Int -> Attrs -> Curses ()
renderLivesOnWindow w l =
    commonRenderStats w ("Lives: " ++ show l) 3 LivesAttr


renderLevelOnWindow :: Window -> Int -> Attrs -> Curses ()
renderLevelOnWindow w l =
    commonRenderStats w ("Level: " ++ show l) 2 LevelAttr


commonRenderStats :: Window  -- window to draw on
                  -> String  -- string to write
                  -> Integer -- line to draw on
                  -> Attr    -- attribute to use
                  -> Attrs   -- all attributes
                  -> Curses ()
commonRenderStats w s l a as =
    updateWindow w $ do
        moveCursor l 1
        setAttrUsing a as
        drawString s
        moveCursor 0 0


-- | render the info window depending on what has happened
renderInfoWindow :: Window  -- window to draw on
                 -> Game    -- data to draw from
                 -> Attrs   -- all atributes
                 -> Curses ()
renderInfoWindow w g as
  | showStart = do
      updateWindow w $ do
        clear
        moveCursor 1 3
        setAttrUsing InfoAttr as
        drawString "Press 's' to start :)"
      drawBorderHelper w
  | showGameOver = do
      updateWindow w $ do
        clear
        moveCursor 1 5
        setAttrUsing GameOverAttr as
        drawString "!!! Game Over !!!"
      drawBorderHelper w
  | g ^. paused = do
      updateWindow w $ do
        clear
        moveCursor 1 8
        setAttrUsing InfoAttr as
        drawString "Paused."
      drawBorderHelper w
  | otherwise = updateWindow w clear
  where
      showStart = g ^. gameState == NotStarted
      showGameOver = case g ^. gameState of
          GameOver _ -> True
          _          -> False


renderAllGameOnWindow :: Window -> Game -> Attrs -> Curses ()
renderAllGameOnWindow w g as =
    updateWindow w $ do
        drawFullMazeUpdate g as
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


drawGridAt :: Game -> Display -> Coord -> Curses ()
drawGridAt g d hw@(V2 h w) =
    updateWindow (windowFor GridWindow d) $ do
        moveCursor (fromIntegral (h+1)) (fromIntegral (w+1))
        (drawCell (attrs d) . cellAt g) hw


drawDebugData :: Game -> Display -> Curses ()
drawDebugData g d = do
    let lines = zip [0..] (determineDebugLines g)
    updateWindow (windowFor DebugWindow d) $ do
        clear
        clearAttrUpdate (attrs d)
        forM_ lines $ \(i, l) -> do
            moveCursor i 0
            drawString l
    render


determineDebugLines :: Game -> [String]
determineDebugLines g
  =  debugPacmanLines g
  ++ debugGameLines g
  ++ debugGhostLines g


debugPacmanLines :: Game -> [String]
debugPacmanLines g =
    if showPacmanDebug
      then
        [ "pacAt:      " ++ show (p ^. pacAt)
        , "pacDir:     " ++ show (p ^. pacDir)
        , "pacNextDir: " ++ show (p ^. pacNextDir)
        , "dead:       " ++ show (p ^. pacDead)
        , "pacAnimate: " ++ show (p ^. pacAnimate)
        , "pacTick:    " ++ show (p ^. pacTick)
        ]
      else []
  where p = g ^. pacman


debugGameLines :: Game -> [String]
debugGameLines g =
    if showGameDebug
      then
        [ "pillsLeft: " ++ show (g ^. pillsLeft)
        , "paused:    " ++ show (g ^. paused)
        , show (g ^. ghostsMode)
        , "state:     " ++ show (g ^. gameState)
        , "score:     " ++ show (g ^. score)
        , "pill f:    " ++ show (g ^. framesSincePill)
        , "g pill #   " ++ show (g ^. globalPillCount)
        ]
      else []

debugGhostLines :: Game -> [String]
debugGhostLines g =
    if showGhostDebug
      then
        [ "ghost:     " ++ show (gd ^. ghostName)
        , "state:     " ++ show (gd ^. ghostState)
        , "tick:      " ++ show (gd ^. ghostTick)
        , "pill #     " ++ show (gd ^. ghostPillCount)
        ]
      else []
  where
      -- change this to 0 -- 3
      gd = (g ^. ghosts) !! 1

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
  | hw == _pacman ^. pacAt = PacmanW _pacman
  | not (null ghs)         = GhostW (head ghs) fleeing
  | c == pillChar          = PillW
  | c == powerupChar       = PowerupW
  | c `elem` wallChars     = WallW c
  | otherwise              = SpaceW
  where ghs      = filter ((==hw).(^. ghostAt)) $ g ^. ghosts
        _pacman  = g ^. pacman
        c        = mazeCharAt g hw
        fleeing  = ghostsAreFleeing g


drawCell :: Attrs -> Cell -> Update ()
drawCell as cell = do
    setAttrUsing attr as
    drawString st
    clearAttrUpdate as
  where
    (attr, st) = case cell of
        PacmanW p   -> (PacmanAttr, strForPacman p)
        GhostW gd f -> ghostAttrAndStr gd f
        SpaceW      -> (EmptyAttr, " ")
        PillW       -> (PillAttr,  [pillChar])
        PowerupW    -> (PowerupAttr, [powerupChar])
        (WallW c)   -> (WallAttr,[c])


-- note this relies on pacTick being reset in game when pacman dies
strForPacman :: PacmanData -> String
strForPacman p = st
  where
    (V2 h w) = p ^. pacAt
    st = if p ^. pacDead
      then if _t >= length pacmanDiesChars
             then " "
             else [pacmanDiesChars !! _t]
      else  let s = fromMaybe "" $ lookup (p ^. pacDir) pacmanChars
                i = _t  `rem` length s
             in [s !! i]
    _t = p ^. pacAnimate


ghostAttrAndStr :: GhostData -> Bool -> (Attr, String)
ghostAttrAndStr gd fleeing = (ghAttr, st)
  where
    eyes = gd ^. ghostState `elem` [GhostGoingHome, GhostEnteringHouse]
    st   = if eyes then "\"" else "M"
    (V2 h w) = gd ^. ghostAt
    ghAttr = if fleeing
               then GhostFleeAttr
               else case gd ^. ghostName of
        Shadow  -> GhostShadowAttr
        Bashful -> GhostBashfulAttr
        Speedy  -> GhostSpeedyAttr
        Pokey   -> GhostPokeyAttr


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
  | LivesAttr
  | LevelAttr
  | InfoAttr
  | GameOverAttr
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
        livesAttr = (whiteID, [])
        levelAttr = (whiteID, [])
        infoAttr  = (yellowID, [AttributeBlink, AttributeBold])
        gameOverAttr = (redID, [AttributeBlink, AttributeBold])
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
                      , livesAttr
                      , levelAttr
                      , infoAttr
                      , gameOverAttr
                      ]
