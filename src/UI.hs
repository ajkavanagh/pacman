{-# LANGUAGE OverloadedStrings #-}
module UI where

import Control.Monad (forever, void, when)
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent (threadDelay, forkIO)
import Data.Maybe (fromMaybe)

import Data.Vector (Vector, (!))
--import qualified Data.Vector as V

import Pacman

import Brick
  ( App(..), AttrMap, BrickEvent(..), EventM, Next, Widget
  , customMain, neverShowCursor
  , continue, halt
  , invalidateCache, invalidateCacheEntry, cached
  , hLimit, vLimit, vBox, hBox
  , padRight, padLeft, padTop, padAll, Padding(..)
  , withBorderStyle
  , str
  , attrMap, withAttr, emptyWidget, AttrName, on, fg
  , (<+>)
  )
import Brick.BChan (newBChan, writeBChan)
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center as C
import qualified Graphics.Vty as V
import Data.Sequence (Seq)
import qualified Data.Sequence as S
import Linear.V2 (V2(..))
import Lens.Micro ((^.))

-- Types

-- | Ticks mark passing of time
--
-- This is our custom event that will be constantly fed into the app.
data Tick = TickPacman
          | TickGhostNormal
          | TickGhostSpeedup1
          | TickGhostSpeedup2
          | TickGhostFlee

-- | Named resources
--
-- Not currently used, but will be easier to refactor
-- if we call this "Name" now.
--type Name = ()
data Name = GridR | StatsR deriving (Eq, Ord)

-- | Things we draw
data Cell = PacmanW PacmanData
          | GhostW GhostData Bool
          | PowerupW
          | PillW
          | WallW Char
          | SpaceW
  deriving (Eq, Show)

-- App definition

app :: App Game Tick Name
app = App { appDraw = drawUI
          , appChooseCursor = neverShowCursor
          , appHandleEvent = handleEvent
          , appStartEvent = return
          , appAttrMap = const theMap
          }

--forkHelper :: ??? -> Name -> Int -> IO ThreadId
forkHelper chan tickType delay = forkIO $ forever $ do
    writeBChan chan tickType
    threadDelay delay

main :: IO ()
main = do
  chan <- newBChan 10
  --forkIO $ forever $ do
    --writeBChan chan Tick
    --threadDelay 10000 -- decides how fast your game moves
  forkHelper chan TickPacman threadDelayPackman
  forkHelper chan TickGhostNormal threadDelayGhostNormal
  forkHelper chan TickGhostSpeedup1 threadDelayGhostSpeedup1
  forkHelper chan TickGhostSpeedup2 threadDelayGhostSpeedup2
  forkHelper chan TickGhostFlee threadDelayGhostFlee
  g <- initGameIO
  void $ customMain (V.mkVty V.defaultConfig) (Just chan) app g

-- Handling events

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

-- Drawing

drawUI :: Game -> [Widget Name]
drawUI g =
    [ cached StatsR (C.center $ padRight (Pad 2) (drawStats g))
    <+> cached GridR (drawGrid g) ]

drawStats :: Game -> Widget Name
--drawStats g = hLimit 11
drawStats g = hLimit 32
  -- $ vBox [ emptyWidget --drawScore (g ^. score)
  $ vBox [ drawScore (g ^. score)
         -- , drawDebugPacman g
         -- , drawDebugGame g
         -- , drawDebugGhost g Shadow
         -- , drawDebugGhost g Bashful
         -- , drawDebugGhost g Speedy
         -- , drawDebugGhost g Pokey
         {-, padTop (Pad 2) $ drawGameOver (g ^. gameover)-}
         , padTop (Pad 2) $ drawGameOverOrPaused g
         ]

drawScore :: Int -> Widget Name
drawScore n = withBorderStyle BS.unicodeBold
  $ B.borderWithLabel (str "Score")
  $ C.hCenter
  $ padAll 1
  $ str $ show n

drawDebugPacman :: Game -> Widget Name
drawDebugPacman g = withBorderStyle BS.unicodeBold
  $ B.borderWithLabel (str "Pacman debug")
  $ vBox [ hBox [str "pacAt:      ", str $ show (p ^. pacAt)]
         , hBox [str "pacDir:     ", str $ show (p ^. pacDir)]
         , hBox [str "pacNextDir: ", str $ show (p ^. pacNextDir)]
         , hBox [str "dying:      ", str $ show (p ^. dying)]
         ]
  where p = g ^. pacman

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

drawGameOver :: Bool -> Widget Name
drawGameOver True  = withAttr gameOverAttr $ C.hCenter $ str "GAME OVER"
drawGameOver False = emptyWidget

gameOverAttr :: AttrName
gameOverAttr = "gameOver"

drawPaused :: Widget Name
drawPaused = withAttr gameOverAttr $ C.hCenter $ str "Paused"

drawGrid :: Game -> Widget Name
drawGrid g = withBorderStyle BS.unicodeBold
  $ B.borderWithLabel (str "Pac-Man")
  $ vBox rows
  where
    height = g ^. mazeHeight
    width = g ^. mazeWidth
    rows         = [hBox $ cellsInRow h | h <- [0..height-1]]
    cellsInRow h = [drawCoord (V2 h w) | w <- [0..width-1]]
    drawCoord    = drawCell . cellAt g

-- we return a Cell which represents what we want to draw, which might well just
-- be the character in the maze
cellAt :: Game -> V2 Int -> Cell
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

-- note this relies on pacTick being reset in game when pacman dies
drawCell (PacmanW p) = withAttr pacmanAttr $
    if p ^. dying
      then if _t >= length pacmanDiesChars
             then str " "
             else str [pacmanDiesChars !! _t]
      else  let s = fromMaybe "" $ lookup (p ^. pacDir) pacmanChars
                i = _t  `rem` length s
             in str [s !! i]
  where _t = p ^. pacTick

-- attr strings for game str
emptyAttr, pillAttr, powerupAttr, wallAttr, pacmanAttr :: AttrName
emptyAttr   = "emptryAttr"
pillAttr    = "pillAttr"
powerupAttr = "powerupAttr"
wallAttr    = "wallAttr"
pacmanAttr  = "pacmanAttr"

-- and ghosts
ghostFleeAttr    = "ghostFleeAttr"
ghostShadowAttr  = "ghostShadowAttr"
ghostBashfulAttr = "ghostBashfulAttr"
ghostSpeedyAttr  = "ghostSpeedyAttr"
ghostPokeyAttr   = "ghostPokeyAttr"

-- now give them attributes
theMap :: AttrMap
theMap = attrMap V.defAttr
  [ (pillAttr, fg $ V.rgbColor 128 128 128)
  , (powerupAttr, fg V.white `V.withStyle` V.bold)
  , (wallAttr, fg V.white)
  , (pacmanAttr, fg V.yellow `V.withStyle` V.bold)
  , (ghostFleeAttr, fg $ V.rgbColor 0 0 135)
  , (ghostShadowAttr, fg V.red `V.withStyle` V.bold)
  , (ghostBashfulAttr, fg V.cyan `V.withStyle` V.bold)
  , (ghostSpeedyAttr, fg (V.rgbColor 255 95 215) `V.withStyle` V.bold)
  , (ghostPokeyAttr, fg V.yellow `V.withStyle` V.bold)
  ]
