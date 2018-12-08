{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell  #-}

module Pacman where

import           Control.Arrow       ((>>>))
import           Control.Applicative ((<|>))
import           Control.Monad       (guard, when, unless, (>=>))
import           Control.Monad.Trans.Writer.Strict (Writer, tell)
import           Data.Maybe          (fromMaybe, fromJust)
import           Data.Vector         (Vector)
import qualified Data.Vector         as V
import           Data.List           (findIndex)
import           Data.DList          (DList, singleton)
import           Data.Sort           (sortOn)

import           Lens.Micro          ((%~), (&), (.~), (^.), ix)
import           Lens.Micro.TH       (makeLenses)
import           Linear.V2           (V2 (..), _x, _y)
import           System.Random       (Random (..), newStdGen, StdGen, mkStdGen, randomR)

-- types
data Game = Game
  { _pacman          :: PacmanData
  , _ghosts          :: [GhostData]
  , _ghostsMode       :: GhostsMode
  , _ghostsModes      :: [GhostsMode]
  , _maze            :: Maze
  , _mazeHeight      :: Int
  , _mazeWidth       :: Int
  , _gameover        :: Bool
  , _state           :: GameState
  , _pillsLeft       :: Int
  , _startPills      :: Int
  , _paused          :: Bool
  , _score           :: Int
  , _gameLevel       :: Int
  , _livesLeft       :: Int
  , _randStdGen      :: StdGen
  } deriving (Show)


data PacmanData = PacmanData
  { _pacAt        :: Coord
  , _pacDir       :: Direction
  , _pacNextDir   :: Direction
  , _dying        :: Bool
  , _pacTick      :: Int
  , _pacAnimate   :: Int
  } deriving (Eq, Show)

data GhostPersonality
  = Shadow      -- "Blinky" is RED
  | Bashful     -- "Inky" is CYAN
  | Speedy      -- "Pinky" is PINK
  | Pokey       -- "Clyde" is ORANGE
  deriving (Show, Eq)

data GhostsState
  = GhostsHold
  | GhostsChase
  | GhostsScatter
  | GhostsFlee GhostsMode  -- we need to remember the current mode when fleeing
  deriving (Eq, Show)

type GhostsMode = (Maybe Int, GhostsState)

data GhostState
  = GhostHouse
  | GhostDead
  | GhostNormal
  deriving (Eq, Show)

data GhostRate
  = GhostRateNormal
  | GhostRateSpeedup1
  | GhostRateSpeedup2
  | GhostRateFlee
  deriving (Eq, Show)

data GhostData = GhostData
  { _ghostAt    :: Coord
  , _ghostDir   :: Direction
  , _ghostState :: GhostState
  , _name       :: GhostPersonality
  , _ghostRate  :: GhostRate
  , _ghostTick  :: Int
  } deriving (Eq, Show)

type Coord = V2 Int

type Maze = Vector (Vector Char)

data Direction
  = North
  | South
  | East
  | West
  | Still
  deriving (Eq, Show)

data GameState
  = NotStarted
  | Playing
  | GameOver
  deriving (Eq, Show)

data EatSomething
  = EatenPill
  | EatenPowerup
  | EatenGhost
  | EatenNothing
  deriving (Eq, Show)


makeLenses ''Game
makeLenses ''GhostData
makeLenses ''PacmanData

maze0 = [ "###########################"
        , "#............#............#"
        , "#.####.#####.#.#####.####.#"
        , "#*#  #.#   #.#.#   #.#  #*#"
        , "#.####.#####.#.#####.####.#"
        , "#.........................#"
        , "#.####.#.#########.#.####.#"
        , "#......#.....#.....#......#"
        , "######.##### # #####.######"
        , "     #.#           #.#     "
        , "######.# ###---### #.######"
        , "      .  #       # #.      "
        , "######.# ######### #.######"
        , "     #.#           #.#     "
        , "######.# ######### #.######"
        , "#............#............#"
        , "#.####.#####.#.#####.####.#"
        , "#*...#...............#...*#"
        , "####.#.#.#########.#.#.####"
        , "#......#.....#.....#......#"
        , "#.##########.#.##########.#"
        , "#.........................#"
        , "###########################"
        ]

data DrawListItem
  = DrawScore
  | DrawLevel
  | DrawLives
  | DrawGridAt Coord
  | DrawEverything
  deriving (Eq, Show)

drawPriority :: DrawListItem -> Int
drawPriority DrawEverything     = 0
drawPriority (DrawGridAt _)     = 1
drawPriority _                  = 2

type DrawListItems = DList DrawListItem

-- this is for the display update list as a writer monad
type DrawList a = Writer DrawListItems a

addDrawListItem :: DrawListItem -> DrawList ()
addDrawListItem dl = tell (singleton dl)

countPills :: [String] -> Int
countPills = sum . map (length . filter (==pillChar))

initialMaze :: [String] -> Maze
initialMaze = V.fromList . map V.fromList

mazeHW :: Maze -> (Int,Int)
mazeHW m = (h,w)
  where h = V.length m
        w = V.length $ m V.! 0

mazeCharAt :: Game -> V2 Int -> Char
mazeCharAt g (V2 h w) = ((g ^. maze) V.! h) V.! w

-- clear the maze location at hw
mazeClearAt :: Maze -> Coord -> Maze
mazeClearAt m (V2 h w) = m V.// [(h, r V.// [(w, ' ')])]
  where r = m V.! h


ghostChar = 'M'
pacmanChars = [ (West, ">}]>")
              , (East, "<[{<")
              , (North, "v|v")
              , (South, "^|^")
              , (Still, "C")
              ]
pacmanDiesChars = "X*:V-^-."
powerupChar = '*'
wallChars = "#-"
pillChar = '.'

-- |"New" ghost rates and speed ups.
framesPerSecond = 60.0
base100frames = 20.0

percentToFrames :: Float -> Int
percentToFrames t = round $ f / ((f/base100frames) *t)
  where f = framesPerSecond

data LevelRates = LevelRates
  { pacNorm :: Int
  , pacNormDots :: Int
  , pacFright :: Int
  , pacFrightDots :: Int
  , ghostNorm :: Int
  , ghostFright :: Int
  , ghostTunnel :: Int
  } deriving (Show)

makeLevelRates :: [Int] -> LevelRates
makeLevelRates (x1:x2:x3:x4:x5:x6:x7:xs) =
    LevelRates x1 x2 x3 x4 x5 x6 x7
makeLevelRates _ = error "Must pass a list of 7 elements to function"

-- next work out the rates from percentages

_percentageLevel :: [[Float]]
_percentageLevel =
  [ [0.80, 0.71, 0.90, 0.79, 0.75, 0.50, 0.40] -- level 1
  , [0.90, 0.79, 0.95, 0.83, 0.85, 0.55, 0.45] -- level 2-4
  , [1.00, 0.87, 1.00, 0.87, 0.95, 0.60, 0.50] -- level 5-20
  , [0.90, 0.76, 1.00, 1.00, 0.95, 1.00, 0.50] -- level 21+
  ]

levelToRates :: Vector LevelRates
levelToRates = V.fromList $
    map (makeLevelRates . map percentToFrames) _percentageLevel

ratesForLevel :: Int -> LevelRates
ratesForLevel n
  | n == 1           = levelToRates V.! 0
  | n > 1 && n < 5   = levelToRates V.! 1
  | n >= 4 && n < 21 = levelToRates V.! 2
  | n >= 21          = levelToRates V.! 3
  | otherwise        = error "No negative rates!"

-- | for each level we need to have times for each state
gameModes :: Vector [GhostsMode]
gameModes =
  V.fromList [
  -- Level 1
    [ (Just (7 * n), GhostsScatter)
    , (Just (20 * n), GhostsChase)
    , (Just (7 * n), GhostsScatter)
    , (Just (20 * n), GhostsChase)
    , (Just (5 * n), GhostsScatter)
    , (Just (20 * n), GhostsChase)
    , (Just (5 * n), GhostsScatter)
    , (Nothing, GhostsChase)]  -- indefinite
  -- level 2-4
  , [ (Just (7 * n), GhostsScatter)
    , (Just (20 * n), GhostsChase)
    , (Just (7 * n), GhostsScatter)
    , (Just (20 * n), GhostsChase)
    , (Just (5 * n), GhostsScatter)
    , (Just (1033 * n), GhostsChase)
    , (Just 1, GhostsScatter)  -- 1/60th of a second!
    , (Nothing, GhostsChase)]  -- indefinite
  -- level 5+
  , [ (Just (5 * n), GhostsScatter)
    , (Just (20 * n), GhostsChase)
    , (Just (5 * n), GhostsScatter)
    , (Just (20 * n), GhostsChase)
    , (Just (5 * n), GhostsScatter)
    , (Just (1037 * n), GhostsChase)
    , (Just 1, GhostsScatter)  -- 1/60th of a second!
    , (Nothing, GhostsChase)]  -- indefinite
  ]
    where n = round framesPerSecond

gameModesForLevel :: Int -> [GhostsMode]
gameModesForLevel n
  | n == 1         = gameModes V.! 0
  | n > 1 && n < 5 = gameModes V.! 1
  | n >=  5        = gameModes V.! 2
  | otherwise      = error "No negative gameModes!"

-- | flee time for ghosts for each level (levels start at 1)
fleeTimesSecs :: [Int]
fleeTimesSecs = [6, 5, 4, 3, 2, 5, 2, 2, 1, 5, 2, 1, 1, 3, 1, 1, 0, 1]

fleeFrames :: Vector Int
fleeFrames = V.fromList $ map convert fleeTimesSecs
  where convert n = round framesPerSecond * n

fleeFramesForLevel :: Int -> Int
fleeFramesForLevel n
  | n < 1 = error "Can't have a level less than 1"
  | n <= V.length fleeFrames = fleeFrames V.! (n-1)
  | otherwise = 0

-- | the rates at for speed up 1 and speed up 2
elroyRates :: [(Int, Float, Int, Float)]
elroyRates =
    [ ( 20, 0.80, 10, 0.85) -- level 1
    , ( 30, 0.90, 15, 0.95)
    , ( 40, 0.90, 20, 0.95)
    , ( 40, 0.90, 20, 0.95)
    , ( 40, 1.00, 20, 1.05) -- level 5
    , ( 50, 1.00, 25, 1.05)
    , ( 50, 1.00, 25, 1.05)
    , ( 50, 1.00, 25, 1.05)
    , ( 60, 1.00, 30, 1.05) -- level 9
    , ( 60, 1.00, 30, 1.05)
    , ( 60, 1.00, 30, 1.05)
    , ( 80, 1.00, 40, 1.05)
    , ( 80, 1.00, 40, 1.05) -- level 13
    , ( 80, 1.00, 40, 1.05)
    , (100, 1.00, 50, 1.05)
    , (100, 1.00, 50, 1.05)
    , (100, 1.00, 50, 1.05) -- level 17
    , (100, 1.00, 50, 1.05)
    , (120, 1.00, 60, 1.05) -- level 19 and on is the same
    ]

elroyFrames :: Vector (Int, Int, Int, Int)
elroyFrames = V.fromList $ map convert elroyRates
  where convert (x, r1, y, r2) = (x, f r1, y, f r2)
        f = percentToFrames

elroyFramesForLevel :: Int -> (Int, Int, Int, Int)
elroyFramesForLevel n
  | n < 1 = error "No levels below 1 please!"
  | n <= V.length elroyFrames = elroyFrames V.! (n-1)
  | otherwise = V.last elroyFrames


initialHoldSeconds = 3
initialHoldFrames = round framesPerSecond * initialHoldSeconds
ghostDeadAt = V2 11 12


-- | The initial game
initGameIO :: IO Game
initGameIO = return initGame

initGame :: Game
initGame
  = let m = initialMaze maze0
        (h, w) = mazeHW m
        numPills = countPills maze0
     in Game           { _pacman = initialPacman
                       , _ghosts = initialGhosts
                       , _ghostsMode = (Just initialHoldFrames, GhostsHold)
                       , _ghostsModes = gameModesForLevel 1
                       , _maze = m
                       , _mazeHeight = h
                       , _mazeWidth = w
                       , _gameover = False
                       , _state = NotStarted
                       , _pillsLeft = numPills
                       , _startPills = numPills
                       , _paused = False
                       , _score = 0
                       , _gameLevel = 1
                       , _livesLeft = 3
                       , _randStdGen = mkStdGen 1 -- pass this from caller!
                       }

initialGhosts :: [GhostData]
initialGhosts =
    --          location   dir  state       name    ghostTick
    [ GhostData (V2 11 11) West GhostHouse  Bashful GhostRateNormal 0
    , GhostData (V2 11 13) West GhostNormal Speedy  GhostRateNormal 0
    , GhostData (V2  9 13) East GhostNormal Shadow  GhostRateNormal 0
    , GhostData (V2 11 15) East GhostHouse  Pokey   GhostRateNormal 0
    ]

initialPacman :: PacmanData
--                         location   dir   next  dying pacTick pacAnimate
initialPacman = PacmanData (V2 17 13) Still Still False 0       0


-- ACTIONS from the UI. i.e. turn, pause, processTick
-- each action called from the UI module has a signature of
-- action :: Game -> DrawList Game
-- this is to allow the action to return a drawlist as well as the modified
-- Game.

-- | pauseAction -- pause and un-pause the game
pauseAction :: Game -> DrawList Game
pauseAction = return . pause

-- | pause and un-pause the game
pause :: Game -> Game
pause = paused %~ not

-- | turnAction -- set up a turn for the user
turnAction :: Direction -> Game -> DrawList Game
turnAction d = return . turn d

-- change the direction using a lens
turn :: Direction -> Game -> Game
turn d g = if (dir /= Still) && checkForWall g hw'
             then g & (pacman . pacNextDir) .~ d
             else g & (pacman . pacDir) .~ d
                    & (pacman . pacNextDir) .~ Still
  where deltaHw = deltaForDir d
        dir = g ^. (pacman . pacDir)
        hw  = g ^. (pacman . pacAt)
        hw' = wrapAroundBoard g (hw + deltaHw)


-- | tickAction -- everything we have to do when it ticks.  Everything called
-- <something>Action returns a Writer Monad.
tickAction :: Game -> DrawList Game
tickAction g =
    if g ^. paused
      then return g
      else (   decPacmanTick
           >=> maybeUpdateGhostsMode
           >=> maybeDoPacman
           ) g


-- Just step the game forward one tick
-- TODO: is game over (i.e. eaten all pills)
{-
stepPacman :: Game -> Game
stepPacman g =
    if g ^. paused
      then g
      else (   decPacmanTick
           >>> maybeUpdateGhostsMode   -- update the ghost mode based on game ticks
           >>> maybeDoPacman
           >>> eatGhostOrBeEaton) g
-}


{-
stepGhostsNormal, stepGhostsSpeedup1, stepGhostsSpeedup2, stepGhostsFlee :: Game -> Game
stepGhostsNormal   = stepGhostsRate GhostRateNormal
stepGhostsSpeedup1 = stepGhostsRate GhostRateSpeedup1
stepGhostsSpeedup2 = stepGhostsRate GhostRateSpeedup2
stepGhostsFlee     = stepGhostsRate GhostRateFlee

stepGhostsRate :: GhostRate -> Game -> Game
stepGhostsRate rate
  =   moveGhostsInRate rate
  >>> eatGhostOrBeEaton
-}

-- TODO: delete this as it's no longer used
{-
step :: Game -> Game
step =
    maybeDoPacman
  . eatGhostOrBeEaton
  . moveGhosts
  . maybeUpdateGhostsMode
-}


-- | move the pacman and check for the pill or powerup
-- | invalidate the render cache if we moved (in movePacman)
{-
maybeDoPacman :: Game -> Game
maybeDoPacman g
  | not onTick = g
  | otherwise = (movePacman >>> eatPillOrPowerUp) g
  where
    onTick = (g ^. (pacman . pacTick)) == 0
-}

maybeDoPacman :: Game -> DrawList Game
maybeDoPacman g
  | not onTick = return g
  | otherwise = (movePacmanAction >=> eatPillOrPowerUpAction) g
  where
    onTick = (g ^. (pacman . pacTick)) == 0


-- | Decrement the pacTick (when to move the pacman). Note, we only increement
-- the pacAnimate tick when we move.
decPacmanTick :: Game -> DrawList Game
decPacmanTick g = return $ g & (pacman . pacTick) %~ pred


-- move the pacman until he hits a wall
-- includes the 'nextDir' to queue up the next turn as needed, so turning
-- is easier
movePacmanAction :: Game -> DrawList Game
movePacmanAction g
  | pickNextDir = do
      addDrawListItem $ DrawGridAt hw
      addDrawListItem $ DrawGridAt hw'
      return $ g
        & (pacman . pacDir) .~ nextDir
        & (pacman . pacAt) .~ nextHw
        & (pacman . pacNextDir) .~ Still
        & (pacman . pacAnimate) .~ 0
  | pickDir = do
      addDrawListItem $ DrawGridAt hw
      addDrawListItem $ DrawGridAt hw'
      return $ g
        & (pacman . pacAt) .~ hw'
        & (pacman . pacAnimate) %~ (+1)
  | otherwise = do
      addDrawListItem $ DrawGridAt hw'
      return $ g
        & (pacman . pacDir) .~ Still
        & (pacman . pacNextDir) .~ Still
        & (pacman . pacAnimate) .~ 0
  where dir = g ^. (pacman . pacDir)
        deltaHw = deltaForDir dir
        hw = g ^. (pacman . pacAt)
        hw' = wrapAroundBoard g (hw + deltaHw)
        nextDir = g ^. (pacman . pacNextDir)
        deltaNextHw = deltaForDir nextDir
        nextHw = wrapAroundBoard g (hw + deltaNextHw)
        pickNextDir = (nextDir /= Still) && not (checkForWall g nextHw)
        pickDir = not (checkForWall g hw')


-- Maybe eat a pill based on the pacman position
eatPillOrPowerUpAction :: Game -> DrawList Game
eatPillOrPowerUpAction g
  | c == pillChar = do
      addDrawListItem DrawScore
      return $ g
        & score %~ (+10)
        & pillsLeft %~ pred
        & maze  %~ (`mazeClearAt` hw)
        & (pacman . pacTick) .~ choosePacTick g EatenPill
  | c == powerupChar = do
      redrawGhosts g
      addDrawListItem DrawScore
      return $ g
        & score %~ (+50)
        & maze %~ (`mazeClearAt` hw)
        & ghostsMode .~ fleePacman g
        & (pacman . pacTick) .~ choosePacTick g EatenPowerup
        & ghosts %~ map (ghostDir %~ reverseDirection)
  | otherwise = return $ g & (pacman . pacTick) .~ choosePacTick g EatenNothing
  where hw = g ^. (pacman . pacAt)
        c = mazeCharAt g hw


-- | redraw all the ghosts - probably due to a mode change.
redrawGhosts :: Game -> DrawList ()
redrawGhosts g =
    mapM_ (\gd -> addDrawListItem (DrawGridAt (gd ^. ghostAt))) (g ^. ghosts)


-- | choose the delay to the next move for pacman
-- A powerup adds 3 frames to a pill. Frightened mode is a bit quicker
choosePacTick :: Game -> EatSomething -> Int
choosePacTick g eaten = nextTick
  where
    rates = ratesForLevel $ g ^. gameLevel
    fleeing = ghostsAreFleeing g
    f = case (eaten, fleeing) of
        (EatenPill, True) -> pacFrightDots
        (EatenPill, False) -> pacNormDots
        (_, True) -> pacFright
        (_, False) -> pacNorm
    nextTick = if eaten == EatenPowerup
                 then choosePacTick g EatenPill + 3
                 else f rates


fleePacman :: Game -> GhostsMode
fleePacman g = if fleeTicks == 0
                 then currentMode
                 else (Just fleeTicks, GhostsFlee currentMode)
  where currentMode = g ^. ghostsMode
        fleeTicks = fleeFramesForLevel $ g ^. gameLevel

-- | simple chase strategy -- just chase for ticks -- no variation
--chasePacman :: Game -> GhostsMode
--chasePacman g = GhostChase $ ghostChaseForTicks + (g ^. gameTick)

-- | scatter from pacman -- just scatter for ticks -- no variation
--scatterFromPacman :: Game -> GhostsMode
--scatterFromPacman g = GhostScatter $ ghostScatterForTicks + (g ^. gameTick)

-- | move the ghosts and check for pacman interface; but only if on tick.
-- | invalidate the render cache if we moved
-- | TODO: delete this as no longer needed
{-
maybeDoGhosts :: Game -> Game
maybeDoGhosts g
  | not onTick = g
  | otherwise = (eatPillOrPowerUp . moveGhosts) g
  where
    onTick = (g ^. gameTick) `rem` ticksPerPacman == 0
-}

eatGhostOrBeEaton :: Game -> Game
eatGhostOrBeEaton g
  | fleeing = if not (null ghs)
                then eatGhost (head ghs) g
                else g
  | otherwise = if null ghs || (g ^. pacman . dying)
                  then g
                  else eatonByGhost g
  where
      xy = g ^. pacman . pacAt
      ghs = filter ((==xy).(^. ghostAt)) $ g ^. ghosts
      fleeing = ghostsAreFleeing g


ghostsAreFleeing :: Game -> Bool
ghostsAreFleeing g = case g ^. ghostsMode of
    (_, GhostsFlee _) -> True
    _ -> False


-- | eat a ghost.  We get 200 for the first, 400 for the second, 800 for the
-- third and 1600 for the fourth with an extra bonus of 1200 for all 4.
eatGhost :: GhostData -> Game -> Game
eatGhost gd g = g & score %~ scoref
                  & (ghosts . ix i . ghostState) .~ GhostDead
                  & (ghosts . ix i . ghostAt) .~ ghostDeadAt
  where
      i = fromMaybe 0 $ findIndex ((==gd ^. name).(^. name)) $ g ^. ghosts
      numDead = length $ filter ((==GhostDead).(^.ghostState)) $ g ^. ghosts
      ghostScore = (2 ^ numDead) * 200
      bonusScore = if numDead == 3 then 1200 else 0
      scoref n = n + ghostScore + bonusScore


nameToGhostData :: GhostPersonality -> Game -> GhostData
nameToGhostData gp g = head $ filter ((==gp).(^. name)) $ g ^. ghosts


eatonByGhost :: Game -> Game
eatonByGhost g = g & gameover .~ True
                   & (pacman . dying) .~ True
                   & (pacman . pacAnimate) .~ 0


-- | check the current Ghost mode and change it if the ticks have expired.
--maybeUpdateGhostMode :: Game -> Game
--maybeUpdateGhostMode g = ifNextMode $ case g ^. ghostMode of
    --(GhostHold t)    -> (initialHoldFrames, chasePacman)
    --(GhostChase t)   -> (t, scatterFromPacman)
    --(GhostScatter t) -> (t, chasePacman)
    --(GhostFlee t _)  -> (t, chasePacman)
  --where
      --ifNextMode :: (Int, Game -> GhostMode) -> Game
      --ifNextMode (_t, f) = if g ^. gameTick > _t
                             --then g & ghostMode .~ f g
                             --else g

-- | see if we choose the next ghost mode
-- Essentially, count down the mode and if zero, pick the next one, unless we
-- are already at the last one (GhostChase Nothing)
maybeUpdateGhostsMode :: Game -> DrawList Game
maybeUpdateGhostsMode g = case g ^. ghostsMode of
    (Nothing, _) -> return g
    (Just 1, m) ->
        case m of
            (GhostsFlee oldMode) -> do
                redrawGhosts g
                return $ g & ghostsMode .~ oldMode
            _ -> let (newMode, modesLeft) = nextGhostsMode g
                  in return $
                      g & ghostsMode .~ newMode
                        & ghostsModes .~ modesLeft
    (Just n, m) -> return $ g & ghostsMode .~ (Just (n-1), m)


nextGhostsMode :: Game -> (GhostsMode, [GhostsMode])
nextGhostsMode g = case g ^. ghostsModes of
    (m:ms) -> (m, ms)
    _      -> (g ^. ghostsMode, [])


-- move all the ghosts, one after another, but returning a function which does
-- it
moveGhosts :: Game -> Game
moveGhosts g = foldr moveGhost g $ zip [0..] (g ^. ghosts)

-- TODO: we don't want to do the fold; we want to use g & ghosts %~ map (some
-- function) which will apply "some function" to each of the ghosts
moveGhostsInRate :: GhostRate -> Game -> Game
moveGhostsInRate rate g
  = foldr moveGhost g
  $ filter ((==rate) . (^. ghostRate) . snd)
  $ zip [0..] (g ^. ghosts)


moveGhost :: (Int, GhostData) -> Game -> Game
moveGhost gh@(i, gd) g = case g ^. ghostsMode of
    (_, GhostsHold)     -> g
    (_, GhostsChase)    -> moveGhostChase gh g
    (_, GhostsScatter)  -> moveGhostScatter gh g
    (_, GhostsFlee _)   -> moveGhostFlee gh g


moveGhostChase :: (Int, GhostData) -> Game -> Game
moveGhostChase _ g = g


moveGhostScatter :: (Int, GhostData) -> Game -> Game
moveGhostScatter _ g = g


moveGhostFlee :: (Int, GhostData) -> Game -> Game
moveGhostFlee _ g = g


-- TODO: delete this, but first we have to use the speedups, etc.
{-
isGhostOnTick :: GhostData -> Game -> Bool
isGhostOnTick gd g
  | fleeing = _ticks `rem` ticksPerGhostFleeing == 0
  | otherwise = _ticks `rem` ticksPerGhost == 0
  where
      fleeing = ghostsAreFleeing g
      _ticks = g ^. gameTick
      sPills = g ^. startPills
      pillsEaton = sPills - (g ^. pillsLeft)
      l1 = pillsEaton > (sPills * fst ghostSpeedUp1) `quot` snd ghostSpeedUp1
      l2 = pillsEaton > (sPills * fst ghostSpeedUp2) `quot` snd ghostSpeedUp2
      ticksPerGhost = case gd ^. name of
          Shadow | l2 -> ticksPerGhostSpeedup2
                 | l1 -> ticksPerGhostSpeedup1
                 | otherwise -> ticksPerGhostNormal
          _ -> ticksPerGhostNormal
-}


chooseGhostTick :: Game -> GhostData -> Int
chooseGhostTick g gd
  | inTunnel  = ghostTunnel rates
  | fleeing   = ghostFright rates
  | isShadow  = frames
  | otherwise = ghostNorm rates
  where
      level    = g ^. gameLevel
      fleeing  = ghostsAreFleeing g
      rates    = ratesForLevel level
      isShadow = gd ^. name == Shadow
      (V2 h w) = gd ^. ghostAt
      inTunnel = h == 11 && ((w <= 5) || (w >= 21))
      ps = g ^. pillsLeft
      (p1, f1, p2, f2) = elroyFramesForLevel level
      frames = case (ps <= p2, ps <= p1) of
          (True, _) -> f2
          (_, True) -> f1
          _         -> ghostNorm rates


-- check if any of the bounds on the V2 Int is outside of the board
-- and wrap them if they are
wrapAroundBoard :: Game -> V2 Int -> V2 Int
wrapAroundBoard g (V2 h w)
  = V2 ((h + height) `rem` height) ((w + width) `rem` width)
  where height = g ^. mazeHeight
        width  = g ^. mazeWidth


-- check for a wall
checkForWall :: Game -> V2 Int -> Bool
checkForWall g hw = mazeCharAt g hw `elem` wallChars


deltaForDir :: Direction -> V2 Int
deltaForDir North = V2 (-1) 0
deltaForDir West  = V2 0 (-1)
deltaForDir East  = V2 0 1
deltaForDir South = V2 1 0
deltaForDir Still = V2 0 0


leftOfDir :: Direction -> Direction
leftOfDir North = West
leftOfDir West  = South
leftOfDir South = East
leftOfDir East  = North


rightOfDir :: Direction -> Direction
rightOfDir North = East
rightOfDir East  = South
rightOfDir South = West
rightOfDir West  = North


-- | Ghost move modes and target tiles

greenDecisionTiles = map (uncurry V2) [
    (1,6), (1,20),
    (5,1), (5,6), (5,8), (5,12), (5,14), (5,18), (5,20), (5,25),
    (7,6), (7,20),
    (11,6), (11,8), (11,18), (11,20),
    (13,8), (13,18),
    (15,6), (15,8), (15,18), (15,20),
    (17,6), (17,8), (17,18), (17,20), -- note no yellows here!
    (19,4), (19,23),
    (21,12), (21,14)]


yellowDecisionTiles = map (uncurry V2) [
    (13,12), (13,14),
    (17,12), (17,14)]


-- | Scatter mode
-- | The target tiles are outside of the map, which causes the ghosts
-- | to flee to the corners
targetTileScatter :: GhostData -> Game -> Coord
targetTileScatter gd g = case gd ^. name of
    Speedy  -> V2 (-2) 2   -- Pink ghost (2 above 2 in from lhs)
    Shadow  ->
        if gd ^. ghostRate /= GhostRateNormal
          && not (any ((==GhostHouse).(^. ghostState)) (g ^. ghosts))
           then targetTileChase gd g  -- Shadow chases in scatter!
           else V2 (-2) 24            -- Red ghost (2 above, 2 in from rhs)
    Bashful -> V2 23   26  -- Cyan ghost (1 bwlow, 0 in from rhs)
    Pokey   -> V2 23   0   -- Orange ghost (1 below, 0 in from lhs)


targetTileChase :: GhostData -> Game -> Coord
targetTileChase gd g = case gd ^. name of
    -- Pink Ghost = 4 frames in front of pacman
    Speedy  -> pacmanHw + 4 * pacmanDeltaHw
    -- Red ghost follows pacman
    Shadow  -> pacmanHw
    -- Cyan ghost does 2 * vector of red -> (pac + 2 in front)
    Bashful -> let twoTiles = pacmanHw + 2 * pacmanDeltaHw
                   shadowGd = head $ filter ((==Bashful).(^.name)) $ g ^. ghosts
                   shadowHw = shadowGd ^. ghostAt
                in 2 * twoTiles - shadowHw
    -- Orange ghost
    Pokey   -> let (V2 dx dy) = pacmanHw - gd ^. ghostAt
                   deltaSquared = (dx * dx) + (dy * dy)
                in if deltaSquared > 64  -- 8 tiles away means target pacman
                     then pacmanHw
                     else targetTileScatter gd g -- otherwise to scatter tile
  where
      pacmanHw = g ^. (pacman . pacAt)
      pacmanDeltaHw = deltaForDir $ g ^. (pacman . pacDir)


-- | move ghost, no decision
-- | Essentially move "forward" in the current direction, unless there
-- | is a wall.  If there is a wall then turn to the wall and change
-- | direction. Also invalidate the cache.
moveGhostNoDecision :: (Int, GhostData) -> Game -> Game
moveGhostNoDecision (i, gd) g
  | notWillBeWall = g & (ghosts . ix i . ghostAt) .~ forwardHw
  | otherwise     = if leftWillBeWall
        then g & (ghosts . ix i . ghostAt) .~ rightOfHw
               & (ghosts . ix i . ghostDir) .~ goRight
        else g & (ghosts . ix i . ghostAt) .~ leftOfHw
               & (ghosts . ix i . ghostDir) .~ goLeft
    where
        dir = gd ^. ghostDir
        hw  = gd ^. ghostAt
        forwardHw = wrapAroundBoard g (hw + deltaForDir dir)
        notWillBeWall = not $ checkForWall g forwardHw
        goLeft = leftOfDir dir
        leftOfHw = wrapAroundBoard g (hw + deltaForDir goLeft)
        goRight = rightOfDir dir
        rightOfHw = wrapAroundBoard g (hw + deltaForDir goRight)
        leftWillBeWall = checkForWall g leftOfHw


-- move ghost green decision
-- we've reached a decision point; for each mode, we may have to chose a  new
-- direction and square.

-- not sure about this; think we should have a range of decisions and then pass
-- those possible decisions to a single set of functions
--moveGhostGreenDecision :: (Int, GhostData) -> Game -> Game
--moveGhostGreenDecision gh@(i, gd) g = case g ^. ghostMode of
      --GhostHold         -> g
      --(GhostChase _)    -> moveGhostChaseGreenDecision gh g
      --(GhostScatter  _) -> moveGhostScatterGreenDecision gh g
      --(GhostFlee _)     -> moveGhostFleeGreenDecision gh g


possibleDecisionsAt :: (Int, GhostData) -> Game -> [(Direction, Coord)]
possibleDecisionsAt (_, gd) g
  = filter (not . checkForWall g . snd) $ zip dirs hws
  where
    dirs = directionsAt $ gd ^. ghostDir
    hw  = gd ^. ghostAt
    hws = [wrapAroundBoard g (hw + deltaForDir d) | d <- dirs]


chooseShortest :: Coord -> [(Direction, Coord)] -> (Direction, Coord)
chooseShortest (V2 x y) ps = snd $ head $ sortOn fst $ zip ms ps
  where
      ms = map (mag . snd) ps
      mag hw = let (V2 x1 y1) = hw
                   xd = x - x1
                   yd = y - y1
                in xd * xd + yd * yd


chooseTargetHw :: GhostData -> Game -> Coord
chooseTargetHw gd g = case g ^. ghostsMode of
    (_, GhostsHold)    -> error "Don't call chooseTargetHw in GhostHold mode!"
    (_, GhostsChase)   -> targetTileChase gd g
    (_, GhostsScatter) -> targetTileScatter gd g
    (_, GhostsFlee _)  -> error "Don't call chooseTargetHw in GhostFlee mode!"


-- | choose a random direction from a selection of possible directions, but
-- | return the new generator as well.
chooseRandomDir :: [Direction] -> StdGen -> (Direction, StdGen)
chooseRandomDir [] _ = error "No directions passed so can't choose one!"
chooseRandomDir ds gen =
    let (i, gen') = randomR (0, length ds -1) gen
        d = fromJust $ lookup i (zip [0..] ds)
    in (d, gen')


-- | make the fleeing decision for this ghost.
makeFleeingDecision :: (Int, GhostData)
                    -- ^ the index and GhostData for this ghost
                    -> [Direction]
                    -- ^ the list of allowable directions at this point
                    -> Game
                    -- ^ the Current game state that includes the random
                    --   number generator state, which needs updating
                    -> Game
makeFleeingDecision gh@(i, gd) ds g =
    let ps = filter ((`notElem` ds).fst) $ possibleDecisionsAt gh g
        (d, newGen) = chooseRandomDir (map fst ps) (g ^. randStdGen)
        hw = fromJust $ lookup d ps
     in g & randStdGen .~ newGen
          & (ghosts . ix i . ghostAt) .~ hw
          & (ghosts . ix i . ghostDir) .~ d


filterOutNorth :: [(Direction, Coord)] -> [(Direction, Coord)]
filterOutNorth = filter ((/=North).fst)


directionsAt :: Direction -> [Direction]
directionsAt North = [North, East,  West]
directionsAt South = [South, East,  West]
directionsAt East  = [East,  North, South]
directionsAt West  = [West,  North, South]


reverseDirection :: Direction -> Direction
reverseDirection North = South
reverseDirection South = North
reverseDirection East  = West
reverseDirection West  = East
