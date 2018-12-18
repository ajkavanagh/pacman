{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell  #-}

module Pacman where

import           Control.Arrow       ((>>>))
import           Control.Applicative ((<|>))
import           Control.Monad       (guard, when, unless, (>=>), forM_)
import           Control.Monad.Trans.Writer.Strict (Writer, tell)
import           Data.Maybe          (fromMaybe, fromJust, isJust)
import           Data.Vector         (Vector)
import qualified Data.Vector         as V
import           Data.List           (findIndex)
import           Data.DList          (DList, singleton)
import           Data.Sort           (sortOn)
import           Data.Map            (Map)
import qualified Data.Map            as M

import           Lens.Micro          ((%~), (&), (.~), (^.), (^..), (?~)
                                     , ix, each, over)
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
  , _gameState       :: GameState
  , _pillsLeft       :: Int
  , _paused          :: Bool
  , _score           :: Int
  , _gameLevel       :: Int
  , _livesLeft       :: Int
  , _framesSincePill :: Int
  , _globalPillCount :: Maybe Int
  , _randStdGen      :: StdGen
  } deriving (Show)


data PacmanData = PacmanData
  { _pacAt        :: Coord
  , _pacDir       :: Direction
  , _pacNextDir   :: Direction
  , _pacDying     :: Bool
  , _pacTick      :: Int
  , _pacAnimate   :: Int
  } deriving (Eq, Show)

data GhostPersonality
  = Shadow      -- "Blinky" is RED
  | Speedy      -- "Pinky" is PINK
  | Bashful     -- "Inky" is CYAN
  | Pokey       -- "Clyde" is ORANGE
  deriving (Show, Eq, Ord, Enum)

data GhostsState
  = GhostsHold
  | GhostsChase
  | GhostsScatter
  | GhostsFlee GhostsMode  -- we need to remember the current mode when fleeing
  deriving (Eq, Show)

type GhostsMode = (Maybe Int, GhostsState)

data GhostState
  = GhostHouse
  | GhostLeavingHouse
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
  { _ghostAt         :: Coord
  , _ghostDir        :: Direction
  , _ghostState      :: GhostState
  , _name            :: GhostPersonality
  , _ghostTick       :: Int
  , _ghostPillCount  :: Int
  , _ghostRandStdGen :: StdGen
  } deriving (Show)

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
        , "      .  #       #  .      "
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
base100frames = 10.0

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


-- | timings/counts for leaving the ghost-house by level

houseDotLimits :: Vector (Map GhostPersonality Int)
houseDotLimits = V.fromList
  [ M.fromList [(Shadow, 0), (Speedy, 0), (Bashful, 30), (Pokey, 60)]
  , M.fromList [(Shadow, 0), (Speedy, 0), (Bashful, 0), (Pokey, 50)]
  ]

ghostHouseDotLimitFor :: GhostPersonality -> Game -> Int
ghostHouseDotLimitFor gp g
  | level > 2 = 0
  | otherwise = fromMaybe 0 $ M.lookup gp (houseDotLimits V.! (level - 1))
  where
      level = g ^. gameLevel


-- | global "dot-not-eaten" timeouts for each level
pillTimerExpired :: Game -> Bool
pillTimerExpired g = g ^. framesSincePill > frames
  where
      timeout = if g ^. gameLevel < 5 then 4 else 3
      frames = round framesPerSecond * timeout

-- | increment the frames since last pill timer, but not in the GhostsHold ghost
-- mode.
incPillTimerAction :: Game -> DrawList Game
incPillTimerAction g
  | gmode == GhostsHold = return g
  | otherwise           = return $ g & framesSincePill %~ succ
  where
      (_, gmode) = g ^. ghostsMode

clearPillTimer :: Game -> Game
clearPillTimer = framesSincePill .~ 0


incGlobalPillCount :: Game -> Game
incGlobalPillCount = globalPillCount %~ fmap succ


initialHoldSeconds = 3
initialHoldFrames = round framesPerSecond * initialHoldSeconds
ghostDeadAt = V2 11 12 -- TODO: delete this for GhostGoingHome state
ghostHouseExitCoord = V2 9 13

dyingComplete = length pacmanDiesChars * 2

-- | The initial game
initGameIO :: Int -> IO Game
initGameIO = return . initGame


initGame :: Int -> Game
initGame seed
  = let m = initialMaze maze0
        (ghostsSeed, stdGen) = random $ mkStdGen seed
        (h, w) = mazeHW m
        numPills = countPills maze0
     in Game           { _pacman = resetPacman
                       , _ghosts = resetGhosts ghostsSeed
                       , _ghostsMode = (Just initialHoldFrames, GhostsHold)
                       , _ghostsModes = gameModesForLevel 1
                       , _maze = m
                       , _mazeHeight = h
                       , _mazeWidth = w
                       , _gameState = NotStarted
                       , _pillsLeft = numPills
                       , _paused = False
                       , _score = 0
                       , _gameLevel = 1
                       , _livesLeft = 3
                       , _framesSincePill = 0
                       , _globalPillCount = Nothing
                       , _randStdGen = stdGen
                       }

resetGhosts :: Int -> [GhostData]
resetGhosts seed = map (\(c, g) -> c g) $ zip ghostIncompletConstructors stdGens
  where
      ghostIncompletConstructors =
        --          location   dir  state       name    ghostTick ghostPillCount
        [ GhostData (V2 11 11) West GhostHouse  Bashful 1         0
        , GhostData (V2 11 13) West GhostHouse  Speedy  1         0
        , GhostData (V2  9 13) East GhostNormal Shadow  1         0
        , GhostData (V2 11 15) East GhostHouse  Pokey   1         0
        ]
      stdGens = map mkStdGen $ randoms (mkStdGen seed)

-- | The initial Pac-man structure -- note that the pacTick is '1' because it
-- decrements FIRST and then gets checked.
--                         location   dir   next  dying pacTick pacAnimate
resetPacman :: PacmanData
resetPacman = PacmanData (V2 17 13) Still Still False 1       0


-- ACTIONS from the UI. i.e. turn, pause, processTick
-- each action called from the UI module has a signature of
-- action :: Game -> DrawList Game
-- this is to allow the action to return a drawlist as well as the modified
-- Game.

-- | startAction - get the game going
startAction :: Game -> Game -> DrawList Game
startAction g _ = do
    let g' = g & gameState .~ Playing
    addDrawListItem DrawEverything
    return g'

-- | pauseAction -- pause and un-pause the game
pauseAction :: Game -> DrawList Game
pauseAction = return . pause

-- | pause and un-pause the game
pause :: Game -> Game
pause g
  | notStarted = g
  | otherwise  = g & paused %~ not
  where
    notStarted = g ^. gameState == NotStarted

-- | debug helper to simulate dying on demand
debugDieAction :: Game -> DrawList Game
debugDieAction = return . eatenByGhost

-- | turnAction -- set up a turn for the user
turnAction :: Direction -> Game -> DrawList Game
turnAction d = return . turn d

-- change the direction using a lens
turn :: Direction -> Game -> Game
turn d g
  | notStarted = g
  | otherwise = if (dir /= Still) && checkForWall g hw'
             then g & (pacman . pacNextDir) .~ d
             else g & (pacman . pacDir) .~ d
                    & (pacman . pacNextDir) .~ Still
  where deltaHw = deltaForDir d
        dir = g ^. (pacman . pacDir)
        hw  = g ^. (pacman . pacAt)
        hw' = wrapAroundBoard g (hw + deltaHw)
        notStarted = g ^. gameState == NotStarted


-- | tickAction -- everything we have to do when it ticks.  Everything called
-- <something>Action returns a Writer Monad.
tickAction :: Game -> DrawList Game
tickAction g =
    if g ^. paused || g ^. gameState == NotStarted
      then return g
      else (   incPillTimerAction
           >=> maybeDoPacmanAction
           >=> maybeUpdateGhostsModeAction
           >=> seeIfGhostShouldLeaveHouseAction
           >=> moveGhostsAction
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


-- | Decrement the tick and move the pacman if it has reached zero.  Also
-- check for the pill or powerup
maybeDoPacmanAction :: Game -> DrawList Game
maybeDoPacmanAction g
  | not onTick = return g'
  | isDying    = whilstDyingAction g'
  | otherwise = (movePacmanAction >=> eatPillOrPowerUpAction) g'
  where
    g' = g & (pacman . pacTick) %~ (\t -> max 0 $ t - 1)
    onTick = (g' ^. (pacman . pacTick)) == 0
    isDying = g' ^. pacman . pacDying


-- | whilstDyingAction -- keep the display updated for each animate tick
-- whilst the pacman is dying.  After a timer is expired (pacAnimate reaches
-- some value), then reset the game state for the next life.
whilstDyingAction :: Game -> DrawList Game
whilstDyingAction g
  -- not dead yet; so keep animating
  | _t < dyingComplete = do
      addDrawListItem $ DrawGridAt $ g ^. pacman . pacAt
      return $ g & (pacman . pacAnimate) %~ succ
                 & (pacman . pacTick) .~ choosePacTick g EatenNothing
  -- Game over -- the game is done, so just leave it animating the ghosts
  | remainingLives == 0 = do
      addDrawListItem DrawEverything
      return $ g & gameState .~ GameOver
  -- we've lost a life, so reset pacman, the ghosts, and enable the global
  -- pill count to get ghosts out of the house
  | otherwise = do
      addDrawListItem DrawEverything
      return $ g & livesLeft .~ remainingLives
                 & pacman .~ resetPacman             -- tell pacman where to go
                 & ghosts .~ resetGhosts ghostsSeed  -- put ghosts back in the house
                 & framesSincePill .~ 0              -- reset global frames since pill
                 & globalPillCount ?~ 0              -- Set to Just 0, to activate global counter
                 & randStdGen .~ newStdGen
  where
      _t = (g ^. pacman . pacAnimate) + 1
      remainingLives = (g ^. livesLeft) - 1
      (ghostsSeed, newStdGen) = random $ g ^. randStdGen


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
        & maybeAddGhostPillCount
        & incGlobalPillCount
        & clearPillTimer
  | c == powerupChar = do
      redrawGhostsAction g
      addDrawListItem DrawScore
      return $ g
        & score %~ (+50)
        & maze %~ (`mazeClearAt` hw)
        & ghostsMode .~ fleePacman g
        & (pacman . pacTick) .~ choosePacTick g EatenPowerup
        & ghosts . each %~ ghostDir %~ reverseDirection
  | otherwise = return $ g & (pacman . pacTick) .~ choosePacTick g EatenNothing
  where hw = g ^. (pacman . pacAt)
        c = mazeCharAt g hw


-- | redraw all the ghosts - probably due to a mode change.
redrawGhostsAction :: Game -> DrawList ()
redrawGhostsAction g =
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
eatGhostOrBeEaton g = case (null ghs, fleeing) of
    (True, _)      -> g
    (False, True)  -> eatGhost (head ghs) g
    (False, False) -> eatenByGhost g
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
                  & ghosts . each %~ killGhost (gd ^. name)
  where
    numDead = length $ filter ((==GhostDead).(^.ghostState)) $ g ^. ghosts
    ghostScore = (2 ^ numDead) * 200
    bonusScore = if numDead == 3 then 1200 else 0
    scoref n = n + ghostScore + bonusScore


-- | kill the ghost with the name
killGhost :: GhostPersonality -> GhostData -> GhostData
killGhost p gd
  | p == gd ^. name = gd & ghostState .~ GhostDead
                         & ghostAt .~ ghostDeadAt
  | otherwise       = gd


-- | Eaten by a ghost -- essentially, we have died.  We need to allow time for
-- the animation (a timer) and then proceed to losing the life, reseting the
-- ghosts and carrying on.  This is handled in the whilstDyingAction function
eatenByGhost :: Game -> Game
eatenByGhost g = g & (pacman . pacDying) .~ True
                   & (pacman . pacAnimate) .~ 0


-- | see if we choose the next ghost mode
-- Essentially, count down the mode and if zero, pick the next one, unless we
-- are already at the last one (GhostChase Nothing)
maybeUpdateGhostsModeAction :: Game -> DrawList Game
maybeUpdateGhostsModeAction g = case g ^. ghostsMode of
    (Nothing, _) -> return g
    (Just 0, m) ->
        case m of
            (GhostsFlee oldMode) -> do
                redrawGhostsAction g
                return $ g & ghostsMode .~ oldMode
            _ -> let (newMode, modesLeft) = nextGhostsMode g
                  in return $
                      g & ghostsMode .~ newMode
                        & ghostsModes .~ modesLeft
                        & ghosts . each %~ ghostDir %~ reverseDirection
    (Just n, m) -> return $ g & ghostsMode .~ (Just (n-1), m)


nextGhostsMode :: Game -> (GhostsMode, [GhostsMode])
nextGhostsMode g = case g ^. ghostsModes of
    (m:ms) -> (m, ms)
    _      -> (g ^. ghostsMode, [])


-- | if we've just eaten a pill, then add 1 to the counter for the relevant
-- ghost in the ghost house, if any, and reset the global timer.
-- TODO: if special count for ghost is reached, move that ghost out, and reset
-- it's count.
-- TODO: reset the count of a ghost when it enters the house (i.e. after it has
-- been eaten).
maybeAddGhostPillCount :: Game -> Game
maybeAddGhostPillCount g
  | isJust gcounter = g
  | otherwise = case i of
      Just i' -> g & ghosts . ix i' . ghostPillCount %~ succ
      Nothing -> g
  where
      gcounter = g ^. globalPillCount
      gds = ghostsInTheHouse g
      i  = if null gds then Nothing else Just $ (fst . head) gds


ghostsInTheHouse :: Game -> [(Int, GhostData)]
ghostsInTheHouse g =
    sortOn (fromEnum.(^.name).snd)
    $ filter ((==GhostHouse).(^.ghostState).snd) -- TODO: add GhostGoingHome state here.
    $ zip [0..] (g ^. ghosts)

-- | Check to see if we've reach the globalDot count for a ghost.
-- Assumes that it is in the House and is the next ghost to leave.
isGlobalPillCountReachedForGhost :: (Int, GhostData) -> Game -> Game
isGlobalPillCountReachedForGhost (i, gd) g = fromMaybe g maybeLeaving
  where
      threashold = case gd ^. name of
          Speedy -> 7       -- "Pinky" is PINK
          Bashful -> 17     -- "Inky" is CYAN
          Pokey -> 32       -- "Clyde" is ORANGE
      ghostIsToLeave = makeGhostLeave i
      resetGlobalCounter = globalPillCount .~ Nothing
      maybeLeaving = do
          c <- g ^. globalPillCount
          if c == threashold
            then Just $ if gd ^. name == Pokey
                          then g & ghostIsToLeave & resetGlobalCounter
                          else g & ghostIsToLeave
            else Nothing

makeGhostLeave :: Int -> Game -> Game
makeGhostLeave i = ghosts . ix i . ghostState .~ GhostLeavingHouse


seeIfGhostShouldLeaveHouseAction :: Game -> DrawList Game
seeIfGhostShouldLeaveHouseAction g
  | null gds = return g
  | otherwise = return $ if pillTimerExpired g
                           then g & makeGhostLeave ((fst.head) gds)
                                  & clearPillTimer
                           else case g ^. globalPillCount of
                               Just _   -> isGlobalPillCountReachedForGhost (head gds) g
                               Nothing  -> isIndividualPillCountReached (head gds) g
  where
      gds = ghostsInTheHouse g


isIndividualPillCountReached :: (Int, GhostData) -> Game -> Game
isIndividualPillCountReached (i, gd) g
  = if breached
      then g & makeGhostLeave i
      else g
  where
      threashold = ghostHouseDotLimitFor (gd ^. name) g
      breached = gd ^. ghostPillCount >= threashold


-- | move all the ghosts, one after another, but returning a function which does
-- it.  This is an action, and it's really, really hard to move the monad into
-- moveGhost, but I like how moveGhost is an applicative lens traversal.
-- Therefore, we cheat a bit and just diff the positions of the ghosts before an
-- after the possible move and then draw them if they have moved.  It's a bit
-- wasteful, and it would be nice if I could push the monad into moveGhost, but
-- then I think I'd need a different approach.
moveGhostsAction :: Game -> DrawList Game
moveGhostsAction g = do
    let hws = g ^.. (ghosts . each . ghostAt)
        g' = g & ghosts . each %~ moveGhost g
        hws' = g' ^.. (ghosts . each . ghostAt)
        diffHws = filter (uncurry (/=)) $ zip hws hws'
    forM_ diffHws $ \(x, y) -> do
        addDrawListItem $ DrawGridAt x
        addDrawListItem $ DrawGridAt y
    return g'


-- | move the ghost if the ghostTick is decremented to 0, otherwise just return
-- the decremented gd with the tick down.  Note the next tick is ONLY choosen if
-- the ghost actually moves, although it may have changed direction.
-- TODO still need to get the ghost OUT OF the ghost house
-- Changes required:
-- 1. Only GhostsHold or GhostHouse means no tick change
-- 2. add in a check for gstate == GhostLeavingHouse (which has a tick) and
-- handle it separately
moveGhost :: Game -> GhostData -> GhostData
moveGhost g gd
  | gmode == GhostsHold || gstate == GhostHouse = gd
  | _t > 0 = gd & ghostTick .~ _t
  | gstate == GhostLeavingHouse = gd & moveGhostOutOfHouse g
  | isDecisionTime = newTickGd & makeDecision g gmode isYellowDecision
  | otherwise = newTickGd & moveGhostNoDecision g
  where
      (_, gmode) = g ^. ghostsMode
      gstate = gd ^. ghostState
      _t = max 0 $ gd ^. ghostTick - 1
      hw = gd ^. ghostAt
      isGreenDecision = hw `elem` greenDecisionTiles
      isYellowDecision = hw `elem` yellowDecisionTiles
      isDecisionTime = isGreenDecision || isYellowDecision
      newTickGd = gd & ghostTick .~ chooseGhostTick g gd


-- | Get the ghost out of the house using the normal rate for the level and
-- move the ghost to the exit above the house.  It's a "to centre of house and
-- then up" movement that should just happen.
-- Once the ghost hits the exit, it then switches to Normal and will proceed
-- as normal.
moveGhostOutOfHouse :: Game -> GhostData -> GhostData
moveGhostOutOfHouse g gd = if newHw == ghostHouseExitCoord
                             then newGd & ghostState .~ GhostNormal
                             else newGd
  where
      (V2 h w) = gd ^. ghostAt
      (V2 th tw) = ghostHouseExitCoord
      newHw = if w /= tw
                then if w > tw then V2 h (w-1) else V2 h (w+1)
                else V2 (h-1) w
      newGd = gd & ghostAt .~ newHw
                 & ghostTick .~ ghostNorm (ratesForLevel (g ^. gameLevel))

-- yellowDecision is greenDecision but not heading north!
-- | make a decision by picking a direction based on the ghoststate
makeDecision :: Game
             -> GhostsState
             -> Bool  -- if True, it's a yellow tile
             -> GhostData -> GhostData
makeDecision g gmode isYellow gd
  = if fleeing
      then gd & makeFleeingDecision g possibleDirections
      else gd & makeNonFleeingDecision g possibleDirections gmode
 where
    fleeing = ghostsAreFleeing g
    f = if isYellow then filterOutNorth else id
    possibleDirections = f $ possibleDecisionsAt g gd


makeFleeingDecision :: Game
                    -> [(Direction, Coord)]
                    -> GhostData -> GhostData
makeFleeingDecision g possibles gd
  = gd & ghostAt .~ hw
       & ghostDir .~ dir
       & ghostRandStdGen .~ gen
  where
    ((dir, hw), gen) = chooseRandomFromList possibles (gd ^. ghostRandStdGen)


makeNonFleeingDecision :: Game
                       -> [(Direction, Coord)]
                       -> GhostsState
                       -> GhostData -> GhostData
makeNonFleeingDecision g possibles gmode gd
  = gd & ghostAt .~ hw
       & ghostDir .~ dir
  where
    targetF = if gmode == GhostsScatter
                then targetTileScatter
                else targetTileChase
    (dir, hw) = selectShortest (targetF g gd) possibles


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
targetTileScatter :: Game -> GhostData -> Coord
targetTileScatter g gd = case gd ^. name of
    Speedy  -> V2 (-2) 2   -- Pink ghost (2 above 2 in from lhs)
    Shadow  ->
        if fastShadow
          && not (any ((==GhostHouse).(^. ghostState)) (g ^. ghosts))
           then targetTileChase g gd  -- Shadow chases in scatter!
           else V2 (-2) 24            -- Red ghost (2 above, 2 in from rhs)
    Bashful -> V2 23   26  -- Cyan ghost (1 bwlow, 0 in from rhs)
    Pokey   -> V2 23   0   -- Orange ghost (1 below, 0 in from lhs)
  where
    (p1, _, _, _) = elroyFramesForLevel $ g ^. gameLevel
    fastShadow = (g ^. pillsLeft) < p1


targetTileChase :: Game -> GhostData -> Coord
targetTileChase g gd = case gd ^. name of
    -- Pink Ghost = 4 frames in front of pacman
    Speedy  -> pacmanHw + 4 * pacmanDeltaHw
    -- Red ghost follows pacman
    Shadow  -> pacmanHw
    -- Cyan ghost does 2 * vector of red -> (pac + 2 in front)
    Bashful -> let twoTiles = pacmanHw + 2 * pacmanDeltaHw
                   shadowGd = head $ filter ((==Shadow).(^.name)) $ g ^. ghosts
                   shadowHw = shadowGd ^. ghostAt
                in 2 * twoTiles - shadowHw
    -- Orange ghost
    Pokey   -> let (V2 dx dy) = pacmanHw - gd ^. ghostAt
                   deltaSquared = (dx * dx) + (dy * dy)
                in if deltaSquared > 64  -- 8 tiles away means target pacman
                     then pacmanHw
                     else targetTileScatter g gd -- otherwise to scatter tile
  where
      pacmanHw = g ^. (pacman . pacAt)
      pacmanDeltaHw = deltaForDir $ g ^. (pacman . pacDir)


-- | move ghost, no decision
-- | Essentially move "forward" in the current direction, unless there
-- | is a wall.  If there is a wall then turn to the wall and change
-- | direction.
moveGhostNoDecision :: Game -> GhostData -> GhostData
moveGhostNoDecision g gd
  | notWillBeWall = gd & ghostAt .~ forwardHw
  | otherwise     = if leftWillBeWall
        then gd & ghostAt .~ rightOfHw
                & ghostDir .~ goRight
        else gd & ghostAt .~ leftOfHw
                & ghostDir .~ goLeft
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


possibleDecisionsAt :: Game -> GhostData -> [(Direction, Coord)]
possibleDecisionsAt g gd
  = filter (not . checkForWall g . snd) $ zip dirs hws
  where
    dirs = directionsAt $ gd ^. ghostDir
    hw  = gd ^. ghostAt
    hws = [wrapAroundBoard g (hw + deltaForDir d) | d <- dirs]


selectShortest :: Coord -> [(a, Coord)] -> (a, Coord)
selectShortest (V2 x y) ps = snd $ head $ sortOn fst $ zip ms ps
  where
      ms = map (mag . snd) ps
      mag hw = let (V2 x1 y1) = hw
                   xd = x - x1
                   yd = y - y1
                in xd * xd + yd * yd


-- | Choose a random element from a list
chooseRandomFromList :: [a] -> StdGen -> (a, StdGen)
chooseRandomFromList [] _ = error "Empty list passed so can't choose element!"
chooseRandomFromList ds gen =
    let (i, gen') = randomR (0, length ds -1) gen
        d = fromJust $ lookup i (zip [0..] ds)
    in (d, gen')


-- | filter out the North direction for yellow decision tiles.
filterOutNorth :: [(Direction, a)] -> [(Direction, a)]
filterOutNorth = filter ((/=North).fst)


directionsAt :: Direction -> [Direction]
directionsAt North = [North, East,  West]
directionsAt South = [South, East,  West]
directionsAt East  = [East,  North, South]
directionsAt West  = [West,  North, South]
directionsAt Still = []


reverseDirection :: Direction -> Direction
reverseDirection North = South
reverseDirection South = North
reverseDirection East  = West
reverseDirection West  = East
reverseDirection Still = Still

