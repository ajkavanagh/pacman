{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell  #-}

module Pacman where

import           Control.Applicative ((<|>))
import           Control.Monad       (guard)
import           Data.Maybe          (fromMaybe, fromJust)
import           Data.Vector         (Vector)
import qualified Data.Vector         as V
import           Data.List           (findIndex)
import           Data.Sort           (sortOn)

import           Lens.Micro          ((%~), (&), (.~), (^.), ix)
import           Lens.Micro.TH       (makeLenses)
import           Linear.V2           (V2 (..), _x, _y)
import           System.Random       (Random (..), newStdGen, StdGen, mkStdGen, randomR)

-- types
data Game = Game
  { _pacman          :: PacmanData
  , _ghosts          :: [GhostData]
  , _ghostMode       :: GhostMode
  , _maze            :: Maze
  , _mazeHeight      :: Int
  , _mazeWidth       :: Int
  , _gameover        :: Bool
  , _state           :: GameState
  , _pillsLeft       :: Int
  , _startPills      :: Int
  , _paused          :: Bool
  , _score           :: Int
  , _nextMove        :: Int -- in ticks
  , _gameTick        :: Int -- counts ticks, used for next move and animations
  , _doInvalidateCache :: Bool
  , _randStdGen        :: StdGen
  } deriving (Show)


data PacmanData = PacmanData
  { _pacAt        :: Coord
  , _pacDir       :: Direction
  , _pacNextDir   :: Direction
  , _dying        :: Bool
  , _pacTick      :: Int
  } deriving (Eq, Show)

data GhostPersonality
  = Shadow      -- "Blinky" is RED
  | Bashful     -- "Inky" is CYAN
  | Speedy      -- "Pinky" is PINK
  | Pokey       -- "Clyde" is ORANGE
  deriving (Show, Eq)

data GhostMode
  = GhostHold
  | GhostChase Int
  | GhostScatter Int
  | GhostFlee Int
  deriving (Eq, Show)

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
  | Normal
  | GameOver
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

-- | some constants that are interesting
ghostFleeForTicks = 1000
ghostChaseForTicks = 1000
initialWait = 1000
ghostDeadAt = V2 11 12

-- | speeds for pacman and ghosts
threadDelayBase = 100000
ghostNormalRatio = (105, 100)
ghostSpeedup1Ratio = (95, 100)
ghostSpeedup2Ratio = (90, 100)
ghostFleeingRatio = (125, 100)

_calcThreadDelay :: Int -> (Int, Int) -> Int
_calcThreadDelay r (m,d) = (r * m) `quot` d

-- | calculated thread delays
threadDelayPackman       = threadDelayBase
threadDelayGhostNormal   = _calcThreadDelay threadDelayBase ghostNormalRatio
threadDelayGhostSpeedup1 = _calcThreadDelay threadDelayBase ghostSpeedup1Ratio
threadDelayGhostSpeedup2 = _calcThreadDelay threadDelayBase ghostSpeedup2Ratio
threadDelayGhostFlee     = _calcThreadDelay threadDelayBase ghostFleeingRatio

-- THIS is legacy -- we'll do it via timer delays
ticksPerPacman = 20
ticksPerGhostNormal = 21
ticksPerGhostSpeedup1 = 19
ticksPerGhostSpeedup2 = 18  -- 10% faster than pacman
ticksPerGhostFleeing = 25
ghostSpeedUp1 = (1,3)       -- 1/3rd of the dots.
ghostSpeedUp2 = (2,3)       -- 2/3rd of the dots.


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
                       , _ghostMode = GhostHold
                       , _maze = m
                       , _mazeHeight = h
                       , _mazeWidth = w
                       , _gameover = False
                       , _state = NotStarted
                       , _pillsLeft = numPills
                       , _startPills = numPills
                       , _paused = False
                       , _score = 0
                       , _nextMove = initialWait
                       , _gameTick = 0
                       , _doInvalidateCache = False
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
--                         location   dir   next  dying pacTick
initialPacman = PacmanData (V2 17 13) Still Still False 0

-- | pause and un-pause the game
pause :: Game -> Game
pause = paused %~ not

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


-- | use |> to be flipped (.) so we can compose forwards
(|>) :: (a->b) -> (b->c) -> a -> c
(|>) = flip (.)

-- Just step the game forward one tick
-- TODO: is game over (i.e. eaten all pills)
stepPacman :: Game -> Game
stepPacman
  =  clearInvalidateCache
  |> incGameTick
  |> incPacmanTick
  |> maybeUpdateGhostMode   -- update the ghost mode based on game ticks
  |> maybeDoPacman
  |> eatGhostOrBeEaton


stepGhostsNormal, stepGhostsSpeedup1, stepGhostsSpeedup2, stepGhostsFlee :: Game -> Game
stepGhostsNormal   = stepGhostsRate GhostRateNormal
stepGhostsSpeedup1 = stepGhostsRate GhostRateSpeedup1
stepGhostsSpeedup2 = stepGhostsRate GhostRateSpeedup2
stepGhostsFlee     = stepGhostsRate GhostRateFlee

stepGhostsRate :: GhostRate -> Game -> Game
stepGhostsRate rate 
  =  clearInvalidateCache
  |> moveGhostsInRate rate
  |> eatGhostOrBeEaton

-- TODO: delete this as it's no longer used
step :: Game -> Game
step =
    maybeDoPacman
  . eatGhostOrBeEaton
  . moveGhosts
  . maybeUpdateGhostMode
  . incGameTick
  . clearInvalidateCache


clearInvalidateCache :: Game -> Game
clearInvalidateCache = doInvalidateCache .~ False

setInvalidateCache :: Game -> Game
setInvalidateCache = doInvalidateCache .~ True

-- | move the pacman and check for the pill or powerup
-- | invalidate the render cache if we moved (in movePacman)
maybeDoPacman :: Game -> Game
maybeDoPacman g
  -- | not onTick = g
  | g ^. gameover = setInvalidateCache g
  | otherwise = (movePacman |> eatPillOrPowerUp |> setInvalidateCache) g
  --where
    --onTick = (g ^. gameTick) `rem` ticksPerPacman == 0

-- only run a, b if the game is not over.
--(.$.) :: (Game -> Game) -> (Game -> Game) -> (Game -> Game)
--(.$.) a b g = if g ^. gameover then g else a (b g)

incGameTick :: Game -> Game
incGameTick g = if g ^. paused then g else g & gameTick %~ (+1)

incPacmanTick :: Game -> Game
incPacmanTick g = if g ^. paused then g else g & (pacman . pacTick) %~ (+1)

-- move the pacman until he hits a wall
-- includes the 'nextDir' to queue up the next turn as needed, so turning
-- is easier
movePacman :: Game -> Game
movePacman g
  | pickNextDir = g
    & (pacman . pacDir) .~ nextDir
    & (pacman . pacAt) .~ nextHw
    & (pacman . pacNextDir) .~ Still
  | pickDir = g & (pacman . pacAt) .~ hw'
  | otherwise = g
    & (pacman . pacDir) .~ Still
    & (pacman . pacNextDir) .~ Still
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
eatPillOrPowerUp :: Game -> Game
eatPillOrPowerUp g
  | c == pillChar = g & score %~ (+1)
                      & pillsLeft %~ pred
                      & maze  %~ (`mazeClearAt` hw)
  | c == powerupChar = g & score %~ (+100)
                         & maze %~ (`mazeClearAt` hw)
                         & ghostMode .~ fleePacman g
  | otherwise = g
  where hw = g ^. (pacman . pacAt)
        c = mazeCharAt g hw

fleePacman :: Game -> GhostMode
fleePacman g = GhostFlee $ ghostFleeForTicks + (g ^. gameTick)

-- | simple chase strategy -- just chase for ticks -- no variation
chasePacman :: Game -> GhostMode
chasePacman g = GhostChase $ ghostChaseForTicks + (g ^. gameTick)

-- | scatter from pacman -- just scatter for ticks -- no variation
scatterFromPacman :: Game -> GhostMode
scatterFromPacman g = GhostFlee $ ghostFleeForTicks + (g ^. gameTick)

-- | move the ghosts and check for pacman interface; but only if on tick.
-- | invalidate the render cache if we moved
-- | TODO: delete this as no longer needed
maybeDoGhosts :: Game -> Game
maybeDoGhosts g
  | not onTick = g
  | otherwise = (setInvalidateCache . eatPillOrPowerUp . moveGhosts) g
  where
    onTick = (g ^. gameTick) `rem` ticksPerPacman == 0

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
ghostsAreFleeing g = case g ^. ghostMode of
            (GhostFlee _) -> True
            _ -> False


eatGhost :: GhostData -> Game -> Game
eatGhost gd g = g & score %~ (+1000)
                  & (ghosts . ix i . ghostState) .~ GhostDead
                  & (ghosts . ix i . ghostAt) .~ ghostDeadAt
  where
      i = fromMaybe 0 $ findIndex ((==gd ^. name).(^. name)) $ g ^. ghosts

eatonByGhost :: Game -> Game
eatonByGhost g = g & gameover .~ True
                   & (pacman . dying) .~ True
                   & (pacman . pacTick) .~ 0

-- | check the current Ghost mode and change it if the ticks have expired.
maybeUpdateGhostMode :: Game -> Game
maybeUpdateGhostMode g = ifNextMode $ case g ^. ghostMode of
    GhostHold        -> (initialWait, chasePacman)
    (GhostChase t)   -> (t, scatterFromPacman)
    (GhostScatter t) -> (t, chasePacman)
    (GhostFlee t)    -> (t, chasePacman)
  where
      ifNextMode :: (Int, Game -> GhostMode) -> Game
      ifNextMode (_t, f) = if g ^. gameTick > _t
                             then g & ghostMode .~ f g
                             else g

-- move all the ghosts, one after another, but returning a function which does
-- it
moveGhosts :: Game -> Game
moveGhosts g = foldr moveGhost g $ zip [0..] (g ^. ghosts)

moveGhostsInRate :: GhostRate -> Game -> Game
moveGhostsInRate rate g
  = foldr moveGhost g
  $ filter ((==rate) . (^. ghostRate) . snd)
  $ zip [0..] (g ^. ghosts)

moveGhost :: (Int, GhostData) -> Game -> Game
moveGhost gh@(i, gd) g = case g ^. ghostMode of
      GhostHold         -> g
      (GhostChase _)    -> moveGhostChase gh g
      (GhostScatter  _) -> moveGhostScatter gh g
      (GhostFlee _)     -> moveGhostFlee gh g

moveGhostChase :: (Int, GhostData) -> Game -> Game
moveGhostChase _ g = g

moveGhostScatter :: (Int, GhostData) -> Game -> Game
moveGhostScatter _ g = g

moveGhostFlee :: (Int, GhostData) -> Game -> Game
moveGhostFlee _ g = g


-- TODO: delete this, but first we have to use the speedups, etc.
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

reverseGhosts :: Game -> Game
reverseGhosts g = foldr reverser g $ zip [0..] $ g ^. ghosts
  where reverser (i, gd) g = g & (ghosts . ix i . ghostDir) %~ reverseDirection


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
chooseTargetHw gd g = case g ^. ghostMode of
    GhostHold        -> error "Don't call chooseTargetHw in GhostHold mode!"
    (GhostChase _)   -> targetTileChase gd g
    (GhostScatter _) -> targetTileScatter gd g
    (GhostFlee _)    -> error "Don't call chooseTargetHw in GhostFlee mode!"

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
