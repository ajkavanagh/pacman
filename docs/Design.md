# How pacman works

Pacman is a multi-threaded ncurses application, which tries quite hard to be like the original.

# Threads and timers

So we have to have at least two threads:

1. A UI thread which draws the UI and gets keyboard events.  For simplicity this will also run the main game code.
2. A timer thread that ticks at 1/60 of second.

The game moves at increments of 1/60th of a second.  The display only updates when something moves, something being pacman, ghost or the score or some other element on the screen.

The pacman and ghosts move at "number of 1/60s ticks", where the relative ratio determines the speeds of different elements on the screen.

# TODO -- things we still need to work out

* DONE - Getting the ghost out of the ghost house - animation, states
* Getting the ghost back to the ghost house
* DONE - Losing a life
* Starting a game (press 's' to start)
* Game over
* DONE - Pause the game key
* Animation whilst waiting to start the game.
* Resolving the `display` 'reader' in the UI section

## DONE - Getting a ghost out of the house (animation)

* DONE - New state `GhostLeavingHouse` -- the ghost traces a path from their location to immediately above the ghost house at a pre-determined rate.
* DONE - We ensure the ghosts are in the house in the leaving order (213) where 1 = Pinky, 2 = Inky, 3 = clyde
* DONE - Use the `ghostNorm ratesForLevel g ^. gameLevel` ticks for each move
* DONE - Just track to the centre, and then go up to the exit spot; then move as per the `GhostNormal` ghosts (i.e they are out and about).
* DONE - Ghost's can't leave the house whilst fleeing.

## Getting the ghost back into the house (as a pair of ")

* Remove state `GhostDead` - as ghosts don't 'die', they just return to the ghost house.
* Add state `GhostGoingHome` -- the ghost traces a path (quickly) from their dying location back into the house to their start position.  Blinky (Shadow) needs to slot into the house to a location and then will immediately come back out.
* The speed ought to be fast (say double normal speed, or just a fixed fast rate - easier).
* We can flash the " character to show him returning home.  Ought to be light grey too.
* The decisions will all be automated.  The first will be shortest to get the 'eyes' going, and thereafter, every decision point will be from a lookup table, which we just have to work out beforehand.
* Normal rules for getting the ghost out then apply (dot counts, etc.)

## DONE - Losing a life

* DONE - Animate losing a life (already done)
* DONE - Enable global dot count to 0
* DONE - Drop lives by 1
* DONE - Reset ghosts
* DONE - Update display
* DONE - Reset pacman position
* DONE - Need a timer (using `pacAnimate`) to count to the next life (or when it hits zero to just stay at zero if all lives are over).

## Starting a game (s)

* "Press 's' to start
* DONE - Game state is `NotStarted` -> no ticks (like paused) -- it's also the demo state if we get that going (ever)
* Game state could be `GameOver n` if the game over timer is still running.
* Keys don't make sense apart from `q` for quit.
* Pressing 's' resets the game from the Random Number generator.  This presents a problem as it is in the `Game` variable, and we want to fix that for demos.  Probably need to wrap `Curses` with a `StateT` so that we can keep the random generator outside of the `Game` state.

## Game over

* When we've lost all the lives, the game needs to end, and transfer the score to the high-score.  Another 'overarching' state is required for the game (probably in the UI) to hold the high score and the random number generator.
* The ghosts continue to animate for a period. This implies that `GameOver` has a timer that counts up or down, and when complete reverts to `NotStarted`
* Remove the `_gameover` member of `Game` as that is covered better by `GameState`.
* Pacman is `dying`, but rename it as `pacDead`.  If out of lives, then it never moves on from this state, and thus doesn't get redrawn.
* Need a way of keeping Pacman OFF the display?
* Transition to `NotStarted` needs to be the same as at the initialise for the game (e.g. flash press 's' to start, etc.)

## Animation whilst waiting to start the game

* Game in `NotStarted`  -- should have a timer that resets it.
* Pacman needs to move with a predetermined set of turns.
* Specific seed given to `initGame` to make the ghosts do predetermined things.
* How to get the moves/turns to Pacman?
