# How pacman works

Pacman is a multi-threaded ncurses application, which tries quite hard to be like the original.

# Threads and timers

So we have to have at least two threads:

1. A UI thread which draws the UI and gets keyboard events.  For simplicity this will also run the main game code.
2. A timer thread that ticks at 1/60 of second.

The game moves at increments of 1/60th of a second.  The display only updates when something moves, something being pacman, ghost or the score or some other element on the screen.

The pacman and ghosts move at "number of 1/60s ticks", where the relative ratio determines the speeds of different elements on the screen.
