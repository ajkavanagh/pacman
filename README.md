# pac-man

**This is the Brick/Vty version which has been abandoned**

This app started as a Brick/Vty application, but I switched it to ncurses as
the performance was terrible.  This is due to the way that Brick has to
*always* see if it needs to update the display after every event.  Pac-man
uses `Tick` events to time various movements, *even* if there is no
display update.  However, because of how Brick works, it would still check
to see if the display had changed.  This used all of the CPU just checking
for whether anything had moved, which in most cases, nothing had.

Thus the switch to ncurses is due to that change.

Useful links:

* Video of ASCII pacman: https://www.youtube.com/watch?v=wZorbnsFeHQ
* Game Internals: how pac-man ghost behaviour works: http://gameinternals.com/post/2072558330/understanding-pac-man-ghost-behavior
