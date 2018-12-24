# pac-man

I wrote this as an exercise in improving my Haskell, which I started this year
(2018) and for learning how to use Lenses for a presentation I'm going to do
for my local Functional Programming group (FP-NorthEast in Newcastle upon Tyne,
UK).  I'm not suggesting that it's good Haskell - think of it as beginner's
Haskell; I've been a programmer for more than 30 years, just not in Haskell!

## Screen Shot

![Pac-man Screenshot](docs/pac-man-screen-shot.png)

## Keys

The following may prove useful if you decide to try the game:

* 'q' quit!
* 'hjkl' movement if you are a Vim user
* Arrow keys for everybody else
* 'p' pause the game to get a screenshot

Best played on a terminal with no theme set, so that the original colours can
be seen.  It *needs* a colour terminal, otherwise it'll probably crash on
start-up.

## Building and running it

I've only tested this on Linux, specifically Ubuntu 18.04LTS ('bionic').  YMMV
with other Linux distributions, although it really should work.  It should also
work on Mac OS X.  I've no idea if it will work on Windows; reports welcome!

To build it:

* [Download and install Slack](https://docs.haskellstack.org/en/stable/README/#how-to-install)
* Run `stack run` in the root of the distribution.

## TODOs

* Use an IO Random seeded random number generator.  I've just not got around to
  it yet!  At the moment, every game is the same, for ghosts when they flee.
* Implement Fruit bonuses.
* Implement (probably a `StateT` monad) to 'pass' the display structure around
  the `UI.hs` module.

## Minor Implementation Details

This is a relatively full implementation of the game.  The only thing missing
are fruit bonuses; patches welcome!

This is the ncurses version of the Pac-man game.  Originally I started to build
the game using the Brick/Vty library, but there were major performance issues
with that approach, which aren't really the fault of Brick/Vty *per se*.

The game relies on a `Tick` event which happens every 1/60th second to provide
the relative speeds for the Pac-man and ghosts.  The Brick application model
merges custom events (like a timer tick) and keyboard events into a single
handler.  When that handler returns to Brick, Brick then evaluates changes to
the virtual display to work out whether to update the display.  And this
happens regardless of whether there *have* been any changes.  In my pac-man
implementation, *most* ticks result in no update to the display.  This means
Brick spent a lot of time doing nothig, in terms of seeing if the display had
been updated.

On investigating Brick's internals, it's apparent that this is very much *baked
in* to the Brick application architecture, and would be difficult (for me, at
my current Haskell experience level) to work around without replicating much of
what Brick is doing.  Therefore, I decided to switch to using ncurses more
directly where I can have much more control over when the display is updated;
the consequence is that the main Pac-man game engine code has to do more work
in determining when to do updates, rather than just leaving it up to Brick.

This does upset the *purity* of the `Pacman.hs` game code, which originally was
intended to be just a set of Lenses function updates.  It now includes
a `WriterT` based monad to capture which bits of the display need to be
updated.

This is one of the costs of optimising for efficiency over ease-of-programming.
However, this is a game, and so it is worth the cost.

For other terminal applications, I would **definitely** use Brick/Vty as it is
a much better option with scrolling windows, handling events, etc.  A really
nice library, just not the sweet spot for this game.


## Useful links:

* Game Internals: how pac-man ghost behaviour works: http://gameinternals.com/post/2072558330/understanding-pac-man-ghost-behavior
* John Millikin's Ncurses library: https://john-millikin.com/software/haskell-ncurses
* Stack: https://docs.haskellstack.org/en/stable/README/
