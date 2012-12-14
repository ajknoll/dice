# dice

Simple CLI dice roller for tabletop gaming.
You can roll multiple pools of dice at once, with arbitrarily long strings of modifiers (+x, -x, *x, /x) attached to each pool.

## Versions

I've been using this project to play around with languages. As such, there are now
versions of approximately the same application in Python, Haskell and Scala.

For ease of use, `python dice.py` is the simplest version to run at a reasonable speed.
To get a faster version with equally pretty printing, the scala version can be compiled
with `scalac Dice.scala` (from inside the `Dice` directory) and run with `scala Dice`.

The Haskell version can be compiled with `ghc dice.hs` and run as `./dice`, but the
output is less complete.

## Examples:

    python dice.py 1d6    # or scala Dice ...
               ... 1d20+14 1d8-1
               ... 1d3+2+4-3+1
               ... 1d12*2
               ... 4r4d10

Seriously, give that last one a try ;)
