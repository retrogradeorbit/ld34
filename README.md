# Let It Grow!

Who said money doesn't grow on trees?

Grow your way to a million dollars!

My entry for Ludum Dare 43. Themes were 'Growing' and 'Two button controls'

## Overview

HTML5 game in clojurescript. Everything from scratch in 72 hours.

Play it now!

[ld34.procrustes.net](http://ld34.procrustes.net)

## Instructions

Mouse and two mouse buttons.

Left button interact with game. Right button, hold down and drag to scroll.

To move the player, click on a damp spot on the ground as a destination.

To plant a seed, click on the player and then click the top 'plant seed' icon. Wait for the seed planting to complete.

To harvest a bush, move the player right on top of the bush. Then click 'harvest' icon. The bush must be fully mature to harvest.

Once you have some money you can use bug spray. Click the player and then the 'spray' icon when you are near bugs.

Bugs stunt plant growth.

Hippies smoke the plant and hurt your final yield.

## Powerups

Click the caravan. Top power up is 'level up player'. Makes them chop and plant faster. (cost 1000, then 2000, 4000, 8000 etc)

The bottom right icon, arrow up, is 'level up fertiliser'. Doubles the growth rate of future plants. (cost 5000, then 10000, 20000, 30000 etc)

The bottom left icon, a little seed, is 'level up seeding'. Doing this increases the possible maxium seeds that a plant will give you when harvested by one. (cost 10000, then 30000, 90000)

## To win

Earn a million dollars!


## Bugs

Sometimes you can spend more than you have and your money goes negative.
Cant move player to light green grass.
No win condition detected.
Needs tool tips and on boarding.
Hippies and Flies need AI tuning to scale game difficulty.
Various other bugs, too.

## Development Setup

To get an interactive development environment run:

    lein figwheel

and open your browser at [localhost:3449](http://localhost:3449/).
This will auto compile and send all changes to the browser without the
need to reload. After the compilation process is complete, you will
get a Browser Connected REPL. An easy way to try it is:

    (js/alert "Am I connected?")

and you should see an alert in the browser window.

To clean all compiled files:

    lein clean

To create a production build run:

    lein cljsbuild once min

And open your browser in `resources/public/index.html`. You will not
get live reloading, nor a REPL.

## License

Copyright © 2015 Crispin Wellington

Distributed under the Eclipse Public License either version 1.0 or (at your option) any later version.
