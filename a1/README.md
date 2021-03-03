Suguru is a game in which the player has to fill
cells carefully placed inside a grid of changeble size.

In the grid, there will be regions with N cells to
be completed and, completing those, the player must
use only numbers from 1 to N. (e. g. if the region has
5 cells, the player must use 1, 2, 3, 4 and 5 to complete
that said region).

The only constraints given are that _the player cannot
repeat the same exact number in adjacent cells, horizontally,
vertically or diagonally._

ex:

2|2 4

1|3 5 -> **Bad game**, as we have two 2's at the first row and two 5's next to one another at the bottom-right.

4 5|1     

> So, how regions are made (or chosen)?

Ramdonly. In order to solve that problem, as we are dealing with
code, we must pass each region's coordinate inside the file to
be parsed. 

ex: (0, 1), (0, 2), (0, 3), (1, 0), (1, 1), (1, 2), (1, 3)     
    
    x x x
    -----
    y y y
