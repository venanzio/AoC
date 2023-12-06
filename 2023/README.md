# Advent of Code 2023

## Day 1: Trebuchet?!

I use a list of strings with the names of the digits to search digits spelled in letters:

**["zero","one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]**

I started with "zero", even if the specification only requires digits 1 to 9: since the indexing of a list starts at 0, it makes it easy to get the right value.

For every line: create a list of matching digits and select the first and last to compute the calibration.

## Day 2: Cube Conundrum

Parsing of the input was slghtly challenging.
Once we parsed it into a data structure (list of games, where each game is a list of triples of number of coloured cubes), the solutions are easy.

I use my own simple parser library: FunParser, based on Ch.13 of Graham Hutton's book.

For each extraction, some colours may not be present and they are not in the same order every time.
We start with a triple of colours with three 0s, and change the corresponding number for every extracted colour.

For **Part 1**, I initinally checked that every extraction was possible.
But since we have to calculate the minimum number of each colour cubes in **Part 2**, I changed also Part 1 to only check the triple of minimums.

## Day 3: Gear Ratios

The parsing was again the only challenging part.
It would be easier if the parsing library automatically kept track of position in the source input (line and character). This should be an improvement to make to FunParser.hs.

Missig that, I did the parsing "by hand", checking each coordinate in the input,
creating lists of the parts numbers with coordinates, and lists for all symbols and for just the gear symbols.
This is not very efficient but its sufficient for the puzzle.

With these lists, solutions are easy.

## Day 4: Scratchcards

**Part 1** is straightforward.

**Part 2**: Actually duplicating the cards would result in a combiatorial explosion.
Instead, construct a list of numbers of copies of each card.

## Day 5: If You Give A Seed A Fertilizer

**Part 1**: Apply every map sequentially to each seed number and then take the minimum.

**Part 2**: It's impossible to apply the maps element-wise to each seed number: the ranges are too big.
Instead we must work with ranges themselves, expressed as pairs **(x,y)** denoting **[x..y]**.
I defined a few auxiliary functions to work with lists of ranges (these can still be improved, for example ensuring that the ranges are ordered and without duplications): intersection, union, difference, minimum.
They are in the library *AoCTools.hs*.
With these I could define how the maps operate on ranges.
Then the solution is very similar to Part 1.

## Day 6: Wait For It

For time **t** and record distance **d**: If you hold the botton for **x** milliseconds, you gain speed **x** and the boat travels for **t - x** milliseconds, so the travelled distance is **x(t-x)**. This should be larger than **d**.

So it all comes down to solving the inequality **x^2 - x t + d < 0**.
The quadratic solution formula tells us that this happen between the two roots **x1 = (t + sqrt(t^2 - 4 d)** and **x2 = (t + sqrt(t^2 - 4 d)**.

The first time that beats the record is **ceiling x1** (plus one if **x1** is integer).
The las is **floor x2** (minus one if **x2** is integer).
