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

For Part 1, I initinally checked that every extraction was possible.
But since we have to calculate the minimum number of each colour cubes in Part 2, I changed also Part 1 to only check the triple of minimums.

## Day 3: Gear Ratios

The parsing was again the only challenging part.
It would be easier if the parsing library automatically kept track of position in the source input (line and character). This should be an improvement to make to FunParser.hs.

Missig that, I did the parsing "by hand", checking each coordinate in the input,
creating lists of the parts numbers with coordinates, and lists for all symbols and for just the gear symbols.
This is not very efficient but its sufficient for the puzzle.

With these lists, solutions are easy.

## Day 4: Scratchcards

Part 1 is straightforwards.

Part 2: Actually duplicating the cards would result in combiatorial explosiong.
Instead, construct a list of number of copies of each card.
