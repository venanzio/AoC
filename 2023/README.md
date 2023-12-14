# Advent of Code 2023

## Day 1: Trebuchet?!

I use a list of strings with the names of the digits to search digits spelled in letters:

**["zero","one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]**

I started with "zero", even if the specification only requires digits 1 to 9: since the indexing of a list starts at 0, it makes it easy to get the right value.

For every line: create a list of matching digits and select the first and last to compute the calibration.

## Day 2: Cube Conundrum

Parsing of the input was slightly challenging.
Once we parsed it into a data structure (list of games, where each game is a list of triples of number of coloured cubes), the solutions are easy.

I use my own simple parser library: FunParser, based on Ch.13 of Graham Hutton's book.

For each extraction, some colours may not be present and they are not in the same order every time.
We start with a triple of colours with three 0s, and change the corresponding number for every extracted colour.

For **Part 1**, I initially checked that every extraction was possible.
But since we have to calculate the minimum number of each colour cubes in **Part 2**, I changed also Part 1 to only check the triple of minimums.

## Day 3: Gear Ratios

The parsing was again the only challenging part.
It would be easier if the parsing library automatically kept track of position in the source input (line and character). This should be an improvement to make to FunParser.hs.

Missing that, I did the parsing "by hand", checking each coordinate in the input,
creating lists of the parts numbers with coordinates, and lists for all symbols and for just the gear symbols.
This is not very efficient but its sufficient for the puzzle.

With these lists, solutions are easy.

## Day 4: Scratchcards

**Part 1** is straightforward.

**Part 2**: Actually duplicating the cards would result in a combinatorial explosion.
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

For time **t** and record distance **d**: If you hold the button for **x** milliseconds, you gain speed **x** and the boat travels for **t - x** milliseconds, so the travelled distance is **x(t-x)**. This should be larger than **d**.

So it all comes down to solving the inequality **x^2 - x t + d < 0**.
The quadratic solution formula tells us that this happens between the two roots **x1 = (t - sqrt(t^2 - 4 d))/2** and **x2 = (t + sqrt(t^2 - 4 d))/2**.

The first time that beats the record is **ceiling x1** (plus one if **x1** is already integer).
The last is **floor x2** (minus one if **x2** is integer).

This solves both parts instantly.

## Day 7: Camel Cards

I computed **hand types** by just counting occurrences of each card label and putting them in a decreasing list.
For example **TTT98** has three occurrences of **T**, one of **9**, one of **8**, and zero for all other card labels: its hand type is then **[3,1,1,0,0,0,0,0,0,0,0,0,0]**.
These types are automatically ordered lexicographically.

In **Part 2**, the hand type is modified by adding the number of occurrences of **J** to the first value of the hand type of the rest of the hand. 
(No need to try all possible substitutions for **J**: the highest type is always obtained by substituting it with the card that already has the most occurrences).

## Day 8: Haunted Wasteland

**Part 1** consists in just following the nodes until we reach **ZZZ*, counting the steps.

**Part 2** is in principle much more difficult.
For any starting **A** node, we may have to go through many **Z** nodes,
the number of steps from **A** to the first **Z** and between two different **Z** nodes along the way may all be different.  And even if you reach the same **Z** node twice, that doesn't necessarily mean that you found a cycle: You could be at different points in the instruction list and next time you may end somewhere else. This would be challenging to solve ...

However, the input file I got has a much simpler structure:
From a given **A** node we only repeatedly get to the same **Z**:

**AAA ---> ZZZ ---> ZZZ ---> ZZZ ...**

**PDA ---> XBZ ---> XBZ ---> XBZ ...**

etc.

Moreover the number of steps from the **A** node to the **Z** node and between two consecutive **Z** nodes are equal and a multiple of the number of instructions, so we always go from the **Z** node on instruction number 1, repeating the same cycle.

This means that from an **A** node, the number of steps to reach any occurrence of the **Z** node is a multiple of that number of steps.

Given this much simplified input, the solution is simply the least common multiple of the numbers for each **A** node.

My Haskell program also prints information on the **Z** nodes reached and the number of steps, to verify the structure of the input.

*I have an idea of how I would solve the general problem, but I will rest content of the simple solution for now!*

## Day 9: Mirage Maintenance

**Part 1**: Compute the next value recursively: if the sequence is all zero, the next value is zero, otherwise add the last element of the sequence to the next value of the sequence of differences.

**Part 2**: Compute the previous value similarly, but in the recursive case, instead of summing it to the last element, subtract it from the first element.

Alternative, you can just apply the solutuon of Part 1 to the reverse of the list (trick by <a href="https://mathstodon.xyz/@jas_hughes@fosstodon.org/111548919078164877">Jasmine Hughes</a>).

## Day 10: Pipe Maze

I constructed a map of the pipes in the field.
Each tile containing a pipe is mapped to the two tiles it is connected to.
For the tile with **S**, just check which surrounding tiles connect to it.

**Part 1**: Construct the loop by following the pipes around starting from **S** until you get back to **S**. The answer to Part 1 is half of the length of the loop.

**Part 2**: The points inside the loop are those that have a non-zero winding number. This is the number of times that the loop circles around the point. We can determine it by just looking in one direction, for example north. Every time the loop crosses straight north of the point, add 1 to the winding number if it goes from west to east, subtract 1 if it goes from east to west.

## Day 11: Cosmic Expansion

First find the coordinates of all the galaxies in the original image.
Then expand their coordinates: Find lists of the rows and columns to expand; add to the x-coordinate the number of columns to expand that are lower than that coordinate, multiplied by the expansion rate (minus one because the single column was already counted in the original coordinate); similarly with the y-coordinate, using the list of rows to expand. 

This works for Parts 1 and 2 with a different expansion rate.

*Don't try to expand the whole image, that would be too big for Part 2!*

## Day 12: Hot Springs

**Part 1** can be solved by just pattern matching the group numbers with the input rows.

**Part 2** is too large to use the simplistic strategy of Part 1.
Since we are going to recompute the same patterns several times, I used dynamic programming to compute them the first time we encounter and then reuse the result.

## Day 13: Point of Incidence

**Part 1**: Checking a vertical reflection around **Row i** is done by simply matching the rows before i with the reverse of those after. For horizontal reflection, just apply the same operation to the transpose.

**Part 2**: Like Part 1, but instead of just checking equality of matching rows (or columns), count the differences: The right line of reflection is the one with just one difference.

## Day 14: Parabolic Reflector Dish

**Part 1**: Simply slide the round rocks north one by one in the right order.

**Part 2**: Define the sliding the the other three direction and one cycle of sliding in all for directions. Then we're suppose to repeat the cycle a billion times, which is computationally impossible.
But the repetition will get into a loop quite early: from the length of the orbit leading to the loop and the size of the loop itself, you can compute which configuration the rocks will be in after a billion cycles.
