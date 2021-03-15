# AoC
Advent of Code 2020: https://adventofcode.com/
  
## Solutions in Haskell

*haskell_quick* contains the solutions I wrote when I did the puzzles on the day they were released.

*haskell_slow* contains revised solutions, where I tried to improve programming style and efficiency.

## Solutions in Python

I used AoC to learn some Python and put my solutions in the *python* directory.

## Interesting Topics

Some observation on puzzles that stimulate some ideas on data structures and algorithms.

### Day 7 - Transitive Closure

The puzzle input gives container-content information about bag types: One kind of bag contains a prescribed number of other kinds of bags. For example *dark orange bags contain 3 bright white bags, 4 muted yellow bags*.

The first part uses only the relation between container and content, ignoring the quantities. 
So *dark orange bag* is related to *bright white bag* and *muted yellow bag*.

A relation can be represented by a set of related pairs.
We could read the input and store the information as a set (or list) of related pairs.

A relation can also be represented as a graph: imagine that there is an edge between two related bag types.
There are many ways to represent graphs as a data structure, each is more or less convenient depending on the properties of the graph and what we want to do with it.

Part one of the puzzle asks us to compute which bags can contain a *shiny gold bag* **recursively**: we must find not just those that contain it directly (for example **muted yellow bags** *contain 2 shiny gold bags, 9 faded blue bags*) but also indirectly (for example **light red bags** *contain 1 bright white bag, 2 muted yellow bags*, because in turn *muted yellow bags* contain *2 shiny gold bags* and so on).

if we represents the containement relation as **C** and write, for example, **my C sg** to say that a *muted yellow bag* contains some *shiny gold bags*, what we want to do is to find the **transitive** closure of **C**.
This is a relation, usually written **C\***, generated by two rules:
- If **x C y**, then also **x C\* y**
- If **x C\* y** and  **y C\* z**, then **x C\* z**.

Fortunately for us, the relation **C** that we get from the input has no *cycles*:
no sequences of related elements that loop back, for example **x C y**, **y C z**, **z C w**, **w C x**.
Instead, every sequence of related elements will eventually *bottom out* by reaching an element that is not related to anything.
This fact allows us to compute the transitive closure by using a *lazy dynamic programming* approach.

One possible representation of a relation is as a function that maps every element to the set of elements it is related to.
For example **C** could be represented by a map **fC** so that, for example, **fC: my -> {sg,fb}** expressing *muted yellow bags contain 2 shiny gold bags, 9 faded blue bags*.
Then the two rules of transitive closure are summarized by:
**fC\* x = union of (fC x) and (fC\* y) for all ys in  (fC x)**.
This is a circular definition (using **fC\*** in its own definition).
But there are no cycles, which means that an evaluation of this definition will always terminate.

Using finite maps in Haskell, we can direcly define the transitive closure function as above and the relation will automatically be computed lazily.
