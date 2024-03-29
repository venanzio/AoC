# AoC 2020

Advent of Code 2020: https://adventofcode.com/

I originally solved the puzzles of Advent of Code for the year 2020 in Haskell.
My solutions were quick, just enough to get the correct answer, and often not very elemgant or optimized.

Afterwards, I went through them with more time and tried to write clearer and nicer code.
I also took the opportunity to use AoC to learn Python.

The folder *haskell_quick* contains the solutions I wrote when I did the puzzles on the day they were released.

*haskell_slow* contains revised solutions, where I tried to improve programming style and efficiency.

Finally, the folder *python* contains my Python solutions.

Here are comments on some of the puzzles that are interesting from a theorical point of view: the solutions are nice illustrations of some classic algorithmics.

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
**fC\* x = union of (fC x) and all (fC\* y)s for all y in  (fC x)**.
This is a circular definition (using **fC\*** in its own definition).
But there are no cycles, which means that an evaluation of this definition will always terminate.

Using finite maps in Haskell, we can direcly define the transitive closure function as above and the relation will automatically be computed lazily.

### Day 10 - Dynamic Programming

Part 2 of the puzzle asks us to determine the total number of possible arrangements of the adapters.
Each adapter can connect to up to 3 other adapters, causing an exponential increase of the number of arrangements as we make them longer. It is impossible to compute each arrangement individually.

Instead we use a *dynamic programming* approach: we compute an array mapping each adapter to the number of arrangements starting from that adapter and going up to the device. (The array will contain also a 0 entry for the outlet and one for the device with value equal to the highest adapter plus 3.)
The value we want is the array entry for 0 (the outlet).

We need to compute each entry in the array only once, so it can be reused every time the corresponding adapter is part of an arrangement.
In dynamic programming we compute each entry the first time it is needed and reuse it subsequently.
Lazy evaluation means that we can just define the computation of each entry in terms of the others and they will compute only once when they are first called.

A further optimization is to sort the initial list of adapter rating, so for each of them we only need to check at most the 3 following ones to find those who can connect.

## Day 11 - Functions as Objects

All the puzzle of Advent of Code are divided into two parts that use the same input.
You have to solve Part 1 before you can see Part 2.
Often Part 2 is a variation of Part 1 that changes some of the problem in a more complicated way.
It is common to solve Part 2 by copying Part 1 and modifying it accordingly.

However, it is more compact and elegant if we have a single generalized program that solves both parts (and potentially other variations of the same puzzle).
This program would have some extra parameters for the parts of the puzzle that changes between Part 1 and Part 2.

Specifically for the puzzles of Day 11 the general program repeatedly changes the status of each location: seats change from free to occupied or vice-versa according to a certain rule, until no change happens any more.
The rule itself looks, for every location, into the eight directions: up, down, left, right, and the four diagonal directions.
The general solution of the puzzle is unique, computed by the function *final* in Haskell, *finalOccupied* in Python.
These functions take parameters that specify the rule and are called with different arguments for Part 1 and Part 2.
The parameters can be simple values, for example the minimum number of neighbours that determine that an occupied seat become empty, 4 for Part 1, 5 for Part 2.
But they can also be more complicated and have a function type.

In Part 1 we just look at the immediate neighbour in that direction, in Part 2 we look along that direction until we see a seat, either free or occupied.
In both cases, this can be defined as a function that takes a starting location and a direction and return the state of the end location (either a free or occupied seat, or a *NoSeat* value if it is outside the area). So it has the type *Area -> (Int,Int) -> (Int,Int) -> Seat*.
We define two functions of this type: *view1* for Part 1 and *view2* for Part 2.

Another important remark about this puzzle is the use of type synonyms.
I'm very verbose about the typing of my Haskell functions: I always explicitly write their type.
However, sometimes the type becomes complex and difficult to read and understand.
Often such a complex type has an intuitive meaning and it is convenient to define a type synonyms for it.
So I define the type synonym *View* for *Area -> (Int,Int) -> (Int,Int) -> Seat*.
This makes the type of functions easier to read, especially when the function of type *View* is a parameter of another function.
Similarly, I defined the type synonym *Rule* for *Seat -> Int -> Seat*, for the rule that changes the free/occupied status of a seat according to the number of neighbours.
Although this type is simple, having a different name for it clarifies the meaning of those functions and parameters that have this type.
We could think, as another example, to introduce a type synonym *Location* for *(Int,Int)* for the same purpose.
