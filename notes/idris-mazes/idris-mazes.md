\newpage

# Generating mazes

- I've been trying to learn Idris, a programming language which is 
like Haskell but more so

- I (wrongly) decided I understood it well enough to try writing a program to 
generate mazes

- The simple things were hard, and the hard things were simple...

![](images/dark-labyrinth.jpg)\

\newpage

# The mission, should you choose to accept it

The idea is that the maze will look like this:


 
![](images/my-maze.png)\


I also decided to output this in text mode using quarter-square graphics.

Once the basic algorithm is done, it can output bigger and badder mazes.

\newpage

# How do you generate a maze, anyway?

Abstracting away all the irrelevant details, the underlying skeleton of the
maze is something like this: 

![](images/skeleton-maze.jpg){#id .class width=200 height=200px}\
 
This is a rectangular grid of cells where we've connected just enough
pairs of adjacent cells for the whole graph to be one piece, 

or, equivalently:

as many pairs of adjacent cells as we can without forming a circuit.

\newpage

# CompSci description of the problem

Given a rectangular grid of cells, make it into a graph by connecting
every pair of adjacent cells with an edge.

We have to find a *spanning forest* of this graph.

This is pretty simple: you just accumulate a list of edges,
only adding ones that don't create a circuit.    

\newpage

# But, here's what happens if you do that 

![](images/boring-maze.jpg){#id .class width=200 height=200px}\

which is not an acceptable solution because it's a boring maze.

We have to introduce randomness, i.e. present the edges in a random order.

It turned out this was the hardest part of the project.

\newpage

# So I'm going to skate lightly over the actual Spanning Forest algorithm

but here it is, anyway: 

![](images/spanning-forest.png){#id .class width=400 height=300px}\

This was hard too, but in a good way. 

It's basically just a fold - accumulating a list of edges - but
you have to efficiently keep track of which cells are connected.
You also have to use the right data structures in Idris, and figure out how to test-drive it...
all of which took a while.

\newpage

# But then, my mazes kept looking like this:

![](images/mangled-maze.jpg){#id .class width=400 height=300px}\

After staring at this for hours, I realized that 

(1) the bottom and right edges were
mangled because of a fencepost error, which was easily fixed

(2) the randomizer wasn't working properly.

But all it had to do was shuffle the list of edges, i.e. 
generate a uniformly random permutation.

\newpage

# Why is randomness so hard?

![](images/roulette-wheel.png){#id .class width=400 height=300px}\

It isn't functional! A method that returns random numbers isn't permissible in
Idris because it can return a different value on every call, violating the semantics.
Same goes for a function that returns the date and time.

So you have to generate random numbers in the context of a special monad, and use the 
Effects library to get it to interoperate with all the other monads you have to use for
anything that is not strictly functional.

Yes, I know this is why not everyone would want to use languages like Idris. 

Also, it turned out there was a BUG in the run time library which was interacting
adversely with the perhaps overcomplicated "Godel numbering scheme for permutations"
I had decided to use, which was fun to implement but not, as it turned out, practical.

\newpage

# Why is randomness so hard? (continued)

Instead, the solution was to look up "generating uniformly random permutations" on
Wikipedia which tells you to use the Knuth shuffle.

But even that is hard! The algorithm involves swapping successive pairs of elements in an 
array to generate the permutation by composing transpositions. And that's non-functional - arrays
are not mutable in Idris.  

I managed to write a recursive algorithm to do it by disassembling and recombining the array,
but it was very slow for mazes of any size.

I searched the web to find out how Haskellers get round this problem, and it turns out they do it by
using a special hack to mutate the array, because Haskell is a more mature (and possibly more pragmatic) 
language than Idris.

I conjecture that generating random permutations in an efficient yet functionally pure way 
should be achievable, but for now, this seems to be the stuff of CompSci PhD theses. 

Meanwhile, here is my not-too-chronically-slow compromise solution: 

![](images/shuffle-algorithm.png){#id .class width=400 height=300px}\ 
  
\newpage

# ... which finally makes it possible ...

... to generate mazes in Idris

(switch to command line for demo)

# THANK YOU   

