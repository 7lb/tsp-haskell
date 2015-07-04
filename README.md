#tsp-haskell
An Haskell implementation of a genetic algorithm to solve the travelling salesman problem

tsp.hs is the standard version, while nn.hs applies the nearest neighbor algorithm to generate part of the first population

For the purpose of this code, these considerations apply:
- a City is a gene
- a Tour is an individual

The code does not (yet?) support command line parameters.
The following configuration options are editable:

**mutRate** should be a low value (1%-3%). Represents how likely a single gene is to be mutated.
Every individual is tried for mutation a number of times equal to its number of genes.

**elitism** a good value is the ceiling of popSize / 10. Represents how many of the best individuals
from the current population should be saved to the next.

**popSize** can be any value. Beware that after a certain point too big a population doesn't
make the algorithm find better solutions any faster, and will instead be detrimental.
Generally speaking a decent value is 50-70.

**numCities** the number of cities to generate. Every individual is made up of this number of
genes. Logically a higher number of cities means the algorithm needs more time to find good
solutions, also the more cities the slower it'll be, since every generation is made up of
*popSize* individuals, each of which is made up of *numCities* genes.

**Edge cases**: A *popSize* of 0 is an edge case, 1 and 2 may be as well. Those are untested,
and the exact same consideration is also true for *numCities*.
*popSize* > *elitism* must be true, or the program will (probably) crash. Again, I didn't test this.

For the nearest neighbor version (nn.hs) *popSize* > *numCities* + *elitims* must be true, or the
program will (probably) crash.

To compile the project, you will need these libraries:
- [parallel](http://hackage.haskell.org/package/parallel)
- [GLUT](http://hackage.haskell.org/package/GLUT)
- [random-shuffle](http://hackage.haskell.org/package/random-shuffle)

Compile like 'ghc --make -threaded -with-rtsopts="-N" -O3 tsp.hs'

The output is a simple graphical representation of the cities and the best tour, along with
generation number and current distance.