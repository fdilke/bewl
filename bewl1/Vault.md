# some thoughts on The Vault

This is a scheme for moving towards an architecture that would allow
distributed Bewl using an Akka cluster.
The idea is that all the caches live in a 'heavyweight' object
called the *vault* which lives on a fixed machine.
Topos implementations are then portable, lightweight things
which use the vault as their object store.
Probably the Vault is wrapped in a Source (in the language of Akka streams)
or is used to 'materialize' the topos implementations.
Note: what would a 'pipeline' look like here?
Should there be a compute engine as an Akka actor, using Sums?

# The caches

Current memoization mechanism does the following for operations
    * = x, +, ^ on dots:

for each dot X, there's a cache that stores X * Y for other dots Y.

I'm not bothering to define proper hashcodes for dots, so this is probably
monstrously inefficient. But it does meet the requirement of well-defined
products, so there is only one X x Y, and similarly for the other operations.

# Towards a better cache

Plan to make Vault a common object which everybody uses, and later, an actor
or maybe system of actors. It can do all the work handled by Memoize at the
moment. This has to be a set of singleton objects because it's calculate-once
information, and access to it has to be synchronized.

Consider this scheme:

We are more or less committed to saving all dots permanently in memory anyway,
so let's have an official roster of all of them.
There could even be 'topos statistics'.

So each dot has an index number. Now we can have an efficient cache which
maps Int to appropriate products, exponentials, etc.

# To do

- Check we only use Memoize for functions on dots.
(some other, inessential uses? optimize away?)
Is there an argument for doing all this for arrow composition?
Should we also cache function values in FiniteSetArrow?
Potential massive savings...?

- Test-drive the Vault, after making sure what the usage patterns will be.

- Experiment with Akka - streams, addressed sums a la MTSE

