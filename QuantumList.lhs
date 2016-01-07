> {-# LANGUAGE FlexibleInstances #-}
> {-# LANGUAGE ViewPatterns #-}
> {-# LANGUAGE Arrows #-}
> module Quantum where

We need to load the arrow interface

> import Control.Arrow
> import Control.Category
> import Prelude hiding (id, (.))
> import Control.Concurrent
> import Control.Concurrent.STM
> import qualified Data.List as L
> import Data.Monoid
> import Data.Maybe
> import Data.Random.Shuffle.Weighted
> import Debug.Trace

Quantum runs an environment, where variables can be entangled to each other.
Entanglement is a natural phenomena, which arises when a quantum systems (of at least 2 particles) wave
function is in a superposition and when a quantity must be conserved. (Conservation is called parity).

This means that certain properties are invariant under measurement. An example
of such a property is spin, which mirrors classical angular momentum.



Electrons always have a spin of +/- 1/2. Thus an electron can be described as:

e = a|-> + b |+>

where |a|^2 and |b|^2 are the probabilities of the specific state. And |-> |+> are ket's,
these are a syntactic sugar for vectors (in this case).

Say both states are equally probable, we get:

e = 1/sqrt(2) |-> + 1/sqrt(2) |+>

When we have a two particles system, it can be in the following state:

s = a|++> + b|--> + c|+-> + d|-+>

c|-+> + b|+-> are untangled states. Electrons are fermions, which means they
are subjected to the pauli exclusion principle. This states, that electrons
cannot occupy the same state, if they are in the same system. The underlying
reason is that interchanging particles in a system of electrons, yield:

P f(e1, e2) =  - f (e2, e1)

Where |f|^2 describes the probability to find the system in a certain state.

If we say now, that e1 = e2, we get:

P f(e1, e1) = - f(e1,e1), which means f = 0.

Thus the change to find a system of electrons, where some electrons are in the same state
is 0.


E.g. Entangled states are found  in helium atoms.

s_entangled = c|+-> + d|-+>

Now comes the interesting part. If we pull the electrons from the helium atom (we have to be very careful doing this)
and we send them each in a separate direction. The invariants of the systems state (spin is 0) will be conserved.


And as long as there is no measurement, the systems invariants (in this case spin) still stands. When there is
a measurement, the system collapse (under copenhagen interpretation) and one of the possibilities become real.

Amazingly entanglement works over any distance and the correlation. And the objects, which can be
entangled are getting bigger and bigger (buckyballs and small diamonds are examples).

Even more tantalizing, states can be entangled through time.

> import Data.Ratio
> import Data.Complex
> import qualified Data.Vector as V
> import Numeric.LinearAlgebra.Data
> import Data.Random


Represents a probability

> type Amplitude = Double

Our quantum store

> newtype Universe a = Universe {
>                unUniverse :: (V.Vector a, V.Vector Amplitude)
>          }

We can change the contents of a universe piece wise

> instance Functor Universe where
>        fmap t (Universe (f, g)) = Universe (fmap t f, g)

And we need to create an universe:

> createUniverse :: [(a, Amplitude)] -> Universe a
> createUniverse xs = Universe $ V.unzip $ V.fromList xs

The quantum type pulls one universe to another universe


> newtype Quantum a b =  Quantum {
>                 runQuantum :: Universe a -> Universe b
>             }

A category defines composition. In our case it dictates how a quantum operation is composed with another quantum operation.
id is the do nothing operation
(.) is the composition operation

> instance Category Quantum where
>         id = Quantum id
>         (.) (Quantum f) (Quantum g) = Quantum (f . g)


arr let's us push classical operations into the quantum sphere.
first will let us work on one part of the entangled pair.

> instance Arrow Quantum where
>        arr f = Quantum (fmap f)
>        first (Quantum g) = Quantum $ \p -> remap g p
>                    where remap g (Universe (c, pm) ) =  let (as, ds) = V.unzip c
>                                                             (Universe (c', pm')) = g  $ Universe (as, pm)
>                                                          in Universe (V.zip c' ds, normalize $ V.zipWith (*) pm' pm)

> instance ArrowChoice Quantum where

For a choice arrows, we need to separate the universes and then let it come together again, while keeping order of the elements.
This poses some problems. Luckily we can tag all the objects in an universe with some natural denoting it's place there and
then rip the universe in two pieces, apply the function and sew it back again. Humans won't ever notice.

this is somewhat dirty, but that first
                |
                \/ there is a view pattern, it applies the function before pattern matching.

>       left (first -> Quantum g) = Quantum $ \p -> let (ls, rs) = splitEither p
>                                           in zipEither (g $ ls) rs



We have to split an universe full of choices:


> splitEither :: Universe (Either a b) -> (Universe (a,Int), Universe (b, Int))
> splitEither (Universe (c, pm)) = let xs = V.toList $ V.zip (V.zip c $ V.fromList [0..]) pm
>                                      (ls, rs) = foldr step ([], []) xs
>                                      (uls, lpm) = unzip ls
>                                      (urs, rpm) = unzip rs
>                                  in (Universe (V.fromList uls, V.fromList lpm), Universe (V.fromList urs, V.fromList rpm))
>                      where step t@( (Left a, n), p) (l,r) = (((a,n),p) :l, r)
>                            step t@ ((Right a,n),p) (l,r) = (l, ((a,n),p) :r)


> zipEither :: Universe (a, Int) -> Universe (b, Int) -> Universe (Either a b)
> zipEither (Universe (a, pm)) (Universe (b, pm'))  = let apm = V.zip a pm
>                                                         bpm = V.zip b pm'
>                                                         ls = fmap (first (first Left)) apm
>                                                         rs = fmap (first (first Right)) bpm
>                                                         ss = sortVectorBy (\((a,n),p) ((b,o),q) -> n `compare` o) $ ls <> rs
>                                                         xs = fmap (first fst) ss
>
>                                                     in Universe (V.unzip xs)
> sortVectorBy f = V.fromList . L.sortBy f . V.toList


And split and merge universes of products:

> zipUniverse :: Universe a -> Universe b -> Universe (a,b)
> zipUniverse (Universe (a, apm)) (Universe (b, bpm)) = let ab = V.zip a b 
>                                                           lab = V.length ab
>                                                       in  Universe (ab, V.zipWith (*) (V.take lab apm) (V.take lab bpm))

> unzipUniverse :: Universe (a,b) -> (Universe a, Universe b)
> unzipUniverse (Universe (ab, pm)) = let (a, b) = V.unzip ab
>                                     in (Universe (a, pm), Universe (b, pm))

Now we can create ArrowLoop

> instance ArrowLoop Quantum where 
>    loop (Quantum f) = Quantum $ \a -> let (b, d) = unzipUniverse (f (zipUniverse a d)) in b

Arrow plus is quite interesting, there it enlarges the state space.

But first we need to create the zero universe. The universe with no states:

> instance ArrowZero Quantum where
>           zeroArrow  = Quantum (\_ -> Universe (mempty, mempty))


ArrowZero is even worse than just a zero universe. It destroys an universe.

Now plus, which adds universes together:

> instance ArrowPlus Quantum where
>      (<+>) (Quantum f) (Quantum g) = Quantum $ \u -> let Universe (a, apm) =  f u
>                                                          Universe (b, bpm) =  g u
>                                                      in Universe (a <> b, apm <> bpm )


Now we need a way to run our computation, but for that we need to pick stuff out of a list, which is not
a simple problem in itself. We cummulate all

> pickVector :: V.Vector Double -> RVar Int
> pickVector xs = head <$> weightedSample 1 (V.toList (fmap (\x -> abs x ^ 2) $ normalize xs) `zip` [0..])
>


Copenhagen interpretation (you can have only one)

> observeQuantum :: Quantum a b -> Universe a -> RVar b
> observeQuantum (Quantum f) a = let (Universe (b, pm)) =  f a
>                                in do
>                                   t <- pickVector (fmap (\x -> abs x ^ 2 ) pm)
>                                   return $ b V.! t

Many worlds, please have all

> observeAll :: Quantum a b -> Universe a -> V.Vector (b, Double)
> observeAll (Quantum f) a = let (Universe (b,normalize -> pm)) = f a
>                             in V.zip b (fmap (\x -> abs x ^ 2) pm)
>


Normalize a vector.

> normalize :: V.Vector Double -> V.Vector Double
> normalize xs = let norm = sqrt $ V.sum $ fmap (\x -> abs x ^ 2) xs
>                in (/norm) <$> xs

Please be aware, that possibilities gets cut of if there is no universe

> entangleUniverse :: Universe b -> Quantum a (a,b)
> entangleUniverse (Universe (c,pm)) = Quantum $ \(Universe (d, dpm)) -> let xs = d `V.zip` c
>                                                                            l = V.length xs
>                                                                        in Universe (xs, V.zipWith (*) (V.take l pm) (V.take l dpm))


Lets build a little experiment:

> data Cat = Dead | Alive
>       deriving (Show, Eq)
> data Led = On | Off
>       deriving (Show, Eq)

First we setup two universes:

> universeCat = createUniverse [(Dead, 0.5), (Alive, 0.5)]
> universeSwitch = createUniverse [(On, 0.5), (Off, 0.5)]
> universeBoth :: Quantum Cat (Cat, Led)
> universeBoth = entangleUniverse (universeSwitch)

Or in arrow notation: 

> universeBoth' :: Quantum Cat (Cat, Led)
> universeBoth' = proc x -> do 
>                                  catled <- entangleUniverse universeSwitch -< x
>                                  returnA -< catled
 

Maybe we want to give kitty a disadvantage and repeat the experiment a couple of times:
