-----------------------------------------------------------------------------
-- |
-- Module      :  ForSyDe.Shallow.Communication
-- Copyright   :  ...
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  ...
-- Stability   :  experimental
-- Portability :  portable
--
-- ...
-----------------------------------------------------------------------------
module ForSyDe.Shallow.Patterns.Communication(
                                     replacePN, headPN, lastPN,
	                                 groupPN, attachPN, catPN, concatPN, reversePN, 
                                     shiftlPN, shiftrPN, rotrPN, rotlPN, replicatePN,
                                     zipxPN, unzipxPN,
                                     selectIdxPN, selectIdx1PN, selectIdx1AdpPN,
                                     gatherPN, gather1PN, gatherAdpPN,
                                     -- special cases/compositions of selectIdxPN
                                     tailPN, initPN, takePN, dropPN, splitatPN, stridedSelPN,
                                     oddsPN, evensPN, bitrevPN, 
                                     -- patterns which make sense only for specific MoCs
                                     zipSYPN, unzipSYPN, dualsSYPN, undualsSYPN
                                    ) where

import ForSyDe.Shallow.CoreLib
import ForSyDe.Shallow.MoCLib


-- |  The function 'replacePN' replaces an element in a vector.
-- |  TODO: index>length
replacePN :: Vector (Signal a) -> Int -> Signal a -> Vector (Signal a)
replacePN = replaceV


-- | The functions 'headPN' returns the first element of a vector.
headPN :: Vector (Signal a) -> Signal a
headPN = headV

-- | The function 'lastV' returns the last element of a vector.
lastPN :: Vector (Signal a) -> Signal a
lastPN = lastV

-- | The function 'groupPN' groups a vector into a vector of vectors of size n.
groupPN :: Int -> Vector (Signal a) -> Vector (Vector (Signal a))
groupPN = groupV

-- | The operator attachPN attaches a signal at the end of a vector.
attachPN :: Vector (Signal a) -> Signal a -> Vector (Signal a)
attachPN = (<:)

-- | The operator 'catPN' concatinates two vectors.
catPN :: Vector (Signal a) -> Vector (Signal a) -> Vector (Signal a)
catPN = (<+>)

-- | The function 'concatV' transforms a vector of vectors to a single vector. 
concatPN   :: Vector (Vector (Signal a)) -> Vector (Signal a)
concatPN = concatV

-- | The function 'reversePN' reverses the order of elements in a vector. 
reversePN  :: Vector (Signal a) -> Vector (Signal a)
reversePN = reverseV

-- | The function 'shiftlV' shifts a value from the left into a vector. 
shiftlPN :: Vector (Signal a) -> Signal a -> Vector (Signal a) 
shiftlPN = shiftlV

-- | The function 'shiftrV' shifts a value from the right into a vector. 
shiftrPN :: Vector (Signal a) -> Signal a -> Vector (Signal a)
shiftrPN = shiftrV

-- | The function 'rotlV' rotates a vector to the left. Note that this fuctions does not change the size of a vector.
rotlPN   :: Vector (Signal a) -> Vector (Signal a)
rotlPN = rotlV

-- | The function 'rotrV' rotates a vector to the right. Note that this fuction does not change the size of a vector.
rotrPN :: Vector (Signal a) -> Vector (Signal a)
rotrPN = rotrV

-- | The function 'replicatePN' generates a vector with a given number of copies of the same signal. 
replicatePN :: (Num a, Eq a) => a -> Signal b -> Vector (Signal b)
replicatePN = copyV

-- | 'zipxPN' transforms a vector of signals into a signal of vectors.

zipxPN              :: Vector (Signal a) -> Signal (Vector a)
zipxPN NullV        = NullS
zipxPN (NullS :> _) = NullS
zipxPN s            = (mapV headS s) :- zipxPN (mapV tailS s)

-- | 'unzipxPN' is the inverse of 'zipxPN'
unzipxPN              :: Signal (Vector a) -> Vector (Signal a)
unzipxPN NullS        = NullV
unzipxPN (NullV :- _) = NullV
unzipxPN v            = (mapS headV v) :> unzipxPN (mapS tailV v)
    where 
      mapS _ NullS   = NullS
      mapS f (x:-xs) = f x :- mapS f xs

-- | 'selectIdxPN' selects signals based on a function of indexes in a vector of signals
selectIdxPN :: (Int -> Bool) -> Vector (Signal a) -> Vector (Signal a)
selectIdxPN f = mapV snd . filterV (\(idx,v) -> f idx) . zipV idxs 
    where idxs = vector $ iterate (+1) 1

-- | special cases of 'filteridxPN'
tailPN        :: Vector (Signal a) -> Vector (Signal a)
tailPN        = selectIdxPN (>1)
initPN        :: Vector (Signal a) -> Vector (Signal a)
initPN v      = selectIdxPN (< (lengthV) v) v
takePN        :: Int ->  Vector (Signal a) -> Vector (Signal a)
takePN n      = selectIdxPN (<=n)
dropPN        :: Int -> Vector (Signal a) -> Vector (Signal a)
dropPN n      = selectIdxPN (>n)
splitatPN     :: Int ->  Vector (Signal a) -> Vector (Signal a)
splitatPN n v = catPN (takePN n v) (dropPN n v)
oddsPN        :: Vector (Signal a) -> Vector (Signal a)
oddsPN        = selectIdxPN (odd)
evensPN       :: Vector (Signal a) -> Vector (Signal a)
evensPN       = selectIdxPN (even)
stridedSelPN  :: Int -> Int -> Int -> Vector (Signal a) -> Vector (Signal a)
stridedSelPN  = selectV

bitrevPN :: Vector (Signal a) -> Vector (Signal a)
bitrevPN (x:>NullV) = x:>NullV
bitrevPN xs = bitrevPN (evensPN xs) <+> bitrevPN (oddsPN xs)

-- | 'selectIdx1PN' selects signals based on a vector of indexes in a vector of signals
selectIdx1PN      :: Vector Int -> Vector (Signal a) -> Vector (Signal a)
selectIdx1PN ix v = mapV (\i -> v `atV` i) ix

-- | 'selectIdx1AdpPN' selects signals based on signal of  ector of indexes in a vector of signals
selectIdx1AdpPN      :: Signal (Vector Int) -> Vector (Signal a) -> Vector (Signal a)
selectIdx1AdpPN ixs v = unzipxPN $ zipWithS selfunc ixs (zipxPN v)
    where zipWithS f (x:-xs) (y:-ys) = f x y :- (zipWithS f xs ys)
          zipWithS _ _       _       = NullS
          selfunc ixv vec            = mapV (\i -> vec `atV` i) ixv

-- | 'gather' distributes a vector of signals towards different "worker" vectors based on a gather rule, with respect to a vector of idexes. TODO: make a typeclass for b ( = a | [a] | [[a]] ... )
gatherPN :: (Int -> Vector a -> b) 
         -> Vector Int 
         -> Vector (Signal a) 
         -> Vector (Signal b)
gatherPN r ix v = mapV (\i -> mapS (r i) (zipxPN v)) ix
    where mapS _ NullS   = NullS
          mapS f (x:-xs) = f x :- mapS f xs

-- | 'gather1' extends 'gather' by vectorizing the gather rule. Thus now each worker has its own rule.
gather1PN :: Vector (Int -> Vector a -> b) 
          -> Vector Int 
          -> Vector (Signal a) 
          -> Vector (Signal b)
gather1PN vr ix v = zipWithV (\r i -> mapS (r i) (zipxPN v)) vr ix
    where mapS _ NullS   = NullS
          mapS f (x:-xs) = f x :- mapS f xs

-- | 'gatherAdpPN' is the adaptive version of 'gather'. It inputs the indexes through a signal of vectors
gatherAdpPN :: (Int -> Vector a -> b) 
            -> Signal (Vector Int) 
            -> Vector (Signal a) 
            -> Vector (Signal b)
gatherAdpPN r ixs v = unzipxPN $ zipWithS (\i tok -> mapV (\x -> (r x tok)) i) ixs (zipxPN v)
    where zipWithS f (x:-xs) (y:-ys) = f x y :- (zipWithS f xs ys)
          zipWithS _ _       _       = NullS


-- * MoC-specific patterns

-- | 'unzipSYPN' is a MoC-specific pattern for SY which unzips a vector of signals of tuples into a tuple of vectors of signals
unzipSYPN :: Vector (Signal (a, b)) -> (Vector (Signal a), Vector (Signal b))
unzipSYPN = foldrV f (NullV, NullV) . mapV unzipSY
    where f x tp = (fst x:>fst tp, snd x:>snd tp)

-- | 'zipSYPN' is the inverse of 'unzipSYPN'
zipSYPN :: Vector (Signal a) -> Vector (Signal b) -> Vector (Signal (a, b))
zipSYPN a b = zipWithV zipSY a b

dualsSYPN :: Vector (Signal a) -> Vector (Signal (a,a))
dualsSYPN v = zipSYPN (takeV k v) (dropV k v)
	where k = lengthV v `div` 2

undualsSYPN :: Vector (Signal (a,a)) -> Vector (Signal a)
undualsSYPN v = x <+> y
	where (x,y) = unzipSYPN v

