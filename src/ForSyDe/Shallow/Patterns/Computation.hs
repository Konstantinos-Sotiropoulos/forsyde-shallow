-----------------------------------------------------------------------------
-- |
-- Module      :  ForSyDe.Shallow.Computation
-- Copyright   :  ...
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  ...
-- Stability   :  experimental
-- Portability :  portable
--
-- ...
-----------------------------------------------------------------------------
module ForSyDe.Shallow.Patterns.Computation(
                                   farmPN, farm1PN, farm2PN, --farm3PN,
                                   pipePN, pipe1PN, --pipe2PN, pipe3PN,
                                   scanPN, scan1PN, --scan2PN, scan3PN,
                                   reducePN, 
                                   vsourcePN, --vsource1PN, vsource2PN, vsource3PN
                                  -- patterns which make sense only for specific MoCs
                                   --filterSYPN, maskSYPN
                                  ) where

import ForSyDe.Shallow.CoreLib
import ForSyDe.Shallow.MoCLib

farmPN :: (Signal a -> Signal b) -> Vector (Signal a) -> Vector (Signal b) 
farmPN = mapV

farm1PN :: (c -> Signal a -> Signal b) -> Vector c
      -> Vector (Signal a) -> Vector (Signal b) 
farm1PN = zipWithV

farm2PN :: (c -> d -> Signal a -> Signal b) -> Vector c -> Vector d 
      -> Vector (Signal a) -> Vector (Signal b) 
farm2PN f (x:>xs) (y:>ys) (z:>zs) = f x y z :> (farm2PN f xs ys zs)
farm2PN _ _       _       _       = NullV


pipePN :: Int -> (Signal a -> Signal a) -> Signal a  -> Signal a
pipePN n p s = foldrV (\x y -> x y) s $ copyV n p

pipe1PN :: (a -> Signal b -> Signal b) -> Vector a  -> Signal b  -> Signal b
pipe1PN p v s = foldrV p s v

scanPN :: Int -> (Signal a -> Signal a) -> Signal a  -> Vector (Signal a)
scanPN n p s = scanrV (\x y -> x y) s $ copyV n p

scan1PN :: (a -> Signal b -> Signal b) -> Vector a  -> Signal b  -> Vector (Signal b)
scan1PN p v s = scanrV p s v

reducePN :: (Signal a -> Signal a -> Signal a) -> Vector (Signal a) -> Signal a 
reducePN = foldr1V

vsourcePN :: Int -> Signal a -> Vector (Signal a)
vsourcePN = copyV

------------------------------------------------
scanrV    :: (b -> a -> a) -> a -> Vector b -> Vector a
scanrV _ _ NullV      = NullV
scanrV f a (x:>NullV) = f x a :> NullV
scanrV f a (x:>xs)    = f x y :> ys 
                      where ys@(y:>_) = scanrV f a xs

foldr1V              :: (a -> a -> a) -> Vector a -> a
foldr1V _ NullV      = error "cannot fold an empty vector"
foldr1V _ (x:>NullV) = x 
foldr1V f (x:>xs)    = f x (foldr1V f xs)
