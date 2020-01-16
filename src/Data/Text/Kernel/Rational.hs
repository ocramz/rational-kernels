{-# language OverloadedStrings #-}
{-# language TemplateHaskell #-}
-----------------------------------------------------------------------------
-- |
-- Module     : Data.Text.Kernel.Rational
-- Copyright  : (c) Marco Zocca 2020
-- License    : see the file LICENSE
-- Maintainer : ocramz
-- Stability  : experimental
--
-- Rational kernels
-----------------------------------------------------------------------------
module Data.Text.Kernel.Rational where

import Data.Maybe (listToMaybe)
import Data.Semigroup (Semigroup(..))


-- algebraic-graphs
import Algebra.Graph.ToGraph (ToGraph(..))
import qualified Algebra.Graph.Labelled as GL (Graph, empty, vertex, connect, overlay, edges, hasEdge, edgeList)
import Algebra.Graph.Label (Semiring(..), zero, (<+>))
-- containers
import qualified Data.Set as S (Set, fromList, toList)
-- microlens
-- import Lens.Micro
-- microlens-th
import Lens.Micro.TH (makeLenses)
-- -- semirings
-- import Data.Semiring (Semiring(..), sum, product)


import Prelude hiding (sum, product)



data Transition i o w = Transition {
  _transitionInput :: i
  , _transitionOutput :: o 
  , _transitionWeight :: w
                                   } deriving (Eq, Show)
makeLenses ''Transition

instance Semigroup (Transition i o w)
instance Monoid (Transition i o w)

product, sum :: (Foldable t, Semiring b) => t b -> b
product = foldl (<.>) one
sum = foldl (<+>) zero





data RationalKernel i o w s = RK {
    _rkGraph :: GL.Graph (Transition i o w) s
  , _rkInitWeightFunction :: s -> w
  , _rkFinalWeightFunction :: s -> w
                                  }
makeLenses ''RationalKernel

transition :: (Ord c, Eq i, Eq o, Eq w) =>
              Transition i o w
           -> RationalKernel i o w c
           -> Maybe (Transition i o w, c, c)
transition e (RK g _ _) = listToMaybe $ filter (\(l, _, _) -> l == e) $ GL.edgeList g

origin, destination :: (Ord s, Eq i, Eq o, Eq w) =>
                       Transition i o w
                    -> RationalKernel i o w s
                    -> Maybe s
origin e rk = (\(_, x, _) -> x) <$> transition e rk
destination e rk = (\(_, _, y) -> y) <$> transition e rk

weight :: (Ord c, Eq i, Eq o, Eq w) =>
          Transition i o w
       -> RationalKernel i o w c
       -> Maybe w
weight e rk = (\(w, _, _) -> _transitionWeight w) <$> transition e rk

-- pathOrigin, pathDestination :: (Ord s, Eq i, Eq o, Eq w) =>
--                        [Transition i o w]
--                     -> RationalKernel i o w s
--                     -> Maybe s
-- pathOrigin (e:_) rk = origin e rk
-- pathOrigin [] _  = Nothing
-- pathDestination es rk = destination (last es) rk
-- pathDestination [] rk = Nothing

pathWeight :: Semiring w => [Transition i o w] -> w
pathWeight = product . map _transitionWeight

lambda, rho :: RationalKernel i o w s -> s -> w
lambda = _rkInitWeightFunction
rho = _rkFinalWeightFunction


pathsConnecting :: (Eq e, Monoid e, Ord v) => v -> v -> GL.Graph e v -> [v]
pathsConnecting p q gr = filter (== q) $ reachable p gr


{-
how to test whether a string belongs to the Kleene star of an alphabet?
--}

kleene alpha = map (\a -> a : concat (kleene alpha)) alpha


-- transducerWeight1 path rk = l <.> w <.> r
--   where
--     l = lambda <$> pathOrigin path rk
--     w = pathWeight path
--     r = rho <$> pathDestination path rk
