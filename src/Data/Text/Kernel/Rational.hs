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
-- Rational kernels, as defined in [1]
--
-- C. Cortes, P. Haffner, M. Mohri - Rational kernels : theory and algorithms - JMLR 1 (2004) 1-50
-----------------------------------------------------------------------------
module Data.Text.Kernel.Rational where

-- import Control.Category (Category(..))
-- import Data.List (unfoldr)
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

-- import Prelude hiding (sum, product, id, (.))
import Prelude hiding (sum, product)



data Transition i o w = Transition {
  _transitionInput :: i
  , _transitionOutput :: o
  , _transitionWeight :: w } deriving (Eq, Show)
makeLenses ''Transition

instance Semigroup (Transition i o w) -- FIXME
instance Monoid (Transition i o w)  -- FIXME

product, sum :: (Foldable t, Semiring b) => t b -> b
product = foldl (<.>) one
sum = foldl (<+>) zero

-- | Weighted finite-state transducer
data WFST w s i o = WFST {
    _wfstGraph :: GL.Graph (Transition i o w) s -- ^ Transition graph
  , _wfstInitWeightFunction :: s -> w -- ^ Initial weight function 
  , _wfstFinalWeightFunction :: s -> w  -- ^ Final weight function
                                  }
makeLenses ''WFST

-- instance Category (WFST w s) where  -- FIXME

transition :: (Monoid i, Monoid o, Monoid w, Ord c, Eq i, Eq o, Eq w) =>
              Transition i o w
           -> WFST w c i o
           -> Maybe (Transition i o w, c, c)
transition e (WFST g _ _) = listToMaybe $ filter (\(l, _, _) -> l == e) $ GL.edgeList g


origin, destination :: (Monoid i, Monoid o, Monoid w, Ord c, Eq i, Eq o, Eq w) =>
                       Transition i o w
                    -> WFST w c i o
                    -> Maybe c
origin e rk = (\(_, x, _) -> x) <$> transition e rk
destination e rk = (\(_, _, y) -> y) <$> transition e rk


weight :: (Monoid i, Monoid o, Monoid b, Ord c, Eq i, Eq o, Eq b) =>
          Transition i o b
       -> WFST b c i o
       -> Maybe b
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
pathWeight ts = product (map _transitionWeight ts)

lambda, rho :: WFST w s i o  -> s -> w
lambda = _wfstInitWeightFunction
rho = _wfstFinalWeightFunction


verticesConnecting :: (Eq e, Monoid e, Ord v) => v -> v -> GL.Graph e v -> [v]
verticesConnecting p q gr = filter (== q) $ reachable p gr


{-
how to test whether a string belongs to the Kleene star of an alphabet?
--}

-- kleene n alpha = unfoldr f []
--   where
--     f acc
--       | length acc < n = let acc' = alpha : acc in Just (concat acc' , acc')
--       | otherwise = Nothing

-- kleene alpha = go 0 []
--   where
--     go i acc = map (\a -> a : acc) alpha : go (i + 1)

-- kleene alpha = go []
--   where
--     go acc = let l = map (: acc) alpha in go l 

-- [[], [1], [2], ..., [N], [1,1], [1,2], ..., [1,N], [1,1,1], [1,1,2], ...]


-- transducerWeight1 path rk = l <.> w <.> r
--   where
--     l = lambda <$> pathOrigin path rk
--     w = pathWeight path
--     r = rho <$> pathDestination path rk
