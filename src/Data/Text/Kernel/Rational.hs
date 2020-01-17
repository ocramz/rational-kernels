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

import Control.Category (Category(..))
import Data.Bifunctor (Bifunctor(..))
-- import Data.List (unfoldr)
import Data.Tree (Tree(..), Forest, foldTree)
import Data.Maybe (listToMaybe)
import Data.Semigroup (Semigroup(..), Any(..))
import GHC.Exts (IsList (..))

-- algebraic-graphs
import Algebra.Graph.ToGraph (ToGraph(..))
import qualified Algebra.Graph.Labelled as GL (Graph, empty, vertex, edge, overlay, edges, hasEdge, edgeList, vertexList, emap)
import Algebra.Graph.Label (Semiring(..), zero, (<+>), StarSemiring(..), Dioid(..), Label(..), isZero)
-- containers
import qualified Data.Map as M (Map, fromList, lookup, filterWithKey)
import qualified Data.Set as S (Set, fromList, toList)
-- microlens
-- import Lens.Micro
-- microlens-th
import Lens.Micro.TH (makeLenses)

import Prelude hiding (sum, product, id, (.))
-- import Prelude hiding (sum, product) 



-- fromList [A,B] :: Label Alpha :: Label Alpha
-- λ> :t star (fromList [A,B]) :: Label Alpha
data Alpha = A | B | C deriving (Eq, Show, Ord, Enum, Bounded)





data Transition w i o = Transition {
  _transitionInput :: i
  , _transitionOutput :: o
  , _transitionWeight :: w } deriving (Eq, Show)
makeLenses ''Transition

transpose :: Transition w i o -> Transition w o i
transpose (Transition is os w) = Transition os is w

instance Semigroup (Transition w i o) -- FIXME
instance Monoid (Transition w i o)  -- FIXME

instance Bifunctor (Transition w) where
  bimap f g (Transition x y w) = Transition (f x) (g y) w

products, sums :: (Foldable t, Semiring b) => t b -> b
products = foldl (<.>) one
sums = foldl (<+>) zero

-- | Weighted finite-state transducer
data WFST w s i o = WFST {
    _wfstGraph :: GL.Graph (Transition w i o) s -- ^ Transition graph
  , _wfstInitWeightFunction :: s -> w -- ^ Initial weight function 
  , _wfstFinalWeightFunction :: s -> w  -- ^ Final weight function
                                  }
makeLenses ''WFST

-- | The inverse T^-1 of a transducer T is obtained by swapping the input and output labels (and alphabets) of each transition
invert :: WFST w s o i -> WFST w s i o
invert (WFST gr l r) = WFST (GL.emap transpose gr) l r

-- | A positive-definite symmetric kernel can be obtained by composing a transducer with its inverse ([1], Proposition 7, p. 11)
pds :: WFST w s i o -> WFST w s o o 
pds t = t . invert t

-- | Initial and final weight functions ([1], p.4)
lambda, rho :: WFST w s i o  -> s -> w
lambda = _wfstInitWeightFunction
rho = _wfstFinalWeightFunction

type WFA w s x = WFST w s x x -- weighted finite automaton

instance Category (WFST w s) where  -- FIXME

-- transition :: (Monoid i, Monoid o, Monoid w, Ord c, Eq i, Eq o, Eq w) =>
--               Transition i o w
--            -> WFST w c i o
--            -> Maybe (Transition i o w, c, c)
-- transition e (WFST g _ _) = listToMaybe $  filter (\(l, _, _) -> l == e) $ GL.edgeList g


-- origin, destination :: (Monoid i, Monoid o, Monoid w, Ord c, Eq i, Eq o, Eq w) =>
--                        Transition i o w
--                     -> WFST w c i o
--                     -> Maybe c
-- origin e rk = (\(_, x, _) -> x) <$> transition e rk
-- destination e rk = (\(_, _, y) -> y) <$> transition e rk


-- weight :: (Monoid i, Monoid o, Monoid b, Ord c, Eq i, Eq o, Eq b) =>
--           Transition i o b
--        -> WFST b c i o
--        -> Maybe b
-- weight e rk = (\(w, _, _) -> _transitionWeight w) <$> transition e rk



pathWeight :: Semiring w => [Transition w i o] -> w
pathWeight ts = products (map _transitionWeight ts)


-- foldTree f = go where
--     go (Node x ts) = f x (map go ts)

-- foldTree'' f = go [] where
--   go acc (Node _ []) = acc
--   go acc (Node x ts) = let acc' = x : acc in f $ map (go acc') ts

foldTree' f = go [] where
  go acc (Node _ []) = acc
  go acc (Node x ts) = let acc' = x : acc in f x (map (go acc') ts)

-- -- pathsConnecting :: (Ord a, Monoid e, Eq e) =>  GL.Graph e a -> a -> a -> M.Map a [a]
-- pathsConnecting gr p q =
--   concat $ M.filterWithKey (\ v vs -> v == p && q `elem` vs) $ reachableMap gr


reachableMap :: (Ord k, Monoid e, Eq e) => GL.Graph e k -> M.Map k [k]
reachableMap g' = M.fromList $ map (\v -> (v, reachable v g')) $ GL.vertexList g'


reachableSkeleton :: (Ord s, Eq w) =>
                     GL.Graph (Transition w (Label i) (Label o)) s
                  -> M.Map s [s]
reachableSkeleton = reachableMap . skeleton 

-- | Relabel edges because 'Label' does not allow an Eq instance
skeleton :: GL.Graph (Transition w (Label i) (Label o)) s
         -> GL.Graph (Transition w Any       Any      ) s
skeleton = GL.emap (bimap unlabel unlabel)

unlabel :: Label a -> Any
unlabel = Any . not . isZero


-- verticesConnecting :: (Eq e, Monoid e, Ord v) => v -> v -> GL.Graph e v -> [v]
-- verticesConnecting p q gr = filter (== q) $ reachable p gr




-- -- | Transducer weight
-- --
-- -- [1], Equation 1

transducerW1 path t = lambda t p <.> rho t n -- <.> pathWeight path <.> rho t n
  where
    p = head path
    n = last path

-- transducerWeight1 path rk = l <.> w <.> r
--   where
--     l = lambda <$> pathOrigin path rk
--     w = pathWeight path
--     r = rho <$> pathDestination path rk


-- | Rational kernel
--
-- [1], Definition 3
data RKernel w s i o = RKernel {
    _rkTransducer :: WFST w s i o
  , _rkValueFunction :: w -> Double  -- ^ \Psi()
                               }
