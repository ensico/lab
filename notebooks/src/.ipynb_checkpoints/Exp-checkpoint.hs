
-- (c) MP-I (1998/9-2006/7) and CP (2005/6-2023/24)

module Exp where

import Cp
import IHaskell.Display
import System.Process
import Data.List
--- import BTree
--- import LTree
--- import FTree
--- import GHC.IO.Exception
--- import St
--- import List hiding (lookup)
--- 

-- (1) Datatype definition -----------------------------------------------------

data Exp v o =   Var v              -- expressions are either variables
               | Term o [ Exp v o ] -- or terms involving operators and
                                    -- subterms
               deriving (Show,Eq)

inExp = either Var (uncurry Term)
outExp(Var v) = i1 v
outExp(Term o l) = i2(o,l)

baseExp f g h = f -|- (g >< map h)

-- (2) Ana + cata + hylo -------------------------------------------------------

recExp x = baseExp id id x

cataExp g = g . recExp (cataExp g) . outExp

anaExp g = inExp . recExp (anaExp g) . g

hyloExp h g = cataExp h . anaExp g

-- (3) Map ---------------------------------------------------------------------

instance BiFunctor Exp
         where bmap f g = cataExp ( inExp . baseExp f g id )

-- (4) Examples ----------------------------------------------------------------

mirrorExp = cataExp (inExp . (id -|- (id><reverse)))

expLeaves :: Exp a b -> [a]
expLeaves = cataExp (either singl (concat . p2))

expOps :: Exp a b -> [b]
expOps = cataExp (either nil (cons . (id><concat)))

expWidth :: Exp a b -> Int
expWidth = length . expLeaves

expDepth :: Exp a b -> Int
expDepth = cataExp (either (const 1) (succ . (foldr max 0) . p2))

nodes :: Exp a a -> [a]
nodes = cataExp (either singl g) where g = cons . (id >< concat)

graph :: Exp (a, b) (c, d) -> Exp a c
graph = bmap fst fst

-- (5) Graphics (DOT / HTML) ---------------------------------------------------

cExp2Dot :: Exp (Maybe String) (Maybe String) -> String
cExp2Dot x = beg ++ main (deco x) ++ end where
     main b = concat $ (map f . nodes) b  ++ (map g . lnks . graph) b
     beg = "digraph G {\n    /* edge [label=0]; */\n    graph [ranksep=0.5];\n"
     end = "}\n"
     g(k1,k2) = "    " ++ show k1 ++ " -> " ++ show k2 ++ "[arrowhead=none];\n"
     f(k,Nothing) = "    " ++ show k ++ " [shape=plaintext, label=\"\"];\n"
     f(k,Just s) = "    " ++ show k ++ " [shape=circle, style=filled, fillcolor=\"#FFFF00\", label=\"" ++ s ++ "\"];\n"

lnks :: Exp a a -> [(a, a)]
lnks (Var n) = []
lnks (Term n x) = (x >>= lnks) ++ [ (n,m) | Term m _ <- x ] ++ [ (n,m) | Var m <- x ]

deco :: Num n => Exp v o -> Exp (n, v) (n, o)
deco e = fst (st (f e) 0) where
     f (Var e) = do {n <- get ; put(n+1); return (Var(n,e)) }
     f (Term o l) = do { n <- get ; put(n+1);
                         m <- sequence (map f l);
                         return (Term (n,o) m)
                       }
------------------------------------------------------------------
-- NB: This is a small, pointfree "summary" of Control.Monad.State
------------------------------------------------------------------

data St s a = St { st :: (s -> (a, s)) }

inSt  = St
outSt = st

-- NB: "values" of this monad are actions rather than states.
--     So this should be called the "action monad" and not the
--     "state monad".
--     (Unfortunately, it is to late to change terminology.)

--------------------------------------------------------------------------
instance Monad (St s) where
         return       = St . (curry id)
         (St x) >>= g = St   (uncurry(st . g) . x )
{-- ie:
         (St x) >>= g = St (\s -> let (a,s') = x s
                                      St k   = g a
                                  in k s')
--}
--------------------------------------------------------------------------
instance Functor (St s) where
         fmap f t = do { a <- t ; return (f a) }  -- as in every monad
-- ie:   fmap f (St g) = St(\s -> let (a,s') = g s in (f a,s'))

--------------------------------------------------------------------------
-- generic actions 

get   :: St s s                           -- as in class MonadState
get = St(split id id) 

modify :: (s -> s) -> St s ()
modify f = St(split (!) f)

put :: s -> St s ()                       -- as in class MonadState
put s = modify (const s)

--------------------------------------------------------------------------
instance Applicative (St s) where
   (<*>) = aap 
   pure  = return
--------------------------------------------------------------------------
