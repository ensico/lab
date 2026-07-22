{-# OPTIONS_GHC -XNPlusKPatterns #-}

-- (c) IPM, University of Minho (2008/09-2021/22)
-- Version 1.2b

module Ipm where

import Cp
import Reducer
import Data.Ratio
import Data.Char
import Data.List
import IHaskell.Display
import System.Process

-- (0) Score simplified format --------------------

data P2 a = [a] :|: [a] |               -- two treble parts
            [a] :||: [a] |              -- two part (treble, bass)
            P [[a]] deriving Show        -- n parts n staves

instance Functor P2
   where fmap f (l :|: r) = (map f l) :|: (map f r) 
         fmap f (l :||: r) = (map f l) :||: (map f r) 
         fmap f (P xs) = P (map (map f) xs)

top (x :|: y) = x
top (x :||: y) = x
top (P x) = head x

bot (x :|: y) = x
bot (x :||: y) = x
bot (P x) = last x

-- (1) Sampling

class Samplable t 
      where sample :: (Ord b, Num b) => [b] -> t (a, b) -> t(a, b)

instance  Samplable []
   where sample s x = sample_l s x
--sample_l :: (Num d, Ord d) => [d] -> [(n, d)] -> [(n, d)]

instance Samplable P2
   where sample i (a :|: b) = (sample i a) :|: (sample i b)
         sample i (a :||: b) = (sample i a) :||: (sample i b)
         sample i (P xs) = P (map (sample i) xs)

-- (2) Bars

class Div t 
      where divide :: (Ord b, Num b) => [b] -> t (a, b) -> t(N (a, b))

instance Div []
   where divide = dividl

instance Div P2
   where divide r (a :|: b) = (divide r a) :|: (divide r b)
         divide r (a :||: b) = (divide r a) :||: (divide r b)
         divide r (P xs) = P (map (divide r) xs)

-- (3) Augmentation, diminution

class Scalable t 
   where scale :: (Num a, Ord a) => a -> t -> t 

instance (Scalable b) => Scalable (P2 b)
   where scale i (a :|: b) = (scale i a) :|: (scale i b)
         scale i (a :||: b) = (scale i a) :||: (scale i b)
         scale i (P xs) = P (map (scale i) xs)

instance  (Scalable b, Scalable c) => Scalable (b,c)
   where scale a = (scale a) >< (scale a)

instance (Scalable a) => Scalable [a]
   where scale i = map (scale i)

instance Scalable Char
   where scale i = id

instance (Integral a) => Scalable (Ratio a)  -- dummy version...
    where scale i x | i < 1 =  x * (1%2)
                    | otherwise = x * 2

-- (4) Pattern matching

findPattern :: (Eq b) => [b] -> [b] -> [Int]
findPattern p l = [ i | i <-inds l , (match p l i) ]
 
match p l i = p `isPrefixOf` (drop i l)

-- (5) Repetitions

-- repeat anything 

rep x = map (const x) [0..]

-- repeat rythmic cell forever: Data.List cycle function
-- repeat :: forall a. a -> [a] available from GHC.List

ntimes :: (Integral t) => [a] -> t -> [a]
ntimes x = for (x++) []

-- abbreviation:
x !* y = ntimes x y

expand_(x,n) = take n (repeat x)

copy = curry (expand_ . swap)

twice = flip ntimes 2

-- abstract from repeated notes
--nrep [] = []
--nrep ((n,d):l) = aux n d l
--             where aux n d [] = [(n,d)]
--                   aux n d ((n',d'):l)
--                        | n==n' = aux n (d+d') l
--                        | n/=n' = (n,d):(aux n' d' l)
nrep [] = []
nrep [x] = [x]
nrep ((n,d):(n',d'):l)
     | n==n' = nrep ((n,d+d'):l)
     | n!=n' = (n,d):nrep((n',d'):l)

-- (6) Sampling for analysis

sample_l :: (Ord d, Num d) => [d] -> [(n, d)] -> [(n, d)]
sample_l [] _ = []
sample_l _ [] = []
sample_l (y:r) ((a,x):t)
        | y < 0 && x + y == 0  = sample_l r t
        | y < 0 && x + y >  0  = sample_l r ((a,x+y):t)
        | y < 0 && x + y <  0  = sample_l ((x+y):r) t
        | y > 0 && y < x       = (a,y) : sample_l r ((a,x-y):t)
        | y > 0 && y > x       = (a,y) : sample_l ((x-y):r) t
        | y > 0 && y == x      = (a,y) : sample_l r t

-- (7) Introducing bars and spacing

data N a = N a | Bar | TBar | Tie | BTie | ETie | Sp deriving Show

dividl :: (Ord b, Num b) => [b] -> [(a, b)] -> [N (a, b)]
dividl [] _ = []
dividl _ [] = []
dividl (y:r) ((a,x):t)
         | y < x       = [ N (a,y) , TBar ] ++ dividl r ((a,x-y):t)
         | y > x       = ( N (a,x)) : dividl ((y-x):r) t
         | y == x      = [ N (a,x) , Bar ] ++ dividl r t
 
-- (8) Generic derivative / integral

gderiv :: a -> (a -> a -> c) -> [a] -> [c]
gderiv z f s = ttl (zipWith f s (z:s))
              where ttl [] = []
                    ttl (a:t) = t

ginteg :: (a -> b -> b) -> b -> [a] -> [b]
ginteg f i s = i:aux i s
      where aux i [] = []
            aux i (h:t) = i':(aux i' t)
                          where i'=f h i

-- (8.1) Simple discrete derivative / integral

dint = ginteg (+)

dde  = gderiv  0 (-)

-- diat b a = (b-a) + if b < a then -1 else 1

-- (9) Misc.

cut :: Int -> Int -> [a] -> [a]
cut n m = (take (m-n+1)) . drop (n-1)

inds :: [b] -> [Int]
inds = (map fst) . (zip [1..])

elems :: (Ord a) => [a] -> [a]
elems = sort . nub

-- histogram given population list

hist l = nub [ (x, count x l) | x <- l ]
         where count a l = length [ x | x <- l, x == a]

-- dist l = [ (x, ((fromRational n) / (fromRational t))) | (x,n) <- hist l ] where t = length l

-- ndcopy is the nub function

ndcopy [] = []
ndcopy [x] = [x]
ndcopy (x:r) = x:(filter (/=x)(ndcopy r))

-- ncdcopy: no consecutive duplicates

ncdcopy [] = []
ncdcopy [x] = [x]
ncdcopy (x:y:l)
     | x==y =   ncdcopy(x:l)
     | x/=y = x:ncdcopy(y:l)

-- matching

patternIndices p l = [ (i,i+length p-1) | (x,i) <- zip l [0..], match p l i ]
 
-- misc

sing a = [a]
dup a = [a,a]

a != b = not(a == b)

a |-> b = (a,b)

supermap f [] = []
supermap f (a:l) = a:(map f (supermap f l))

--- list of pairs to list

lp2l :: [(b, b)] -> [b]
lp2l = (concat . map f) where f(a,b)=[a,b]

name2music w = map (g.f) w
        where g c = [c]
              f ' ' = 'z'
              f 'H' = 'B'
              f 'h' = 'b'
              f c | c `elem` ['A'..'Z']++['a'..'z'] = chr(rem(ord c - b) 8 + b)
                  | otherwise = 'z'
                    where  b = if c `elem` ['A'..'Z'] then ord('A') else ord('a')

notVowel c = not(c `elem` "aeiouAEIOU")

---------
--dotted1 = fst . (tmap (5%4*) (1%2*) id)
tmap f g h = split f1 (split f2 f3)
      where
      f1 [] = []
      f1 (a:l) = (f a):f2 l
      f2 [] = [] 
      f2 (a:l) = (g a):f3 l
      f3 [] = [] 
      f3 (a:l) = (h a):f1 l

frac n m = n%m

retrog m = let (l,r) = unzip m in zip (reverse l) r

omit = id -- used in hiding information in lhs2tex

invert s = nub [(x, findIndices (==x) s) | x <- s ]

-- bar patterns

bin :: [Ratio Int]
bin = (repeat (2%4))

tern :: [Ratio Int]
tern = (repeat (3%4))

quatern :: [Ratio Int]
quatern = (repeat (4%4))

hexa :: [Ratio Int]
hexa = (repeat (6%4))

-- for combinator

for b i 0 = i
for b i (n+1) = b(for b i n)

n_gram n []  = []
n_gram n [_] = []
n_gram n xs  = take n xs : n_gram n (tail xs)

showhist :: (Show a, Ord a, Integral c) => [(a, c)] -> IO [Char]
showhist = fmap (const "-----------------------") . sequence . (map print) . (map ((uncurry (++)) . (showx >< (ntimes "X")))) . sort
           where showx x = (show x) ++ " - "

rosalia f n l = (concat . (supermap(map f)) . (take n) . repeat) l

med m = m'!!i where
     m' = sort m
     i = if even l then x-1 else x
     x = div l 2
     l = length m

presort f = map snd . sort . (map (split f id)) -- pre-sorting on f-preorder

apl :: [a -> b] -> [a] -> [b]
apl f l = map ap (zip f l)

flat  :: [(b, c)] -> [(b, Ratio Int)]
flat = map (id >< (const 0)) --- remove rythmic information

flats :: (Enum a, Num a) => a -> [a]   -- eg. flats Eb = Bb,Eb,Ab
flats t = reverse [(t-1)..(-2)]

sharps :: (Enum a, Num a) => a -> [a]        -- eg. sharps E = F#,C#,G#,E#
sharps t = [6..t+5]

--- HTML minimal API

tag t l x = "<"++t++" "++ps++">"++x++"</"++t++">"
             where ps = unwords [concat[t,"=",v]| (t,v)<-l]

htm = tag "html" []

strong  = tag "strong" []

tr  = tag "tr" []

img fn = tag "img" [ "src" |-> (show fn) ] ""

uli = ul . (>>= li) where
    li  = tag "li" []
    ul  = tag "ul" []

-- nest t p f = (tag t p) . (>>=f)

-- table = nest "table" [ "border" |-> "1" , "data-toggle" |-> "\"table\"" ]
--              (nest "tr" [] (tag "td" [ "align" |-> "center"]))

--- DOT, GraphViz API

dot' = showDot . g2dot
    where f((a,b),c) = show a ++ " -> " ++ show b ++ " [ label=\"" ++ show c ++ "\" ];"
          cincat s = concat . intersperse s

showDot d = do { writeFile "_.dot" d ;
                 system "dot -Tsvg _.dot -o _.svg";
                 (return . (IHaskell.Display.html) . htm . img) "_.svg"
                 }

reduced s = let r = reduce s
                h = (htm . dt . uli . map pts. snd) r
                dt s = img "_.svg" ++ s
                eq = (" = ":)
                pts = unwords . cons .(strong >< eq)
            in do { showDot . toDot $ r;  return(IHaskell.Display.html h) }

g2dot g = "digraph G {\n" ++ cincat "\n" (map f g) ++ "\n}\n"
   where f((a,b),c) = k a ++ " -> " ++ k b ++ " [ label=\"" ++ k c ++ "\" ];"
         cincat s = concat . intersperse s
         k = map cl

--- remover acentos, se necessário
cl '\237'= 'i'
cl '\243'= 'o'
cl '\227'= 'a'
cl '\231'= 'c'
cl c     = c

------- DATA --------

carnaval_serrano :: [(String,Ratio Int)]
carnaval_serrano = [("B",3 % 8),("A",1 % 8),("G",1 % 2),("G",1 % 4),("A",1 % 8),("B",1 % 8),("B",1 % 4),("B",1 % 4),("B",1 % 4),("A",1 % 4),("G",1 % 2),("G",1 % 4),("A",1 % 8),("B",1 % 8),("B",1 % 4),("B",1 % 4),("B",1 % 4),("A",1 % 4),("G",1 % 2),("G",1 % 4),("A",1 % 4),("G",1 % 4),("G",1 % 4),("A",1 % 4),("G",1 % 4),("E",3 % 4),("B",1 % 4),("E",3 % 4),("B",1 % 4),("E",1 % 2),("B",3 % 8),("A",1 % 8),("G",1 % 2),("G",1 % 4),("A",1 % 8),("B",1 % 8),("B",1 % 4),("B",1 % 8),("d",1 % 8),("B",1 % 4),("A",1 % 4),("G",1 % 2),("G",1 % 4),("A",1 % 8),("B",1 % 8),("B",1 % 4),("B",1 % 4),("B",1 % 4),("A",1 % 4),("G",1 % 2),("G",1 % 4),("A",1 % 4),("G",1 % 4),("G",1 % 4),("A",1 % 4),("G",1 % 4),("E",3 % 4),("B",1 % 4),("E",3 % 4),("B",1 % 4),("E",1 % 2),("B",3 % 8),("A",1 % 8)]


