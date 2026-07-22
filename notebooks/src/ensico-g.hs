module ENSICO.Graphics where

import Data.Char
import Data.List
import System.Process
import IHaskell.Display

--import Data.List.Split
chunksOf :: Int -> [a] -> [[a]]
chunksOf n [] = []
chunksOf n x = take n x : chunksOf n (drop n x)

--import Cp
ap :: (a -> b,a) -> b
ap = uncurry ($)

--- PT -> EN (but note EN is not a direct translation of PT!) ----

readQR = mensagemQR

-- drawQR = desenhaQR_A . mensagemQR_A

drawQR = desenhaQR . mensagemQR

encodeQR s i = (map (c' AP) . ensure 13 $ s) !! (i-1) 

drawing = pinta

cQR2m z = map z [1..13] 

---------- Really basic ------------------------------------------ 

a |-> b = (a,b)

swap(a,b) = (b,a)

(f >< g)(a,b) = (f a, g b)

x .><. y = [(a,b) | a <- x, b <- y]  -- Cartesian product

_fst (a,_,_) = a
_snd (_,b,_) = b
_trd (_,_,c) = c

ensure n =  take n . (++ repeat ' ')

--- transforma, cf. fadas

magic m  = map (map ap . uncurry zip) . zip m

--- composition ---

(|>) = flip ($)
(>>) = flip (.)

---

troca 0 = 1
troca 1 = 0

---

mul = uncurry (*)

add = uncurry (+)

--- Binary <-> decimal

bin2dec xs = sum (map (uncurry (*)) (zip (reverse xs) [ 2^i | i <- [0..length xs-1] ]))

dec2bin 0 = [0]
dec2bin n = dec2bin m ++ [b] where (m,b) = (div n 2, mod n 2)

dec2byte :: Int -> [Int]
dec2byte = reverse . take 8 . (++zeros) . reverse . dec2bin where zeros = 0:zeros

byte2dec = bin2dec

--- fita perfurada

punchtape x = do { putStrLn ""; mapM pt x ; putStrLn "" }
   where pt = putStrLn . punchbyte . ensurebyte
         punchbyte b = "|" ++ map f (take 5 b) ++ "." ++ map f (drop 5 b) ++ "|"
         f 0 = space ; f 1 = bullet
         space  = ' '
         bullet = '\8226'
         ensurebyte = dec2byte . bin2dec

furafita = punchtape

teletype = furafita . map ascii

--- ENSICO-QR ----------------------------------

mensagemQR z = concatMap ((l AP ) . z) [1..13]

mensagemQR_A z = concatMap ((l A ) . z) [1..13]

desenhaQR s = cod2QR' . map ((c AP ) . (:[])) $ s ++ concat (replicate n "-") where n = 13 - length s

desenhaQR_A s = cod2QR' . map ((c A ) . (:[])) $ s ++ concat (replicate n "-") where n = 13 - length s

data Alpha = A | AP deriving (Eq,Show)

data T = N | R | H | VH deriving (Eq,Show)
data MBit = M [[Int]] T deriving (Eq,Show)

pinta m = M m H

retro m = M m R

cod2QR z = M (qrcode 0 z) N

cod2QR' z = M (qrcode' 0 z) N

cqr2str z = concatMap ((l A   ) . z) [1..13]

str2QR s = cod2QR' . map ((c A   ) . (:[])) $ s ++ concat (replicate n "-") where n = 13 - length s

cifra = map succ

str2QR'' s = str2QR . cifra $ s

size t = case t of
    H  -> "8"
    VH -> "4"
    __ -> "16"

color t = case t of
    R -> ("black","green","grey")
    _ -> ("white","black","grey")

doubleBits = concatMap doubleBit
    where doubleBit 0 = [0]--[0,0]
          doubleBit 1 = [1]--[1,1]
          doubleBit _ = []

--- ENSICO QR-code display ----

instance IHaskellDisplay MBit where
  display bit = return $ Display [html code]
    where
    code = draw bit --concat [ draw bit ]

cssR t = cssB where
    cssB 0 = "<td width=\"" ++ (size t) ++ "\" height=\"" ++ (size t) ++ "\" bgcolor=\"" ++ (_fst (color t)) ++ "\"></td>"
    cssB 1 = "<td width=\"" ++ (size t) ++ "\" height=\"" ++ (size t) ++ "\" bgcolor=\"" ++ (_snd (color t)) ++ "\"></td>"
    cssB _ = "<td width=\"" ++ (size t) ++ "\" height=\"" ++ (size t) ++ "\" bgcolor=\"" ++ (_trd (color t)) ++ "\"></td>"

--begT = "<table width=\"352\" height=\"320\" cellspacing=\"0\" cellpadding=\"0\" border=\"0\">"
--begT = "<table style=\"width:400px height:400px\" cellspacing=\"0px\" cellpadding=\"0px\" border=\"0\">"
--endT = "</table>"

tab = tag "table" [ "width" |-> show2 352 , "height" |-> show2 320, "cellspacing" |-> show2 0, "cellpadding" |-> show2 0, "border" |-> show2 0] where show2 = show . show

--begR = "<tr>"
--endR = "</tr>"

draw (M b r) = tab (draw' b r)

draw' [] _ = ""
draw' (h:t) r = tr (item r) ++ draw' t r where
    item t = case t of
        N -> concatMap (cssR N) h
        R -> concatMap (cssR R) h
        H -> concatMap (cssR H) (doubleBits h)
        VH -> concatMap (cssR VH) (doubleBits . doubleBits $ h)

----

nave n = case n of
    1 ->    [   [0,0,1,0,0,1,0,0],
                [0,0,0,1,1,0,0,0],
                [1,0,1,1,1,1,0,1],
                [1,1,0,1,1,0,1,1],
                [1,1,1,1,1,1,1,1],
                [1,0,1,1,1,1,0,1],
                [1,0,0,1,1,0,0,1],
                [1,1,0,0,0,0,1,1] ]
    2 ->    [   [0,0,0,0,0,0,0,0,0,0,0],
                [0,0,1,0,0,0,0,0,1,0,0],
                [0,0,0,1,0,0,0,1,0,0,0],
                [0,0,1,1,1,1,1,1,1,0,0],
                [0,1,1,0,1,1,1,0,1,1,0],
                [1,1,1,1,1,1,1,1,1,1,1],
                [1,0,1,1,1,1,1,1,1,0,1],
                [1,0,1,0,0,0,0,0,1,0,1],
                [0,0,0,1,1,0,1,1,0,0,0],
                [0,0,0,0,0,0,0,0,0,0,0] ]
    3 ->    [   [0,0,1,0,0,0,0,0,0,1,0,0],
                [1,0,0,1,0,0,0,0,1,0,0,1],
                [1,0,1,1,1,1,1,1,1,1,0,1],
                [1,0,1,0,1,1,1,1,0,1,0,1],
                [1,1,1,1,1,1,1,1,1,1,1,1],
                [0,1,1,1,1,1,1,1,1,1,1,0],
                [0,0,1,0,0,0,0,0,0,1,0,0],
                [0,1,0,0,0,0,0,0,0,0,1,0] ]
    4 ->    [   [0,0,0,1,0,0,0],
                [0,0,1,1,1,0,0],
                [0,1,1,1,1,1,0],
                [0,1,0,0,0,1,0],
                [1,1,1,1,1,1,1],
                [1,0,1,1,1,0,1],
                [1,0,0,0,0,0,1] ]
    _ -> []

letra n = case n of
    1 -> [  [0,0,0,0,0,1,1,1,0,0],
            [0,0,0,0,1,1,1,0,0,0],
            [0,0,0,0,1,1,0,0,0,0],
            [0,0,0,0,1,1,0,0,0,0],
            [0,0,0,0,1,1,0,0,0,0],
            [0,0,0,1,1,1,1,0,0,0],
            [0,0,0,1,1,1,1,0,0,0],
            [0,0,1,1,0,1,1,1,0,0],
            [0,0,1,1,0,1,1,1,0,0],
            [0,0,1,1,1,1,1,1,0,0],
            [0,1,1,0,0,0,1,1,1,0],
            [0,1,1,0,0,0,0,1,1,0],
            [1,1,1,0,0,0,1,1,1,1] ]
    2 -> [  [0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,0,0,0,0,0],
            [0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,0,0,0,0,0],
            [0,0,0,0,0,0,0,0,0,0,1,1,1,1,0,0,0,0,0,0],
            [0,0,0,0,0,0,0,0,0,1,1,1,1,0,0,0,0,0,0,0],
            [0,0,0,0,0,0,0,0,1,1,1,1,0,0,0,0,0,0,0,0],
            [0,0,0,0,0,0,0,0,1,1,0,0,0,0,0,0,0,0,0,0],
            [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
            [0,0,0,0,0,0,0,0,0,1,1,0,0,0,0,0,0,0,0,0],
            [0,0,0,0,0,0,0,0,0,1,1,0,0,0,0,0,0,0,0,0],
            [0,0,0,0,0,0,0,0,1,1,1,1,0,0,0,0,0,0,0,0],
            [0,0,0,0,0,0,0,0,1,1,1,1,0,0,0,0,0,0,0,0],
            [0,0,0,0,0,0,0,1,1,1,1,1,1,0,0,0,0,0,0,0],
            [0,0,0,0,0,0,0,1,1,1,1,1,1,0,0,0,0,0,0,0],
            [0,0,0,0,0,0,0,1,1,0,1,1,1,0,0,0,0,0,0,0],
            [0,0,0,0,0,0,1,1,1,0,1,1,1,1,0,0,0,0,0,0],
            [0,0,0,0,0,0,1,1,0,0,1,1,1,1,0,0,0,0,0,0],
            [0,0,0,0,0,1,1,1,0,0,0,1,1,1,1,0,0,0,0,0],
            [0,0,0,0,0,1,1,0,0,0,0,1,1,1,1,0,0,0,0,0],
            [0,0,0,0,0,1,1,0,0,0,0,1,1,1,1,0,0,0,0,0],
            [0,0,0,0,1,1,1,0,0,0,0,0,1,1,1,1,0,0,0,0],
            [0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0],
            [0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0],
            [0,0,0,1,1,0,0,0,0,0,0,0,0,1,1,1,1,0,0,0],
            [0,0,0,1,1,0,0,0,0,0,0,0,0,1,1,1,1,1,0,0],
            [0,0,1,1,1,0,0,0,0,0,0,0,0,0,1,1,1,1,0,0],
            [0,0,1,1,0,0,0,0,0,0,0,0,0,0,1,1,1,1,0,0],
            [0,1,1,1,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,0],
            [1,1,1,1,1,1,0,0,0,0,0,0,1,1,1,1,1,1,1,1] ]
    _ -> []

z n = case n of
    1 -> [0,0,0,1]
    2 -> [1,0,0,0]
    3 -> [0,1,0,1]
    4 -> [0,1,0,0]
    5 -> [0,0,1,0]
    6 -> [1,1,1,0]
    7 -> [1,1,0,0]
    8 -> [0,0,0,0]
    9 -> [1,0,1,1]
    10 -> [0,1,0,1]
    11 -> [0,0,0,0]
    12 -> [0,0,1,0]
    13 -> [0,1,0,0]
    __ -> []

l abc c = case c of
    [0,0,0,0] -> "A"
    [0,0,0,1] -> "B"
    [0,0,1,0] -> "C"
    [0,0,1,1] -> "D"
    [0,1,0,0] -> "E"
    [0,1,0,1] -> "F"
    [0,1,1,0] -> "G"
    [0,1,1,1] -> "H"
    [1,0,0,0] -> "I"
    [1,0,0,1] -> "J"
    _________ -> case abc of
                     A    -> case c of
                             -- ENSICO-QR
                             [1,0,1,0] -> "L"
                             [1,0,1,1] -> "M"
                             [1,1,0,0] -> "N"
                             [1,1,0,1] -> "O"
                             [1,1,1,0] -> "."
                             [1,1,1,1] -> " "
                             _________ -> ""
                             -- ENSICO-QR
                     AP   -> case c of
                             [1,0,1,0] -> "K"
                             [1,0,1,1] -> "L"
                             [1,1,0,0] -> "M"
                             [1,1,0,1] -> "N"
                             [1,1,1,0] -> "O"
                             [1,1,1,1] -> "P"
                             _________ -> ""

c' abc l = case l of
    'A' -> [0,0,0,0]
    'B' -> [0,0,0,1]
    'C' -> [0,0,1,0]
    'D' -> [0,0,1,1]
    'E' -> [0,1,0,0]
    'F' -> [0,1,0,1]
    'G' -> [0,1,1,0]
    'H' -> [0,1,1,1]
    'I' -> [1,0,0,0]
    'J' -> [1,0,0,1]
    ___ -> case abc of
               A    -> case l of
                       -- ENSICO-QR
                       'L' -> [1,0,1,0]
                       'M' -> [1,0,1,1]
                       'N' -> [1,1,0,0]
                       'O' -> [1,1,0,1]
                       '.' -> [1,1,1,0]
                       ' ' -> [1,1,1,1]
                       ___ -> [-1,-1,-1,-1]
                       -- ENSICO-QR
               AP   -> case l of
                       'K' -> [1,0,1,0]
                       'L' -> [1,0,1,1]
                       'M' -> [1,1,0,0]
                       'N' -> [1,1,0,1]
                       'O' -> [1,1,1,0]
                       'P' -> [1,1,1,1]
                       ___ -> [-1,-1,-1,-1]

c abc l = case l of
    "A" -> [0,0,0,0]
    "B" -> [0,0,0,1]
    "C" -> [0,0,1,0]
    "D" -> [0,0,1,1]
    "E" -> [0,1,0,0]
    "F" -> [0,1,0,1]
    "G" -> [0,1,1,0]
    "H" -> [0,1,1,1]
    "I" -> [1,0,0,0]
    "J" -> [1,0,0,1]
    ___ -> case abc of
               A    -> case l of
                       -- ENSICO-QR
                       "L" -> [1,0,1,0]
                       "M" -> [1,0,1,1]
                       "N" -> [1,1,0,0]
                       "O" -> [1,1,0,1]
                       "." -> [1,1,1,0]
                       " " -> [1,1,1,1]
                       ___ -> [-1,-1,-1,-1]
                       -- ENSICO-QR
               AP   -> case l of
                       "K" -> [1,0,1,0]
                       "L" -> [1,0,1,1]
                       "M" -> [1,1,0,0]
                       "N" -> [1,1,0,1]
                       "O" -> [1,1,1,0]
                       "P" -> [1,1,1,1]
                       ___ -> [-1,-1,-1,-1]

qrcode n z = case n of
    0 -> [  [1,1,1,1,0,z 11!!0,z 11!!1,0,1,1,1,1],
            [1,0,0,1,0,z 11!!2,z 11!!3,0,1,0,0,1],
            [1,0,0,1,0,z 12!!0,z 10!!0,0,1,0,0,1],
            [1,1,1,1,0,z 12!!1,z 10!!1,0,1,1,1,1],
            [0,0,0,0,0,z 12!!2,z 10!!2,0,0,0,0,0],
            [-1,-1,-1,-1,1,z 12!!3,z 10!!3,1,z 4!!0,z 4!!1,z 4!!2,z 4!!3],
            [-1,-1,-1,-1,0,z 13!!0,z 9!!0,z 9!!1,z 5!!0,z 5!!1,z 3!!0,z 3!!1],
            [0,0,0,0,0,z 13!!1,z 9!!2,z 9!!3,z 5!!2,z 5!!3,z 3!!2,z 3!!3],
            [1,1,1,1,0,z 13!!2,z 8!!0,z 8!!1,z 6!!0,z 6!!1,z 2!!0,z 2!!1],
            [1,0,0,1,0,z 13!!3,z 8!!2,z 8!!3,z 6!!2,z 6!!3,z 2!!2,z 2!!3],
            [1,0,0,1,0,-1,z 7!!0,z 7!!1,z 7!!2,z 7!!3,z 1!!0,z 1!!1],
            [1,1,1,1,0,-1,-1,-1,-1,-1,z 1!!2,z 1!!3] ]
    _ -> []

qrcode' n z = case n of
    0 -> [  [1,1,1,1,0,    z!!10!!0,z!!10!!1,0,1,1,1,1],
            [1,0,0,1,0,    z!!10!!2,z!!10!!3,0,1,0,0,1],
            [1,0,0,1,0,    z!!11!!0,z!!9!!0,0,1,0,0,1],
            [1,1,1,1,0,    z!!11!!1,z!!9!!1,0,1,1,1,1],
            [0,0,0,0,0,    z!!11!!2,z!!9!!2,0,0,0,0,0],
            [-1,-1,-1,-1,1,z!!11!!3,z!!9!!3,1,      z!!3!!0,z!!3!!1,z!!3!!2,z!!3!!3],
            [-1,-1,-1,-1,0,z!!12!!0,z!!8!!0,z!!8!!1,z!!4!!0,z!!4!!1,z!!2!!0,z!!2!!1],
            [0,0,0,0,0,    z!!12!!1,z!!8!!2,z!!8!!3,z!!4!!2,z!!4!!3,z!!2!!2,z!!2!!3],
            [1,1,1,1,0,    z!!12!!2,z!!7!!0,z!!7!!1,z!!5!!0,z!!5!!1,z!!1!!0,z!!1!!1],
            [1,0,0,1,0,    z!!12!!3,z!!7!!2,z!!7!!3,z!!5!!2,z!!5!!3,z!!1!!2,z!!1!!3],
            [1,0,0,1,0,-1, z!!6!!0,z!!6!!1,z!!6!!2,z!!6!!3,z!!0!!0,z!!0!!1],
            [1,1,1,1,0,-1,-1,-1,-1,-1,z!!0!!2,z!!0!!3] ]
    _ -> []


--- DOT, GraphViz API

dot = putStrLn . g2dot

desenha_grafo = showDot . g2dot
    where f((a,b),c) = show a ++ " -> " ++ show b ++ " [ label=\"" ++ show c ++ "\" ];"
          cincat s = concat . intersperse s
          showDot d = do { writeFile "_.dot" d ;
                           system "dot -Tsvg _.dot -o _.svg";
                           (return . (IHaskell.Display.html) . htm . img) "_.svg"
                         }

g2dot g = "digraph G {\n" ++ cincat "\n" (map f g) ++ "\n}\n"
   where f((a,b),c) = (show.k.show) a ++ " -> " ++ (show.k.show) b ++ " [ label=\"" ++ (k.show) c ++ "\" ];"
         cincat s = concat . intersperse s
         k = map cl

--- remover acentos, se necessário
cl '\237'= 'i'
cl '\243'= 'o'
cl '\227'= 'a'
cl '\231'= 'c'
cl c     = c

--- legacy ---

desenha = putStrLn . ps where
    ps = intercalate "\n" . map p
    p = (>>= f)
    w = "  "
    b = "\9632\9632"
    f 0 = w
    f 1 = b

-------------------- Basic functionality ------------------------

--- HTML minimal API

tag t l x = "<"++t++" "++ps++">"++x++"</"++t++">"
             where ps = unwords [concat [t,"=",v]| (t,v)<-l]

htm = tag "html" []

tr  = tag "tr" []

img fn = tag "img" [ "src" |-> (show fn) ] ""

nest t p f = (tag t p) . (>>=f)

table = nest "table" [ "border" |-> "1" , "data-toggle" |-> "\"table\"" ]
              (nest "tr" [] (tag "td" [ "align" |-> "center"]))

---- Haskell -> Lists -> Matrices ------------

mat g x = IHaskell.Display.html (htm (g2htm g x x))

g2htm g x y = table t' where
  t' = zipWith (:) o (i:t)
  h  = zipWith (:) o (i:t)
  t  = map (map show) $ g2lists g x y
  i  = map (blue.show) x
  o = "" : map (blue.show) y
  blue = tag "span" [ "style" |-> "color:blue;font-weight:bold" ]

g2lists g x y = chunksOf (length x) . map (aux g. swap) $ (x .><. y)
     where aux g = (maybe 0 id) . (flip lookup g)

--- rnet = 'recurrent net' usada na adição em binário

rnet :: ((c, (a, b)) -> (c, c), [a], [b], c) -> [c]
rnet(f,a,b,c)  = (cons . ripple) (zip a b) 
    where ripple = mapAccumR (curry f) c
          cons(a,x) = a : x
---
divmod = uncurry divMod
---
