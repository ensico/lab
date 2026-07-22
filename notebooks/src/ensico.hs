module ENSICO where -- (c) Ensico, 22-Jul-26 (!!!!!!!!)

import Data.Char
import Data.List

--- Binary <-> decimal

ascii = dec2byte . ord

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

--- rnet = 'recurrent net' usada na adição em binário

rnet :: ((c, (a,b)) -> (c, c), [a], [b], c) -> [c]
rnet(f,a,b,c)  = (cons . ripple) (zip a b) 
    where ripple = mapAccumR (curry f) c
          cons(a,x) = a : x

---

divmod = uncurry divMod

--- divide & conquer

class Divisible a where
  divide :: a -> (a,a)
  prt1 :: a -> a
  prt2 :: a -> a
  prt1 = fst . divide
  prt2 = snd . divide

instance Divisible [a] where
   divide x = splitAt m x where m = length x `div` 2

--- merge

merge (x,[]) = x
merge ([],y) = y
merge (a:x,b:y) = if (a <= b) then a:(merge (x, (b:y))) else b:(merge ((a:x), y))

a <++> b = merge(a,b)  -- curried
---

