
{-# OPTIONS_GHC -XNPlusKPatterns #-}

-- (c) Ensico (2023)

module Vnc where

import Data.Ratio
import Data.Char
import Data.List
import Cp
import Svg

a .$ b = merge (a,b)

merge (l,[])                  = l
merge ([],r)                  = r
merge (x:xs,y:ys) | x < y     = x : merge(xs,y:ys) 
                  | otherwise = y : merge(x:xs,ys)

-- Data

nave_rgb =
       [["0","0","4","0","0","0","0","0","4","0","0"],
        ["0","0","0","4","0","0","0","4","0","0","0"],
        ["0","0","2","2","2","1","2","2","2","0","0"],
        ["0","1","2","0","2","1","2","0","2","1","0"],
        ["1","1","2","2","2","1","2","2","2","1","1"],
        ["1","0","1","1","1","1","1","1","1","0","1"],
        ["3","0","3","0","0","0","0","0","3","0","3"],
        ["0","0","0","3","3","0","3","3","0","0","0"]]

nave = concat
  [ let (y,x) = (fromIntegral j*d, fromIntegral i*d) in quad x y c t
         | j <- [0..length nave_rgb - 1],
           i <- [0..length (nave_rgb!!0) - 1],
           c <- [reverse nave_rgb!!j!!i],
           c /= "0"
  ] where
   t = 0
   quad i j c t = translate (i+t,j+t) . col c $ sqr d
   col "0" = white
   col "1" = black
   col "2" = red
   col "3" = green
   col "4" = blue
   d = 0.4

