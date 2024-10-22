module ENSICO.Graphics where

import Data.Char
import Data.List
import System.Process
import Data.List.Split
--import IHaskell.Display

--------- Composição ----------

(|>) = flip ($)
(.>) = flip (.)

---------- Sistema Braille ------------------

letra [1,0,0,0,0,0] = 'A'
letra [1,0,1,0,0,0] = 'B'
letra [1,1,0,0,0,0] = 'C'
letra [1,1,0,1,0,0] = 'D'
letra [1,0,0,1,0,0] = 'E' 
letra [1,1,1,0,0,0] = 'F'
letra [1,1,1,1,0,0] = 'G'
letra [1,0,1,1,0,0] = 'H'
letra [0,1,1,0,0,0] = 'I'
letra [0,1,1,1,0,0] = 'J' 
letra [1,0,0,0,1,0] = 'K' 
letra [1,0,1,0,1,0] = 'L' 
letra [1,1,0,0,1,0] = 'M' 
letra [1,1,0,1,1,0] = 'N'
letra [1,0,0,1,1,0] = 'O'
letra [1,1,1,0,1,0] = 'P' 
letra [1,1,1,1,1,0] = 'Q' 
letra [1,0,1,1,1,0] = 'R' 
letra [0,1,1,0,1,0] = 'S' 
letra [0,1,1,1,1,0] = 'T' 
letra [1,0,0,0,1,1] = 'U' 
letra [1,0,1,0,1,1] = 'V' 
letra [0,1,1,1,0,1] = 'W' 
letra [1,1,0,0,1,1] = 'X' 
letra [1,1,0,1,1,1] = 'Y'
letra [1,0,0,1,1,1] = 'Z' 

codigo 'A' = [1,0,0,0,0,0]
codigo 'B' = [1,0,1,0,0,0]
codigo 'C' = [1,1,0,0,0,0]
codigo 'D' = [1,1,0,1,0,0]
codigo 'E' = [1,0,0,1,0,0] 
codigo 'F' = [1,1,1,0,0,0]
codigo 'G' = [1,1,1,1,0,0]
codigo 'H' = [1,0,1,1,0,0]
codigo 'I' = [0,1,1,0,0,0]
codigo 'J' = [0,1,1,1,0,0]
codigo 'K' = [1,0,0,0,1,0]
codigo 'L' = [1,0,1,0,1,0]
codigo 'M' = [1,1,0,0,1,0]
codigo 'N' = [1,1,0,1,1,0]
codigo 'O' = [1,0,0,1,1,0]
codigo 'P' = [1,1,1,0,1,0]
codigo 'Q' = [1,1,1,1,1,0]
codigo 'R' = [1,0,1,1,1,0] 
codigo 'S' = [0,1,1,0,1,0]
codigo 'T' = [0,1,1,1,1,0]
codigo 'U' = [1,0,0,0,1,1]
codigo 'V' = [1,0,1,0,1,1] 
codigo 'W' = [0,1,1,1,0,1]
codigo 'X' = [1,1,0,0,1,1]
codigo 'Y' = [1,1,0,1,1,1]
codigo 'Z' = [1,0,0,1,1,1]

b 0 = 0
b 1 = 1
t 0 = 1
t 1 = 0 

transforma_codigo [] [] = []
transforma_codigo (a:x) (f:y) = (f a) : (transforma_codigo x y) 

transforma_letra' a s = letra (transforma_codigo (codigo a) s)
transforma_letra = flip transforma_letra'

data Elfos = T | B deriving (Eq, Ord, Show)

elfo 0 0 = B
elfo 0 1 = T
elfo 1 0 = T
elfo 1 1 = B

elfos_codigo x y = map (uncurry elfo) (zip x y)

sequencia_elfos a b = elfos_codigo (codigo a) (codigo b)