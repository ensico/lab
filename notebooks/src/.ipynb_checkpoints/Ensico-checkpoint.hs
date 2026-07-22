module Ensico where

import Data.List

desenha = putStrLn . ps

ps = intercalate "\n" . map p where
    p = (>>= f)
    w = "  "
    b = "\9632\9632"
    f 0 = w
    f 1 = b

