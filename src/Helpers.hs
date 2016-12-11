module Helpers where

import Text.Printf

import GHC.IO.Exception


int :: (Integral a, Num b) => a -> b
int = fromIntegral

clamp lbound ubound = max lbound . min ubound


formatIOEx ex = printf "%s: %s"
    (ioe_description ex)
    (maybe "<no file>" (printf "'%s'") $ ioe_filename ex)
