module Render where

import Text.Printf

import Graphics.Vty hiding (update, Style)


showKey key mods = repr  where
    repr     = if   null mods
               then printf "%s"    strKey :: String
               else printf "%s-%s" (map strMod mods) strKey
    strKey   = case key of
        KChar c -> strChar c
        _       -> tail $ show key
    strChar c = case c of
        '\t' -> "Tab"
        ' '  -> "Space"
        _    -> let s = show c
                in  take (length s - 2) $ drop 1 s
    strMod m = case m of
        MShift -> 'S'
        MCtrl  -> 'C'
        MMeta  -> 'M'
        MAlt   -> 'A'
