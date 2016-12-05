module Keymap (
    lookupCommand,
) where

import Data.Char

import qualified Data.Map as M

import Graphics.Vty

import Command.Execute
import Control.General
import Control.LineEditor
import Keymap.Data


lookupCommand ev km = case M.lookup ev (kmHandler km) of
    Nothing  -> case kmForward km of
        Nothing  -> case ev of
            EvKey (KChar ch) [] | isPrint ch -> feedInput ch
            _                                -> unhandledKey ev
        Just km' -> lookupCommand ev km'
    Just cmd -> execute cmd
