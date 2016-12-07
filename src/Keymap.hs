module Keymap (
    lookupScript,
) where

import qualified Data.Map as M

import Graphics.Vty

import Command.Execute
import Control
import Editor.Mode
import Keymap.Data


lookupScript ev mode kms
    = lookupKey ev mode
    $ selectKeymaps mode kms

lookupKey ev _    []       = unhandledKey ev
lookupKey ev mode (km:kms) = case M.lookup ev (km :: Keymap) of
    Nothing  -> case ev of
        EvKey (KChar ch) [] | validChar mode ch -> feedInput executeScript ch
        _                                       -> lookupKey ev mode kms
    Just cmd -> executeScript cmd

selectKeymaps :: InputMode -> Keymaps -> [Keymap]
selectKeymaps mode kms
    = map (`lookupKeymap` kms)
    $ case mode of
        HexOverwrite   -> [HexOverKeys, HexNavKeys]
        HexOverwriting -> [LineNavKeys]
        CharOverwrite  -> [CharOverKeys, HexNavKeys]
        HexInsert      -> [HexInsKeys, HexNavKeys]
        HexInserting   -> [LineNavKeys]
        CharInsert     -> [CharInsKeys, HexNavKeys]
        OffsetInput    -> [LineNavKeys]
        MarkInput      -> [LineNavKeys]
        ScriptInput    -> [LineNavKeys]
