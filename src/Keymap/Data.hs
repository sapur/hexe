module Keymap.Data (
    Keymaps,
    Keymap, mkKeymap,
    KeymapName (..),
    emptyKeymap,
    lookupKeymap,
    showKeymapName,
) where

import Data.Maybe

import           Data.Map (Map)
import qualified Data.Map as M

import Graphics.Vty hiding (Style)

import Command.Data


type Keymaps = Map KeymapName Keymap
type Keymap  = Map Event Command

data KeymapName
    = HexNavKeys
    | LineNavKeys
    | HexOverKeys
    | CharOverKeys
    | HexInsKeys
    | CharInsKeys
    deriving (Eq, Ord, Show)


mkKeymap binds = M.fromList binds :: Keymap

emptyKeymap = M.fromList $ map (\key -> (EvKey key [], Quit True))
                               [KChar 'q', KEsc]

lookupKeymap name kms = fromMaybe emptyKeymap $ M.lookup name kms


showKeymapName name = case name of
    HexNavKeys   -> "Hex Navigation"
    LineNavKeys  -> "Line Navigation"
    HexOverKeys  -> "Hex Overwrite"
    CharOverKeys -> "Char Overwrite"
    HexInsKeys   -> "Hex Insert"
    CharInsKeys  -> "Char Insert"
