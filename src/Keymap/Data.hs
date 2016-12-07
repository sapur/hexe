module Keymap.Data (
    module Keymap.Data.Name,
    Keymaps,
    Keymap, mkKeymap,
    KeymapName (..),
    emptyKeymap,
    lookupKeymap,
    rebindKey,
) where

import Data.Maybe

import           Data.Map (Map)
import qualified Data.Map as M

import Graphics.Vty hiding (Style)

import Command.Data
import Keymap.Data.Name


type Keymaps = Map KeymapName Keymap
type Keymap  = Map Event Script


mkKeymap binds = M.fromList binds :: Keymap

emptyKeymap = M.fromList $ map (\key -> (EvKey key [], [Quit True]))
                               [KChar 'q', KEsc]

lookupKeymap name kms = fromMaybe emptyKeymap $ M.lookup name kms

rebindKey kmn ev script = M.adjust (M.insert ev script) kmn
