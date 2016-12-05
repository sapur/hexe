module Keymap.Data (
    InputState (..), mkInputState,
    Keymap (..),
    Keymaps,
    emptyKeymap, mkKeymap,
    lookupKeymap,
) where

import Data.Maybe

import           Data.Map (Map)
import qualified Data.Map as M

import Graphics.Vty hiding (Style)

import Command.Data


data InputState = InputState
    { istKeymap :: Keymap
    , istMode   :: InputMode
    }

data Keymap = Keymap
    { kmName    :: Image
    , kmHandler :: Map Event Command
    , kmForward :: Maybe Keymap
    }

type Keymaps = Map String Keymap


mkInputState = InputState

emptyKeymap = Keymap
    { kmName    = string (currentAttr `withForeColor` brightRed) "PANIC"
    , kmHandler = M.fromList $ map (\key -> (EvKey key [], Quit True))
                                   [KChar 'q', KEsc]
    , kmForward = Nothing
    }

mkKeymap name color forward mapping = Keymap
    { kmName    = string (currentAttr `withForeColor` color) name
    , kmHandler = M.fromList mapping
    , kmForward = forward
    }

lookupKeymap name kms = fromMaybe emptyKeymap $ M.lookup name kms
