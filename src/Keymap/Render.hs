module Keymap.Render (
    renderKeymapByMode,
    renderKeymapByCategory,
    keymapTable,
    showEvent, showKey,
) where

import Control.Arrow
import Data.Function
import Data.List
import Data.Ord
import Text.Printf

import qualified Data.Map as M

import Graphics.Vty hiding (update, Style)

import Command.Render
import Keymap.Data
import Render


renderKeymapByMode kms = renderTable table  where
    table = map (second $ map (second ((:[]) . snd)))
          $ keymapTable kms

renderKeymapByCategory kms = renderTable table  where
    table = mangleTable $ keymapTable kms

    mangleTable
        = map (second mangleSection)
        . groupOnFst
        . map (\(key, (cat, cmd)) -> (cat, (key, cmd)))
        . concatMap snd
    mangleSection
        = map (second undup)
        . groupOnFst
    groupOnFst
        = map (first head . unzip)
        . groupBy ((==) `on` fst)
        . sortBy (comparing fst)
    undup
        = map head
        . group
        . sort


renderTable :: [(String, [(String, [String])])] -> String
renderTable table = sections  where
    sections = concatMap renderSection table
    renderSection (name, bindings)
        = unlines $ name : (renderBindings bindings ++ [""])
    renderBindings = map renderBinding . sortBy (comparing snd)
    renderBinding (key, cmd0:cmds)
        = intercalate "\n" $      printf "  %-12s %s" key cmd0
                           : map (printf "  %-12s %s" "") cmds


keymapTable :: Keymaps -> [(String, [(String, (String, String))])]
keymapTable kms = map (first showKeymapName >>> second showKm) $ M.toList kms
  where
    showKm km = map (first showEvent >>> second renderScript)
              $ M.toList km

showEvent ev = case ev of
    EvKey key mods -> showKey key mods
    _              -> show ev
