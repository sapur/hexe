module Keymap.Data.Name where


data KeymapName
    = HexNavKeys
    | LineNavKeys
    | HexOverKeys
    | CharOverKeys
    | HexInsKeys
    | CharInsKeys
    deriving (Eq, Ord, Show)


showKeymapName name = case name of
    HexNavKeys   -> "Hex Navigation"
    LineNavKeys  -> "Line Navigation"
    HexOverKeys  -> "Hex Overwrite"
    CharOverKeys -> "Char Overwrite"
    HexInsKeys   -> "Hex Insert"
    CharInsKeys  -> "Char Insert"
