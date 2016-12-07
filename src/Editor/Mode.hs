module Editor.Mode where

import Data.Char


data InputMode
    = HexOverwrite
    | HexOverwriting
    | CharOverwrite
    | HexInsert
    | HexInserting
    | CharInsert
    | OffsetInput
    | MarkInput
    | ScriptInput
    deriving (Show)


validChar mode = case mode of
    HexOverwrite   -> isHexDigit
    HexOverwriting -> isHexDigit
    CharOverwrite  -> isPrint
    HexInsert      -> isHexDigit
    HexInserting   -> isHexDigit
    CharInsert     -> isPrint
    OffsetInput    -> isPrint
    MarkInput      -> isPrint
    ScriptInput    -> isPrint

autoAccept mode txt = case mode of
    HexOverwrite   -> length txt >= 2
    HexOverwriting -> length txt >= 2
    CharOverwrite  -> True
    HexInsert      -> length txt >= 2
    HexInserting   -> length txt >= 2
    CharInsert     -> True
    _              -> False

isInLine mode = case mode of
    OffsetInput    -> False
    MarkInput      -> False
    ScriptInput    -> False
    _              -> True

isEditing mode = case mode of
    HexOverwriting -> True
    HexInserting   -> True
    _              -> not (isInLine mode)

nextMode mode = case mode of
    HexOverwrite   -> Just HexOverwrite
    HexOverwriting -> Just HexOverwrite
    CharOverwrite  -> Just CharOverwrite
    HexInsert      -> Just HexInsert
    HexInserting   -> Just HexInsert
    CharInsert     -> Just CharInsert
    _              -> Nothing

showMode mode = case mode of
    HexOverwrite   -> "Hex Overwrite"
    HexOverwriting -> "Hex Overwriting"
    CharOverwrite  -> "Char Overwrite"
    HexInsert      -> "Hex Insert"
    HexInserting   -> "Hex Inserting"
    CharInsert     -> "Char Insert"
    OffsetInput    -> "Offset Input"
    MarkInput      -> "Mark Input"
    ScriptInput    -> "Script Input"
