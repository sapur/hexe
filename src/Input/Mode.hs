module Input.Mode where

import Data.Char


data InputMode
    = HexOverwrite
    | CharOverwrite
    | HexInsert
    | CharInsert
    | OffsetInput
    deriving (Show)


validChar mode = case mode of
    HexOverwrite  -> isHexDigit
    CharOverwrite -> isPrint
    HexInsert     -> isHexDigit
    CharInsert    -> isPrint
    OffsetInput   -> \ch -> isHexDigit ch || ch `elem` "xXoO"

autoAccept mode txt = case mode of
    HexOverwrite  -> length txt >= 2
    CharOverwrite -> True
    HexInsert     -> length txt >= 2
    CharInsert    -> True
    OffsetInput   -> False

isInLine mode = case mode of
    HexOverwrite  -> True
    CharOverwrite -> True
    HexInsert     -> True
    CharInsert    -> True
    OffsetInput   -> False

isSticky mode = case mode of
    HexOverwrite  -> True
    CharOverwrite -> True
    HexInsert     -> True
    CharInsert    -> True
    _             -> False
