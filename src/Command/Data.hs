module Command.Data (
    Command (..),
    PValue (..),
    PUnit (..),
    Switch (..),
    Direction (..),
    InputMode (..)
) where

import Input.Mode


data Command
    = Quit Bool
    | SetMode String InputMode
    | Store
    | JumpHistory Direction
    | SetCursor (PUnit PValue)
    | SetScroll (PUnit PValue)
    | SetColumnMul PValue
    | Set256Colors Switch
    | SetMark Switch
    | JumpMark Direction
    | Delete Direction
    | CommitInput
    | CancelInput
    | Feed Char
    deriving (Show)

data PValue
    = Abs  Word
    | Rel  Int
    | Frac Float
    deriving (Show)

data PUnit a
    = Char a
    | Word a
    | Line a
    | Page a
    | File a
    deriving (Show)

data Switch
    = On
    | Off
    | Toggle
    deriving (Show)

data Direction
    = Bw
    | Fw
    deriving (Show)
