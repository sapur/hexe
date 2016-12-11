module Command.Data (
    Script,
    Command (..),
    PValue (..),
    PUnit (..),
    Switch (..),
    Direction (..),
    InputMode (..)
) where

import Graphics.Vty

import Editor.Mode
import Keymap.Data.Name


type Script = [Command]

data Command
    = Quit Bool
    | Refresh
    | SetMode InputMode
    | Store
    | JumpHistory Direction
    | SetCursor (PUnit PValue)
    | SetScroll (PUnit PValue)
    | SetColumnWdt PValue
    | Set256Colors Switch
    | SetMark Switch
    | SetNamedMark (PUnit PValue) String
    | JumpMark Direction
    | Delete Direction
    | CommitInput
    | CancelInput
    | Feed Char
    | Bind KeymapName Event Script
    deriving (Show)

data PValue
    = Abs  Word
    | Rel  Int
    | Frac Float
    deriving (Show)

data PUnit   a
    = Char   a
    | Line   a
    | Page   a
    | InLine a
    | InPage a
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
