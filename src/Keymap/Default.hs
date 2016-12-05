module Keymap.Default (
    defaultKeymaps
) where

import qualified Data.Map as M

import Graphics.Vty hiding (update)

import Command.Execute
import Keymap.Data


defaultKeymaps = M.fromList
    [ (HexNavKeys  , hexNav)
    , (LineNavKeys , lineNav)
    , (HexOverKeys , hexOverwrite)
    , (CharOverKeys, charOverwrite)
    , (HexInsKeys  , hexInsert)
    , (CharInsKeys , charInsert)
    ]

hexNav = mkKeymap
    [ (EvKey KLeft        []     , SetCursor $ Char $ Rel (-1))
    , (EvKey KRight       []     , SetCursor $ Char $ Rel 1)
    , (EvKey KUp          []     , SetCursor $ Line $ Rel (-1))
    , (EvKey KDown        []     , SetCursor $ Line $ Rel 1)
    , (EvKey (KChar 'h')  []     , SetCursor $ Char $ Rel (-1))
    , (EvKey (KChar 'l')  []     , SetCursor $ Char $ Rel 1)
    , (EvKey (KChar 'k')  []     , SetCursor $ Line $ Rel (-1))
    , (EvKey (KChar 'j')  []     , SetCursor $ Line $ Rel 1)

    , (EvKey KHome        []     , SetCursor $ Line $ Frac 0)
    , (EvKey KEnd         []     , SetCursor $ Line $ Frac 1)
    , (EvKey (KChar '^')  []     , SetCursor $ Line $ Frac 0)
    , (EvKey (KChar '$')  []     , SetCursor $ Line $ Frac 1)

    , (EvKey (KChar 'H')  []     , SetCursor $ Page $ Frac 0.1)
    , (EvKey (KChar 'M')  []     , SetCursor $ Page $ Frac 0.5)
    , (EvKey (KChar 'L')  []     , SetCursor $ Page $ Frac 0.9)

    , (EvKey KHome        [MCtrl], SetCursor $ File $ Frac 0)
    , (EvKey KEnd         [MCtrl], SetCursor $ File $ Frac 1)
    , (EvKey (KChar 'g')  []     , SetCursor $ File $ Frac 0)
    , (EvKey (KChar 'G')  []     , SetCursor $ File $ Frac 1)

    , (EvKey KPageUp      []     , SetScroll $ Page $ Rel (-1))
    , (EvKey KPageDown    []     , SetScroll $ Page $ Rel 1)
    , (EvKey KUp          [MCtrl], SetScroll $ Line $ Rel (-1))
    , (EvKey KDown        [MCtrl], SetScroll $ Line $ Rel 1)

    , (EvKey (KChar 'g')  [MCtrl], SetMode OffsetInput)

    , (EvKey (KChar '<')  []     , SetColumnWdt $ Rel (-1))
    , (EvKey (KChar '>')  []     , SetColumnWdt $ Rel 1)

    , (EvKey (KChar 't')  []     , Set256Colors Off)
    , (EvKey (KChar 'T')  []     , Set256Colors On)

    , (EvKey (KChar ' ')  []     , SetMark Toggle)
    , (EvKey (KChar 'm')  []     , SetMark Toggle)
    , (EvKey (KChar 'M')  []     , SetMode MarkInput)
    , (EvKey (KChar 'N')  []     , JumpMark Bw)
    , (EvKey (KChar 'n')  []     , JumpMark Fw)
    , (EvKey KLeft        [MCtrl], JumpMark Bw)
    , (EvKey KRight       [MCtrl], JumpMark Fw)

    , (EvKey (KChar 'u')  []     , JumpHistory Bw)
    , (EvKey (KChar 'z')  [MCtrl], JumpHistory Bw)
    , (EvKey (KChar 'r')  [MCtrl], JumpHistory Fw)

    , (EvKey KDel         []     , Delete Fw)
    , (EvKey (KChar 'x')  []     , Delete Fw)

    , (EvKey (KChar 'w')  []     , Store)
    , (EvKey (KChar 's')  [MCtrl], Store)

    , (EvKey (KChar 'q')  []     , Quit False)
    , (EvKey (KChar 'q')  [MCtrl], Quit True)  -- ignored, just for doc
    ]

lineNav = mkKeymap
    [ (EvKey KEsc   [], CancelInput)
    , (EvKey KEnter [], CommitInput)

    , (EvKey KLeft  [], SetCursor $ Char $ Rel (-1))
    , (EvKey KRight [], SetCursor $ Char $ Rel 1)
    , (EvKey KBS    [], Delete Bw)
    , (EvKey KDel   [], Delete Fw)
    , (EvKey KHome  [], SetCursor $ Char $ Frac 0)
    , (EvKey KEnd   [], SetCursor $ Char $ Frac 1)
    ]

hexOverwrite = mkKeymap
    [ (EvKey (KChar '\t') []     , SetMode CharOverwrite)
    , (EvKey KIns         []     , SetMode HexInsert)
    ]

charOverwrite = mkKeymap
    [ (EvKey KIns         [], SetMode CharInsert)
    , (EvKey (KChar '\t') [], SetMode HexOverwrite)
    , (EvKey KEsc         [], SetMode HexOverwrite)
    ]

hexInsert = mkKeymap
    [ (EvKey KIns         [], SetMode HexOverwrite)
    , (EvKey (KChar '\t') [], SetMode CharInsert)
    , (EvKey KEsc         [], SetMode HexOverwrite)
    ]

charInsert = mkKeymap
    [ (EvKey KIns         [], SetMode CharOverwrite)
    , (EvKey (KChar '\t') [], SetMode HexInsert)
    , (EvKey KEsc         [], SetMode HexOverwrite)
    ]
