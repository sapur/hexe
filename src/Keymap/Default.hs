module Keymap.Default where

import qualified Data.Map as M

import Graphics.Vty hiding (update)

import Command.Execute
import Keymap.Data


defaultKeymaps = M.fromList
    [ ("Char Nav"      , charNav)
    , ("Hex Nav"       , hexNav)
    , ("Line Nav"      , lineNav)
    , ("Hex Overwrite" , hexOverwrite)
    , ("Char Overwrite", charOverwrite)
    , ("Hex Insert"    , hexInsert)
    , ("Char Insert"   , charInsert)
    , ("Offset Input"  , offsetInput)
    ]

charNav = mkKeymap "Char Nav" brightBlack Nothing

    [ (EvKey KEsc         []     , Quit False)

    , (EvKey KLeft        []     , SetCursor $ Char $ Rel (-1))
    , (EvKey KRight       []     , SetCursor $ Char $ Rel 1)
    , (EvKey KUp          []     , SetCursor $ Line $ Rel (-1))
    , (EvKey KDown        []     , SetCursor $ Line $ Rel 1)
    , (EvKey KHome        []     , SetCursor $ Line $ Frac 0)
    , (EvKey KEnd         []     , SetCursor $ Line $ Frac 1)
    , (EvKey KHome        [MCtrl], SetCursor $ File $ Frac 0)
    , (EvKey KEnd         [MCtrl], SetCursor $ File $ Frac 1)
    , (EvKey KPageUp      []     , SetScroll $ Page $ Rel (-1))
    , (EvKey KPageDown    []     , SetScroll $ Page $ Rel 1)
    , (EvKey KUp          [MCtrl], SetScroll $ Line $ Rel (-1))
    , (EvKey KDown        [MCtrl], SetScroll $ Line $ Rel 1)

    , (EvKey KDel         []     , Delete Fw)
    , (EvKey (KChar 's')  [MCtrl], Store)

    , (EvKey KLeft        [MCtrl], JumpMark Bw)
    , (EvKey KRight       [MCtrl], JumpMark Fw)

    , (EvKey (KChar 'r')  [MCtrl], JumpHistory Fw)

    , (EvKey (KChar 'g')  [MCtrl], SetMode "Offset Input" OffsetInput)

    , (EvKey (KChar '\t') []     , SetMode "Char Overwrite" CharOverwrite)
    , (EvKey KIns         []     , SetMode "Hex Insert"     HexInsert)
    ]

    --EvKey (KChar c) [] | isHexDigit c -> do
    --    hexInput (Just key) navigation overwriteHex

    --EvKey (KChar ':') [] -> showWarn "Not yet implemented."

    --_ -> unhandledKey key

hexNav = mkKeymap "Char Nav" brightBlack (Just charNav)

    [ (EvKey (KChar 'q')  []     , Quit False)

    , (EvKey (KChar '^')  []     , SetCursor $ Line $ Frac 0)
    , (EvKey (KChar '$')  []     , SetCursor $ Line $ Frac 1)

    , (EvKey (KChar 'H')  []     , SetCursor $ Page $ Frac 0.1)
    , (EvKey (KChar 'M')  []     , SetCursor $ Page $ Frac 0.5)
    , (EvKey (KChar 'L')  []     , SetCursor $ Page $ Frac 0.9)

    , (EvKey (KChar 'h')  []     , SetCursor $ Char $ Rel (-1))
    , (EvKey (KChar 'l')  []     , SetCursor $ Char $ Rel 1)
    , (EvKey (KChar 'k')  []     , SetCursor $ Line $ Rel (-1))
    , (EvKey (KChar 'j')  []     , SetCursor $ Line $ Rel 1)
    , (EvKey (KChar 'g')  []     , SetCursor $ File $ Frac 0)
    , (EvKey (KChar 'G')  []     , SetCursor $ File $ Frac 1)

    , (EvKey (KChar '<')  []     , SetColumnMul $ Rel (-1))
    , (EvKey (KChar '>')  []     , SetColumnMul $ Rel 1)

    , (EvKey (KChar ' ')  []     , SetMark Toggle)
    , (EvKey (KChar 'm')  []     , SetMark Toggle)
    , (EvKey (KChar 'N')  []     , JumpMark Bw)
    , (EvKey (KChar 'n')  []     , JumpMark Fw)

    , (EvKey (KChar 'u')  []     , JumpHistory Bw)

    , (EvKey (KChar 'x')  []     , Delete Fw)
    , (EvKey (KChar 'w')  []     , Store)

    , (EvKey (KChar 't')  []     , Set256Colors Off)
    , (EvKey (KChar 'T')  []     , Set256Colors On)
    ]

lineNav = mkKeymap "Line Nav" brightBlack Nothing

    [ (EvKey KEsc   [], CancelInput)
    , (EvKey KEnter [], CommitInput)

    , (EvKey KLeft  [], SetCursor $ Char $ Rel (-1))
    , (EvKey KRight [], SetCursor $ Char $ Rel 1)
    , (EvKey KBS    [], Delete Bw)
    , (EvKey KDel   [], Delete Fw)
    , (EvKey KHome  [], SetCursor $ Char $ Frac 0)
    , (EvKey KEnd   [], SetCursor $ Char $ Frac 1)
    ]

hexOverwrite = mkKeymap "Nav" brightBlack (Just hexNav) []

charOverwrite = mkKeymap "Char Overwrite" brightYellow (Just charNav)
    [ (EvKey KIns         [], SetMode "Char Insert"   CharInsert)
    , (EvKey (KChar '\t') [], SetMode "Hex Overwrite" HexOverwrite)
    , (EvKey KEsc         [], SetMode "Hex Overwrite" HexOverwrite)
    ]

hexInsert = mkKeymap "Hex Insert" brightRed (Just hexNav)
    [ (EvKey KIns         [], SetMode "Hex Overwrite" HexOverwrite)
    , (EvKey (KChar '\t') [], SetMode "Char Insert"   CharInsert)
    , (EvKey KEsc         [], SetMode "Hex Overwrite" HexOverwrite)
    ]

charInsert = mkKeymap "Char Insert" brightRed (Just charNav)
    [ (EvKey KIns         [], SetMode "Char Overwrite" CharOverwrite)
    , (EvKey (KChar '\t') [], SetMode "Hex Insert"     HexInsert)
    , (EvKey KEsc         [], SetMode "Hex Overwrite"  HexOverwrite)
    ]

offsetInput = mkKeymap "Offset Input" brightYellow (Just lineNav) []
