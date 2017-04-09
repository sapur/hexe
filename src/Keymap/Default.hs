module Keymap.Default (
    defaultKeymaps
) where

import qualified Data.Map as M

import Graphics.Vty hiding (update, char)

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
    [ key   KLeft     ~> [SetCursor $ Char $ Rel (-1)]
    , key   KRight    ~> [SetCursor $ Char $ Rel 1]
    , key   KUp       ~> [SetCursor $ Line $ Rel (-1)]
    , key   KDown     ~> [SetCursor $ Line $ Rel 1]
    , char  'h'       ~> [SetCursor $ Char $ Rel (-1)]
    , char  'l'       ~> [SetCursor $ Char $ Rel 1]
    , char  'k'       ~> [SetCursor $ Line $ Rel (-1)]
    , char  'j'       ~> [SetCursor $ Line $ Rel 1]

    , key   KHome     ~> [SetCursor $ InLine $ Frac 0]
    , key   KEnd      ~> [SetCursor $ InLine $ Frac 1]
    , char  '^'       ~> [SetCursor $ InLine $ Frac 0]
    , char  '$'       ~> [SetCursor $ InLine $ Frac 1]

    , char  'H'       ~> [SetCursor $ InPage $ Frac 0.1,
                          SetCursor $ InLine $ Frac 0.5]
    , char  'M'       ~> [SetCursor $ InPage $ Frac 0.5,
                          SetCursor $ InLine $ Frac 0.5]
    , char  'L'       ~> [SetCursor $ InPage $ Frac 0.9,
                          SetCursor $ InLine $ Frac 0.5]

    , keyC  KHome     ~> [SetCursor $ Char $ Frac 0]
    , keyC  KEnd      ~> [SetCursor $ Char $ Frac 1]
    , char  'g'       ~> [SetCursor $ Char $ Frac 0]
    , char  'G'       ~> [SetCursor $ Char $ Frac 1]

    , key   KPageUp   ~> [SetScroll $ Page $ Rel (-1)]
    , key   KPageDown ~> [SetScroll $ Page $ Rel 1]
    , keyC  KPageUp   ~> [SetScroll $ Page $ Rel (-10)]
    , keyC  KPageDown ~> [SetScroll $ Page $ Rel 10]
    , keyC  KUp       ~> [SetScroll $ Line $ Rel (-1)]
    , keyC  KDown     ~> [SetScroll $ Line $ Rel 1]

    , charC 'g'       ~> [SetMode OffsetInput]

    , char  '<'       ~> [SetColumnWdt $ Rel (-1)]
    , char  '>'       ~> [SetColumnWdt $ Rel 1]

    , char  't'       ~> [Set256Colors Off]
    , char  'T'       ~> [Set256Colors On]

    , char  ' '       ~> [SetMark Toggle]
    , char  'm'       ~> [SetMark Toggle]
    , charA 'm'       ~> [SetMode MarkInput]
    , charC '@'       ~> [SetMode MarkInput]
    , char  'N'       ~> [JumpMark Bw]
    , char  'n'       ~> [JumpMark Fw]
    , keyC  KLeft     ~> [JumpMark Bw]
    , keyC  KRight    ~> [JumpMark Fw]

    , char  'u'       ~> [JumpHistory Bw]
    , charC 'z'       ~> [JumpHistory Bw]
    , charC 'r'       ~> [JumpHistory Fw]

    , key   KDel      ~> [Delete Fw]
    , char  'x'       ~> [Delete Fw]
    , char  'S'       ~> [Slice Bw]
    , char  's'       ~> [Slice Fw]
    , charA 's'       ~> [Splice Bw]
    , charA 'S'       ~> [Splice Fw]

    , char  'w'       ~> [Store]
    , charC 's'       ~> [Store]

    , charC 'x'       ~> [SetMode ScriptInput]
    , char  ':'       ~> [SetMode ScriptInput]

    , char  'q'       ~> [Quit False]
    , charC 'q'       ~> [Quit True]  -- ignored, just for doc
    , charC 'l'       ~> [Refresh]    -- ignored, just for doc

    , key KEnter ~> [Bind HexNavKeys (key KEnter) [SetMark Toggle]]
    ]

lineNav = mkKeymap
    [ key KEsc   ~> [CancelInput]
    , key KEnter ~> [CommitInput]

    , key KLeft  ~> [SetCursor $ Char $ Rel (-1)]
    , key KRight ~> [SetCursor $ Char $ Rel 1]
    , key KBS    ~> [Delete Bw]
    , key KDel   ~> [Delete Fw]
    , key KHome  ~> [SetCursor $ Char $ Frac 0]
    , key KEnd   ~> [SetCursor $ Char $ Frac 1]
    ]

hexOverwrite = mkKeymap
    [ char '\t' ~> [SetMode CharOverwrite]
    , key  KIns ~> [SetMode HexInsert]
    ]

charOverwrite = mkKeymap
    [ key  KIns ~> [SetMode CharInsert]
    , char '\t' ~> [SetMode HexOverwrite]
    , key  KEsc ~> [SetMode HexOverwrite]
    ]

hexInsert = mkKeymap
    [ key  KIns ~> [SetMode HexOverwrite]
    , char '\t' ~> [SetMode CharInsert]
    , key  KEsc ~> [SetMode HexOverwrite]
    ]

charInsert = mkKeymap
    [ key  KIns ~> [SetMode CharOverwrite]
    , char '\t' ~> [SetMode HexInsert]
    , key  KEsc ~> [SetMode HexOverwrite]
    ]


key ~> script = (key, script)

char  c = EvKey (KChar c) []
charC c = EvKey (KChar c) [MCtrl]
charA c = EvKey (KChar c) [MMeta]
key   k = EvKey k         []
keyC  k = EvKey k         [MCtrl]
keyA  k = EvKey k         [MMeta]
