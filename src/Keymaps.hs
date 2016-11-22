module Keymaps where

import Data.Char
import Text.Read

import Graphics.Vty hiding (update)

import Editor
import Commands


navigation = keymap "Nav" brightBlack
           $ \key -> case key of

    EvKey KEsc        []      -> safeQuit

    EvKey KLeft       []      -> cursorRel (-1)  0
    EvKey KRight      []      -> cursorRel   1   0
    EvKey KUp         []      -> cursorRel   0 (-1)
    EvKey KDown       []      -> cursorRel   0   1
    EvKey KHome       []      -> cursorLine 0
    EvKey KEnd        []      -> cursorLine 1
    EvKey KHome       [MCtrl] -> cursorBuf 0
    EvKey KEnd        [MCtrl] -> cursorBuf 1
    EvKey KPageUp     []      -> scroll (-1.0) 0
    EvKey KPageDown   []      -> scroll   1.0  0
    EvKey KUp         [MCtrl] -> scroll   0.0 (-1)
    EvKey KDown       [MCtrl] -> scroll   0.0   1

    EvKey KDel        []      -> deleteByte
    EvKey (KChar 's') [MCtrl] -> storeFile

    EvKey (KChar 'q') []      -> safeQuit

    EvKey (KChar '^') []      -> cursorLine 0
    EvKey (KChar '$') []      -> cursorLine 1

    EvKey (KChar 'H') []      -> cursorPage 0.1 0.5
    EvKey (KChar 'M') []      -> cursorPage 0.5 0.5
    EvKey (KChar 'L') []      -> cursorPage 0.9 0.5

    EvKey (KChar 'h') []      -> cursorRel (-1)  0
    EvKey (KChar 'l') []      -> cursorRel   1   0
    EvKey (KChar 'k') []      -> cursorRel   0 (-1)
    EvKey (KChar 'j') []      -> cursorRel   0   1
    EvKey (KChar 'g') []      -> cursorBuf 0
    EvKey (KChar 'G') []      -> cursorBuf 1

    EvKey (KChar '<') []      -> setColumnMul (-1)
    EvKey (KChar '>') []      -> setColumnMul   1

    EvKey (KChar ' ') []      -> toggleMark
    EvKey (KChar 'm') []      -> toggleMark
    EvKey KLeft       [MCtrl] -> findMark True
    EvKey KRight      [MCtrl] -> findMark False
    EvKey (KChar 'N') []      -> findMark True
    EvKey (KChar 'n') []      -> findMark False

    EvKey (KChar 'u') []      -> undo
    EvKey (KChar 'r') [MCtrl] -> redo

    EvKey (KChar 'x') [] -> deleteByte
    EvKey (KChar 'w') [] -> storeFile

    EvKey (KChar 't') [] -> setStyle color16
    EvKey (KChar 'T') [] -> setStyle color256

    EvKey (KChar 'g') [MCtrl] -> offsetInput navigation

    EvKey (KChar '\t') [] -> setKeymap charOverwrite
    EvKey KIns         [] -> setKeymap hexInsert

    EvKey (KChar c) [] | isHexDigit c -> do
        hexInput (Just key) navigation overwriteHex

    EvKey (KChar ':') [] -> showWarn "Not yet implemented."

    _ -> unhandledKey key


charOverwrite = keymap "Char" brightYellow
              $ \key -> case key of

    EvKey KIns         [] -> setKeymap charInsert
    EvKey (KChar '\t') [] -> setKeymap navigation
    EvKey KEsc         [] -> setKeymap navigation

    EvKey (KChar c) [] | isPrint c -> overwriteCharKey c

    key -> kmHandler navigation key

hexInsert = keymap "Ins" brightRed
          $ \key -> case key of

    EvKey KIns         [] -> setKeymap navigation
    EvKey (KChar '\t') [] -> setKeymap charInsert
    EvKey KEsc         [] -> setKeymap navigation

    EvKey (KChar c) [] | isHexDigit c -> do
        prepareInsert 0
        hexInput (Just key) hexInsert overwriteHex

    key -> kmHandler navigation key

charInsert = keymap "Char Ins" brightRed
           $ \key -> case key of

    EvKey KIns         [] -> setKeymap charOverwrite
    EvKey (KChar '\t') [] -> setKeymap hexInsert
    EvKey KEsc         [] -> setKeymap navigation

    EvKey (KChar c) [] | isPrint c -> do
        prepareInsert 0
        overwriteCharKey c

    key -> kmHandler navigation key

hexInput mbKey nextKm action
    = beginInput isHexDigit ((== 2) . length) True mbKey (input "Hex")
                 nextKm action

offsetInput nextKm
    = beginInput (\ch -> isHexDigit ch || ch `elem` "xX")  (const False)
                 False Nothing (input "Offset")
                 nextKm (maybe (return ()) cursorAbs . readMaybe)

input name = keymap (name++" Input") brightYellow
           $ \key -> do

    mbInp <- getsEditor edPending
    case mbInp of
        Nothing  -> forwardKeymap navigation key  -- FIXME
        Just inp -> inputKey inp key

inputKey inp key = case key of

    EvKey KEsc [] -> do
        cancelInput
        setKeymap (inpNextKm inp)

    EvKey KEnter [] -> commitInput

    EvKey KLeft  [] -> moveInput (-1)
    EvKey KRight [] -> moveInput 1
    EvKey KBS    [] -> deleteInput (-1)
    EvKey KDel   [] -> deleteInput 1
    EvKey KHome  [] -> moveInput minBound
    EvKey KEnd   [] -> moveInput maxBound

    EvKey (KChar c) [] | inpCheck inp c -> do
        addInput c
        pending <- getsEditor edPending
        case pending of
            Just inp | inpAccept inp (inpText inp)
                -> commitInput
            _   -> return ()

    _ -> showWarn "invalid key, abort with ESC"
