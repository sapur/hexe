module Command.Execute (
    module Command.Data,
    execute
) where

import Command.Data
import Control.General
import Control.HexEditor
import Control.LineEditor
import Editor.Types
import Input.Mode
import Keymap.Data


execute :: Command -> EditorT IO ()
execute cmd = case cmd of

    Quit force -> if force then quit else safeQuit

    SetMode kmName mode -> setInput kmName mode

    Store -> storeFile

    JumpHistory dir -> case dir of
        Bw -> undo
        Fw -> redo

    SetCursor pos -> caseMode handleInLine handleEx  where
        handleInLine = case pos of
            File (Frac p) -> cursorBuf p
            Char (Rel  p) -> cursorRel  p 0
            Line (Rel  p) -> cursorRel  0 p
            Line (Frac p) -> cursorLine p
            Page (Frac p) -> cursorPage p 0.5
        handleEx = case pos of
            Char (Rel  p) -> moveInput p
            Char (Frac p) -> moveInput $ round (2*(p-0.5)) * (maxBound`div`2)

    SetScroll pos -> case pos of
        Line (Rel p) -> scroll 0 p
        Page (Rel p) -> scroll (fromIntegral p) 0

    SetColumnMul pos -> case pos of
        Rel p -> setColumnMul p

    Set256Colors sw -> case sw of
        Off -> setStyle color16
        On  -> setStyle color256

    SetMark sw -> case sw of
        Toggle -> toggleMark

    JumpMark dir -> case dir of
        Bw -> findMark True
        Fw -> findMark False

    Delete dir -> caseMode handleInLine handleEx  where
       handleInLine = deleteByte
       handleEx = case dir of
           Bw -> deleteInput (-1)
           Fw -> deleteInput 1

    CommitInput -> commitInput

    CancelInput -> cancelInput

    Feed ch -> feedInput ch


caseMode inline ex = do
    isEx <- getsEditor $ not . isInLine . istMode . edInput
    if isEx then ex else inline
