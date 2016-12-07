module Command.Execute (
    module Command.Data,
    executeScript,
    execute,
) where

import Command.Data
import Control
import Editor.Data
import Helpers


executeScript :: Script -> EditorT IO ()
executeScript = mapM_ execute

execute :: Command -> EditorT IO ()
execute cmd = case cmd of

    Quit force -> if force then quit else safeQuit

    Refresh -> refreshScreen

    SetMode mode -> setMode mode

    Store -> storeFile

    JumpHistory dir -> case dir of
        Bw -> undo
        Fw -> redo

    SetCursor pos -> caseEditor handleHex handleLine  where
        handleHex = case pos of
            File (Abs  p) -> cursorAbs (int p)
            File (Frac p) -> cursorBuf p
            Char (Rel  p) -> cursorRel  p 0
            Line (Rel  p) -> cursorRel  0 p
            Line (Frac p) -> cursorLine p
            Page (Frac p) -> cursorPage p 0.5
            _             -> showWarn "Not supported."
        handleLine = case pos of
            Char (Rel  p) -> moveInput p
            Char (Frac p) -> moveInput $ round (2*(p-0.5)) * (maxBound`div`2)
            _             -> showWarn "Not supported."

    SetScroll pos -> case pos of
        Line (Rel p) -> scroll 0 p
        Page (Rel p) -> scroll (fromIntegral p) 0
        _            -> showWarn "Not supported."

    SetColumnWdt pos -> case pos of
        Abs p -> setColumnWdtAbs (int p)
        Rel p -> setColumnWdtRel p
        _     -> showWarn "Not supported."

    Set256Colors sw -> case sw of
        Off -> setStyle color16
        On  -> setStyle color256
        _   -> showWarn "Not supported."

    SetMark sw -> case sw of
        Toggle -> toggleMark
        _      -> showWarn "Not supported."

    SetNamedMark offset text -> setMarkAt offset (Just text)

    JumpMark dir -> case dir of
        Bw -> findMark True
        Fw -> findMark False

    Delete dir -> caseEditor handleHex handleLine  where
       handleHex = deleteByte
       handleLine = case dir of
           Bw -> deleteInput (-1)
           Fw -> deleteInput 1

    CommitInput -> commitInput executeScript

    CancelInput -> cancelInput

    Feed ch -> feedInput executeScript ch


caseEditor hex line = do
    isEdit <- getsEditor $ isEditing . edMode
    if isEdit then line else hex
