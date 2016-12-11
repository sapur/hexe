module Command.Execute (
    module Command.Data,
    executeScript,
    execute,
) where

import Command.Data
import Control
import Editor
import Helpers

import qualified Buffer as Buf


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
        handleHex  = calculateMotion edCursor setCursor pos
        handleLine = case pos of
            Char (Rel  p) -> moveInput p
            Char (Frac p) -> moveInput $ round (2*(p-0.5)) * (maxBound`div`2)
            _             -> showWarn "Not supported."

    SetScroll pos -> calculateMotion edScroll setScroll pos

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

    SetNamedMark pos text -> case pos of
        Char (Abs p) -> setMarkAt (int p) (Just text)
        _      -> showWarn "Not supported."

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

    Bind kmn ev script -> rebindKeyEd kmn ev script


caseEditor hex line = do
    isEdit <- getsEditor $ isEditing . edMode
    if isEdit then line else hex


calculateMotion get set motion = withEditor $ \ed ->
    let geo     = edGeo ed
        len     = Buf.length (edBuffer ed)
                + if extends (edMode ed) then 1 else 0
        cursor  = get ed
        scroll  = edScroll ed
        pick    = pickFromRange (int len) (int cursor)
        cursor' = case motion of
            Char   v -> pick 1 v
            Line   v -> pick (int $ geoCols  geo) v
            Page   v -> pick (int $ geoCells geo) v
            InLine v -> pickFromSubRange (int scroll) (int cursor)
                                         (int $ geoCols geo) v
            InPage v -> pickFromSubRange (int scroll) (int cursor)
                                         (int $ geoCells geo) v
    in  set (int cursor') ed

pickFromSubRange origin offset mul value = result  where
    f i    = let base = i `div` mul
                 rest = i `mod` mul
             in  base*mul + pickFromValue mul rest value
    result = origin + f (offset-origin)

pickFromRange total offset mul value = offset'  where
    count   = ((total-1) `div` mul) + 1
    index   = offset `div` mul
    rest    = offset `mod` mul
    index'  = pickFromValue count index value
    offset' = min total $ index'*mul + rest

pickFromValue count current value = case value of
    Abs  n -> min (count-1) n
    Rel  i -> int $ clamp 0 (int count-1) (int current+i)
    Frac r -> round (int (count-1) * r)
