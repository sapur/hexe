module Control.LineEditor (
    setMode, cancelInput, commitInput,
    feedInput, moveInput, deleteInput,
) where

import Control.Arrow
import Control.Monad
import Text.Read (readMaybe)

import Editor

import qualified Buffer as Buf

import Control.HexEditor


setMode mode = withEditor $ \ed -> ed
    { edMode     = mode
    , edLastMode = edMode ed
    }

cancelInput = modeReturn

commitInput = do
    ed  <- getsEditor id
    ret <- commitAction (edMode ed) (edLineText ed)
    when ret modeReturn

modeReturn = withEditor $ \ed0 ->
    let ed1 = setCursor (edCursor ed0)
            $ ed0
                { edLineText   = ""
                , edLineCursor = 0
                , edBuffer     = Buf.removeSlack (edCursor ed0) (edBuffer ed0)
                }
    in  case nextMode $ edMode ed1 of
            Just mode -> ed1
                { edMode     = mode
                , edLastMode = mode
                }
            Nothing   -> ed1
                { edMode     = edLastMode ed1
                , edLastMode = edMode ed1
                }

commitAction mode txt = case mode of
    HexOverwrite ->
        return True
    HexOverwriting -> do
        overwriteHex txt
        return True
    CharOverwrite -> do
        whenSingleChar overwriteCharKey
        return True
    HexInsert ->
        return True
    HexInserting -> do
        overwriteHex txt
        return True
    CharInsert -> do
        whenSingleChar overwriteCharKey
        return True
    OffsetInput ->
        case (readMaybe txt :: Maybe Int) of
            Nothing  -> return False
            Just off -> do
                cursorAbs off
                return True
    MarkInput -> do
        setMark (Just txt)
        return True
  where
    whenSingleChar action = case txt of
        [ch] -> action ch
        _    -> return ()

feedInput ch = do
    mode <- getsEditor edMode
    begin mode
    ed' <- getsEditor id
    feed ed' mode
  where
    begin mode = case mode of
        HexOverwrite ->
            setMode HexOverwriting
        HexInsert -> do
            setMode HexInserting
            prepareInsert 0
        CharInsert ->
            prepareInsert 0
        _ -> return ()
    feed ed mode =
        when (validChar mode ch) $ update ed mode
    update ed mode = do
        let txt = edLineText ed
            cur = edLineCursor ed
        withEditor $ \ed -> ed
            { edLineText   = take cur txt ++ [ch] ++ drop cur txt
            , edLineCursor = cur + 1
            }
        txt' <- getsEditor edLineText
        when (autoAccept mode txt') commitInput


moveInput d = withEditor $ clampInput . \ed -> ed
    { edLineCursor = edLineCursor ed + d
    }

deleteInput d = do
    withEditor $ \ed ->
        let txt = edLineText ed
            cur = edLineCursor ed
        in  clampInput $ case d of
                _ | d < 0 -> ed{ edLineCursor = cur - 1
                               , edLineText   = take (cur-1) txt ++ drop cur txt
                               }
                _ | d > 0 -> ed{ edLineText   = take cur txt ++ drop (cur+1) txt
                               }
                _         -> ed
    (mode, txt) <- getsEditor (edMode &&& edLineText)
    when (isInLine mode && null txt) cancelInput

clampInput ed =
    let txt = edLineText ed
        cur = min (length txt) (max 0 (edLineCursor ed))
    in  ed { edLineCursor = cur
           }
