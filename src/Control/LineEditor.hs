module Control.LineEditor (
    setInput, cancelInput, commitInput,
    feedInput, moveInput, deleteInput,
) where

import Control.Monad
import Text.Read (readMaybe)

import Editor

import qualified Buffer as Buf

import Control.HexEditor
import Input.Mode
import Keymap.Data


setInput kmName mode = do
    km <- getsEditor $ lookupKeymap kmName . edKeymaps
    let inSt = mkInputState km mode
    withEditor $ \ed -> ed
        { edInput      = inSt
        , edLastInput  = edInput ed
        }

cancelInput = modeReturn

commitInput = do
    ed  <- getsEditor id
    ret <- commitAction (istMode $ edInput ed) (edLineText ed)
    when ret modeReturn

modeReturn = withEditor $ \ed0 ->
    let ed1 = setCursor (edCursor ed0)
            $ ed0
                { edLineText   = ""
                , edLineCursor = 0
                , edBuffer     = Buf.removeSlack (edCursor ed0) (edBuffer ed0)
                }
    in if   isSticky $ istMode $ edInput ed1
       then ed1
       else ed1
           { edInput     = edLastInput ed1
           , edLastInput = edInput ed1
           }

commitAction mode txt = case mode of
    HexOverwrite -> do
        overwriteHex txt
        return True
    CharOverwrite -> do
        whenSingleChar overwriteCharKey
        return True
    HexInsert -> do
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
  where
    whenSingleChar action = case txt of
        [ch] -> action ch
        _    -> return ()

feedInput ch = do
    mode <- getsEditor $ istMode . edInput
    begin mode
    ed' <- getsEditor id
    feed ed' mode
  where
    begin mode = case mode of
        HexInsert  -> prepareInsert 0
        CharInsert -> prepareInsert 0
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

deleteInput d = withEditor $ \ed ->
    let txt = edLineText ed
        cur = edLineCursor ed
    in  clampInput $ case d of
            _ | d < 0 -> ed{ edLineCursor = cur - 1
                           , edLineText   = take (cur-1) txt ++ drop cur txt
                           }
            _ | d > 0 -> ed{ edLineText   = take cur txt ++ drop (cur+1) txt
                           }
            _         -> ed

clampInput ed =
    let txt = edLineText ed
        cur = min (length txt) (max 0 (edLineCursor ed))
    in  ed { edLineCursor = cur
           }
