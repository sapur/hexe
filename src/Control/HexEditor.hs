module Control.HexEditor (
    cursorAbs, cursorBuf, cursorRel, cursorPage, cursorLine,
    scroll, scrollCursor,
    setColumnWdtAbs, setColumnWdtRel,
    overwriteHex, overwriteCharKey, deleteByte,
    prepareInsert,
    setMark, toggleMark, findMark,
    withUndo, undo, redo,
    setStyle, color16, color256,
) where

import Data.Char
import Numeric
import Text.Printf

import Graphics.Vty hiding (update, Style)

import Editor
import Editor.Style

import qualified Buffer  as Buf
import qualified History as Hist

import Control.General


cursorAbs offset = withEditor $ setCursor offset

cursorBuf x = withEditor $ \ed ->
    let maxOffset = Buf.length (edBuffer ed) - 1
        offset    = round (x * fromIntegral maxOffset)
    in  setCursor offset ed

cursorRel cols rows = withEditor $ \ed ->
    let offset = edCursor ed
               + rows * geoCols (edGeo ed)
               + cols
    in  setCursor offset ed

cursorPage y x = withEditor $ \ed ->
    let (scroll, cells) = ( fromIntegral $ edScroll ed :: Float
                          , fromIntegral $ geoCells $ edGeo ed
                          )
        vertical = round (scroll + y * (cells-1))
    in  setCursorInLine x
      $ setCursor vertical ed

cursorLine x = withEditor $ setCursorInLine x

setCursorInLine x ed = setCursor offset ed  where
    cols   = geoCols (edGeo ed)
    flushL = fromIntegral $ (edCursor ed `div` cols)*cols
    offset = round (flushL + x * fromIntegral (cols-1))


scroll :: Float -> Int -> EditorT IO ()
scroll pages rows = withEditor $ \ed ->
    let geo    = edGeo ed
        offset = edScroll ed
               + round (pages * fromIntegral (geoCells geo))
               + rows * geoCols geo
    in  setScroll offset ed

scrollCursor = withEditor $ \ed ->
    let (geo, cursor) = (edGeo ed, edCursor ed)
        offset        = cursor - (geoCells geo `div` 2)
    in  setCursor cursor ed{ edScroll = offset }


setColumnWdtAbs n = do
    msg <- withEditorSt mod
    showNotice msg
  where
    mod ed = let mul = max 1 (min 128 n)
                 msg  = printf "column width is %d" mul
                 ed'  = relayout ed{ edColMul = mul }
             in  (msg, ed')

setColumnWdtRel d = do
    mul <- getsEditor edColMul
    setColumnWdtAbs (mul + d)


overwriteHex input = withUndo (withEditor int)  where
    int ed = ed'  where
        ed' = setMessage msg
            $ setCursor cursor'
            $ ed { edBuffer  = buf'
                 }
        (buf, cursor) = (edBuffer ed, edCursor ed)
        (buf', cursor', msg) = case readHex input of
            [(b,_)] -> (Buf.setByte cursor b buf, cursor+1, emptyImage)
            _       -> (buf, cursor, msgWarn ed "Not hexy enough")

overwriteCharKey c = withUndo (withEditor overwrite)  where
    overwrite ed = setCursor cursor' ed
        { edBuffer = buf'
        }
      where
        cursor  = edCursor ed
        cursor' = cursor + 1
        buf'    = Buf.setByte cursor (fromIntegral $ ord c) (edBuffer ed)

prepareInsert d = withEditor $ \ed -> ed
    { edBuffer = Buf.insertSlack (edCursor ed + d)
                                 1 (edBuffer ed)
    }


deleteByte = withUndo (withEditor delete)  where
    delete ed = ed
        { edBuffer = Buf.deleteByte (edCursor ed) (edBuffer ed)
        }


setMark b = withEditor $ \ed -> ed
    { edBuffer = Buf.setMark (edCursor ed) b (edBuffer ed)
    }

toggleMark = withEditor $ \ed ->
    let (buf, offset) = (edBuffer ed, edCursor ed)
        mark          = maybe (Just "") (const Nothing)
                              (Buf.getMark offset buf)
    in  ed{ edBuffer = Buf.setMark offset mark buf
          }

findMark rev = do
    offset <- getsEditor (\ed -> Buf.findMark rev (edCursor ed) (edBuffer ed))
    withEditor (setCursor offset)


withUndo f = do
    withEditor histCheckpoint
    (undos, redos) <- getsEditor (Hist.getStats . edHistory)
    f
    showNotice (printf "checkpoint, remaining: %d undos, %d redos" undos redos)
undo = histOp "undo" histUndo
redo = histOp "redo" histRedo

histOp name f = do
    withEditor f
    (undos, redos) <- getsEditor (Hist.getStats . edHistory)
    showNotice (printf "did %s, remaining: %d undos, %d redos" name undos redos)


setStyle sty = withEditor $ \ed -> ed{ edStyle = sty }
