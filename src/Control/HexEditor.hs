module Control.HexEditor (
    setColumnWdtAbs, setColumnWdtRel,
    overwriteHex, overwriteCharKey, deleteByte,
    prepareInsert,
    setMark, setMarkAt, toggleMark, findMark,
    withUndo, undo, redo,
    setStyle, color16, color256,
) where

import Data.Char
import Numeric
import Text.Printf

import Graphics.Vty hiding (update, Style)

import Editor
import Editor.Style
import Helpers

import qualified Buffer  as Buf
import qualified History as Hist

import Control.General


setColumnWdtAbs n = do
    msg <- withEditorSt mod
    showNotice msg
  where
    mod ed = let mul = clamp 1 128 n
                 msg = printf "column width is %d" mul
                 ed' = relayout ed{ edColMul = mul }
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

setMarkAt offset b = withEditor $ \ed -> ed
    { edBuffer = Buf.setMark offset b (edBuffer ed)
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
    withEditor $ \ed0 ->
        let ed1 = f ed0
        in  ed1 { edBuffer = Buf.removeSlack (edCursor ed1) (edBuffer ed1)
                }
    (undos, redos) <- getsEditor (Hist.getStats . edHistory)
    showNotice (printf "did %s, remaining: %d undos, %d redos" name undos redos)


setStyle sty = withEditor $ \ed -> ed{ edStyle = sty }
