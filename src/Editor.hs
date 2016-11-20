module Editor (
    module Editor.Types,
    reshape, relayout,
    setCursor, setScroll,
    modifyEditor,
    histCheckpoint, histUndo, histRedo,
    clearMessage, setMessage,
    updateInfo,
    keymap, handleKey,
    renderView, renderByte,
    msgNotice, msgWarn
) where

import Graphics.Vty hiding (update)

import Editor.Types
import Editor.Style
import Editor.Render

import qualified Buffer  as Buf
import qualified History as Hist


reshape wdt hgt ed = clampCursor ed
    { edGeo = mkGeometry wdt hgt (edColMul ed)
    }

relayout ed = clampCursor ed
    { edGeo = mkGeometry (geoWidth $ edGeo ed) (geoHeight $ edGeo ed)
                         (edColMul ed)
    }


setCursor offset ed = clampCursor ed
    { edCursor = offset
    , edBuffer = Buf.extendCond offset
               $ Buf.removeSlack (edCursor ed) (edBuffer ed)
    }

setScroll offset ed = clampScroll ed{ edScroll = offset }

clampScroll = clampCursorHard . clampCursorSoft . clampScrollHard
clampCursor = clampScrollHard . clampScrollSoft . clampCursorHard

clampScrollHard ed = ed{ edScroll = new }  where
    Geo _ _ cols rows _ = edGeo ed
    bytes       = Buf.length $ edBuffer ed
    dataRows    = (bytes-1) `div` cols + 1
    ubound      = (dataRows - rows) * cols

    aligned     = (edScroll ed `div` cols) * cols
    new         = clamp 0 ubound aligned

clampScrollSoft ed = ed{ edScroll = new }  where
    Geo _ _ cols _ cells = edGeo ed
    scrollOff = 1
    lbound    = edCursor ed - (cells-1) + (scrollOff+1)*cols - 1
    ubound    = edCursor ed - scrollOff*cols
    new       = clamp lbound ubound (edScroll ed)

clampCursorHard ed = ed{ edCursor = new }  where
    ubound = (Buf.length $ edBuffer ed) - 1
    new    = clamp 0 ubound (edCursor ed)

clampCursorSoft ed = ed{ edCursor = new }  where
    lbound = edScroll ed
    ubound = edScroll ed + geoCells (edGeo ed) - 1
    new    = clamp lbound ubound (edCursor ed)

clamp lbound ubound = max lbound . min ubound


modifyEditor fCursor fBuffer ed = clampCursor ed
    { edBuffer = fBuffer (edBuffer ed)
    , edCursor = fCursor (edCursor ed)
    }


histCheckpoint ed = ed
    { edHistory = Hist.setVersion (getCheckpoint ed) (edHistory ed)
    }

histOp f ed = clampCursor $ setCheckpoint ckp ed
    { edHistory = hist'
    }
  where
    curCkp       = getCheckpoint ed
    (ckp, hist') = f curCkp (edHistory ed)

histUndo = histOp Hist.undo
histRedo = histOp Hist.redo


clearMessage ed = ed{ edMessage = Nothing }

setMessage msg ed = ed{ edMessage = Just msg }


updateInfo ed = ed{ edInfo = styled info }  where
    styled = string (styInfo $ edStyle ed)
    info   = "Happy."


keymap :: String -> Color -> (Event -> Command ()) -> Keymap
keymap name color handler = Keymap
    { kmName    = string (currentAttr `withForeColor` color) name
    , kmHandler = handler
    }

handleKey key ed =
    let handle = kmHandler (edKeymap ed)
    in  execCommand (handle key) (mkCommandState ed)


msgNotice ed = string (styNotice $ edStyle ed)
msgWarn   ed = string (styWarn   $ edStyle ed)
