module Editor.Data (
    module Editor.Mode,
    Editor (..), mkEditor, cstEditorE, cstExitE, 
    Geometry (..), mkGeometry,
    Checkpoint (..),
    EditorT,
    EditorState (..), mkEditorState,
    History,
    getCheckpoint, setCheckpoint,
    execCommand, getsEditor, withEditor, withEditorSt
) where

import Control.Monad.Trans.State.Lazy

import Graphics.Vty hiding (Style)

import Buffer (Buffer (..), Offset)
import Editor.Mode
import Editor.Style
import Helpers
import History
import Keymap.Data


data Editor = Editor
    { edVty        :: Vty
    , edStyle      :: Style
    , edBuffer     :: Buffer
    , edHistory    :: History Checkpoint
    , edScroll     :: Offset
    , edCursor     :: Offset
    , edGeo        :: Geometry
    , edColMul     :: Int
    , edInfo       :: Image
    , edMessage    :: Maybe Image
    , edMode       :: InputMode
    , edLastMode   :: InputMode
    , edLineText   :: String
    , edLineCursor :: Int
    , edKeymaps    :: Keymaps
    }

data Geometry = Geo
    { geoWidth  :: Int
    , geoHeight :: Int
    , geoCols   :: Int
    , geoRows   :: Int
    , geoCells  :: Int
    }

data Checkpoint = Checkpoint
    { ckpBuffer :: Buffer
    , ckpCursor :: Offset
    , ckpScroll :: Offset
    }

type EditorT m = StateT EditorState m

data EditorState = EditorState
    { cstEditor :: Editor
    , cstExit   :: Bool
    }


mkEditor vty buf kms = do
    (wdt,hgt) <- displayBounds $ outputIface vty
    return Editor
        { edVty        = vty
        , edStyle      = color256
        , edBuffer     = buf
        , edHistory    = emptyHistory
        , edScroll     = 0
        , edCursor     = 0
        , edGeo        = mkGeometry wdt hgt 1
        , edColMul     = 1
        , edInfo       = string currentAttr "No info."
        , edMessage    = Just (string currentAttr "Hi.")
        , edMode       = HexOverwrite
        , edLastMode   = HexOverwrite
        , edLineText   = ""
        , edLineCursor = 0
        , edKeymaps    = kms
        }

mkGeometry wdt hgt colMul = geo  where
    geo = Geo
        { geoWidth  = wdt
        , geoHeight = hgt
        , geoCols   =
            let unlimCols = (int wdt-11) `div` 4
                maxCols   = floor $ (int wdt-11 :: Double) / (4 + 1/int colMul)
                reqCols   = (maxCols `div` colMul) * colMul
            in  if colMul >= 2 && reqCols > 0 then reqCols else unlimCols
        , geoRows   = hgt-1
        , geoCells  = geoRows geo * geoCols geo
        }

mkEditorState ed = EditorState ed False


getCheckpoint ed = Checkpoint
    { ckpBuffer = edBuffer ed
    , ckpCursor = edCursor ed
    , ckpScroll = edScroll ed
    }

setCheckpoint ckp ed = ed
    { edBuffer = ckpBuffer ckp
    , edCursor = ckpCursor ckp
    , edScroll = ckpScroll ckp
    }


execCommand :: EditorT IO a -> EditorState -> IO EditorState
execCommand = execStateT

cstEditorE f cst = cst{ cstEditor = f (cstEditor cst) }
cstExitE   f cst = cst{ cstExit   = f (cstExit   cst) }

getsEditor f = gets (f . cstEditor)

withEditor :: (Editor -> Editor) -> EditorT IO ()
withEditor = modify . cstEditorE

withEditorSt :: (Editor -> (a, Editor)) -> EditorT IO a
withEditorSt f = state $ \cst ->
    let (a, ed') = f (cstEditor cst)
    in  (a, cst{ cstEditor = ed' })
