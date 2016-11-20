module Editor.Types (
    Editor (..), mkEditor, cstEditorE, cstExitE, 
    Geometry (..), mkGeometry,
    InputEditor (..), mkInputEditor,
    Checkpoint (..),
    Keymap (..),
    Command,
    CommandState (..), mkCommandState,
    History,
    getCheckpoint, setCheckpoint,
    execCommand, getsEditor, withEditor, withEditorSt
) where

import Control.Monad.Trans.State.Lazy

import Graphics.Vty hiding (Style)

import Helpers
import History
import Buffer (Buffer (..), Offset)
import Editor.Style


data Editor = Editor
    { edVty     :: Vty
    , edStyle   :: Style
    , edBuffer  :: Buffer
    , edHistory :: History Checkpoint
    , edPending :: Maybe InputEditor
    , edScroll  :: Offset
    , edCursor  :: Offset
    , edGeo     :: Geometry
    , edColMul  :: Int
    , edInfo    :: Image
    , edKeymap  :: Keymap
    , edMessage :: Maybe Image
    }

data Geometry = Geo
    { geoWidth  :: Int
    , geoHeight :: Int
    , geoCols   :: Int
    , geoRows   :: Int
    , geoCells  :: Int
    }

data InputEditor = InputEditor
    { inpText   :: String
    , inpCursor :: Int
    , inpCheck  :: Char   -> Bool
    , inpAccept :: String -> Bool
    , inpImm    :: Bool
    , inpNextKm :: Keymap
    , inpAction :: String -> Command ()
    }

data Checkpoint = Checkpoint
    { ckpBuffer :: Buffer
    , ckpCursor :: Offset
    , ckpScroll :: Offset
    }

data Keymap = Keymap
    { kmName    :: Image
    , kmHandler :: Event -> Command ()
    }

type Command a = StateT CommandState IO a

data CommandState = CommandState
    { cstEditor :: Editor
    , cstExit   :: Bool
    }


mkEditor vty buf km = do
    (wdt,hgt) <- displayBounds $ outputIface vty
    return Editor
        { edVty     = vty
        , edStyle   = color256
        , edBuffer  = buf
        , edHistory = emptyHistory
        , edPending = Nothing
        , edScroll  = 0
        , edCursor  = 0
        , edGeo     = mkGeometry wdt hgt 1
        , edColMul  = 1
        , edInfo    = string currentAttr "No info."
        , edKeymap  = km
        , edMessage = Just (string currentAttr "Hi.")
        }

mkGeometry wdt hgt colMul = geo  where
    geo = Geo
        { geoWidth  = wdt
        , geoHeight = hgt
        , geoCols   =
            let unlimCols = (int wdt-11) `div` 4
                maxCols   = floor $ (int wdt-11) / (4 + 1/int colMul)
                reqCols   = (maxCols `div` colMul) * colMul
            in  if colMul >= 2 && reqCols > 0 then reqCols else unlimCols
        , geoRows   = hgt-1
        , geoCells  = geoRows geo * geoCols geo
        }

mkInputEditor check accept imm nextKm action = InputEditor
    { inpText   = ""
    , inpCursor = 0
    , inpCheck  = check
    , inpAccept = accept
    , inpImm    = imm
    , inpNextKm = nextKm
    , inpAction = action
    }

mkCommandState ed = CommandState ed False


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


execCommand :: Command a -> CommandState -> IO CommandState
execCommand = execStateT

cstEditorE f cst = cst{ cstEditor = f (cstEditor cst) }
cstExitE   f cst = cst{ cstExit   = f (cstExit   cst) }

getsEditor f = gets (f . cstEditor)

withEditor :: (Editor -> Editor) -> Command ()
withEditor = modify . cstEditorE

withEditorSt :: (Editor -> (a, Editor)) -> Command a
withEditorSt f = state $ \cst ->
    let (a, ed') = f (cstEditor cst)
    in  (a, cst{ cstEditor = ed' })
