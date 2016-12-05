module Control.General (
    storeFile,
    safeQuit,
    setKeymap, setKeymapByName, unhandledKey,
    message, showNotice, showInfo, showWarn,
    quit
) where

import Control.Exception
import Control.Monad.Trans
import Control.Monad.Trans.State.Lazy
import Text.Printf

import Graphics.Vty hiding (update, Style)

import Editor
import Editor.Style
import Keymap.Data
import Keymap.Render

import           Buffer (Buffer (..))
import qualified Buffer as Buf


storeFile = gets cstEditor >>= \ed -> do
    let buf   = edBuffer ed
        fnImg = renderString (edStyle ed) (bufPath buf)
        msgOK = horizCat [ msgNotice ed "Saved ", fnImg , msgNotice ed "."]
    let trySave = do
            buf' <- Buf.writeFile buf
            return (buf', msgOK)
        didn'tWork err = do
            let _ = err :: IOException
            return (buf, msgWarn ed $ "Dang! " ++ show err)
    (buf', msg) <- lift (trySave `catch` didn'tWork)
    withEditor $ const ed{ edBuffer = buf' }
    message msg


safeQuit = do
    mod <- getsEditor (Buf.isModified . edBuffer)
    if mod then
        showWarn "Changes not saved, Ctrl+Q to force quit"
    else
        quit


setKeymap km = withEditor $ \ed -> ed
    { edInput = (edInput ed){ istKeymap = km }
    }

setKeymapByName kmName = withEditor $ \ed -> ed
    { edInput = let km = lookupKeymap kmName (edKeymaps ed)
                in  (edInput ed){ istKeymap = km }
    }


unhandledKey ev = showWarn $ case ev of
    EvKey key mods -> printf "Key not bound: <%s>" (showKey key mods)
    _              -> printf "Unknown event: %s" (show ev)

showInfo   = styMessage styInfo
showNotice = styMessage styNotice
showWarn   = styMessage styWarn

styMessage styAttr msg = withEditor $ \ed ->
    let attr = styAttr (edStyle ed)
    in  setMessage (string attr msg) ed

message msg = withEditor $ setMessage msg

quit :: EditorT IO ()
quit = modify $ cstExitE (const True)
