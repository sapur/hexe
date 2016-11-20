module Commands.General (
    storeFile,
    safeQuit,
    setKeymap, forwardKeymap, unhandledKey,
    message, showNotice, showInfo, showWarn,
    quit
) where

import Control.Exception
import Control.Monad.Trans
import Control.Monad.Trans.State.Lazy
import Data.Char
import Text.Printf

import Graphics.Vty hiding (update, Style)

import Editor
import Editor.Style

import           Buffer (Buffer (..))
import qualified Buffer as Buf


storeFile = gets cstEditor >>= \ed -> do
    let buf   = edBuffer ed
        fnImg = horizCat
              $ map (renderByte (-1) Nothing (edStyle ed) 0 . (\b->(ord b,Buf.Copy,False)))
                    (bufPath buf)
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


setKeymap km = withEditor $ \ed -> ed{ edKeymap = km }

forwardKeymap km key = do
    setKeymap km
    kmHandler km key

unhandledKey ev = showWarn $ case ev of
    EvKey key mods -> printf "Key not bound: %s" (showKey key mods)
    _              -> printf "Unknown event: %s" (show ev)

showKey key mods = repr  where
    repr     = if   null mods
               then printf "<%s>"    strKey :: String
               else printf "<%s-%s>" (map strMod mods) strKey
    strKey   = case key of
        KChar c -> [c]
        _       -> show key
    strMod m = case m of
        MShift -> 'S'
        MCtrl  -> 'C'
        MMeta  -> 'M'
        MAlt   -> 'A'

showInfo   = styMessage styInfo
showNotice = styMessage styNotice
showWarn   = styMessage styWarn

styMessage styAttr msg = withEditor $ \ed ->
    let attr = styAttr (edStyle ed)
    in  setMessage (string attr msg) ed

message msg = withEditor $ setMessage msg

quit :: Command ()
quit = modify $ cstExitE (const True)
