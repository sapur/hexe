{-# LANGUAGE RecordWildCards #-}
module Main where

import Control.Exception
import Control.Monad
import System.IO
import Text.Printf

import Graphics.Vty hiding (update, setMode)

import Control
import Editor
import Helpers
import Keymap
import Keymap.Default
import Keymap.Render
import Options

import qualified Buffer as Buf

import Data.Version
import Paths_hexe


main = do
    opts <- parseOptions
    case opts of
        PrintVersion -> printVersion
        ListKeymap   -> putStr $ renderKeymapByMode     defaultKeymaps
        ListBindings -> putStr $ renderKeymapByCategory defaultKeymaps
        Options{..}  -> run opts

run opts = action `catch` exception  where
    action = do
        buf <- Buf.readFile (optFilename opts)
        runUI buf opts
    exception =
        hPutStrLn stderr . formatIOEx

runUI buf Options{..} = do
    cfg <- standardIOConfig
    vty <- mkVty cfg

    ed0 <- mkEditor vty buf defaultKeymaps

    EditorState ed1 _ <- (`execCommand` mkEditorState ed0) $ do
        setMode HexOverwrite

        setColumnWdtAbs optColumnWdt

        let marks = map (\o     -> (o, Just "")) optMarks
                 ++ map (\(o,t) -> (o, Just t )) optNamedMarks
        forM_ marks $ \(offset, text) -> do
            cursorAbs offset
            setMark text

        cursorAbs optCursor

        showNotice $ printf "Loaded '%s'." optFilename

    eiEx <- try $ mainLoop ed1
    shutdown vty

    case eiEx of
        Left ex -> hPrint stderr (ex :: SomeException)
        Right _ -> return ()


mainLoop ed0 = do
    renderView ed0

    let vty = edVty ed0
        ed1 = clearMessage ed0

    nextEvent vty >>= \ev -> case ev of
        EvKey (KChar 'q') [MCtrl] ->
            return ()
        EvResize wdt hgt ->
            mainLoop (reshape wdt hgt ed1)
        ev -> do
            let cmd = lookupCommand ev (edMode ed1) $ edKeymaps ed1
            EditorState ed2 bQuit <- execCommand cmd (mkEditorState ed1)
            let ed3 = updateInfo ed2
            unless bQuit (mainLoop ed3)


printVersion =
    let msg = unlines
            [ "hexe %s"
            ]
    in  printf msg (showVersion version)
