{-# LANGUAGE RecordWildCards #-}
module Main where

import Control.Monad

import Graphics.Vty hiding (update)

import Control.HexEditor
import Control.LineEditor
import Editor
import Keymap
import Keymap.Default
import Keymap.Render
import Options

import qualified Buffer as Buf

import Keymap.Data
import Input.Mode

import Data.Version
import Paths_hexe


main = do
    opts <- parseOptions
    case opts of
        PrintVersion -> putStrLn (showVersion version)
        ListKeymap   -> putStr $ renderKeymapByMode     defaultKeymaps
        ListBindings -> putStr $ renderKeymapByCategory defaultKeymaps
        Options{..}  -> run opts

run Options{..} = do
    cfg <- standardIOConfig
    vty <- mkVty cfg

    buf <- Buf.readFile optFilename
    ed0 <- mkEditor vty buf HexOverwrite hexOverwrite defaultKeymaps

    EditorState ed1 _ <- (`execCommand` mkEditorState ed0) $ do
        setInput "Hex Overwrite" HexOverwrite
        forM_ optMarks $ \mark -> do
            cursorAbs mark
            setMark True
        cursorAbs optCursor

    mainLoop ed1
    shutdown vty


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
            let cmd = lookupCommand ev $ istKeymap $ edInput ed1
            EditorState ed2 bQuit <- execCommand cmd (mkEditorState ed1)
            let ed3 = updateInfo ed2
            unless bQuit (mainLoop ed3)
