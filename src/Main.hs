{-# LANGUAGE RecordWildCards #-}
module Main where

import Control.Monad

import Graphics.Vty hiding (update)

import Options
import Editor
import Keymaps
import Commands

import qualified Buffer as Buf

import Paths_hexe
import Data.Version


main = do
    opts <- parseOptions
    case opts of
        PrintVersion -> putStrLn (showVersion version)
        Options{..}  -> run opts

run Options{..} = do
    cfg <- standardIOConfig
    vty <- mkVty cfg

    buf <- Buf.readFile optFilename
    ed0 <- mkEditor vty buf navigation

    CommandState ed1 _ <- (`execCommand` mkCommandState ed0) $ do
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
            CommandState ed2 bQuit <- handleKey ev ed1
            let ed3 = updateInfo ed2
            unless bQuit (mainLoop ed3)
