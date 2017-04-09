{-# LANGUAGE RecordWildCards #-}
module Hexe (hexeMain) where

import Control.Exception
import Control.Monad
import Data.Maybe
import System.Directory
import System.FilePath
import System.IO
import Text.Printf

import Graphics.Vty hiding (update, setMode)

import Command.Execute
import Command.Parser
import Control
import Editor
import Helpers
import Keymap
import Keymap.Default
import Keymap.Render
import Options

import qualified Buffer as Buf


hexeMain version gdf = run gdf =<< parseOptions version

run getDataFileName opts@Options{..} = action `catch` exception  where
    action = do
        buf <- case optAction of
            Edit file -> do
                eiBuffer <- try $ Buf.readFile file
                case eiBuffer of
                    Right buf ->
                        return buf
                    Left ex  -> do
                        let _ = ex :: IOException
                        return $ Buf.mkBuffer file
            _ -> return $ Buf.mkBuffer "unnamed"

        let check fileM = do
                file <- fileM
                b    <- doesFileExist file
                return $ if b then Just file else Nothing

        globalCfg <- check $ getDataFileName "config"
        userCfg   <- check $ (</> "config") <$> getXdgDirectory XdgConfig "hexe"

        let files = catMaybes $ globalCfg : userCfg : map Just optScripts

        pScripts <- forM files $ \fn -> do
            raw <- readFile fn
            return $ parseScript fn raw

        let pCdl  = mapM (parseScript "<ARG>") optCommands
            pCmds = (++) <$> sequence pScripts <*> pCdl

        case pCmds of
            Left err ->
                hPrint stderr err
            Right scripts -> do
                let cmds = concat scripts
                runUI buf opts cmds

    exception =
        hPutStrLn stderr . formatIOEx

runUI buf Options{..} cmds = do
    cfg <- standardIOConfig
    vty <- mkVty cfg

    ed0 <- mkEditor vty buf defaultKeymaps

    EditorState ed1 _ <- (`execCommand` mkEditorState ed0) $ do
        when (Buf.length buf == 0)
            $ setMode HexInsert

        (twdt,_) <- displayBounds $ outputIface vty
        setColumnWdtAbs $ if twdt == 80 then 4 else 1

        let colors = contextColorCount $ outputIface vty
        when (colors <= 16) $ setStyle color16

        -- defaults above --
        executeScript cmds
        -- command-line arguments below --

        let ifSet o a = maybe (return ()) a o

        ifSet optColumnWdt setColumnWdtAbs
        ifSet opt256Colors $ setStyle . \b -> if b then color256 else color16
        ifSet optCursor    $ withEditor . setCursor

        let marks = map (\o     -> (o, Just "")) optMarks
                 ++ map (\(o,t) -> (o, Just t )) optNamedMarks
        forM_ marks $ \(offset, text) -> do
            withEditor $ setCursor offset
            setMark text

        showNotice $ printf "Loaded '%s'." (Buf.bufPath buf)

    finalCmd <- case optAction of
        Edit _ -> do
            eiEx <- try $ mainLoop ed1
            return $ case eiEx of
                Left ex -> hPrint stderr (ex :: SomeException)
                Right _ -> return ()
        ListKeymap ->
            return $ putStr $ renderKeymapByMode $ edKeymaps ed1
        ListBindings ->
            return $ putStr $ renderKeymapByCategory $ edKeymaps ed1

    shutdown vty
    finalCmd


mainLoop ed0 = do
    renderView ed0

    let vty = edVty ed0
        ed1 = clearMessage ed0

    nextEvent vty >>= \ev -> case ev of
        EvKey (KChar 'q') [MCtrl] ->
            return ()
        EvKey (KChar 'l') [MCtrl] -> do
            refresh vty
            mainLoop ed1
        EvResize wdt hgt ->
            mainLoop (reshape wdt hgt ed1)
        ev -> do
            let cmd = lookupScript ev (edMode ed1) $ edKeymaps ed1
            EditorState ed2 bQuit <- execCommand cmd (mkEditorState ed1)
            let ed3 = updateInfo ed2
            unless bQuit (mainLoop ed3)
