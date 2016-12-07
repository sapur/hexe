module Command.Render (
    renderScript,
    renderCommand,
) where

import Control.Arrow
import Data.List
import Text.Printf

import Graphics.Vty.Input.Events

import Command.Data
import Editor.Mode
import Keymap.Data.Name
import Render


catGen  = "General"
catMode = "Mode Switching"
catNav  = "Navigation"
catView = "Change View"
catMark = "Bookmarks"
catEdit = "Editing"
catEx   = "Ex Mode"


renderScript :: Script -> (String, String)
renderScript
    = (first head >>> second (intercalate ", "))
    . unzip
    . map renderCommand

renderCommand cmd = case cmd of

    Quit b -> catGen ? if b
        then "quit (without confirmation)"
        else "quit"
    SetMode mode -> case mode of
        MarkInput    -> catMark? "set mark with label at cursor"
        OffsetInput  -> catNav ? "jump to offset"
        ScriptInput  -> catGen ? "prompt for a command"
        _            -> catMode? printf "switch to %s mode" (showMode mode)

    Refresh          -> catGen ? "refresh screen"
    Store            -> catGen ? "save the file"
    JumpHistory dir  -> catEdit? case dir of
        Bw -> "undo"
        Fw -> "redo"
    SetCursor pos    -> catNav ? printf "move cursor %s" (renderPUnit pos)
    SetScroll pos    -> catNav ? printf "scroll %s" (renderPUnit pos)
    SetColumnWdt pos -> catView? printf "set column width %s"
                                        (renderPValue pos)
    Set256Colors sw  -> catView? printf "%s 256 color mode" (renderSwitch sw)
    SetMark sw       -> catMark? printf "%s mark at cursor" (renderSwitch sw)
    SetNamedMark o t -> catMark? printf "set mark '%s' at offset %s" t
                                        (renderPUnit o)
    JumpMark dir     -> catMark? printf "jump to mark %s" (renderDirection dir)
    Delete dir       -> catEdit? printf "delete %s" (renderDirection dir)
    CommitInput      -> catEx  ? "execute command"
    CancelInput      -> catEx  ? "cancel pending command"
    Feed ch          -> catEx  ? printf "type '%c'" ch
    Bind kmn ev scr  -> catGen ? printf "bind '%s' in '%s' to: %s"
                                        (renderKey ev) (showKeymapName kmn)
                                        (snd $ renderScript scr)

renderKey (EvKey key mods) = showKey key mods

renderPValue :: PValue -> String
renderPValue pos = renderValue pos ""

renderPUnit :: PUnit PValue -> String
renderPUnit pos = case pos of
    Char n -> renderValue n "char"
    Word n -> renderValue n "word"
    Line n -> renderValue n "line"
    Page n -> renderValue n "page"
    File n -> renderValue n "file"

renderValue n unit = case n of
    Abs  val -> printf "to %s %d" unit val
    Rel  val -> let sign   = if val    >= 0 then "+" else ""
                    plural = if abs val > 1 && not (null unit) then "s" else ""
                in  printf "%s%d %s%s" sign val unit plural
    Frac val -> let ratio = show (round $ 100*val :: Int) ++ "%"
                in  printf "to %s of %s" ratio unit

renderSwitch sw = case sw of
    On     -> "set"
    Off    -> "clear"
    Toggle -> "toggle"

renderDirection dir = case dir of
    Bw -> "backwards"
    Fw -> "forwards"


a ? b = (a,b)
