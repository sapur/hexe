module Command.Render (
    renderCommand
) where

import Text.Printf

import Command.Data
import Editor.Mode


catGen  = "General"
catMode = "Mode Switching"
catNav  = "Navigation"
catView = "Change View"
catEdit = "Editing"
catEx   = "Ex Mode"


renderCommand cmd = case cmd of

    Quit b           -> catGen ? if b
        then "quit (without confirmation)"
        else "quit"
    SetMode mode     -> catMode? printf "switch to %s mode" (showMode mode)
    Store            -> catGen ? "save the file"
    JumpHistory dir  -> catEdit? case dir of
        Bw -> "undo"
        Fw -> "redo"
    SetCursor pos    -> catNav ? printf "move cursor %s" (renderPUnit pos)
    SetScroll pos    -> catNav ? printf "scroll %s" (renderPUnit pos)
    SetColumnWdt pos -> catView? printf "set column width %s"
                                        (renderPValue pos)
    Set256Colors sw  -> catView? printf "%s 256 color mode" (renderSwitch sw)
    SetMark sw       -> catNav ? printf "%s mark at cursor" (renderSwitch sw)
    JumpMark dir     -> catNav ? printf "jump to mark %s" (renderDirection dir)
    Delete dir       -> catEdit? printf "delete %s" (renderDirection dir)
    CommitInput      -> catEx  ? "execute command"
    CancelInput      -> catEx  ? "cancel pending command"
    Feed ch          -> catEx  ? printf "type '%c'" ch

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
