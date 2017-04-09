{-# LANGUAGE RecordWildCards #-}
module Editor.Render (
    renderView
) where

import Data.Char
import Data.List.Extra
import Data.Maybe
import Data.Word
import Text.Printf

import           Graphics.Vty hiding (update, Style)
import qualified Graphics.Vty as Vty

import Editor.Data
import Editor.Style
import Helpers

import           Buffer (ModState)
import qualified Buffer as Buf


renderView ed = do
    let vty        = edVty ed
        (img, cur) = render ed
    Vty.update vty (picForImage img){ picCursor = cur }

render ed = result  where
    Geo{..}   = edGeo ed
    scrollOff = edScroll ed
    bytes     = Buf.viewRange scrollOff geoCells (edBuffer ed)
    colMul    = edColMul ed
    effColMul = if   colMul >= 2 && colMul <= geoCols
                then colMul
                else maxBound

    chunks          = chunksOf geoCols bytes
    hexLines        = zipWith hexLine [0..] chunks
    hexLine line ch = renderLine (scrollOff + line * geoCols)
                                 (edCursor ed) (edMode ed)
                                 (edLineText ed) geoCols effColMul
                                 (edStyle ed) ch
    padding         = replicate (geoRows - length chunks)
                    $ string (stySpace $ edStyle ed) " "

    hexView            = vertCat $ hexLines ++ padding
    (statusView, ccol) = renderStatus ed

    cursor     = if   isInLine (edMode ed)
                 then NoCursor
                 else Cursor ccol (length chunks + length padding)

    result     = (hexView <-> statusView, cursor)

renderLine
    :: Int -> Int -> InputMode -> String -> Int -> Int -> Style
    -> [(Word8, ModState, Maybe String)]
    -> Image
renderLine offset cursor mode pending perLine colMul sty bytes = horizCat
    [ string styOffset (printf "%08X  " offset)
    , horizCat $ zipWith (renderHex cursor mode pending colMul sty)
                         [offset..] bytes
    , string stySpace (replicate hexPadding ' ')
    , string stySpace " "
    , horizCat $ zipWith (renderByte cursor mode pending sty) [offset..] bytes
    , string stySpace (replicate missing ' ')
    ]
  where
    Style{..}  = sty
    missing    = perLine - length bytes
    hexPadding = 3*missing + (missing + colMul-1) `div` colMul

renderHex cursor mode pending colMul sty offset (b,st,mk)
    = let Style{..} = sty
          str       = case cursor == offset of
              True | isInLine mode && not (null pending)
                -> printf "%-2s" pending
              _ -> hexByte b
      in  string (styleByte cursor mode pending offset st mk sty b) str
      <|> if   offset `mod` colMul /= colMul-1
          then char stySpace ' '
          else string stySpace "  "

hexByte b = [toDigit ms, toDigit ls]  where
    (ms,ls) = (b`div`16, b`mod`16)
    toDigit n | n < 10 = chr (int n      + ord '0')
    toDigit n          = chr (int n - 10 + ord 'A')

renderByte cursor mode pending sty offset (b,st,mk) =
    let c = chr (int b)
    in  char (styleByte cursor mode pending offset st mk sty b)
             (if isPrint c then c else subst c)

styleByte cursor mode pending offset st mk sty b = style  where
    Style{..} = sty
    c         = chr (int b)
    baseStyle =
        if   isJust mk
        then styMark
        else case st of
                Buf.Alt         -> styAlt
                Buf.Ins         -> styIns
                Buf.Slack       -> stySlack
                _ | c == '\NUL' -> styNull
                _ | isPrint c   -> styPrint
                _               -> styNonPrint
    isImm = case () of
        _ | isInLine mode && not (null pending) -> True
        _                                       -> False
    style | cursor-offset /= 0 = baseStyle
          | isImm              = baseStyle   `withStyle` reverseVideo
          | otherwise          = styChanging `withStyle` reverseVideo

renderStatus ed = final  where
    Style{..} = edStyle ed
    wdt       = geoWidth (edGeo ed)
    pos       = (edCursor ed + 2) * 100 `div` (Buf.length (edBuffer ed) + 1)
    left      = string styOffset (printf "%8X  " (edCursor ed))
    right     = string stySpace "  "
            <|> renderMode (edMode ed)
            <|> (if   Buf.isModified (edBuffer ed)
                 then string styAlt " * "
                 else string stySpace " ")
            <|> string styOffset (printf " %3d%%" pos)
    centerWdt = wdt - imageWidth left - imageWidth right
    (centerRaw,off) = statusCenter ed
    center    = resizeWidth centerWdt centerRaw
    cursorCol = imageWidth left + off
    final     = (left <|> center <|> right, cursorCol)

statusCenter Editor{..} = (final, coff)  where
    Style{..} = edStyle
    chunk     = Buf.viewRange edCursor 1 edBuffer
    hasLine   = not (isInLine edMode)

    mark str  = string styMark "  "
            <|> string styInput (" " ++ str)

    coff      = edLineCursor

    final = case chunk of
        _ | hasLine -> inputLine edStyle edLineMarker (edLineText ++" ")
        _           -> fromMaybe tryMark edMessage
    tryMark = case chunk of
        [(_,_,Just str)] | not (null str) -> mark str
        _                                 -> edInfo

inputLine Style{..} emk txt
    = horizCat (zipWith inputChar [0..] (txt++" "))
  <|> maybe emptyImage (string styWarn . (" "++) . snd) emk
  where
    inputChar offset c =
        let sty = case emk of
                Just (o,_) | offset == o   -> styInputError
                _                          -> styInput
        in  char sty c

renderMode mode = case mode of
    HexOverwrite   -> title brightBlack  "Hex "
    HexOverwriting -> title brightYellow "Hex*"
    CharOverwrite  -> title brightYellow "Char"
    HexInsert      -> title brightRed    "Hex Ins "
    HexInserting   -> title brightRed    "Hex Ins*"
    CharInsert     -> title brightRed    "Char Ins"
    OffsetInput    -> title white        "Offset"
    MarkInput      -> title green        "Mark"
    ScriptInput    -> title white        "Script"
  where
    title color = string (currentAttr `withForeColor` color)
