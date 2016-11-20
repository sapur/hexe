{-# LANGUAGE RecordWildCards #-}
module Editor.Render (
    renderView, renderByte
) where

import Data.Char
import Data.List.Extra
import Data.Maybe
import Data.Maybe
import Data.Word
import Text.Printf

import           Graphics.Vty hiding (update, Style)
import qualified Graphics.Vty as Vty

import Helpers
import Editor.Types
import Editor.Style

import           Buffer (ModState)
import qualified Buffer as Buf


renderView ed = do
    let vty = edVty ed
        img = render ed
    Vty.update vty (picForImage img)

render ed = result  where
    scrollOff = edScroll ed
    cells     = geoCells (edGeo ed)
    bytes     = Buf.viewRange scrollOff cells (edBuffer ed)
    cols      = geoCols (edGeo ed)
    colMul    = edColMul ed
    effColMul = if   colMul >= 2 && colMul <= cols
                then colMul
                else maxBound

    chunks          = chunksOf cols bytes
    hexLines        = zipWith hexLine [0..] chunks
    hexLine line ch = renderLine (scrollOff + line * cols)
                                 (edCursor ed) (edPending ed)
                                 cols effColMul (edStyle ed) ch

    hexView    = vertCat hexLines
    statusView = renderStatus ed

    result     = hexView <-> statusView

renderLine
    :: Int -> Int -> Maybe InputEditor -> Int -> Int -> Style
    -> [(Word8, ModState, Bool)]
    -> Image
renderLine offset cursor pending perLine colMul sty bytes = horizCat
    [ string styOffset (printf "%08X  " offset)
    , horizCat $ zipWith (renderHex cursor pending colMul sty) [offset..] bytes
    , string stySpace (replicate (3*padding) ' ')
    , string stySpace " "
    , horizCat $ zipWith (renderByte cursor pending sty) [offset..] bytes
    , string stySpace (replicate padding ' ')
    ]
  where
    Style{..} = sty
    padding   = perLine - length bytes

renderHex cursor pending colMul sty offset (b,st,mk)
    = let Style{..} = sty
          str       = case (cursor == offset, pending) of
              (True, Just inp@InputEditor{ inpImm = True })
                -> printf "%-2s" (inpText inp)
              _ -> hexByte b
      in  string (styleByte cursor pending offset st mk sty b) str
      <|> if   offset `mod` colMul /= colMul-1
          then char stySpace ' '
          else string stySpace "  "

hexByte b = [toDigit ms, toDigit ls]  where
    (ms,ls) = (b`div`16, b`mod`16)
    toDigit n | n < 10 = chr (int n      + ord '0')
    toDigit n          = chr (int n - 10 + ord 'A')

renderByte cursor pending sty offset (b,st,mk) =
    let c = chr (int b)
    in  char (styleByte cursor pending offset st mk sty b)
             (if isPrint c then c else subst c)
  where
    subst c = case c of
        '\NUL' -> '.'
        '\ESC' -> '@'
        '\r'   -> 'R'
        '\n'   -> '/'
        '\t'   -> '>'
        '\b'   -> '<'
        _      -> '+'

styleByte cursor pending offset st mk sty b = style  where
    Style{..} = sty
    c         = chr (int b)
    baseStyle =
        if   mk
        then styMark
        else case st of
                Buf.Alt         -> styAlt
                Buf.Ins         -> styIns
                Buf.Slack       -> stySlack
                _ | c == '\NUL' -> styNull
                _ | isPrint c   -> styPrint
                _               -> styNonPrint
    isImm = case pending of
        Just inp | inpImm inp -> True
        _                     -> False
    style | cursor-offset /= 0 = baseStyle
          | isImm              = baseStyle   `withStyle` reverseVideo
          | otherwise          = styChanging `withStyle` reverseVideo

renderStatus ed = final  where
    Style{..} = edStyle ed
    wdt       = geoWidth (edGeo ed)
    pos       = (edCursor ed + 2) * 100 `div` (Buf.length (edBuffer ed) + 1)
    left      = string styOffset (printf "%08x  " (edCursor ed))
            <|> case edPending ed of
                    Just inp@InputEditor{ inpImm = False }
                      -> input (inpCursor inp) (inpText inp ++ " ")
                    _ -> fromMaybe (edInfo ed) (edMessage ed)
    input c s = string styInput (take c s)
            <|> string (styInput `withStyle` reverseVideo) [s !! c]
            <|> string styInput (drop (c+1) s ++ " ")
    right     = string stySpace "  "
            <|> kmName (edKeymap ed)
            <|> (if   Buf.isModified (edBuffer ed)
                 then string styAlt " * "
                 else string stySpace " ")
            <|> string styNotice (printf " %3d%%" pos)
    final     = resizeWidth (wdt - imageWidth right) left <|> right
