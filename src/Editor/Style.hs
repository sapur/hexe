module Editor.Style (
    Style (..),
    color16, color256,
    renderString,
    subst
) where

import Data.Char

import Graphics.Vty hiding (Style)


data Style = Style
    { stySpace      :: Attr
    , styOffset     :: Attr

    , styNull       :: Attr
    , styMark       :: Attr
    , styPrint      :: Attr
    , styNonPrint   :: Attr
    , styChanging   :: Attr
    , styAlt        :: Attr
    , styIns        :: Attr
    , stySlack      :: Attr
    , styInput      :: Attr
    , styInputError :: Attr

    , styInfo       :: Attr
    , styNotice     :: Attr
    , styEmph       :: Attr
    , styWarn       :: Attr
    }


color16 = style  where
    base  = clearStyle currentAttr `withBackColor` black
    style = Style
        { stySpace      = base
        , styOffset     = base `withForeColor` brightWhite

        , styNull       = base `withForeColor` cyan
        , styMark       = base `withBackColor` green `withForeColor` brightWhite
        , styPrint      = base `withForeColor` brightWhite
        , styNonPrint   = base `withForeColor` cyan
        , styChanging   = base `withForeColor` yellow
        , styAlt        = base `withForeColor` yellow `withStyle` bold
        , styIns        = base `withForeColor` red `withStyle` bold
        , stySlack      = base `withForeColor` red
        , styInput      = base `withForeColor` brightWhite
        , styInputError = styInput style `withBackColor` red

        , styInfo       = base `withForeColor` white
        , styNotice     = base
        , styEmph       = base `withForeColor` brightYellow
        , styWarn       = base `withForeColor` brightRed
        }

color256 = style  where
    base  = clearStyle currentAttr `withBackColor` ISOColor 235
    style = Style
        { stySpace      = base
        , styOffset     = base `withForeColor` brightWhite

        , styNull       = base `withForeColor` ISOColor 240
        , styMark       = base `withBackColor` green `withForeColor` brightWhite
        , styPrint      = base `withForeColor` brightWhite
        , styNonPrint   = base `withForeColor` cyan
        , styChanging   = base `withForeColor` yellow
        , styAlt        = base `withForeColor` yellow `withStyle` bold
        , styIns        = base `withForeColor` red `withStyle` bold
        , stySlack      = base `withForeColor` red
        , styInput      = base `withForeColor` brightWhite
        , styInputError = styInput style `withBackColor` red

        , styInfo       = base `withForeColor` brightBlack
        , styNotice     = base
        , styEmph       = base `withForeColor` brightYellow
        , styWarn       = base `withForeColor` brightRed
        }


clearStyle attr = attr{ attrStyle = SetTo defaultStyleMask }


renderString sty = horizCat . map render  where
    render ch = let style = if isPrint ch then styPrint else styNonPrint
                in  char (style sty) ch


subst c = case c of
    '\NUL' -> '.'
    '\ESC' -> '@'
    '\r'   -> 'R'
    '\n'   -> '/'
    '\t'   -> '>'
    '\b'   -> '<'
    _      -> '+'
