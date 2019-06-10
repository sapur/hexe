module Command.Parser (
    parseScript,
    parseValue, parseUnitValue, parseDirection,
    getInlineError,
    script,
    command,
) where

import Control.Monad

import Data.Char
import Data.List
import Graphics.Vty.Input.Events
import Text.Parsec hiding (token)
import Text.Parsec.Error
import Text.Parsec.String

import Command.Data
import Helpers
import Keymap.Data.Name


parseScript = parse script

parseValue         = parse $ pValue <* eof
parseUnitValue def = parse $ pUnitValue def <* eof
parseDirection     = parse $ direction <* eof

getInlineError err = (col, msg)  where
    col            = sourceColumn (errorPos err) - 1
    msg            = "expected: " ++ options
    options        = intercalate ", " $ concatMap fmt $ errorMessages err
    fmt (Expect w) = [w]
    fmt _          = []

script :: Parser Script
script = do
    stmts    <- many (try statement)
    lastStmt <- lastStatement
    eof
    return $ concat $ stmts ++ [lastStmt]

statement     = skipSpaces >> commands <* endOfLine
lastStatement = skipSpaces >> commands <* (void endOfLine <|> eof)
commands      = command `sepBy` charTok ';'
bindCommands  = command `sepBy` charTok ','

command
    = do name "quit"
         b <- option False (strTok "force" *>= True)
         return (Quit b)
  <|> name "refresh"      *>= Refresh
  <|> name "set-mode"     *>  (SetMode <$> inputMode)
  <|> name "store"        *>= Store
  <|> name "undo"         *>= JumpHistory   Bw
  <|> name "redo"         *>= JumpHistory   Fw
  <|> name "cursor"       *>  (SetCursor <$> pUnitValue Char)
  <|> name "scroll"       *>  (SetScroll <$> pUnitValue Line)
  <|> name "column-width" *>  (SetColumnWdt <$> pValue)
  <|> name "256colors"    *>  (Set256Colors <$> switch)
  <|> name "mark-here"    *>  (SetMark <$> switch)
  <|> name "mark"         *>  (SetNamedMark <$> pUnitValue Char <*> strLit)
  <|> name "jump-mark"    *>  (JumpMark <$> direction)
  <|> name "delete"       *>  (Delete <$> direction)
  <|> name "slice"        *>  (Slice <$> direction)
  <|> name "commit"       *>= CommitInput
  <|> name "cancel"       *>= CancelInput
  <|> name "feed"         *>  (Feed <$> charLit)
  <|> name "bind"         *>  (Bind <$> keymapName <*> event <*> bindCommands)


event = choice
    [ try $ do mods <- pModifiers
               key  <- pKey
               return $ EvKey key mods
    , (\k -> EvKey k []) <$> pKey
    ]
  where
    pKey          = specialKey <|> charKey
    specialKey    = choice
        [ name "Esc"       *>= KEsc
        , name "BS"        *>= KBS
        , name "Enter"     *>= KEnter
        , name "Left"      *>= KLeft
        , name "Right"     *>= KRight
        , name "Up"        *>= KUp
        , name "Down"      *>= KDown
        , name "UpLeft"    *>= KUpLeft
        , name "UpRight"   *>= KUpRight
        , name "DownLeft"  *>= KDownLeft
        , name "DownRight" *>= KDownRight
        , name "Center"    *>= KCenter
        , name "BackTab"   *>= KBackTab
        , name "PrtScr"    *>= KPrtScr
        , name "Pause"     *>= KPause
        , name "Ins"       *>= KIns
        , name "Home"      *>= KHome
        , name "PageUp"    *>= KPageUp
        , name "Del"       *>= KDel
        , name "End"       *>= KEnd
        , name "PageDown"  *>= KPageDown
        , name "Begin"     *>= KBegin
        , name "Menu"      *>= KMenu
        , name "Tab"       *>= KChar '\t'
        , name "Space"     *>= KChar ' '
        , name "Hash"      *>= KChar '#'
        , char 'F'         >>  KFun <$> natLit
        ]
    charKey       = token "key" $ KChar <$> satisfy isPrint

    pModifiers    = many mod <* char '-'
    mod           = modFromChar <$> oneOf "SCMA"
    modFromChar c = case c of
        'S' -> MShift
        'C' -> MCtrl
        'M' -> MMeta
        'A' -> MAlt

keymapName = choice
    [ name "hex-nav"   *>= HexNavKeys
    , name "line-nav"  *>= LineNavKeys
    , name "hex-over"  *>= HexOverKeys
    , name "char-over" *>= CharOverKeys
    , name "hex-ins"   *>= HexInsKeys
    , name "char-ins"  *>= CharInsKeys
    ]


pUnitValue def = token "value-with-unit" $ choice
    [ try $ pUnit pValue
    , try $ pValue >>= \val -> return $ case val of
        Abs  _ -> def val
        Rel  _ -> def val
        Frac _ -> def val
    , def . Abs <$> natLit
    ]

pValue = token "value" p  where
    p  = try (optional tEqual *> (Frac . (/ 100) <$> floatLit) <* tPcnt)
     <|> tEqual *> (Abs          <$> natLit)
     <|> tPlus  *> (Rel          <$> natLit)
     <|> tMinus *> (Rel . negate <$> natLit)

pUnit p = compound "unit" $ do
    unit <- anyOf
        [ try tChar *>= Char
        , tLine     *>= Line
        , tPage     *>= Page
        , tInLine   *>= InLine
        , tInPage   *>= InPage
        ]
    val  <- p
    return (unit val)

switch = compound "switch"
    $ anyOf [try tOn *>= On, tOff *>= Off, tToggle *>= Toggle]

direction = compound "direction"
    $ anyOf [tBw *>= Bw, tFw *>= Fw]

inputMode = compound "input-mode"
    $ name "hex-over"     *>= HexOverwrite
  <|> name "char-over"    *>= CharOverwrite
  <|> name "hex-ins"      *>= HexInsert
  <|> name "char-ins"     *>= CharInsert
  <|> name "offset-input" *>= OffsetInput
  <|> name "mark-input"   *>= MarkInput


tPlus  = char '+'
tMinus = char '-'
tEqual = char '='
tPcnt  = char '%'

tChar   = string "char"
tLine   = string "line"
tPage   = string "page"
tInLine = string "col"
tInPage = string "in-page"

tOn     = strTok "on"
tOff    = strTok "off"
tToggle = strTok "toggle"

tBw = charTok '<'
tFw = charTok '>'

name str = try (strTok str)

charTok c = token ("'"  ++ [c] ++ "'" ) (char   c)
strTok  s = compound ("\"" ++  s  ++ "\"") $ do
   string s
   notFollowedBy alphaNum
   skipSpaces

charLit = token "character"
    $ char '\'' >> anyChar <* char '\''

strLit = token "string"
       $ between (char '"') (char '"')
       $ many $ noneOf "\"\r\n"

natLit :: (Read a, Num a) => Parser a
natLit = token "natural-number" $ choice
    [ try (string "0x") >> fromBase 16 hexDigit
    , try (string "0o") >> fromBase 16 octDigit
    , try (string "0b") >> fromBase  2 (oneOf "01")
    , fromBase 10 digit
    ]

fromBase base digit = do
    digits <- map conv . reverse <$> many1 digit
    let (n,_) = foldl (\(total,fac) d -> (total + d*fac, fac*base))
                      (0,1) digits
    return n
  where
    [c0,c9,cA,cZ,ca,cz] = map ord "09AZaz"
    conv = int . convW . ord
    convW w = case () of
        _ | w >= c0 && w <= c9 ->  0 + w-c0
        _ | w >= cA && w <= cZ -> 10 + w-cA
        _ | w >= ca && w <= cz -> 10 + w-ca

floatLit :: Parser Float
floatLit = token "floating-point-number" $ do
    a <- digits
    b <- option "0" (char '.' *> digits)
    return $ read $ concat [a, ".", b]


compound, token :: String -> Parser a -> Parser a
compound desc p = p <?> desc
token    desc p = (p <* skipSpaces) <?> desc

skipSpaces = skipMany (ws <|> lineCmt)
ws         = compound "whitespace" $ void $ oneOf " \t"
lineCmt    = compound "line-comment" $ void $ do
    char '#'
    many (noneOf "\r\n")

anyOf  = foldl1 (<|>)
digits = many1 digit


p *>= v = p *> return v
