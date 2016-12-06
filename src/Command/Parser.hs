module Command.Parser (
    parseScript,
    script,
    command,
) where

import Control.Monad

import Text.Parsec        hiding (token)
import Text.Parsec.String

import Command.Data


parseScript = parse script

script :: Parser Script
script = do
    stmts    <- many (try statement)
    lastStmt <- lastStatement
    eof
    return $ concat $ stmts ++ [lastStmt]

statement     = skipSpaces >> commands <* endOfLine
lastStatement = skipSpaces >> commands <* (void endOfLine <|> eof)
commands      = command `sepBy` charTok ';'

command
    = do name "quit"
         b <- option False (strTok "force" *>= True)
         return (Quit b)
  <|> name "refresh"      *>= Refresh
  <|> name "set-mode"     *>  (SetMode <$> inputMode)
  <|> name "store"        *>= Store
  <|> name "undo"         *>= JumpHistory   Bw
  <|> name "redo"         *>= JumpHistory   Fw
  <|> name "cursor"       *>  (SetCursor <$> pUnit pValue)
  <|> name "scroll"       *>  (SetScroll <$> pUnit pValue)
  <|> name "column-width" *>  (SetColumnWdt <$> pValue)
  <|> name "256colors"    *>  (Set256Colors <$> switch)
  <|> name "mark"         *>  (SetMark <$> switch)
  <|> name "jump-mark"    *>  (JumpMark <$> direction)
  <|> name "delete"       *>  (Delete <$> direction)
  <|> name "commit"       *>= CommitInput
  <|> name "cancel"       *>= CancelInput
  <|> name "feed"         *>  (Feed <$> charLit)


pValue = token "value" p  where
    p  = try (tEqual *> (Frac <$> frac) <* tPcnt)
     <|> tEqual *> (Abs            <$> word)
     <|> tPlus  *> (Rel            <$> word)
     <|> tMinus *> ((Rel . negate) <$> word)

    word :: Read a => Parser a
    word = read <$> digits

    frac :: Parser Float
    frac = do
        a <- digits
        b <- option "0" (char '.' *> digits)
        return $ read $ concat [a, ".", b]

    digits = many1 digit

pUnit p = compound "unit" $ do
    unit <- anyOf
        [ tChar *>= Char
        , tWord *>= Word
        , tLine *>= Line
        , tPage *>= Page
        , tFile *>= File
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

tChar = string "char"
tWord = string "word"
tLine = string "line"
tPage = string "page"
tFile = string "file"

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


compound, token :: String -> Parser a -> Parser a
compound desc p = p <?> desc
token    desc p = (p <* skipSpaces) <?> desc

skipSpaces = skipMany (ws <|> lineCmt)
ws         = compound "whitespace" $ void $ oneOf " \t"
lineCmt    = compound "line-comment" $ void $ do
    char '#'
    many (noneOf "\r\n")

anyOf = foldl1 (<|>)

p *>= v = p *> return v
