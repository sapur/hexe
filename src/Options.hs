{-# LANGUAGE NoMonomorphismRestriction #-}
module Options (
    Options (..),
    Action (..),
    parseOptions,
) where

import Data.Char
import Data.List
import Text.Printf

import Options.Applicative

import qualified Options.Applicative.Help     as Doc

import Data.Version
import Paths_hexe


data Options = Options
    { optAction     :: Action
    , optColumnWdt  :: Maybe Int
    , opt256Colors  :: Maybe Bool
    , optCursor     :: Maybe Int
    , optNamedMarks :: [(Int, String)]
    , optMarks      :: [Int]
    , optCommands   :: [String]
    , optScripts    :: [String]
    }
    deriving (Show, Read)

data Action
    = Edit String
    | ListKeymap
    | ListBindings
    deriving (Show, Read)


parseOptions = customExecParser prefs
    $ info (helper <*> infoOptions <*> options)
    $ fullDesc
   <> header (printf "hexe %s - A Hex Editor " (showVersion version))
   <> footerDoc description
  where
    prefs = defaultPrefs
        { prefDisambiguate = True
        }

infoOptions
    = infoOption versionText (mconcat
        [ withshort 'V' "version"
        , help "Print version number."
        ])

options = Options
    <$> ( flag' ListKeymap (mconcat
            [ long "list-keymap"
            , help "List key bindings by input mode."
            ])
      <|> flag' ListBindings (mconcat
            [ long "list-bindings"
            , help "List key bindings by category."
            ])
      <|> argument (Edit <$> str) (mconcat
            [ filevar
            , help "File to open. Need not exist."
            ]))
    <*> optional (option auto (mconcat
            [ withshort 'w' "column-width"
            , metavar "N"
            , help "Set the initial column width to N."
            ]))
    <*> optional (option boolReader (mconcat
            [ withshort 'C' "256-colors"
            , boolvar
            , help "Switch between 16 [n] and 256 colors [y] palette."
            ]))
    <*> optional (option auto (mconcat
            [ withshort 'c' "cursor"
            , metavar "OFFSET"
            , help "Moves the cursor after opening the file."
            ]))
    <*> many (option namedMarkReader (mconcat
            [ withshort 'M' "mark"
            , metavar "OFFSET[=TEXT]"
            , help "Place a single mark with optional text."
            ]))
    <*> option marksReader (mconcat
            [ withshort 'm' "marks"
            , defaultvar "MARKS" []
            , help "List of offsets to mark. Format: 0,0x3"
            ])
    <*> many (strOption (mconcat
            [ noshort "cmd"
            , metavar "CMD"
            , help "Execute CMD on startup."
            ]))
    <*> many (strOption (mconcat
            [ noshort "script"
            , filevar
            , help "Load script from FILE and execute it."
            ]))

noshort       l = hidden <> long l
withshort  s  l = hidden <> long l <> short s
defaultvar nm v = metavar nm <> value v
boolvar         = metavar "Y/N" <> boolCompleter
filevar         = metavar "FILE" <> action "file"


namedMarkReader = eitherReader $ \arg ->
    let (offset, text) = break (== '=') arg
    in  case reads offset of
            [(n, "")] -> return (n, drop 1 text)
            _         -> Left $ printf "cannot parse mark '%s'" arg

marksReader = eitherReader $ \arg -> case reads ("[" ++ arg ++ "]") of
    [(r, "")] -> return r
    _         -> Left "cannot parse offset list"


trueVals  = ["y", "yes", "t", "true" , "1", "on" ]
falseVals = ["n", "no" , "f", "false", "0", "off"]

boolReader = eitherReader $ \arg ->
    let val = map toLower arg
    in  case () of
            _ | val `elem` trueVals  -> return True
            _ | val `elem` falseVals -> return False
            _ -> Left $ printf "expected 'y' or 'n', but got '%s'" arg

boolCompleter = completeWith $ trueVals ++ falseVals


description = Doc.unChunk $ Doc.vsepChunks
    [ Doc.vcatChunks
      [ Doc.paragraph "hexe can generate a bash completion script. To enable completion in the current shell, run:"
      , Doc.stringChunk "  "
      , Doc.stringChunk "  source <(hexe --bash-completion-script `command -v hexe`)"
      ]
    , Doc.paragraph "Note: hexe will automatically detect how many colors the terminal supports from the environment variable TERM. However, for compatibility reasons, terminals often announce less features than they have. To enable full colors, either set TERM to a proper value, e.g. `xterm-256color', or force 256 color mode via command-line option `-C', by pressing capital `T' in the editor, or adding `256colors on' to the configuration file."
    ]


versionText = intercalate "\n"
    [ printf "hexe %s" (showVersion version)
    ]
