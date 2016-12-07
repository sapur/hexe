module Options (
    Options (..),
    Action (..),
    parseOptions,
) where

import Options.Applicative
import Text.Printf

import Data.Version
import Paths_hexe


data Options = Options
    { optAction     :: Action
    , optColumnWdt  :: Maybe Int
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
            [ metavar "FILE"
            , action "file"
            , help "File to open. Need not exist."
            ]))
    <*> optional (option auto (mconcat
            [ withshort 'w' "column-width"
            , metavar "N"
            , help "Set the initial column width to N."
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
            , metavar "FILE"
            , help "Load script from FILE and execute it."
            ]))

noshort       l = hidden <> long l
withshort  s  l = hidden <> long l <> short s
defaultvar nm v = metavar nm <> value v


namedMarkReader = eitherReader $ \arg ->
    let (offset, text) = break (== '=') arg
    in  case reads offset of
            [(n, "")] -> return (n, drop 1 text)
            _         -> Left $ "cannot parse mark: " ++ arg

marksReader = eitherReader $ \arg -> case reads ("[" ++ arg ++ "]") of
    [(r, "")] -> return r
    _         -> Left "cannot parse offset list"


versionText = unlines
    [ printf "hexe %s" (showVersion version)
    ]
