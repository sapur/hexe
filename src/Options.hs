module Options (
    Options (..),
    parseOptions,
) where

import Options.Applicative
import Text.Printf

import Data.Version
import Paths_hexe


data Options
    = PrintVersion
    | ListKeymap
    | ListBindings
    | Options
        { optFilename   :: String
        , optColumnWdt  :: Int
        , optCursor     :: Int
        , optNamedMarks :: [(Int, String)]
        , optMarks      :: [Int]
        }
    deriving (Show, Read)


parseOptions = customExecParser prefs
    $ info (helper <*> options)
    $ fullDesc
   <> header (printf "hexe %s - A Hex Editor " (showVersion version))
  where
    prefs = defaultPrefs
        { prefDisambiguate = True
        }

options
    = flag' PrintVersion (mconcat
        [ withshort 'V' "version"
        , help "Print version number."
        ])
  <|> flag' ListKeymap (mconcat
        [ noshort "list-keymap"
        , help "List key bindings by input mode."
        ])
  <|> flag' ListBindings (mconcat
        [ noshort "list-bindings"
        , help "List key bindings by category."
        ])
  <|> Options
        <$> strArgument (mconcat
                [ metavar "FILE"
                , action "file"
                , help "File to open. Need not exist."
                ])
        <*> option auto (mconcat
                [ withshort 'w' "column-width"
                , defaultvar "N" 0
                , help "Set the initial column width to N."
                ])
        <*> option auto (mconcat
                [ withshort 'c' "cursor"
                , defaultvar "OFFSET" 0
                , help "Moves the cursor after opening the file."
                ])
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
