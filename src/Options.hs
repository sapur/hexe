module Options (
    Options (..),
    parseOptions,
) where

import Options.Applicative


data Options = Options
    { optFilename :: String
    , optCursor   :: Int
    , optMarks    :: [Int]
    }
    deriving (Show, Read)


parseOptions = execParser
    $ info (helper <*> options)
    $ fullDesc
   <> header "hexe - A Hex Editor"

options = Options
      <$> strArgument ( metavar "FILE" <> action "file"
            <> help "File to open. Need not exist.")
      <*> option auto ( short 'c' <> long "cursor" <> hidden
            <> metavar "OFFSET" <> value 0
            <> help "Moves the cursor after opening the file.")
      <*> option marksReader ( short 'm' <> long "marks" <> hidden
            <> metavar "MARKS" <> value []
            <> help "List of offsets to mark. Format: 0,0x3")


marksReader = eitherReader $ \arg -> case reads ("[" ++ arg ++ "]") of
    [(r, "")] -> return r
    _         -> Left $ "cannot parse offset list"
