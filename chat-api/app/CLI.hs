-- | Describes CLI application options.

module CLI
  ( Options(..)
  , parser
  , usage
  ) where

import Options.Applicative

-- | CLI config.
newtype Options = Options
  { port :: Int
  } deriving (Show)

parser :: Parser Options
parser = Options <$> option auto
  (  long "port"
  <> short 'p'
  <> metavar "PORT"
  <> value 9160
  <> showDefault
  <> help "Port number"
  )

-- | Describes what the program does.
-- | To be displayed in the help screen.
usage :: ParserInfo Options
usage = info (parser <**> helper)
  (  fullDesc
  <> progDesc "Simple chat API"
  <> header "chat - websocket multi-user chat API"
  )
