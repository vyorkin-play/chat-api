module Main (main) where

import Chat (chat, runChat)
import Chat.Env (newEnv)
import CLI (usage)
import qualified CLI
import Colog (LogAction, cmapM, defaultFieldMap, fmtRichMessageDefault,
              liftLogIO, logTextStdout, upgradeMessageAction, withLogTextFile)
import Data.Text (Text)
import Options.Applicative (execParser)
import Prelude hiding (log)
import System.Directory (createDirectoryIfMissing)

main :: IO ()
main = do
  options <- execParser usage
  createDirectoryIfMissing True logsDir
  withLogTextFile logPath (start (CLI.port options))
  where
    logPath = logsDir ++ "/chat.log"
    logsDir = "logs"

start :: Int -> LogAction IO Text -> IO ()
start portNumber logTextFile = do
  env <- newEnv portNumber log
  runChat env chat
  where
    logText = logTextStdout <> logTextFile
    logRich = cmapM fmtRichMessageDefault logText
    logFull = upgradeMessageAction defaultFieldMap logRich
    log     = liftLogIO logFull
