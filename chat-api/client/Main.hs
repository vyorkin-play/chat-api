module Main (main) where

import Control.Concurrent (forkIO)
import Control.Monad (forever, unless, void)
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Network.Socket (withSocketsDo)
import qualified Network.WebSockets as WS

app :: WS.ClientApp ()
app conn = do
  putStrLn "connected"
  void $ forkIO $ forever $ do
    msg <- WS.receiveData conn
    liftIO $ Text.putStrLn msg
  loop conn
  WS.sendClose conn ("bye!" :: Text)

loop :: WS.ClientApp ()
loop conn = do
  line <- Text.getLine
  unless (Text.null line) $ WS.sendTextData conn line
  loop conn

main :: IO ()
main = withSocketsDo $ WS.runClient "127.0.0.1" 9160 "/" app
