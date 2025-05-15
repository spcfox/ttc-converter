module TTCConverter.Main

import TTCConverter.Converter
import TTCConverter.CommandLine
import TTCConverter.IO

import Data.FilePath
import System

showHelp : HasIO io => io ()
showHelp = putStrLn usage

handleTTC : HasIO io => ConvertConfig -> io ()
handleTTC config = do
  Right () <- maybe (pure $ Right ()) mkDir $ parentDir config.output
    | Left err => die $ show err
  Right () <- convert config
    | Left err => die $ show err
  pure ()

handleCommand : HasIO io => Command -> io ()
handleCommand Help             = showHelp
handleCommand (Convert config) = handleTTC config

main : IO ()
main = getCommand >>= either (die . show) handleCommand
