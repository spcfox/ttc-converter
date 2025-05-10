module TTCConverter.Main

import TTCConverter.Converter
import TTCConverter.CommandLine
import TTCConverter.IO

import System

main : IO ()
main = do
  Right config <- getConfig
    | Left err => die $ show err
  Right () <- mkParentDir $ config.output
    | Left err => die $ show err
  Right () <- convert config
    | Left err => die $ show err
  pure ()
