module TTCConverter.Converter

import Idris.Syntax.TTC

import Core.Binary.Extra
import Core.Metadata.Extra
import Core.Metadata.JSON
import Idris.Syntax.JSON
import TTCConverter.Config
import TTCConverter.Error
import TTCConverter.IO
import TTCConverter.JSON.Encoder.Erasing

import Data.FilePath
import System

public export
0 Converter : Type
Converter = forall io. HasIO io => (erase : List String) -> (input, output : FilePath) -> io (Either ConverterError ())

export
mkConverter : ToJSON a => (FilePath -> Core a) -> Converter
mkConverter read erase input output
  = liftIO $ coreRun (read input) (pure . Left . CoreError) $ \val =>
      do Right () <- writeJSON (Erasing erase) output val
           | Left err => pure $ Left $ FileError err
         pure $ Right ()

export
convertTTC : Converter
convertTTC = mkConverter $ readTTC {extra=SyntaxInfo}

export
convertTTM : Converter
convertTTM = mkConverter readTTM

export
convert : HasIO io => ConvertConfig -> io (Either ConverterError ())
convert config = do
  converter <- case config.format of TTC => pure convertTTC
                                     TTM => pure convertTTM
                                     Unknown ext => do -- TODO: Show warning
                                                       pure convertTTC
  Right () : Either _ () <- converter (map show config.erase) config.input config.output
    | Left err => pure $ Left err
  if config.jq
     then formatJSON config.output
     else pure $ Right ()
