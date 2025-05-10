module TTCConverter.Converter

import Idris.Syntax.TTC

import Core.Binary.Extra
import Core.Metadata.Extra
import Core.Metadata.JSON
import Idris.Syntax.JSON
import TTCConverter.Config
import TTCConverter.Error
import TTCConverter.JSON.ErasingJSON

import System

public export
0 Converter : Type
Converter = forall io. HasIO io => (erase : List String) -> (input, output : String) -> io (Either ConverterError ())

export
mkConverter : ToJSON a => (String -> Core a) -> Converter
mkConverter read erase input output
  = liftIO $ coreRun (read input) (pure . Left . CoreError) $ \val =>
      do Right () <- writeJSONvia (ErasingJSON erase) output val
           | Left err => pure $ Left $ FileError err
         pure $ Right ()

export
convertTTC : Converter
convertTTC = mkConverter $ readTTC {extra=SyntaxInfo}

export
convertTTM : Converter
convertTTM = mkConverter readTTM

export
convert : HasIO io => Config -> io (Either ConverterError ())
convert (MkConfig input output format erase) = do
  converter <- case format of TTC => pure convertTTC
                              TTM => pure convertTTM
                              Unknown ext => do -- TODO: Show warning
                                                pure convertTTC
  converter (map show erase) input output
