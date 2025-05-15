module TTCConverter.IO

import System
import Data.FilePath
import Data.List1
import Data.String

import TTCConverter.Error

export
escape : Interpolation a => a -> String
escape = escapeArg . interpolate

export
sys : HasIO io => String -> io (Either ConverterError ())
sys cmd = do
  0 <- system cmd
    | code => pure $ Left $ NonZeroExitCode code
  pure $ Right ()

export
mkDir : HasIO io => FilePath -> io (Either ConverterError ())
mkDir ""  = pure $ Right ()
mkDir dir = sys "mkdir -p \{escape dir}"

export
formatJSON : HasIO io => FilePath -> io (Either ConverterError ())
formatJSON file = do
  let tmpFile = file <.> "tmp"
  Right () <- sys "jq . \{escape file} > \{escape tmpFile}"
    | Left err => pure $ Left err
  Right () <- sys "mv \{escape tmpFile} \{escape file}"
    | Left err => pure $ Left err
  pure $ Right ()
