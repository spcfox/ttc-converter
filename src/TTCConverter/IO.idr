module TTCConverter.IO

import System
import Data.String
import Data.List1
import TTCConverter.Error

export
sys : HasIO io => List String -> io (Either ConverterError ())
sys cmd = do
  0 <- system cmd
    | code => pure $ Left $ NonZeroExitCode code
  pure $ Right ()

export
mkDir : HasIO io => String -> io (Either ConverterError ())
mkDir ""  = pure $ Right ()
mkDir dir = sys ["mkdir", "-p", dir]

export
parentDir : String -> String
parentDir = joinBy "/" . init . split ('/' ==)

export
mkParentDir : HasIO io => String -> io (Either ConverterError ())
mkParentDir = mkDir . parentDir