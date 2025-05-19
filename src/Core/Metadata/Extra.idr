module Core.Metadata.Extra

import Core.Context
import Core.Core
import Core.Binary
import public Core.Metadata

import Data.FilePath
import System.File

export
readTTM : FilePath -> Core TTMFile
readTTM file = do
  Right buffer <- coreLift $ readFromFile $ interpolate file
    | Left err => throw (InternalError ("\{file}: \{show err}"))
  bin <- newRef Prims.Bin buffer
  fromBuf