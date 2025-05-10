module Core.Metadata.Extra

import Core.Context
import Core.Core
import Core.Binary
import public Core.Metadata

import System.File

export
readTTM : String -> Core TTMFile
readTTM file = do
  Right buffer <- coreLift $ readFromFile file
    | Left err => throw (InternalError (file ++ ": " ++ show err))
  bin <- newRef Prims.Bin buffer
  fromBuf bin