module TTCConverter.Error

import public Core.Core
import public System.File

public export
data ConverterError = EmptyArguments
                    | InvalidOptions
                    | MissingInputFile
                    | CoreError Core.Error
                    | FileError FileError
                    | NonZeroExitCode Int

export
covering
Show ConverterError where
  show EmptyArguments   = "Empty arguments"
  show InvalidOptions   = "Invalid options"
  show MissingInputFile = "Missing input file"
  show (CoreError err)  = "Core error: " ++ show err
  show (FileError err)  = "File error: " ++ show err
  show (NonZeroExitCode code) = "Non-zero exit code: " ++ show code
