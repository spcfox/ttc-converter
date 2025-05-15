module TTCConverter.CommandLine

import Data.FilePath
import Data.Either
import Data.List
import System
import System.GetOpts

import public TTCConverter.Config
import public TTCConverter.Error

%default total

record Options where
  constructor MkOptions
  help   : Bool
  input  : Maybe String
  output : Maybe String
  format : Maybe TTCFormat
  erase  : List EraseTag
  jq     : Bool

initOptions : Options
initOptions = MkOptions
  { help   = False
  , input  = Nothing
  , output = Nothing
  , format = Nothing
  , erase  = []
  , jq     = False }

data Option = Help
            | Input String
            | Output String
            | Format TTCFormat
            | Erase EraseTag
            | JQ

applyOption : Option -> Options -> Options
applyOption Help           = { help   := True }
applyOption (Input fname)  = { input  := Just fname }
applyOption (Output fname) = { output := Just fname }
applyOption (Format fmt)   = { format := Just fmt }
applyOption (Erase tag)    = { erase  $= (tag ::) }
applyOption JQ             = { jq     := True }

applyOptions : List Option -> Options
applyOptions = foldl (flip applyOption) initOptions

predictFormat : FilePath -> TTCFormat
predictFormat fname = case extension fname of
  Just "ttc" => TTC
  Just "ttm" => TTM
  ext        => Unknown $ interpolate <$> ext

parseFile : String -> Either ConverterError FilePath
parseFile fname = maybeToEither (InvalidFilePath fname) $ parse fname

checkConvertConfig : Options -> Either ConverterError ConvertConfig
checkConvertConfig config = do
  input <- case config.input of
    Just f  => parseFile f
    Nothing => Left MissingInputFile
  output <- case config.output of
    Just f  => parseFile f
    Nothing => pure $ input <.> "json"
  let format = case config.format of
                  Nothing => predictFormat input
                  Just f  => f
  let erase = nub config.erase
  pure $ MkConvertConfig input output format erase config.jq

checkConfig : Options -> Either ConverterError Command
checkConfig config =
  if config.help
     then pure Help
     else Convert <$> checkConvertConfig config

descs : List $ OptDescr Option
descs =
  [ MkOpt ['?', 'h'] ["help"]     (NoArg Help)              "Show help"
  , MkOpt ['i']      ["input"]    (ReqArg Input  "<file>")  "Input file name"
  , MkOpt ['o']      ["output"]   (ReqArg Output "<file>")  "Output file name"
  , MkOpt []         ["ttc"]      (NoArg $ Format TTC)      "TTC format"
  , MkOpt []         ["ttm"]      (NoArg $ Format TTM)      "TTM format"
  , MkOpt []         ["erase-fc"] (NoArg $ Erase FCTag)     "Do not write FC in JSON"
  , MkOpt []         ["erase-mn"] (NoArg $ Erase MNTag)     "Do not write MN indexes in JSON"
  , MkOpt []         ["erase-bf"] (NoArg $ Erase BufferTag) "Do not write buffer data in JSON"
  , MkOpt []         ["jq"]       (NoArg $ JQ)              "Use jq to format JSON"
  ]

export
usage : String
usage = usageInfo "Usage: ttc-converter [options] <file>\n\nOptions:" descs

export
getCommand : HasIO io => io (Either ConverterError Command)
getCommand = do
  (_ :: args) <- getArgs
    | [] => pure $ Left EmptyArguments
  let (MkResult opts [] [] []) = getOpt (ReturnInOrder Input) descs args
    | _ => pure $ Left InvalidOptions
  let config = applyOptions opts
  pure $ checkConfig config
