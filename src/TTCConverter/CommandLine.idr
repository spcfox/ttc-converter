module TTCConverter.CommandLine

import Data.List
import System
import System.Path
import System.GetOpts

import public TTCConverter.Config
import public TTCConverter.Error

%default total

record PreConfig where
  constructor MkPreConfig
  input  : Maybe String
  output : Maybe String
  format : Maybe TTCFormat
  erase  : List EraseTag
  jq     : Bool

initPreConfig : PreConfig
initPreConfig = MkPreConfig Nothing Nothing Nothing [] False

data Option = Input String
            | Output String
            | Format TTCFormat
            | Erase EraseTag
            | JQ

applyOption : Option -> PreConfig -> PreConfig
applyOption (Input fname)  = { input  := Just fname }
applyOption (Output fname) = { output := Just fname }
applyOption (Format fmt)   = { format := Just fmt }
applyOption (Erase tag)    = { erase  $= (tag ::) }
applyOption JQ             = { jq     := True }

createPreConfig : List Option -> PreConfig
createPreConfig = foldl (flip applyOption) initPreConfig

predictFormat : String -> TTCFormat
predictFormat fname = case extension fname of
  Just "ttc" => TTC
  Just "ttm" => TTM
  ext        => Unknown ext

checkConfig : PreConfig -> Either ConverterError Config
checkConfig config = do
  let Just input = config.input
    | Nothing => Left MissingInputFile
  let output = case config.output of
                  Nothing => input ++ ".json"
                  Just o  => o
  let format = case config.format of
                  Nothing => predictFormat input
                  Just f  => f
  let erase = nub config.erase
  pure $ MkConfig input output format erase config.jq

descs : List $ OptDescr Option
descs =
  [ MkOpt ['i'] ["input"]    (ReqArg Input  "<file>")  "Input file name"
  , MkOpt ['o'] ["output"]   (ReqArg Output "<file>")  "Output file name"
  , MkOpt []    ["ttc"]      (NoArg $ Format TTC)      "TTC format"
  , MkOpt []    ["ttm"]      (NoArg $ Format TTM)      "TTM format"
  , MkOpt []    ["erase-fc"] (NoArg $ Erase FCTag)     "Do not write FC in JSON"
  , MkOpt []    ["erase-mn"] (NoArg $ Erase MNTag)     "Do not write MN indexes in JSON"
  , MkOpt []    ["erase-bf"] (NoArg $ Erase BufferTag) "Do not write buffer data in JSON"
  , MkOpt []    ["jq"]       (NoArg $ JQ)              "Use jq to format JSON"
  ]

export
getConfig : HasIO io => io (Either ConverterError Config)
getConfig = do
  (_ :: args) <- getArgs
    | [] => pure $ Left EmptyArguments
  let (MkResult opts [] [] []) = getOpt (ReturnInOrder Input) descs args
    | _ => pure $ Left InvalidOptions
  let config = createPreConfig opts
  pure $ checkConfig config
