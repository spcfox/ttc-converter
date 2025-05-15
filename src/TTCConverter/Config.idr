module TTCConverter.Config

import Data.FilePath

public export
data EraseTag = FCTag
         | MNTag
         | BufferTag

export
Show EraseTag where
  show FCTag     = "FCTag"
  show MNTag     = "MNTag"
  show BufferTag = "BufferTag"

export
Eq EraseTag where
  FCTag     == FCTag     = True
  MNTag     == MNTag     = True
  BufferTag == BufferTag = True
  _         == _         = False

public export
data TTCFormat = TTC | TTM | Unknown (Maybe String)

public export
record ConvertConfig where
  constructor MkConvertConfig
  input  : FilePath
  output : FilePath
  format : TTCFormat
  erase  : List EraseTag
  jq     : Bool

public export
data Command = Help
             | Convert ConvertConfig
