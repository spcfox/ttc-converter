module TTCConverter.Config

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
record Config where
  constructor MkConfig
  input : String
  output : String
  format : TTCFormat
  erase : List EraseTag
