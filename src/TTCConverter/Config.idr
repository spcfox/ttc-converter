module TTCConverter.Config

public export
data TTCFormat = TTC | TTM | Unknown (Maybe String)

public export
record Config where
  constructor MkConfig
  input : String
  output : String
  format : TTCFormat
