module TTCConverter.Utils

import public JSON.Encoder
import public JSON.ToJSON

import public TTCConverter.Config

export
constructorField : String
constructorField = "_constructor"

export
jconstructor : Encoder v => (constructorName : String) -> List (String, v) -> v
jconstructor con []     = string con
jconstructor con fields = object (jpair constructorField con :: fields)

export
ToJSON EraseTag where
  toJSON = string . show

export
erase : Encoder v => EraseTag -> v -> v
erase tag x = object [jpair "_erase" tag, ("_value", x)]

export
jerase : ToJSON a => Encoder v => EraseTag -> a -> v
jerase tag x = object [jpair "_erase" tag, jpair "_value" x]

export
jepair : ToJSON a => Encoder v => EraseTag -> String -> a -> (String, v)
jepair tag key val = (key, jerase tag val)