module TTCConverter.JSON.ErasingJSON

import JSON

export
data ErasingJSON' : JSON -> List String -> Type where
  JNull     : ErasingJSON' p es
  JInteger  : Integer -> ErasingJSON' p es
  JDouble   : Double -> ErasingJSON' p es
  JBool     : Bool -> ErasingJSON' p es
  JString   : String -> ErasingJSON' p es
  JArray    : List (ErasingJSON' p es) -> ErasingJSON' p es
  JObject   : List (String, ErasingJSON' p es) -> ErasingJSON' p es
  JErasable : (tag : String) -> ErasingJSON' p es -> ErasingJSON' p es

public export
ErasingJSON : List String -> Type
ErasingJSON = ErasingJSON' $ string "<erased>"

export
toJSON : {p, es : _} ->  ErasingJSON' p es -> JSON
toJSON JNull        = JNull
toJSON (JInteger x) = JInteger x
toJSON (JDouble x)  = JDouble x
toJSON (JBool x)    = JBool x
toJSON (JString x)  = JString x
toJSON (JArray x)   = JArray $ map toJSON x
toJSON (JObject x)  = JObject $ map @{Compose} toJSON x
toJSON (JErasable tag x)
  = if tag `elem` es
       then p
       else toJSON x

export
{p, es : _} -> Encoder (ErasingJSON' p es) where
  stringify = stringify . toJSON
  string    = JString
  double    = JDouble
  integer   = JInteger
  boolean   = JBool
  array     = JArray
  null      = JNull

  object [("_erase", JString tag), ("_value", x)] = JErasable tag x
  object obj = JObject obj
