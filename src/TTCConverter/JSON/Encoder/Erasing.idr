module TTCConverter.JSON.Encoder.Erasing

import public JSON.Encoder

parameters (placeholder : JSON) (erasing : List String)
  export
  [Erasing'] Encoder JSON where
    stringify = show
    string    = JString
    double    = JDouble
    integer   = JInteger
    boolean   = JBool
    array     = JArray
    null      = JNull

    object [("_erase", JString tag), ("_value", val)]
      = if tag `elem` erasing
           then placeholder
           else val
    object obj = JObject obj

export
Erasing : List String -> Encoder JSON
Erasing = Erasing' $ string "<erased>"
