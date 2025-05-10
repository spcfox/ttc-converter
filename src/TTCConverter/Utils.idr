module TTCConverter.Utils

import public JSON.Encoder
import public JSON.ToJSON

export
constructorField : String
constructorField = "_constructor"

export
jconstructor : Encoder v => (constructorName : String) -> List (String, v) -> v
jconstructor con []     = string con
jconstructor con fields = object (jpair constructorField con :: fields)

export
[Erased] ToJSON a where
  toJSON _ = string "<erased>"
