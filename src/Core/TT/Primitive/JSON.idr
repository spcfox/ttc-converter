module Core.TT.Primitive.JSON

import public Core.TT.Primitive
import JSON.Derive
import TTCConverter.Utils

%language ElabReflection

%runElab derive "Core.TT.Primitive.PrimType" [ToJSON]

export
ToJSON Core.TT.Primitive.Constant where
  toJSON (I i)    = jconstructor "I"        [jpair "value" i]
  toJSON (I8 i)   = jconstructor "I8"       [jpair "value" i]
  toJSON (I16 i)  = jconstructor "I16"      [jpair "value" i]
  toJSON (I32 i)  = jconstructor "I32"      [jpair "value" i]
  toJSON (I64 i)  = jconstructor "I64"      [jpair "value" i]
  toJSON (BI i)   = jconstructor "BI"       [jpair "value" i]
  toJSON (B8 i)   = jconstructor "B8"       [jpair "value" i]
  toJSON (B16 i)  = jconstructor "B16"      [jpair "value" i]
  toJSON (B32 i)  = jconstructor "B32"      [jpair "value" i]
  toJSON (B64 i)  = jconstructor "B64"      [jpair "value" i]
  toJSON (Str s)  = jconstructor "Str"      [jpair "value" s]
  toJSON (Ch c)   = jconstructor "Ch"       [jpair "value" c]
  toJSON (Db d)   = jconstructor "Db"       [jpair "value" d]
  toJSON (PrT t)  = jconstructor "PrT"      [jpair "value" t]
  toJSON WorldVal = jconstructor "WorldVal" []

export
ToJSON (PrimFn n) where
  toJSON (Add ty)      = jconstructor "Add"           [jpair "type" ty]
  toJSON (Sub ty)      = jconstructor "Sub"           [jpair "type" ty]
  toJSON (Mul ty)      = jconstructor "Mul"           [jpair "type" ty]
  toJSON (Div ty)      = jconstructor "Div"           [jpair "type" ty]
  toJSON (Mod ty)      = jconstructor "Mod"           [jpair "type" ty]
  toJSON (Neg ty)      = jconstructor "Neg"           [jpair "type" ty]
  toJSON (ShiftL ty)   = jconstructor "ShifL"         [jpair "type" ty]
  toJSON (ShiftR ty)   = jconstructor "ShiftR"        [jpair "type" ty]
  toJSON (BAnd ty)     = jconstructor "BAnd"          [jpair "type" ty]
  toJSON (BOr ty)      = jconstructor "BOr"           [jpair "type" ty]
  toJSON (BXOr ty)     = jconstructor "BXOr"          [jpair "type" ty]
  toJSON (LT ty)       = jconstructor "LT"            [jpair "type" ty]
  toJSON (LTE ty)      = jconstructor "LTE"           [jpair "type" ty]
  toJSON (EQ ty)       = jconstructor "EQ"            [jpair "type" ty]
  toJSON (GTE ty)      = jconstructor "GTE"           [jpair "type" ty]
  toJSON (GT ty)       = jconstructor "GT"            [jpair "type" ty]
  toJSON StrLength     = jconstructor "StrLength"     []
  toJSON StrHead       = jconstructor "StrHead"       []
  toJSON StrTail       = jconstructor "StrTail"       []
  toJSON StrIndex      = jconstructor "StrIndex"      []
  toJSON StrCons       = jconstructor "StrCons"       []
  toJSON StrAppend     = jconstructor "StrAppend"     []
  toJSON StrReverse    = jconstructor "StrReverse"    []
  toJSON StrSubstr     = jconstructor "StrSubstr"     []
  toJSON DoubleExp     = jconstructor "DoubleExp"     []
  toJSON DoubleLog     = jconstructor "DoubleLog"     []
  toJSON DoublePow     = jconstructor "DoublePow"     []
  toJSON DoubleSin     = jconstructor "DoubleSin"     []
  toJSON DoubleCos     = jconstructor "DoubleCos"     []
  toJSON DoubleTan     = jconstructor "DoubleTan"     []
  toJSON DoubleASin    = jconstructor "DoubleASin"    []
  toJSON DoubleACos    = jconstructor "DoubleACos"    []
  toJSON DoubleATan    = jconstructor "DoubleATan"    []
  toJSON DoubleSqrt    = jconstructor "DoubleSqrt"    []
  toJSON DoubleFloor   = jconstructor "DoubleFloor"   []
  toJSON DoubleCeiling = jconstructor "DoubleCeiling" []
  toJSON BelieveMe     = jconstructor "BelieveMe"     []
  toJSON Crash         = jconstructor "Crash"         []
  toJSON (Cast x y)    = jconstructor "Cast"          [ jpair "from" x
                                                      , jpair "to" y ]