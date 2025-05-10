module Core.CompileExpr.JSON

import public Core.CompileExpr

import JSON.Derive

import Core.FC.JSON
import Core.Name.JSON
import Core.TT.JSON
import TTCConverter.Utils

%language ElabReflection

%runElab derive "Core.CompileExpr.InlineOk" [ToJSON]

export
ToJSON ConInfo where
  toJSON DATACON  = jconstructor "DATACON" []
  toJSON TYCON    = jconstructor "TYCON"   []
  toJSON NIL      = jconstructor "NIL"     []
  toJSON CONS     = jconstructor "CONS"    []
  toJSON (ENUM n) = jconstructor "ENUM"    [jpair "id" n]
  toJSON NOTHING  = jconstructor "NOTHING" []
  toJSON JUST     = jconstructor "JUST"    []
  toJSON RECORD   = jconstructor "RECORD"  []
  toJSON ZERO     = jconstructor "ZERO"    []
  toJSON SUCC     = jconstructor "SUCC"    []
  toJSON UNIT     = jconstructor "UNIT"    []

ToJSON (CExp vs)

export
ToJSON (CConAlt vs) where
  toJSON (MkConAlt nm ci t as sc)
    = object [ jpair "name"            nm
             , jpair "constructorInfo" ci
             , jpair "tag"             t
             , jpair "arguments"       as
             , jpair "scope"           sc ]

export
ToJSON (CConstAlt vs) where
  toJSON (MkConstAlt c sc)
    = object [ jpair "constructor" c
             , jpair "scope" sc ]

export
ToJSON (CExp vs) where
  toJSON (CLocal {idx} fc _)
    = jconstructor "CLocal" [ jpair "idx" idx
                            , jpair "fc"  fc ]
  toJSON (CRef fc nm) =
    jconstructor "CRef" [ jpair "fc"   fc
                        , jpair "name" nm ]
  toJSON (CLam fc nm sc) =
    jconstructor "CLam" [ jpair "fc"    fc
                        , jpair "name"  nm
                        , jpair "scope" sc ]
  toJSON (CLet fc nm inl val sc) =
    jconstructor "CLet" [ jpair "fc"     fc
                        , jpair "name"   nm
                        , jpair "inline" inl
                        , jpair "value"  val
                        , jpair "scope"  sc ]
  toJSON (CApp fc fn args) =
    jconstructor "CApp" [ jpair "fc"        fc
                        , jpair "function"  fn
                        , jpair "arguments" args ]
  toJSON (CCon fc nm ci t args) =
    jconstructor "CCon" [ jpair "fc"              fc
                        , jpair "name"            nm
                        , jpair "constructorInfo" ci
                        , jpair "tag"             t
                        , jpair "arguments"       args ]
  toJSON (COp {arity} fc fn args) =
    jconstructor "COp" [ jpair "arity"     arity
                       , jpair "fc"        fc
                       , jpair "function"  fn
                       , jpair "arguments" args ]
  toJSON (CExtPrim fc nm args) =
    jconstructor "CExtPrim" [ jpair "fc"    fc
                            , jpair "name"  nm
                            , jpair "args"  args ]
  toJSON (CForce fc lr sc) =
    jconstructor "CForce" [ jpair "fc"         fc
                          , jpair "lazyReason" lr
                          , jpair "scope"      sc ]
  toJSON (CDelay fc lr sc) =
    jconstructor "CDelay" [ jpair "fc"         fc
                          , jpair "lazyReason" lr
                          , jpair "scope"      sc ]
  toJSON (CConCase fc sc alts def) =
    jconstructor "CConCase" [ jpair "fc"           fc
                            , jpair "scope"        sc
                            , jpair "alternatives" alts
                            , jpair "def"          def ]
  toJSON (CConstCase fc sc alts def) =
    jconstructor "CConstCase" [ jpair "fc"           fc
                              , jpair "scope"        sc
                              , jpair "alternatives" alts
                              , jpair "def"          def ]
  toJSON (CPrimVal fc c) =
    jconstructor "CPrimVal" [ jpair "fc"    fc
                            , jpair "value" c ]
  toJSON (CErased fc) = jconstructor "CErased" [ jpair "fc" fc ]
  toJSON (CCrash fc msg) =
    jconstructor "CCrash" [ jpair "fc"      fc
                          , jpair "message" msg ]

export
ToJSON CFType where
  toJSON CFUnit       = jconstructor "CFUnit"       []
  toJSON CFInt        = jconstructor "CFInt"        []
  toJSON CFInteger    = jconstructor "CFInteger"    []
  toJSON CFInt8       = jconstructor "CFInt8"       []
  toJSON CFInt16      = jconstructor "CFInt16"      []
  toJSON CFInt32      = jconstructor "CFInt32"      []
  toJSON CFInt64      = jconstructor "CFInt64"      []
  toJSON CFUnsigned8  = jconstructor "CFUnsigned8"  []
  toJSON CFUnsigned16 = jconstructor "CFUnsigned16" []
  toJSON CFUnsigned32 = jconstructor "CFUnsigned32" []
  toJSON CFUnsigned64 = jconstructor "CFUnsigned64" []
  toJSON CFString     = jconstructor "CFString"     []
  toJSON CFDouble     = jconstructor "CFDouble"     []
  toJSON CFChar       = jconstructor "CFChar"       []
  toJSON CFPtr        = jconstructor "CFPtr"        []
  toJSON CFGCPtr      = jconstructor "CFGCPtr"      []
  toJSON CFBuffer     = jconstructor "CFBuffer"     []
  toJSON CFForeignObj = jconstructor "CFForeignObj" []
  toJSON CFWorld      = jconstructor "CFWorld"      []
  toJSON (CFFun x y)
    = jconstructor "CFFun" [ jpair "argument"   x
                           , jpair "returnType" y ]
  toJSON (CFIORes x) = jconstructor "CFIORes" [jpair "result" x]
  toJSON (CFStruct nm xs)
    = jconstructor "CFStruct" [ jpair "name"   nm
                              , jpair "fields" xs ]
  toJSON (CFUser nm xs)
    = jconstructor "CFUser" [ jpair "name"      nm
                            , jpair "arguments" xs ]

export
ToJSON CDef where
  toJSON (MkFun args expr)
    = jconstructor "MkFun" [ jpair "arguments"  args
                           , jpair "returnType" expr ]
  toJSON (MkCon t arity pos)
    = jconstructor "MkCon" [ jpair "tag"             t
                           , jpair "arity"           arity
                           , jpair "newtypePosition" pos ]
  toJSON (MkForeign cs args ret)
    = jconstructor "MkForeign" [ jpair "bindings"   cs
                               , jpair "arguments"  args
                               , jpair "returnType" ret ]
  toJSON (MkError cexpr) = jconstructor "MkError" [jpair "error" cexpr]
