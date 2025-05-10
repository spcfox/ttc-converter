module Core.Context.JSON

import public Core.Context
import Libraries.Data.SparseMatrix

import JSON.Derive

import Prims.JSON
import Core.CompileExpr.JSON
import Core.CaseTree.JSON
import Core.Env.JSON
import Core.FC.JSON
import Core.Name.JSON
import Core.TT.JSON
import TTCConverter.Utils

%language ElabReflection

%runElab derive "Core.Context.BuiltinType" [ToJSON]
%runElab derive "SCCall"                   [ToJSON]

export
ToJSON HoleInfo where
  toJSON NotHole        = jconstructor "NotHole"    []
  toJSON (SolvedHole n) = jconstructor "SolvedHole" [jpair "arguments" n]

%runElab derive "PMDefInfo" [ToJSON]
%runElab derive "TypeFlags" [ToJSON]

export
ToJSON Def where
  toJSON None = jconstructor "None" []
  toJSON (PMDef pi args ct _ pats)
    = jconstructor "PMDef" [ jpair "pminfo"    pi
                           , jpair "arguments" args
                           , jpair "caseTree"  ct
                           , jpair "patterns"  pats ]
  toJSON (ExternDef arity) = jconstructor "ExternDef" [jpair "arity" arity]
  toJSON (ForeignDef arity cs)
    = jconstructor "ForeignDef" [ jpair "arity" arity
                                , jpair "conventions" cs ]
  -- Not saved in TTC
  toJSON (Builtin _) = jconstructor "Builtin" []
  toJSON (DCon t arity nt)
    = jconstructor "DCon" [ jpair "tag"        t
                          , jpair "arity"      arity
                          , jpair "newtypeArg" nt ]
  toJSON (TCon t arity parampos detpos f ms datacons dets)
    = jconstructor "TCon" [ jpair "tag"          t
                          , jpair "arity"        arity
                          , jpair "parameters"   parampos
                          , jpair "determining"  detpos
                          , jpair "flags"        f
                          , jpair "mutual"       ms
                          , jpair "constructors" datacons
                          , jpair "detagabbleBy" dets ]
  toJSON (Hole numlocs flags)
    = jconstructor "Hole" [ jpair "numLocals" numlocs
                          , jpair "implbind" (implbind flags) ]
  toJSON (BySearch c depth def)
    = jconstructor "BySearch" [ jpair "count"    c
                              , jpair "depth"    depth
                              , jpair "defining" def ]
  toJSON (Guess guess envb constraints)
    = jconstructor "Guess" [ jpair "guess"       guess
                           , jpair "envbind"     envb
                           , jpair "constraints" constraints]
  toJSON ImpBind = jconstructor "ImpBind" []
  toJSON Delayed = jconstructor "Delayed" []
  toJSON (UniverseLevel u) = jconstructor "UniverseLevel" [jpair "level" u]

export
ToJSON DefFlag where
  toJSON Inline          = jconstructor "Inline"       []
  toJSON NoInline        = jconstructor "NoInline"     []
  toJSON Deprecate       = jconstructor "Deprecate"    []
  toJSON Invertible      = jconstructor "Invertible"   []
  toJSON Overloadable    = jconstructor "Overloadable" []
  toJSON TCInline        = jconstructor "TCInline"     []
  toJSON (SetTotal t)    = jconstructor "SetTotal"     [jpair "totality" t]
  toJSON BlockedHint     = jconstructor "BlockedHint"  []
  toJSON Macro           = jconstructor "Macro"        []
  toJSON (PartialEval _) = jconstructor "PartialEval"  []
  toJSON AllGuarded      = jconstructor "AllGuarded"   []
  toJSON (ConType ci)    = jconstructor "ConType"      [jpair "info" ci]
  toJSON (Identity n)    = jconstructor "Identity"     [jpair "argument" n]

export
ToJSON GlobalDef where
  toJSON gdef
    = object $ [ jpair "compexpr"         gdef.compexpr
               , jpair "refersToRuntimeM" gdef.refersToRuntimeM
               , jpair "location"         gdef.location
               , jpair "multiplicity"     gdef.multiplicity
               , jpair "fullname"         gdef.fullname
               , jpair "refersToM"        gdef.refersToM
               , jpair "definition"       gdef.definition ]
         ++ if isUserName (gdef.fullname)
               then []
               else [ jpair "type"          gdef.type
                    , jpair "eraseArgs"     gdef.eraseArgs
                    , jpair "safeErase"     gdef.safeErase
                    , jpair "specArgs"      gdef.specArgs
                    , jpair "inferrable"    gdef.inferrable
                    , jpair "localVars"     gdef.localVars
                    , jpair "visibility"    gdef.visibility
                    , jpair "totality"      gdef.totality
                    , jpair "isEscapeHatch" gdef.isEscapeHatch
                    , jpair "flags"         gdef.flags
                    , jpair "invertible"    gdef.invertible
                    , jpair "noCycles"      gdef.noCycles
                    , jpair "sizeChange"    gdef.sizeChange ]

export
ToJSON Transform where
  toJSON (MkTransform {vars} n env lhs rhs)
    = jconstructor "MkTransform" [ jpair "vars" vars
                                 , jpair "name" n
                                 , jpair "env"  env
                                 , jpair "lhs"  lhs
                                 , jpair "rhs"  rhs ]
