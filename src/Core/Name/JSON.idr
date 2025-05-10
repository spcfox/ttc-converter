module Core.Name.JSON

import public Core.Name
import TTCConverter.Utils

export
ToJSON Namespace where
  toJSON = toJSON . unsafeUnfoldNamespace

export
ToJSON ModuleIdent where
  toJSON = toJSON . unsafeUnfoldModuleIdent

export
ToJSON UserName where
  toJSON (Basic val) = jconstructor "Basic" [ jpair "value" val ]
  toJSON (Field val) = jconstructor "Field" [ jpair "value" val ]
  toJSON Underscore  = jconstructor "Underscore" []

export
ToJSON Name where
  toJSON (NS ns nm) =
    jconstructor "NS" [ jpair "namespace" ns
                      , jpair "name"      nm ]
  toJSON (UN un) = jconstructor "UN" [jpair "value"     un ]
  toJSON (MN str i)
    = jconstructor "MN" [ jpair "str" str
                        , jpair "i"   i ]
  toJSON (PV nm i)
    = jconstructor "PV" [ jpair "name"  nm
                        , jpair "index" i ]
  toJSON (DN d nm)
    = jconstructor "DN" [ jpair "display" d
                        , jpair "name"    nm ]
  toJSON (Nested (outer, idx) nm)
    = jconstructor "Nested" [ jpair "outer" outer
                            , jpair "index" idx
                            , jpair "name"  nm ]
  toJSON (CaseBlock outer nm)
    = jconstructor "CaseBlock" [ jpair "outer" outer
                               , jpair "name"  nm ]
  toJSON (WithBlock outer nm)
    = jconstructor "WithBlock" [ jpair "outer" outer
                               , jpair "name"  nm ]
  -- Not saved in TTC
  toJSON (Resolved i) = jconstructor "Resolved" [jpair "id" i]
