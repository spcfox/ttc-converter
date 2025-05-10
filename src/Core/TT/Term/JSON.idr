module Core.TT.Term.JSON

import public Core.TT

import JSON.Derive

import Core.FC.JSON
import Core.Name.JSON
import Core.TT.Primitive.JSON
import Prims.JSON
import TTCConverter.Utils

%language ElabReflection

%runElab derive "Core.TT.PartialReason"   [ToJSON]
%runElab derive "Core.TT.Terminating"     [ToJSON]
%runElab derive "Core.TT.TotalReq"        [ToJSON]
%runElab derive "Core.TT.Term.LazyReason" [ToJSON]
%runElab derive "Core.TT.Term.UseSide"    [ToJSON]
%runElab derive "Core.TT.Visibility"      [ToJSON]

export
ToJSON a => ToJSON (Core.TT.Binder.PiInfo a) where
  toJSON Implicit        = jconstructor "Implicit"     []
  toJSON Explicit        = jconstructor "Explicit"     []
  toJSON AutoImplicit    = jconstructor "AutoImplicit" []
  toJSON (DefImplicit x) = jconstructor "DefImplicit"  [jpair "value" x]

export
ToJSON Core.TT.Term.NameType where
  toJSON Bound = jconstructor "Bound" []
  toJSON Func  = jconstructor "Func"  []
  toJSON (DataCon t a)
    = jconstructor "DataCon" [ jpair "tag" t
                             , jpair "arity" a ]
  toJSON (TyCon t a)
    = jconstructor "TyCon" [ jpair "tag" t
                           , jpair "arity" a ]

export
ToJSON a => ToJSON (Binder a) where
  toJSON (Lam _ c x ty)
    = jconstructor "Lam" [ jpair "count"  c
                         , jpair "piInfo" x
                         , jpair "type"   ty ]
  toJSON (Let _ c val _)
    = jconstructor "Let" [ jpair "count" c
                         , jpair "value" val ]
  toJSON (Pi _ c x ty)
    = jconstructor "Pi"  [ jpair "count"  c
                         , jpair "piInfo" x
                         , jpair "type"   ty ]
  toJSON (PVar _ c x ty)
    = jconstructor "PVar" [ jpair "count"  c
                          , jpair "piInfo" x
                          , jpair "type"   ty ]
  toJSON (PLet _ c val _)
    = jconstructor "PLet" [ jpair "count" c
                          , jpair "value" val ]
  toJSON (PVTy _ c _)   = jconstructor "PVTy" [ jpair "count" c ]

export
ToJSON (Term vs) where
  toJSON (Local _ c idx _)
    = jconstructor "Local" [ jpair "isLet" c
                           , jpair "index" idx ]
  toJSON (Ref _ nt nm)
    = jconstructor "Ref" [ jpair "nameType" nt
                         , jpair "name"     nm ]
  toJSON (Meta _ nm _ sc)
    = jconstructor "Meta" [ jpair "name"  nm
                          , jpair "scope" sc ]
  toJSON (Bind _ nm bnd sc)
    = jconstructor "Bind" [ jpair "name"   nm
                          , jpair "binder" bnd
                          , jpair "scope"  sc ]
  toJSON (App _ fn arg)
    = jconstructor "App" [ jpair "function" fn
                         , jpair "argument" arg ]
  toJSON (As _ s as tm)
    = jconstructor "As" [ jpair "as"      as
                        , jpair "useSide" s
                        , jpair "term"    tm ]
  toJSON (TDelayed _ r tm)
    = jconstructor "TDelayed" [ jpair "lazyReason" r
                              , jpair "term"       tm ]
  toJSON (TDelay _ r ty tm)
    = jconstructor "TDelay" [ jpair "lazyReason" r
                            , jpair "type"       ty
                            , jpair "term"       tm ]
  toJSON (TForce _ r tm)
    = jconstructor "TForce" [ jpair "lazyReason" r
                            , jpair "term"       tm ]
  toJSON (PrimVal _ val) = jconstructor "TForce" [jpair "value" val]
  toJSON (Erased _ _)    = jconstructor "Erased" []
  toJSON (TType _ u)     = jconstructor "TType"  [jpair "universe" u]

%runElab derive "Core.TT.Covering" [ToJSON]
%runElab derive "Core.TT.Totality" [ToJSON]
