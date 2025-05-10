module TTImp.TTImp.JSON

import public TTImp.TTImp

import JSON.Derive

import Core.JSON
import Prims.JSON
import TTCConverter.Utils

%language ElabReflection

export
ToJSON TTImp.TTImp.BindMode where
  toJSON (PI r)   = jconstructor "Pi"       [jpair "count" r]
  toJSON PATTERN  = jconstructor "Pattern"  []
  toJSON COVERAGE = jconstructor "Coverage" []
  toJSON NONE     = jconstructor "None"     []

export
ToJSON TTImp.TTImp.DataOpt where
  toJSON (SearchBy ps) = jconstructor "SearchBy"      [jpair "parameters" ps]
  toJSON NoHints       = jconstructor "NoHints"       []
  toJSON UniqueSearch  = jconstructor "UniqueSearch"  []
  toJSON External      = jconstructor "External"      []
  toJSON NoNewtype     = jconstructor "NoNewtype"     []

export
ToJSON nm => ToJSON (RawImp' nm)

%runElab derive "ImpTy'" [ToJSON]

export
ToJSON nm => ToJSON (ImpClause' nm) where
  toJSON (PatClause fc lhs rhs)
    = jconstructor "PatClause" [ jpair "fc"  fc
                               , jpair "lhs" lhs
                               , jpair "rhs" rhs]
  toJSON (WithClause fc lhs rig wval prf _ cs)
    = jconstructor "WithClause" [ jpair "fc"        fc
                                , jpair "lhs"       lhs
                                , jpair "count"     rig
                                , jpair "scrutinee" wval
                                , jpair "prf"       prf
                                , jpair "clauses"   cs ]
  toJSON (ImpossibleClause fc lhs)
    = jconstructor "ImpossibleClause" [ jpair "fc"  fc
                                      , jpair "lhs" lhs ]

export
ToJSON nm => ToJSON (FnOpt' nm) where
  toJSON Unsafe             = jconstructor "Unsafe"        []
  toJSON Inline             = jconstructor "Inline"        []
  toJSON NoInline           = jconstructor "NoInline"      []
  toJSON Deprecate          = jconstructor "Deprecate"     []
  toJSON TCInline           = jconstructor "TCInline"      []
  toJSON (Hint d)           = jconstructor "Hint"          [jpair "isDirect" d]
  toJSON (GlobalHint f)     = jconstructor "GlobalHint"    [jpair "useAsDefault" f]
  toJSON ExternFn           = jconstructor "ExternFn"      []
  toJSON (ForeignFn cs)     = jconstructor "ForeignFn"     [jpair "conventions" cs]
  toJSON (ForeignExport cs) = jconstructor "ForeignExport" [jpair "conventions" cs]
  toJSON Invertible         = jconstructor "Invertible"    []
  toJSON Macro              = jconstructor "Macro"         []
  toJSON (Totality t)       = jconstructor "Totality"      [jpair "value" t]
  toJSON (SpecArgs args)    = jconstructor "SpecArgs"      [jpair "arguments" args]

%runElab derive "TTImp.TTImp.IClaimData" [ToJSON]

ToJSON nm => ToJSON (ImpData' nm)
ToJSON nm => ToJSON (ImpRecord' nm)

export
ToJSON nm => ToJSON (ImpDecl' nm) where
  toJSON (IClaim claim) = jconstructor "IClaim" [jpair "claim" claim]
  toJSON (IData fc vis mbtot body)
    = jconstructor "IData" [ jpair "fc"         fc
                           , jpair "visibility" vis
                           , jpair "totality"   mbtot
                           , jpair "body"       body ]
  toJSON (IDef fc nm xs)
    = jconstructor "IDef" [ jpair "fc"      fc
                          , jpair "name"    nm
                          , jpair "clauses" xs ]
  toJSON (IParameters fc ps body)
    = jconstructor "IParameters" [ jpair "fc"         fc
                                 , jpair "parameters" ps
                                 , jpair "body"       body ]
  toJSON (IRecord fc ns vis mbtot body)
    = jconstructor "IRecord" [ jpair "fc"              fc
                             , jpair "nestedNamespace" ns
                             , jpair "totality"        mbtot
                             , jpair "body"            body ]
  toJSON (INamespace fc ns body)
    = jconstructor "INamespace" [ jpair "fc"        fc
                                , jpair "namespace" ns
                                , jpair "body"      body ]
  toJSON (ITransform fc nm lhs rhs)
    = jconstructor "ITransform" [ jpair "fc"   fc
                                , jpair "name" nm
                                , jpair "lhs"  lhs
                                , jpair "rhs"  lhs ]
  toJSON (IRunElabDecl fc tm)
    = jconstructor "IRunElabDecl" [ jpair "fc"   fc
                                  , jpair "term" tm ]
  toJSON (ILog topic) = jconstructor "ILog" [ jpair "topic" topic ]
  toJSON (IBuiltin fc type nm)
    = jconstructor "IBuiltin" [ jpair "fc"    fc
                              , jpair "type"  type
                              , jpair "nm"    nm ]
  -- Not saved in TTC
  toJSON (IPragma _ _ f) = jconstructor "IPragma" []
  toJSON (IFail _ _ _)   = jconstructor "IFail"   []

export
ToJSON nm => ToJSON (IFieldUpdate' nm) where
  toJSON (ISetField p val)
    = jconstructor "ISetField" [ jpair "path" p
                               , jpair "value" val ]
  toJSON (ISetFieldApp p f)
    = jconstructor "ISetFieldApp" [ jpair "path" p
                                  , jpair "function" f ]

export
ToJSON nm => ToJSON (AltType' nm) where
  toJSON FirstSuccess        = jconstructor "FirstSuccess" []
  toJSON Unique              = jconstructor "Unique"       []
  toJSON (UniqueDefault val) = jconstructor "UniqueDefault" [jpair "value" val]

export
ToJSON nm => ToJSON (RawImp' nm) where
  toJSON (IVar fc nm)
    = jconstructor "IVar" [ jpair "fc" fc
                          , jpair "name" nm ]
  toJSON (IPi fc r info nm argTy retTy)
    = jconstructor "IPi" [ jpair "fc"           fc
                         , jpair "count"        r
                         , jpair "info"         info
                         , jpair "name"         nm
                         , jpair "argumentType" argTy ]
  toJSON (ILam fc r info nm argTy scope)
    = jconstructor "ILam" [ jpair "fc"           fc
                          , jpair "count"        r
                          , jpair "info"         info
                          , jpair "name"         nm
                          , jpair "argumentType" argTy
                          , jpair "scope"        scope ]
  toJSON (ILet fc lhsFC r nm ty val scope)
    = jconstructor "ILet" [ jpair "fc"    fc
                          , jpair "lhsFC" lhsFC
                          , jpair "count" r
                          , jpair "name"  nm
                          , jpair "value" val
                          , jpair "type"  ty
                          , jpair "scope"  scope ]
  toJSON (ICase fc opts s ty xs)
    = jconstructor "ICase" [ jpair "fc" fc
                           , jpair "options" opts
                           , jpair "scrutinee" s
                           , jpair "scrutineeType" ty
                           , jpair "clauses" xs ]
  toJSON (ILocal fc xs sc)
    = jconstructor "ILocal" [ jpair "fc"    fc
                            , jpair "body"  xs
                            , jpair "scope" sc ]
  -- Saved in TTC without tag. Is this bug?
  toJSON (ICaseLocal fc _ _ _ sc) = jconstructor "ICaseLocal" []
  toJSON (IUpdate fc fs rec)
    = jconstructor "IUpdate" [ jpair "fc"     fc
                             , jpair "fields" fs
                             , jpair "record" rec ]
  toJSON (IApp fc fn arg)
    = jconstructor "IApp" [ jpair "fc"       fc
                          , jpair "function" fn
                          , jpair "argument" arg ]
  toJSON (INamedApp fc fn nm arg)
    = jconstructor "INamedApp" [ jpair "fc"           fc
                               , jpair "function"     fn
                               , jpair "argumentName" nm
                               , jpair "argument"     arg ]
  toJSON (IAutoApp fc fn arg)
    = jconstructor "IAutoApp" [ jpair "fc"       fc
                              , jpair "function" fn
                              , jpair "argument" arg ]
  toJSON (IWithApp fc fn arg)
    = jconstructor "IWithApp" [ jpair "fc"       fc
                              , jpair "function" fn
                              , jpair "argument" arg ]
  toJSON (ISearch fc depth)
    = jconstructor "ISearch" [ jpair "fc"    fc
                             , jpair "depth" depth ]
  toJSON (IAlternative fc ty xs)
    = jconstructor "IAlternative" [ jpair "fc"          fc
                                  , jpair "type"        ty
                                  , jpair "alternative" xs ]
  toJSON (IRewrite fc x y)
    = jconstructor "IRewrite" [ jpair "fc"   fc
                              , jpair "from" x
                              , jpair "to"   y ]
  toJSON (ICoerced fc tm)
    = jconstructor "ICoerced" [ jpair "fc"   fc
                              , jpair "term" tm ]
  toJSON (IBindHere fc m tm)
    = jconstructor "IBindHere" [ jpair "fc"   fc
                               , jpair "mode" m
                               , jpair "term" tm ]
  toJSON (IBindVar fc nm)
    = jconstructor "IBindVar" [ jpair "fc"   fc
                              , jpair "name" nm ]
  toJSON (IAs fc nameFC s nm pattern)
    = jconstructor "IAs" [ jpair "fc"      fc
                         , jpair "nameFC"  nameFC
                         , jpair "useSide" s
                         , jpair "name"    nm
                         , jpair "pattern" pattern ]
  toJSON (IMustUnify fc _ pattern)
    = jconstructor "IMustUnify" [ jpair "fc"      fc
                                , jpair "pattern" pattern ]
  toJSON (IDelayed fc r tm)
    = jconstructor "IDelayed" [ jpair "fc"         fc
                              , jpair "lazyReason" r
                              , jpair "term"       tm ]
  toJSON (IDelay fc tm)
    = jconstructor "IDelay" [ jpair "fc"   fc
                            , jpair "term" tm ]
  toJSON (IForce fc tm)
    = jconstructor "IForce" [ jpair "fc"   fc
                            , jpair "term" tm ]
  toJSON (IQuote fc tm)
    = jconstructor "IQuote" [ jpair "fc"   fc
                            , jpair "term" tm ]
  toJSON (IQuoteName fc nm)
    = jconstructor "IQuoteName" [ jpair "fc"   fc
                                , jpair "name" nm ]
  toJSON (IQuoteDecl fc ds)
    = jconstructor "IQuoteDecl" [ jpair "fc"           fc
                                , jpair "declarations" ds ]
  toJSON (IUnquote fc tm)
    = jconstructor "IUnquote" [ jpair "fc"   fc
                              , jpair "term" tm ]
  toJSON (IRunElab fc re tm)
    = jconstructor "IRunElab" [ jpair "fc"               fc
                              , jpair "requireExtension" re
                              , jpair "term"             tm ]
  toJSON (IPrimVal fc val)
    = jconstructor "IPrimVal" [ jpair "fc"    fc
                              , jpair "value" val ]
  toJSON (IType fc) = jconstructor "IType" [ jpair "fc" fc ]
  toJSON (IHole fc nm)
    = jconstructor "IHole" [ jpair "fc" fc
                           , jpair "name" nm ]
  toJSON (IUnifyLog fc lvl x) = jconstructor "IUnifyLog" []
  toJSON (Implicit fc b)
    = jconstructor "Implicit" [ jpair "fc" fc
                              , jpair "bindIfUnsolved" b ]
  toJSON (IWithUnambigNames fc ns rhs)
    = jconstructor "IWithUnambigNames" [ jpair "fc" fc
                                       , jpair "names" ns
                                       , jpair "rhs"   rhs ]

ToJSON nm => ToJSON (IField' nm) where
  toJSON (MkIField fc c p n ty)
    = object [ jpair "fc" fc
             , jpair "count" c
             , jpair "info" p
             , jpair "name" n
             , jpair "type" ty ]

export
ToJSON nm => ToJSON (ImpData' nm) where
  toJSON (MkImpData fc nm tycon opts cons)
    = jconstructor "MkImpData" [ jpair "fc"              fc
                               , jpair "name"            nm
                               , jpair "typeConstructor" tycon
                               , jpair "options"         opts
                               , jpair "constructors"    cons ]
  toJSON (MkImpLater fc nm tycon)
    = jconstructor "MkImpLater" [ jpair "fc"              fc
                                , jpair "name"            nm
                                , jpair "typeConstructor" tycon ]

export
ToJSON nm => ToJSON (ImpRecord' nm) where
  toJSON (MkImpRecord fc nm ps con opts fs)
    = object [ jpair "fc" fc
             , jpair "name" nm
             , jpair "parameters" ps
             , jpair "options" opts
             , jpair "constructorName" con
             , jpair "fields" fs ]