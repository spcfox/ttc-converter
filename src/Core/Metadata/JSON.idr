module Core.Metadata.JSON

import Prims.JSON
import JSON.Derive

import public Core.Metadata
import Core.Name.JSON

import Core.TT.JSON
import Core.FC.JSON

%language ElabReflection

%runElab derive "Decoration" [ToJSON]

export
ToJSON Metadata where
  toJSON m = object
    [ jpair "lhsApps"              m.lhsApps
    , jpair "names"                m.names
    , jpair "tydecls"              m.tydecls
    , jpair "holeLHS"              m.holeLHS
    , jpair "nameLocMap"           m.nameLocMap
    , jpair "sourceIdent"          m.sourceIdent
    , jpair "semanticHighlighting" m.semanticHighlighting
    , jpair "semanticAliases"      m.semanticAliases
    , jpair "semanticDefaults"     m.semanticDefaults
    ]

%runElab derive "TTMFile" [ToJSON]
