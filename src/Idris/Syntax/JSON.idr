module Idris.Syntax.JSON

import public Idris.Syntax

import JSON.Derive

import Core.JSON
import Prims.JSON
import TTImp.TTImp.JSON
import TTCConverter.Utils

%language ElabReflection

%runElab derive "Import"          [ToJSON]
%runElab derive "BindingModifier" [ToJSON]
%runElab derive "Fixity"          [ToJSON]
%runElab derive "FixityInfo"      [ToJSON]
%runElab derive "Method"          [ToJSON]
%runElab derive "IFaceInfo"       [ToJSON]

export
ToJSON SyntaxInfo where
  toJSON syn = object
    [ jpair "fixities"      syn.fixities
    , jpair "modDocstrings" syn.modDocstrings
    , jpair "modDocexports" syn.modDocexports
    , jpair "ifaces"        syn.ifaces
    , jpair "defDocstrings" syn.defDocstrings
    , jpair "bracketholes"  syn.bracketholes
    , jpair "startExpr"     syn.startExpr
    , jpair "holeNames"     syn.holeNames
    ]