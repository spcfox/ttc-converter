module Core.Options.JSON

import public Core.Options
import Core.Name.JSON
import JSON.Derive
import TTCConverter.Utils

%language ElabReflection

%runElab derive "PairNames"    [ToJSON]
%runElab derive "RewriteNames" [ToJSON]
%runElab derive "PrimNames"    [ToJSON]

export
ToJSON CG where
  toJSON Chez         = jconstructor "Chez"         []
  toJSON ChezSep      = jconstructor "ChezSep"      []
  toJSON Racket       = jconstructor "Racket"       []
  toJSON Gambit       = jconstructor "Gambit"       []
  toJSON Node         = jconstructor "Node"         []
  toJSON Javascript   = jconstructor "Javascript"   []
  toJSON RefC         = jconstructor "RefC"         []
  toJSON VMCodeInterp = jconstructor "VMCodeInterp" []
  toJSON (Other name) = jconstructor "Other"        [jpair "name" name]
