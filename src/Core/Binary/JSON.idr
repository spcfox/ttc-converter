module Core.Binary.JSON

import public Core.Binary
import Core.TTC

import JSON.Derive

import Core.Context.JSON
import Core.Name.JSON
import Core.Options.JSON
import Core.TT.JSON
import Prims.JSON
import TTCConverter.Utils

%language ElabReflection

ToJSON Binary where
  toJSON b = object [ jpair "size" b.used
                    , jpair "content" b.buf ]

%runElab derive "TTCFile" [ToJSON]
