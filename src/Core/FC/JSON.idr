module Core.FC.JSON

import public Core.FC

import JSON.Derive

import Core.Name.JSON
import TTCConverter.Utils

%language ElabReflection

%runElab derive "Core.FC.VirtualIdent" [ToJSON]

export
ToJSON Core.FC.OriginDesc where
  toJSON (PhysicalIdrSrc ident) = jconstructor "PhysicalIdrSrc" [jpair "identifier" ident]
  toJSON (PhysicalPkgSrc fname) = jconstructor "PhysicalPkgSrc" [jpair "filename"   fname]
  toJSON (Virtual ident)        = jconstructor "Virtual"        [jpair "identifier" ident]

export
ToJSON Core.FC.FC where
  toJSON (MkFC file start end)
    = erase FCTag $ jconstructor "MkFC" [ jpair "file" file
                                        , jpair "start" start
                                        , jpair "end" end]
  toJSON (MkVirtualFC file start end)
    = erase FCTag $ jconstructor "MkVirtualFC" [ jpair "file" file
                                               , jpair "start" start
                                               , jpair "end" end]
  toJSON EmptyFC = jconstructor "EmptyFC" []

%runElab derive "Core.FC.WithFC" [ToJSON]
