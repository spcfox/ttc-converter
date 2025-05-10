module Prims.JSON

import Algebra
import Algebra.SizeChange
import public Data.Buffer

import public Libraries.Data.ANameMap
import public Libraries.Data.PosMap
import public Libraries.Data.SortedMap
import public Libraries.Data.NameMap
import public Libraries.Data.WithDefault

import Core.Name.JSON
import TTCConverter.Utils

export
ToJSON Buffer where
  toJSON = toJSON . unsafePerformIO . bufferData'

export
ToJSON SizeChange where
  toJSON Smaller = jconstructor "Smaller" []
  toJSON Same    = jconstructor "Same"    []
  toJSON Unknown = jconstructor "Unknown" []

export
ToJSON RigCount where
  toJSON = elimSemi (string "0") (string "1") (const $ string "w")

export
ToJSON a => (forall x. ToJSON (b x)) => ToJSON (DPair a b) where
  toJSON (x ** y) = array [toJSON x, toJSON y]

export
ToJSON k => ToJSON v => ToJSON (SortedMap k v) where
  toJSON = toJSON . SortedMap.toList

export
ToJSON a => ToJSON (PosMap a) where
  toJSON = toJSON . toList

export
ToJSON a => ToJSON (NameMap a) where
  toJSON = toJSON . NameMap.toList

export
ToJSON a => ToJSON (ANameMap a) where
  toJSON = toJSON . ANameMap.toList

export
ToJSON a => ToJSON (WithDefault a d) where
  toJSON = onWithDefault (jconstructor "DefaultedValue" [])
                  $ \v => jconstructor "SpecifiedValue" [jpair "value" v]
