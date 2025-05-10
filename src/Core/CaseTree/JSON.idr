module Core.CaseTree.JSON

import public Core.Case.CaseTree
import JSON.Encoder
import JSON.ToJSON

import Core.Name.JSON
import Core.TT.JSON
import TTCConverter.Utils

ToJSON (CaseAlt vs)

export
ToJSON (CaseTree vs) where
  toJSON (Case {name} idx sc scTy alts)
      = jconstructor "Case" [ jpair "name"         name
                            , jpair "idx"          idx
                            , jpair "alternatives" alts ]
  toJSON (STerm _ tm)    = jconstructor "STerm" [jpair "term" tm]
  toJSON (Unmatched msg) = jconstructor "Unmatched" [jpair "message" msg]
  toJSON Impossible      = jconstructor "Impossible" []

export
ToJSON (CaseAlt vs) where
  toJSON (ConCase x n args rhs)
      = jconstructor"DelayCase" [ jpair "name"      x
                                , jpair "n"         n
                                , jpair "arguments" args
                                , jpair "rhs"       rhs ]
  toJSON (DelayCase ty arg rhs)
      = jconstructor "DelayCase" [ jpair "type"     ty
                                 , jpair "argument" arg
                                 , jpair "rhs"      rhs ]
  toJSON (ConstCase val rhs)
      = jconstructor "ConstCase" [ jpair "value" val
                                 , jpair "rhs"   rhs ]
  toJSON (DefaultCase rhs) = jconstructor "DefaultCase" [jpair "rhs" rhs]
