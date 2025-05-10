module Core.Env.JSON

import public Core.Env
import Core.Name.Scoped

import JSON.Encoder
import JSON.ToJSON

import Core.TT.JSON

export
ToJSON (Env Term vs) where
  toJSON {v} = array . toJSONList
    where
    toJSONList : forall vs. Env Term vs -> List v
    toJSONList []        = []
    toJSONList (x :: xs) = toJSON x :: toJSONList xs
