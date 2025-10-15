module TableList
  ( Table
  , empty, insert
  , lookup
  ) where

import Prelude hiding (lookup)
import Data.Maybe

data Table k v = Table [(k, v)] deriving Show

empty :: Table k v
empty = Table []

keys :: Table k v -> [k]
keys (Table kvs) = map fst kvs

lookup :: Eq k => k -> Table k v -> Maybe v
lookup key (Table kvs) = case filter ((== key) . fst) kvs of
  []         -> Nothing
  [(_, val)] -> Just val
  _          -> error "lookup: duplicate entries!"

member :: Eq k => k -> Table k v -> Bool
member key = isJust . lookup key

insert :: Eq k => k -> v -> Table k v -> Table k v
insert key val t@(Table kvs) 
  | member key t = t
  | otherwise    = Table ((key, val) : kvs)
