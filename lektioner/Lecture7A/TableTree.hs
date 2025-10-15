module TableTree
  ( -- * Type
    Table
    -- * Construction
  , empty, insert
    -- * Querying
  , lookup
    -- * Conversion
  , fromList, toList
  ) where

import Prelude hiding (lookup)
-- import qualified Data.Tree as T
import Test.QuickCheck

-- data Table k v

-- leaf :: k -> v -> Table k v
-- leaf key val = Node Empty key val Empty

-- t :: Table String Int
-- t = Node (Node (leaf "Anna" 2048) "Dave" 1018 (leaf "John" 1001)) "Koen" 5425 (Node (leaf "Mary" 1013) "Thomas" 1942 (leaf "Örjan" 1024))

-- empty :: Table k v

-- lookup

-- insert :: Ord k => k -> v -> Table k v -> Table k v

-- toList :: Table k v -> [(k, v)]

-- fromList :: Ord k => [(k, v)] -> Table k v

-- keys :: Table k v -> [k]

-- invariant :: Ord k => Table k v -> Bool
-- invariant table = case table of
--   Empty        -> True
--   Node l k _ r -> and [ all (< k) (keys l), all (> k) (keys r)
--                       , invariant l, invariant r]

-- genKeyVal :: Gen (Char, Int)
-- genKeyVal = do
--   k <- choose ('a', 'z')
--   v <- choose (1, 100)
--   return (k, v)

-- genTable :: Gen (Table Char Int)

-- prop_invariant :: Property

-- prop_lookup :: Property

-- printTree :: (Show k, Show v) => Table k v -> IO ()
-- printTree = putStrLn . T.drawTree . toDataTree
--  where
--   toDataTree t = case t of
--     Empty                -> T.Node "X" []
--     Node Empty k v Empty -> T.Node (show (k, v)) []
--     Node l k v r         -> T.Node (show (k, v)) [toDataTree r, toDataTree l]
