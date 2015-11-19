module Model where

import Prelude
import Data.List

type Ranking = List Entry

data Entry = Entry
  { name :: String
  , value :: Int
  }


instance showEntry :: Show Entry where
  show (Entry x) = x.name ++ ":" ++ show x.value

instance eqEntry :: Eq Entry where
  eq (Entry x) (Entry y) = x.value == y.value

instance ordEntry :: Ord Entry where
  compare (Entry x) (Entry y) = compare x.value y.value



ascending :: Ranking -> Ranking
ascending = sort

descending :: Ranking -> Ranking
descending = reverse
