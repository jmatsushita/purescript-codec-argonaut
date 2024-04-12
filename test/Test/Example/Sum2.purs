module Test.Example.Sum2 where

import Prelude

import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Variant as CAV
import Data.Either (Either(..))
import Data.Profunctor (dimap)
import Data.Variant as V

data SomeValue2 = Str String | Int Int | Neither

codec ∷ CA.JsonCodec SomeValue2
codec =
  dimap toVariant fromVariant $ CAV.variantMatch
    { str: Right CA.string
    , int: Right CA.int
    , neither: Left unit
    }
  where
    toVariant = case _ of
      Str s → V.inj @"str" s
      Int i → V.inj @"int" i
      Neither → V.inj @"neither" unit
    fromVariant = V.match
      { str: Str
      , int: Int
      , neither: \_ → Neither
      }
