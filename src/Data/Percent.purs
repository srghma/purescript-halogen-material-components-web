module Data.Percent (Percent, calcPercent, unsafePercent, toNumber) where

import Protolude
import Unsafe.Coerce (unsafeCoerce)

newtype Percent = Percent Number

derive newtype instance eqPercent :: Eq Percent
derive newtype instance ordPercent :: Ord Percent
derive newtype instance showPercent :: Show Percent

instance boundedPercent :: Bounded Percent where
  top = Percent 1.0
  bottom = Percent 0.0

derive newtype instance semiringPercent :: Semiring Percent
derive newtype instance ringPercent :: Ring Percent
derive newtype instance commutativeRingPercent :: CommutativeRing Percent
derive newtype instance euclideanRingPercent :: EuclideanRing Percent
derive newtype instance divisionRingPercent :: DivisionRing Percent

calcPercent :: Number -> Number -> Number -> Percent
calcPercent min max value =
  if value < min then Percent 0.0
  else if value > max then Percent 1.0
  else Percent $ (value - min) / (max - min)

-- should be from 0.0 to 1.0
unsafePercent :: Number -> Percent
unsafePercent = unsafeCoerce

toNumber :: Percent -> Number
toNumber = unsafeCoerce
