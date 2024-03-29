-- | This module defines a `Decimal` data type for arbitrary precision numbers.
module Data.Decimal
  ( Decimal(..)
  , fromString
  , fromInt
  , fromNumber
  , toNumber
  , toString
  , toPrecision
  , toFixed
  , isFinite
  , isInteger
  , toSignificantDigits
  , abs
  , acos
  , acosh
  , acot
  , acoth
  , acsc
  , acsch
  , asec
  , asech
  , asin
  , asinh
  , atan
  , atan2
  , atanh
  , ceil
  , clamp
  , cos
  , cosh
  , cot
  , coth
  , csc
  , csch
  , exp
  , floor
  , ln
  , log2
  , log10
  , max
  , min
  , modulo
  , pow
  , round
  , sec
  , sech
  , sin
  , sinh
  , sqrt
  , tan
  , tanh
  , truncated
  , e
  , pi
  , gamma
  , factorial
  ) where

import Prelude

import Data.Maybe (Maybe(..))

-- | An arbitrary precision number.
foreign import data Decimal ∷ Type

-- | Convert an integer to a Decimal.
foreign import fromInt ∷ Int → Decimal

-- | Convert a number to a Decimal.
foreign import fromNumber ∷ Number → Decimal

-- | Converts a Decimal to a Number, possibly resulting in loss of precision.
foreign import toNumber ∷ Decimal → Number

foreign import fromStringImpl ∷ ∀ a. Maybe a → (a → Maybe a) → String → Maybe Decimal

-- | Parse a string into a `Decimal`, assuming a decimal representation. Returns
-- | `Nothing` if the parse fails.
-- |
-- | Examples:
-- | ```purescript
-- | fromString "42.001"
-- | fromString "857981209301293808359384092830482"
-- | fromString "1e100"
-- | fromString "0.12301928309128183579487598149533"
-- | ```
fromString ∷ String → Maybe Decimal
fromString = fromStringImpl Nothing Just

foreign import dEquals ∷ Decimal → Decimal → Boolean

instance Eq Decimal where
  eq = dEquals

foreign import dCompare ∷ Decimal → Decimal → Int

instance Ord Decimal where
  compare x y =
    case dCompare x y of
      1 → GT
      0 → EQ
      _ → LT

-- | A representation of the `Decimal` as a `String`.
foreign import toString ∷ Decimal → String

-- | A representation of the `Decimal` as a `String`, rounded to the given
-- | number of significant digits
foreign import toPrecision ∷ Int → Decimal → String

-- | A representation of the `Decimal` as a `String` in fixed-point notation
-- | rounded to the given number of decimal places. Never returns exponential
-- | notation. Uses the default rounding mode.
foreign import toFixed ∷ Int → Decimal → String

-- | Returns true if the value of this `Decimal` is a finite number (not
-- | infinite, not `NaN`).
foreign import isFinite ∷ Decimal → Boolean

-- | Returns true if the value of this `Decimal` is a whole number.
foreign import isInteger ∷ Decimal → Boolean

instance Show Decimal where
  show x = "(fromString \"" <> toString x <> "\")"

foreign import dAdd ∷ Decimal → Decimal → Decimal
foreign import dMul ∷ Decimal → Decimal → Decimal

instance Semiring Decimal where
  add  = dAdd
  zero = fromInt 0
  mul  = dMul
  one  = fromInt 1

foreign import dSub ∷ Decimal → Decimal → Decimal

instance Ring Decimal where
  sub = dSub

foreign import dDiv ∷ Decimal → Decimal → Decimal

instance CommutativeRing Decimal

instance EuclideanRing Decimal where
  div = dDiv
  mod _ _ = zero
  degree _ = one

instance DivisionRing Decimal where
  recip = dDiv one

-- | Round to the given number of significant digits.
foreign import toSignificantDigits ∷ Int → Decimal → Decimal

-- | The absolute value.
foreign import abs ∷ Decimal → Decimal

-- | Inverse cosine.
foreign import acos ∷ Decimal → Decimal

-- | Inverse hyperbolic cosine.
foreign import acosh ∷ Decimal → Decimal

-- | Hyperbolic sine.
foreign import asin ∷ Decimal → Decimal

-- | Inverse hyperbolic sine.
foreign import asinh ∷ Decimal → Decimal

-- | Inverse tangent.
foreign import atan ∷ Decimal → Decimal

-- | Inverse hyperbolic tangent.
foreign import atan2 ∷ Decimal → Decimal → Decimal

-- | Inverse hyperbolic tangent.
foreign import atanh ∷ Decimal → Decimal

-- | Rounded to next whole number in the direction of `+inf`.
foreign import ceil ∷ Decimal → Decimal

-- | Clamp a number to the range delineated by the first two parameters.
foreign import clamp ∷ Decimal → Decimal → Decimal → Decimal

-- | Cosine.
foreign import cos ∷ Decimal → Decimal

-- | Hyperbolic cosine.
foreign import cosh ∷ Decimal → Decimal

-- | Exponential function.
foreign import exp ∷ Decimal → Decimal

-- | Rounded to next whole number in the direction of `-inf`.
foreign import floor ∷ Decimal → Decimal

-- | Natural logarithm.
foreign import ln ∷ Decimal → Decimal

-- | Logarithm with base `2`.
foreign import log2 ∷ Decimal → Decimal

-- | Logarithm with base `10`.
foreign import log10 ∷ Decimal → Decimal

-- | The larger of two numbers.
foreign import max ∷ Decimal → Decimal → Decimal

-- | The smaller of two numbers.
foreign import min ∷ Decimal → Decimal → Decimal

-- | Modulo operation.
foreign import modulo ∷ Decimal → Decimal → Decimal

-- | Exponentiation for `Decimal`.
foreign import pow ∷ Decimal → Decimal → Decimal

-- | Round to the nearest whole number.
foreign import round ∷ Decimal → Decimal

-- | Sine.
foreign import sin ∷ Decimal → Decimal

-- | Hyperbolic sine.
foreign import sinh ∷ Decimal → Decimal

-- | Square root.
foreign import sqrt ∷ Decimal → Decimal

-- | Tangent.
foreign import tan ∷ Decimal → Decimal

-- | Hyperbolic tangent.
foreign import tanh ∷ Decimal → Decimal

-- | Truncate to an integer by removing the mantissa.
foreign import truncated ∷ Decimal → Decimal

-- | Euler's number.
foreign import e ∷ Decimal

-- | Pi, the ratio of a circle's circumference to its diameter.
foreign import pi ∷ Decimal

-- | The gamma function.
foreign import gamma ∷ Decimal → Decimal

-- | The factorial function.
factorial ∷ Decimal → Decimal
factorial n | n < zero = one / zero
            | otherwise = gamma $ ceil (n + one)

-- | Inverse secant.
asec ∷ Decimal → Decimal
asec x = acos (one / x)

-- | Inverse cosecant.
acsc ∷ Decimal → Decimal
acsc x = asin (one / x)

-- | Inverse cotangent.
-- |
-- | Note that this is a multivalued function. The definition here
-- | refers to the principal value of 'acot' without a discontinuity at x=0, and a domain
-- | of (0, π).
-- | See https://mathworld.wolfram.com/InverseCotangent.html for more information.
acot ∷ Decimal → Decimal
acot x =
  if x > zero
    then atan (one / x)
    else atan (one / x) + pi

-- | Secant.
sec ∷ Decimal → Decimal
sec x = one / cos x

-- | Cosecant.
csc ∷ Decimal → Decimal
csc x = one / sin x

-- | Cotangent.
cot ∷ Decimal → Decimal
cot x = one / tan x

-- | Hyperbolic cosecant.
csch ∷ Decimal → Decimal
csch x = one / sinh x

-- | Hyperbolic secant.
sech ∷ Decimal → Decimal
sech x = one / cosh x

-- | Hyperbolic cotangent.
coth ∷ Decimal → Decimal
coth x = one / tanh x

-- | Inverse hyperbolic cosecant.
acsch ∷ Decimal → Decimal
acsch x = asinh (one / x)

-- | Inverse hyperbolic secant.
asech ∷ Decimal → Decimal
asech x = acosh (one / x)

-- | Inverse hyperbolic cotangent.
acoth ∷ Decimal → Decimal
acoth x = atanh (one / x)
