module Test.Main where

import Prelude hiding (min, max)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Decimal (Decimal, abs, fromInt, fromNumber, pow, fromString,
                     toNumber, toString, acos, acosh, asin, asinh, atan, atanh,
                     atan2, ceil, cos, cosh, exp, floor, ln, log10, max,
                     min, round, sin, sinh, sqrt, tan, tanh, e, pi,
                     toSignificantDigits, isInteger, isFinite)
import Data.Maybe (Maybe(..))
import Test.Assert (ASSERT, assert)
import Control.Monad.Eff.Random (RANDOM())
import Control.Monad.Eff.Exception (EXCEPTION())
import Test.QuickCheck (QC(), quickCheck)
import Test.QuickCheck.Arbitrary (class Arbitrary, arbitrary)
import Data.Int as Int
import Data.Number.Approximate ((≅))
import Math as M

-- | Arbitrary instance for Decimal
newtype TestDecimal = TestDecimal Decimal

instance arbitraryDecimal ∷ Arbitrary TestDecimal where
  arbitrary = (TestDecimal <<< fromNumber) <$> arbitrary

-- | Test if a simple function holds before and after converting to Decimal.
testFn ∷ ∀ eff. (Decimal → Decimal) → (Number → Number) → QC eff Unit
testFn f g = quickCheck (\(TestDecimal x) → toNumber (f x) ≅ (g (toNumber x)))

-- | Test if a binary relation holds before and after converting to Decimal.
testBinary ∷ ∀ eff. (Decimal → Decimal → Decimal)
           → (Number → Number → Number)
           → QC eff Unit
testBinary f g = quickCheck (\x y → toNumber ((fromNumber x) `f` (fromNumber y)) ≅ (x `g` y))

main ∷ ∀ eff. Eff (console ∷ CONSOLE, assert ∷ ASSERT, random ∷ RANDOM, exception ∷ EXCEPTION | eff) Unit
main = do
  log "Simple arithmetic operations and conversions from Int"
  let two = one + one
  let three = two + one
  let four = three + one
  assert $ fromInt 3 == three
  assert $ two * two == four
  assert $ two * three * (three + four) == fromInt 42
  assert $ two - three == fromInt (-1)

  log "Parsing strings"
  assert $ fromString "2" == Just two
  assert $ fromString "a" == Nothing
  assert $ fromString "2.1" == Just (fromNumber 2.1)
  assert $ fromString "123456789" == Just (fromInt 123456789)
  assert $ fromString "1e7" == Just (fromInt 10000000)
  quickCheck $ \(TestDecimal a) → (fromString <<< toString) a == Just a

  log "Conversions between String, Int and Decimal should not loose precision"
  quickCheck (\n → fromString (show n) == Just (fromInt n))
  quickCheck (\n → Int.toNumber n == toNumber (fromInt n))

  log "Binary relations between integers should hold before and after converting to Decimal"
  testBinary (+) (+)
  testBinary (-) (-)
  testBinary (*) (*)
  testBinary (/) (/)
  testBinary mod mod
  testBinary div div

  log "It should perform multiplications which would lead to imprecise results using Int"
  assert $ Just (fromInt 333190782 * fromInt 1103515245) == fromString "367681107430471590"

  log "compare, (==)"
  assert $ one + one == two
  assert $ two /= one
  assert $ two > one
  assert $ not (two < one)
  assert $ not (two < two)
  assert $ two >= two
  assert $ two <= two

  log "toSignificantDigits"
  let a = fromNumber 123.456789
  assert $ toSignificantDigits 2 a == fromNumber 120.0
  assert $ toSignificantDigits 4 a == fromNumber 123.5
  assert $ toSignificantDigits 20 a == a

  log "isInteger"
  assert $ isInteger (fromNumber 118327913791272.0)
  assert $ not $ isInteger (fromNumber 118327913791272.1)

  log "isFinite"
  assert $ isFinite (fromNumber 118327913791272.0)
  assert $ not $ isFinite (fromNumber 1.0 / fromNumber 0.0)
  assert $ not $ isFinite (sqrt $ fromNumber (-1.0))

  log "pow should perform exponentiation"
  assert $ three `pow` four == fromInt 81
  assert $ four `pow` (fromNumber 0.5) == two
  assert $ four `pow` (fromNumber (-0.5)) == one / two
  assert $ three `pow` zero == one
  assert $ zero `pow` zero == one

  log "Absolute value"
  quickCheck $ \(TestDecimal x) → abs x == if x > zero then x else (-x)

  log "Other functions"
  testFn acos M.acos
  testFn asin M.asin
  testFn atan M.atan
  testFn cos M.cos
  testFn sin M.sin
  testFn tan M.tan
  testFn exp M.exp
  testFn ln M.log
  testFn ceil M.ceil
  testFn floor M.floor
  testFn round M.round
  testFn sqrt M.sqrt
  testFn cosh $ \x -> 0.5 * (M.exp x + M.exp (-x))
  testFn sinh $ \x -> 0.5 * (M.exp x - M.exp (-x))
  testFn tanh $ \x -> (M.exp x - M.exp (-x)) / (M.exp x + M.exp (-x))
  testFn (asinh <<< sinh) id
  testFn (acosh <<< cosh) id
  testFn (atanh <<< tanh) id
  testBinary atan2 M.atan2
  testBinary max M.max
  testBinary min M.min
  assert $ log10 (fromInt 1000) == fromInt 3

  log "e and pi"
  assert $ cos pi == - one
  assert $ ln (e `pow` two) == two

