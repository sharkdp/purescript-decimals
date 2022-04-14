module Test.Main (main) where

import Prelude hiding (clamp, min, max)
import Data.Decimal (Decimal, abs, clamp, fromInt, fromNumber, pow, fromString,
                     toNumber, toString, toFixed, acos, acosh, asin, asinh, atan, atanh,
                     atan2, ceil, cos, cosh, exp, floor, ln, log10, max,
                     min, modulo, round, truncated, sin, sinh, sqrt, tan, tanh, e, pi, gamma,
                     toSignificantDigits, isInteger, isFinite, factorial)
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.Number.Approximate ((≅))
import Effect (Effect)
import Effect.Console (log)
import Math as M
import Test.Assert (assert)
import Test.QuickCheck (quickCheck)
import Test.QuickCheck.Arbitrary (class Arbitrary, arbitrary)

-- | Arbitrary instance for Decimal
newtype TestDecimal = TestDecimal Decimal

instance Arbitrary TestDecimal where
  arbitrary = (TestDecimal <<< fromNumber) <$> arbitrary

-- | Test if a simple function holds before and after converting to Decimal.
testFn ∷ (Decimal → Decimal) → (Number → Number) → Effect Unit
testFn f g = quickCheck \(TestDecimal x) → toNumber (f x) ≅ g (toNumber x)

-- | Test if a binary relation holds before and after converting to Decimal.
testBinary ∷ (Decimal → Decimal → Decimal)
           → (Number → Number → Number)
           → Effect Unit
testBinary f g = quickCheck \x y → toNumber ((fromNumber x) `f` (fromNumber y)) ≅ (x `g` y)

main ∷ Effect Unit
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
  quickCheck \(TestDecimal a) → (fromString <<< toString) a == Just a

  log "toFixed"
  let a = fromNumber 123.456789
  assert $ toFixed 0 a == "123"
  assert $ toFixed 3 a == "123.457"
  assert $ toFixed 8 a == "123.45678900"

  log "Conversions between String, Int and Decimal should not lose precision"
  quickCheck \n → fromString (show n) == Just (fromInt n)
  quickCheck \n → Int.toNumber n == toNumber (fromInt n)

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

  log "modulo"
  assert $ zero `modulo` three == zero
  assert $ one `modulo` three == one
  assert $ two `modulo` three == two
  assert $ three `modulo` three == zero
  assert $ four `modulo` three == one
  assert $ (-one) `modulo` three == two
  assert $ (-three) `modulo` three == zero
  assert $ one `modulo` (-three) == one

  log "Absolute value"
  quickCheck \(TestDecimal x) → abs x == if x > zero then x else -x

  log "clamp"
  assert $ clamp three four one == three
  assert $ clamp zero two zero == zero

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
  testFn truncated \x -> if x >= zero then M.floor x else M.ceil x
  testFn sqrt M.sqrt
  testFn cosh \x -> 0.5 * (M.exp x + M.exp (-x))
  testFn sinh \x -> 0.5 * (M.exp x - M.exp (-x))
  testFn tanh \x -> (M.exp x - M.exp (-x)) / (M.exp x + M.exp (-x))
  testFn (asinh <<< sinh) identity
  testFn (acosh <<< cosh) identity
  testFn (atanh <<< tanh) identity
  testBinary atan2 M.atan2
  testBinary max M.max
  testBinary min M.min
  assert $ log10 (fromInt 1000) == fromInt 3

  log "e and pi"
  assert $ cos pi == -one
  assert $ ln (e `pow` two) == two

  log "gamma"
  let eps = fromNumber 1.0e-10
      approxEq yStr x =
        case fromString yStr of
          Just y → assert $ abs (x / y - one) < eps
          Nothing → do
            log "Could not parse string"
            assert false

  approxEq "1.24216934450430540491" (gamma (fromNumber 2.4))
  approxEq "9.51350769866873183629" (gamma (fromNumber 0.1))
  approxEq "2.65927187288003053999" (gamma (fromNumber (-1.4)))
  approxEq "9.33262154439441526817e+155" (gamma (fromNumber (100.0)))

  log "factorial"
  assert $ factorial zero == one
  assert $ factorial one == one
  assert $ factorial (fromInt 5) == fromInt (2 * 3 * 4 * 5)
  assert $ factorial (fromInt 10) == fromInt 3628800
  assert $ factorial (fromInt 10) == fromInt 3628800
  assert $ factorial (fromInt 15) == fromNumber 1307674368000.0
  approxEq "2432902008176640000" (factorial (fromInt 20))
  approxEq "10888869450418352160768000000" (factorial (fromInt 27))
  approxEq "263130836933693530167218012160000000" (factorial (fromInt 32))
  approxEq "30414093201713378043612608166064768844377641568960512000000000000" (factorial (fromInt 50))

  log "done"
