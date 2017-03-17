# purescript-decimals

A library for calculations with arbitrary precision numbers. This is a simple
wrapper around [decimal.js](http://mikemcl.github.io/decimal.js).


## Module documentation

- [Published on Pursuit](http://pursuit.purescript.org/packages/purescript-decimals/)

## Example

```purescript
> let x = fromMaybe zero (fromString "12345.67898765432123456789")
> let y = fromInt 100000
> x + y
(fromString "112345.67898765432123456789")

> toString (x `pow` y)
"3.1479315197661937753e+409151"

> let two = fromNumber 2.0
> sin (pi / two)
(fromString "1")
```

## Installation and usage
You can install this package via Bower. You will also need
[decimal.js](http://mikemcl.github.io/decimal.js), which can be installed
via `npm`:
```
bower install purescript-decimals
npm install decimal.js
```
For the browser, remember to bundle `decimal.js` with your code.

## Development
```
bower install
npm install
```
Then, use `pulp` to build and run tests.
