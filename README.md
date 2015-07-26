# hoon-fp

## Completed

* Basic operations, rounding (not very fast, but code is short and running time is bounded to input and output precision)
* Conversion between IEEE754 format
* Float-to-decimal (Dragon4), Decimal-to-float
* Fix parsers, printing
* Elementary functions (trigonometric, logarithm, exponent...)

## In progress

* Jets for multiple-precision (basic operations will have jets, elementary functions moved to library)
* Jets for ++rd, ++rs, ++rq (SoftFloat)
* ++rh should not have math operations, just conversions to @rs and back

## Planned

* Fractal demo

## Not planned ATM

* Flags, signalling NaN
* Decimal floating point operations
* Power function
