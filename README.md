Either: the ‘Either’ type in R
================

<!-- badges: start -->
[![R-CMD-check](https://github.com/stla/Either/workflows/R-CMD-check/badge.svg)](https://github.com/stla/Either/actions)
<!-- badges: end -->

``` r
library(Either)
```

``` r
## ------------------------------------------------
## Method `Either$new`
## ------------------------------------------------

Either$new("left", 123)
## Left
## [1] 123


## ------------------------------------------------
## Method `Either$isLeft`
## ------------------------------------------------

Either$new("right", 999)$isLeft()
## [1] FALSE

## ------------------------------------------------
## Method `Either$isRight`
## ------------------------------------------------

Either$new("right", 999)$isRight()
## [1] TRUE

## ------------------------------------------------
## Method `Either$getLeft`
## ------------------------------------------------

Either$new("right", 999)$getLeft("abc")
## [1] "abc"

## ------------------------------------------------
## Method `Either$getRight`
## ------------------------------------------------

Either$new("right", 999)$getRight("abc")
## [1] 999

## ------------------------------------------------
## Method `Either$toMaybe`
## ------------------------------------------------

Either$new("right", 999)$toMaybe()
## Just
## [1] 999
Either$new("left", 999)$toMaybe()
## Nothing

## ------------------------------------------------
## Method `Either$mapLeft`
## ------------------------------------------------

Either$new("left", 999)$mapLeft(is.numeric)
## Left
## [1] TRUE
Either$new("right", 999)$mapLeft(is.numeric)
## Right
## [1] 999

## ------------------------------------------------
## Method `Either$mapRight`
## ------------------------------------------------

Either$new("left", 999)$mapRight(is.numeric)
## Left
## [1] 999
Either$new("right", 999)$mapRight(is.numeric)
## Right
## [1] TRUE

## ------------------------------------------------
## Method `Either$either`
## ------------------------------------------------

Either$new("left", 999)$either(is.numeric, is.character)
## Left
## [1] TRUE
Either$new("right", 999)$either(is.numeric, is.character)
## Right
## [1] FALSE
```

``` r
## ------------------------------------------------
## Functions `Left` and `Right` (shortcuts)
## ------------------------------------------------
Left("abc")
## Left
## [1] "abc"
Right("abc")
## Right
## [1] "abc"
```

``` r
## ------------------------------------------------
## Function `eitherFromMaybe` 
## ------------------------------------------------
library(maybe)
eitherFromMaybe("abc", nothing())
## Left
## [1] "abc"
eitherFromMaybe("abc", just(123))
## Right
## [1] 123
```
