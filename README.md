
<!-- README.md is generated from README.Rmd. Please edit that file -->

# callmethod

> A methods package with some simpler syntax at the cost of being a less
> transferable

## Installation

You can install the released version of callmethod from Github with:

``` r
## install from Github
remotes::install_github("mkearney/callmethod")
```

## Examples

Define a method that’s a wrapper around `base::rnorm()`:

``` r
## define method
r_norm <- function(n, m = 0, sd = 1) call_method("r_norm")

## set default
r_norm.default <- function(...) rnorm(...)

## call method
r_norm(10, 3)
#>  [1] 3.90309 3.26198 1.99421 2.32652 2.12486 1.53368 3.66209 1.69597
#>  [9] 4.50497 3.11017
```

Define a method that generates random ID strings:

``` r
## define method
rstring <- function(n, collapse = "") call_method("random_string")

## define method default
random_string.default <- function(...) {
  list(...)[[1]]
}

random_string.character <- function(...) {
  dots <- list(...)
  n <- nchar(dots[[1]])
  collapse <- dots[[2]]
  random_string.numeric(n, collapse)
}

## define method for numeric
random_string.numeric <- function(...) {
  dots <- list(...)
  n <- dots[[1]]
  collapse <- dots[[2]]
  fl <- sample(letters, 2, replace = TRUE)
  paste(c(fl[1],
    sample(c(rep(0:9, 3), letters, toupper(letters)), n - 2, replace = TRUE),
       fl[2]), collapse = collapse)
}

## call random_string method
rstring(20)
#> [1] "t8v8Tb196k6a03oN68sz"

rstring("thislong")
#> [1] "z573NICa"
```
