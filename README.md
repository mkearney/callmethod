
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

## Example

A basic example of defining and using a method

``` r
## define method
r_norm <- function(n, m = 0, sd = 1) call_method("r_norm")

## set default
r_norm.default <- function(...) rnorm(...)

## call method
r_norm(10, 3)
#>  [1] 2.34234 3.70959 3.42103 3.90140 3.64369 2.39683 4.31812 2.15651
#>  [9] 2.67518 3.05810
```
