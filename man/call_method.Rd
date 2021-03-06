% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/call.R
\name{call_method}
\alias{call_method}
\title{Call method}
\usage{
call_method(method, env = parent.frame())
}
\arguments{
\item{method}{String, naming method to call}

\item{env}{Environment with method arguments.}
}
\value{
Returns output from method.
}
\description{
Call a particular set of methods (functions)
}
\examples{

## define method
r_norm <- function(n, m = 0, sd = 1) call_method("r_norm")

## set default
r_norm.default <- function(...) rnorm(...)

## call method
r_norm(10, 3)

## define method
rstring <- function(n, collapse = "") call_method("random_string")

## define method default
random_string.default <- function(...) {
  list(...)[[1]]
}

##  define method for numeric
random_string.numeric <- function(...) {
  dots <- list(...)
  n <- dots[[1]]
  collapse <- dots[[2]]
  fl <- sample(letters, 2, replace = TRUE)
  paste(c(fl[1],
    sample(c(rep(0:9, 3), letters, toupper(letters)), n - 2, replace = TRUE),
	   fl[2]), collapse = collapse)
}

## call method
rstring(20)

}
