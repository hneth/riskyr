## init_freq.R | riskyR
## 2018 01 22
## -----------------------------------------------
## Define and initialize basic frequencies
## -----------------------------------------------

## -----------------------------------------------
## Table of current terminology:

# 9 probabilities:                  9 frequencies:
# ----------------                  ------------------
# (A) basic:
#                                          N
# prev                              n.true | n.false

# sens = hit rate = TPR             hi = TP
# mirt = miss rate = FNR            mi = FN
# fart = false alarm rate = FPR     fa = FP
# spec = true negative rate = TNR   cr = TN


# (B) derived:
#                                   dec.pos | dec.neg

# PPV = pos. pred. value
# FDR = false detection rate
# FOR = false omission rate
# NPV = neg. pred. value

## -----------------------------------------------
## Two basic directions:

## 1: Bayesian: starting with 3 basic probabilities:
## - given:   prev;  sens, spec
## - derived: all other values

## 2: Natural frequencies:
## - given:   N;  hi, mi, fa, cr
## - derived: all other values

## -----------------------------------------------
## (A) Basic frequencies:
## -----------------------------------------------
## (0) N:

#' The number of individuals in the population.
#'
#' \code{N} defines how many individuals make up
#' the current population (i.e., the overall
#' number of cases considered).
#'
#' A frequency (integer) value.
#'
#' Relationships: For natural frequencies
#' in a population of size \code{N}
#' the following relationships hold:
#'
#' \enumerate{
#'
#'   \item N = n(condition true) + n(condition false)
#'
#'   \item n(positive decisions) + n(negative decisions)
#'
#'   \item N = [n(hi) + n(mi)] + [n(fa) + n(cr)]
#' }
#'
#' The current frequency information is computed by
#' \code{\link{comp_freq}} and contained in a list
#' \code{\link{freq}}.
#'
#' If \code{N} is unknown, a suitable minimum value
#' can be computed by \code{\link{comp_min_N}}.
#'
#' @references
#' Consult \href{https://en.wikipedia.org/wiki/Statistical_population}{Wikipedia} for additional information.
#'
#' @family basic parameters
#'
#' @seealso
#' \code{\link{comp_min_N}} computes a suitable
#' minimum value of \code{N} for given probabilities;
#' \code{\link{num}} contains basic numeric variables;
#' \code{\link{init_num}} initializes basic numeric variables;
#' \code{\link{freq}} contains basic frequency variables;
#' \code{\link{comp_freq}} computes natural frequencies from probabilities;
#' \code{\link{comp_prob}} computes derived probabilities
#'
#' @examples
#' N <- 1000   # => sets a population size of 1000
#' is_freq(N)  # => TRUE
#' is_prob(N)  # => FALSE (as N is no probability)

N <- 100  # default population size


## -----------------------------------------------
## -----------------------------------------------
## -----------------------------------------------
## (+) ToDo:

# cond.true  cond.false
# hi mi fa cr
# dec.pos  dec.neg

## -----------------------------------------------
## eof.
