## init_freq.R | riskyR
## 2018 01 24
## -----------------------------------------------
## Define and initialize ALL frequencies
## -----------------------------------------------

## -----------------------------------------------
## Table of current terminology:

# Probabilities (9):                Frequencies (9):
# ------------------                ------------------
# (A) Basic:
# non-conditional:                          N
# prev*                           cond.true | cond.false

# conditional:
# sens* = hit rate = TPR                hi* = TP
# mirt  = miss rate = FNR               mi* = FN
# fart  = false alarm rate = FPR        fa* = FP
# spec* = true negative rate = TNR      cr* = TN

# [Note: *...is essential]


# (B) Derived probabilities:     Combined frequencies:

# PPV = pos. pred. value           dec.pos | dec.neg
# FDR = false detection rate
# FOR = false omission rate
# NPV = neg. pred. value

## -----------------------------------------------
## Data flow: Two basic directions:

## 1: Bayesian: starting with 3 basic probabilities:
## - given:   prev;  sens, spec
## - derived: all other values

## 2: Natural frequencies:
## - given:   N = hi, mi, fa, cr
## - derived: all other values

## -----------------------------------------------

## -----------------------------------------------
## (A) Define and initializes BASIC frequencies:
## -----------------------------------------------
## (0) N: population size

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
#' @family basic frequencies
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

N <- 99  # default population size

## -----------------------------------------------
## *essential frequencies: hi mi fa cr
## -----------------------------------------------
## (1) hi = TP:

#' Frequency of hits or true positives (TP).
#'
#' \code{hi} is the frequency of hits or true positives (TP)
#' in a population of \code{\link{N}} individuals.
#'
#' @aliases
#' TP
#'
#' @family basic frequencies
#'
#' @seealso
#' \code{\link{num}} contains basic numeric variables;
#' \code{\link{freq}} contains basic frequency variables.

hi <- NA

## -----------------------------------------------
## (2) mi = FN:
## -----------------------------------------------
## (3) fa = FP:
## -----------------------------------------------
## (4) cr = TN:


## -----------------------------------------------
## (B) Define and initialize COMBINED frequencies:
## -----------------------------------------------

## -----------------------------------------------
## (a) cond.true cond.false
## -----------------------------------------------

## cond.true  cond.false

## -----------------------------------------------
## (b) dec.pos dec.neg
## -----------------------------------------------

## -----------------------------------------------
## dec.pos  dec.neg

## -----------------------------------------------
## (+) ToDo:





## -----------------------------------------------
## eof.
