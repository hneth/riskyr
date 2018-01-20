## comp_freq.R | riskyR
## 2018 01 20
## -----------------------------------------------
## Compute all current frequencies (freq) based on num
## (using only the 4 necessary parameters of num):

## Note: Always use num (essential) rather than env (NON-essential)!

## -----------------------------------------------
## Compute all current frequencies:

#' Compute frequencies from basic probabilities.
#'
#' \code{comp_freq} is a function that computes natural frequencies (typically
#' rounded integers) for a population of \code{N} individuals
#' given basic probabilities \code{prev}, \code{sens}, \code{spec},
#' or \code{fart} (\code{spec = 1 - fart}).
#'
#' By default, \code{\link{comp_freq}} rounds to nearest integers
#' to avoid decimal values in frequencies \code{\link{freq}}.
#'
#' @param prev The condition's prevalence value (i.e., the probability of condition being TRUE).
#' @param sens A decision's sensitivity value (i.e., the conditional probability
#' of a positive decision provided that the condition is TRUE).
#' @param spec A specificity value (i.e., the conditional probability
#' of a negative decision provided that the condition is FALSE).
#' @param round A Boolean value that determines whether frequencies are
#' rounded to the nearest integer. Default: \code{round = TRUE}.
#'
#' @return A list \code{freq} containing 9 frequency values.
#'
#' @examples
#' comp_freq() # initializes default values
#'
#' @family functions turning probabilities into frequencies
#' @seealso \code{\link{comp_min_N}} computes a suitable population size N

comp_freq <- function(N = num$N,
                      prev = num$prev, sens = num$sens, spec = num$spec, fart = num$fart,
                      round = TRUE) {

  ## (0) Initialize freq as a list:
  freq <- list(

    ## Population size:
    "N" = NA, # Number of cases overall

    ## Number of true cases by condition:
    "cond.true"  = NA, # N of cond TRUE
    "cond.false" = NA, # N of cond FALSE

    ## Number of decisions:
    "dec.pos" = NA, # N of dec POS [was: dec.pos]
    "dec.neg" = NA, # N of dec NEG [was: dec.neg]

    ## SDT combinations:
    "hi" = NA, # true positive
    "mi" = NA, # false negative
    "cr" = NA, # true negative
    "fa" = NA  # false positive

  )

  ## (1) ToDo: Check 4 basic parameters for
  ## - is_prob,
  ## - is_sufficient, and
  ## - is_complement

  ## (2) Compute all values of freq based on arguments:
  freq$N <- N # copy N from argument/num (input)

  ## (3) Number of true cases by condition:
  ##     (= 1st level of natural frequency tree):
  if (round) {
    freq$cond.true <- round((N * prev), 0)  # 1a. cond.true  = N x prev [rounded to nearest integer]
  } else {
    freq$cond.true <- (N * prev)            # 1b. cond.true  = N x prev [not rounded]
  }
  freq$cond.false <- (N - freq$cond.true)   # 2. cond.false = complement of cond.true (to N)

  ## (4) Number of SDT combinations:
  ##     (= 2nd level/leaves of natural frequency tree):
  if (round) {
    freq$hi <- round((sens * freq$cond.true), 0)  # a1. N of hi [rounded to nearest integer]
  } else {
    freq$hi <- (sens * freq$cond.true)            # a2. N of hi [not rounded]
  }
  freq$mi <- (freq$cond.true - freq$hi)           # b.  N of mi = complement of hi (to cond.true)

  if (round) {
    freq$cr <- round((spec * freq$cond.false), 0) # c1. N of cr [rounded to nearest integer]
  } else {
    freq$cr <- (spec * freq$cond.false)           # c2. N of cr [not rounded]
  }
  freq$fa <- (freq$cond.false - freq$cr)          # d.  N of fa - complement of cr (to cond.false)

  ## (5) Number of decisions:
  freq$dec.pos <- freq$hi + freq$fa # 1. positive decisions (true & false positives)
  freq$dec.neg <- freq$mi + freq$cr # 2. negative decisions (false & true negatives)

  ## (6) Check:
  if ((freq$cond.true != freq$hi + freq$mi) |
      (freq$cond.false != freq$fa + freq$cr) |
      (freq$N != freq$cond.true + freq$cond.false) |
      (freq$N != freq$hi + freq$mi + freq$fa + freq$cr)) {
    warning("Warning: Frequencies do not add up (to n.true, n.false, or N).")
  }

  ## (7) Return entire list freq:
  return(freq)

}

## Apply:
freq <- comp_freq()
# freq

## -----------------------------------------------
## (+) ToDo:

## - See ToDo above:
##   Allow providing fart as an alternative to spec
##   (spec = 1 - fart) and check entries for validity

## -----------------------------------------------
## eof.
