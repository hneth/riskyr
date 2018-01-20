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
#' rounded integers) given basic probabilities --
#' \code{prev} and \code{sens}, and \code{spec} or \code{fart} (\code{spec = 1 - fart})
#' -- for a population of \code{N} individuals.
#'
#' By default, \code{\link{comp_freq}} rounds frequencies to nearest integers
#' to avoid decimal values in \code{\link{freq}}.
#'
#' @param prev The condition's prevalence value (i.e., the probability of condition being TRUE).
#' @param sens A decision's sensitivity value (i.e., the conditional probability
#' of a positive decision provided that the condition is TRUE).
#' @param spec A specificity value (i.e., the conditional probability
#' of a negative decision provided that the condition is FALSE).
#' \code{spec} is optional when is complement \code{fart} is provided.
#' @param fart A false alarm rate (i.e., the conditional probability
#' of a positive decision provided that the condition is FALSE).
#' \code{fart} is optional when its complement \code{spec} is provided.
#'
#' @param N A population size value \code{N}.
#'
#' @param round A Boolean value that determines whether frequencies are
#' rounded to the nearest integer. Default: \code{round = TRUE}.
#'
#' @return A list \code{freq} containing 9 frequency values.
#'
#' @examples
#' comp_freq()                  # => ok, using current defaults
#' length(comp_freq())          # => 9
#'
#' # Ways to succeed:
#' comp_freq(1, 1, 1, NA, 100)  # => ok, N hits
#' comp_freq(1, 1, NA, 1, 100)  # => ok, N hits
#' comp_freq(1, 0, 1, NA, 100)  # => ok, N misses
#' comp_freq(1, 0, NA, 1, 100)  # => ok, N misses
#' comp_freq(0, 1, 1, NA, 100)  # => ok, N correct rejections
#' comp_freq(0, 1, NA, 1, 100)  # => ok, N false alarms
#'
#' comp_freq(1, 1, 1, 0, N = NA)  # => ok, but warning that N = 1 was computed
#' comp_freq(1, 1, 1, 0, N =  0)  # => ok, but all 0
#' comp_freq(.5, .5, .5, NA, N = 10, round = TRUE)  # => ok, but all rounded (increasing errors: mi and fa)
#' comp_freq(.5, .5, .5, NA, N = 10, round = FALSE) # => ok, but not rounded
#'
#' # Ways to fail:
#' comp_freq(NA, 1, 1, NA, 100)  # => NAs + warning: prev not numeric
#' comp_freq(1, NA, 1, NA, 100)  # => NAs + warning: sens not numeric
#' comp_freq(8,  1, 1, NA, 100)  # => NAs + warning: prev no probability
#' comp_freq(1,  8, 1, NA, 100)  # => NAs + warning: sens no probability
#' comp_freq(1,  1, 1,  1, 100)  # => NAs and warning: is_complement not in tolerated range
#'
#' @family functions turning probabilities into frequencies
#'
#' @seealso \code{\link{freq}} contains current frequency information;
#' \code{\link{num}} contains basic numeric variables;
#' \code{\link{init_num}} initializes basic numeric variables;
#' \code{\link{is_valid}} verifies the validity of probability inputs;
#' \code{\link{comp_complement}} computes complementary probability (if missing);
#' \code{\link{comp_min_N}} computes a suitable population size N (if missing)

comp_freq <- function(prev = num$prev, sens = num$sens,
                      spec = num$spec, fart = num$fart,
                      N = num$N,
                      round = TRUE) {

  ## (0) Initialize freq as a list:
  freq <- list(

    ## Population size:
    "N" = NA, # Number of cases overall

    ## Number of true cases by condition:
    "cond.true"  = NA, # N of cond TRUE
    "cond.false" = NA, # N of cond FALSE

    ## Number of decisions:
    "dec.pos" = NA, # N of dec POS [was: dec.true]
    "dec.neg" = NA, # N of dec NEG [was: dec.false]

    ## SDT combinations:
    "hi" = NA, # true positive
    "mi" = NA, # false negative
    "fa" = NA, # false positive
    "cr" = NA  # true negative
  )

  ## (1) Only if basic quadruple of probabilities is valid:
  if (is_valid(prev, sens, spec, fart)) {

    ## (2) Compute missing fart or spec (4th argument) value (if applicable):
    cur.spec.fart <- comp_complement(spec, fart)
    spec <- cur.spec.fart[1] # 1st argument
    fart <- cur.spec.fart[2] # 2nd argument

    ## (3) Compute missing population size value N (if applicable):
    if (is.na(N)) {
      N <- comp_min_N(prev, sens, spec, min.freq = 1)
      warning(paste0("Population size N is missing.\n A minimum value of N = ", N, " was computed."))
    }

    ## (4) Set or compute all values of freq:
    freq$N <- N # copy N from argument OR num (input)

    ## (5) Number of true cases by condition:
    ##     (= 1st level of natural frequency tree):
    if (round) {
      freq$cond.true <- round((N * prev), 0)  # 1a. cond.true  = N x prev [rounded to nearest integer]
    } else {
      freq$cond.true <- (N * prev)            # 1b. cond.true  = N x prev [not rounded]
    }
    freq$cond.false <- (N - freq$cond.true)   # 2. cond.false = complement of cond.true (to N)

    ## (6) Number of SDT combinations:
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

    ## (7) Number of decisions:
    freq$dec.pos <- freq$hi + freq$fa # 1. positive decisions (true & false positives)
    freq$dec.neg <- freq$mi + freq$cr # 2. negative decisions (false & true negatives)

    ## (8) Check:
    if ((freq$cond.true != freq$hi + freq$mi) |
        (freq$cond.false != freq$fa + freq$cr) |
        (freq$N != freq$cond.true + freq$cond.false) |
        (freq$N != freq$hi + freq$mi + freq$fa + freq$cr)) {
      warning("Warning: Frequencies do not add up (to n.true, n.false, or N).")
    }

  } # if (is_valid(prev, sens, spec, fart))

  ## (9) Return entire list freq:
  return(freq)

}

## Check:
{
  # comp_freq()                  # => ok, using current defaults
  # length(comp_freq())          # => 9
  #
  # ## Ways to succeed:
  # comp_freq(1, 1, 1, NA, 100)  # => ok, N hits
  # comp_freq(1, 1, NA, 1, 100)  # => ok, N hits
  # comp_freq(1, 0, 1, NA, 100)  # => ok, N misses
  # comp_freq(1, 0, NA, 1, 100)  # => ok, N misses
  # comp_freq(0, 1, 1, NA, 100)  # => ok, N correct rejections
  # comp_freq(0, 1, NA, 1, 100)  # => ok, N false alarms
  #
  # comp_freq(1, 1, 1, 0, N = NA)  # => ok, but warning that N = 1 was computed
  # comp_freq(1, 1, 1, 0, N =  0)  # => ok, but all 0
  # comp_freq(.5, .5, .5, NA, N = 10, round = TRUE)  # => ok, but all rounded (increasing errors: mi and fa)
  # comp_freq(.5, .5, .5, NA, N = 10, round = FALSE) # => ok, but not rounded
  #
  # ## Ways to fail:
  # comp_freq(NA, 1, 1, NA, 100)  # => NAs + warning: prev not numeric
  # comp_freq(1, NA, 1, NA, 100)  # => NAs + warning: sens not numeric
  # comp_freq(8,  1, 1, NA, 100)  # => NAs + warning: prev no probability
  # comp_freq(1,  8, 1, NA, 100)  # => NAs + warning: sens no probability
  # comp_freq(1,  1, 1,  1, 100)  # => NAs and warning: is_complement not in tolerated range
}

## Apply:
freq <- comp_freq()
# freq

## -----------------------------------------------
## (+) ToDo:

## -----------------------------------------------
## eof.
