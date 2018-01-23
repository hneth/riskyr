## comp_freq_prob.R | riskyR
## 2018 01 22
## -----------------------------------------------
## Compute frequencies from probabilities:
## -----------------------------------------------

## -----------------------------------------------
## Table of current terminology:

# probabilities (9):                frequencies (9):
# ------------------                ------------------
# (A) basic:
#                                          N
# prev*                             n.true | n.false

# sens* = hit rate = TPR             hi* = TP
# mirt  = miss rate = FNR            mi* = FN
# fart  = false alarm rate = FPR     fa* = FP
# spec* = true negative rate = TNR   cr* = TN

# [Note: *...is essential]


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
## (1) Determine a suitable population size N:
##     Criterion: All 4 SDT cells should have a minimal frequency of min.freq:

#' Compute a suitable minimum population size value N.
#'
#' \code{comp_min_N} is a function that computes a population size value \code{\link{N}} (an integer
#' as a power of 10) so that the frequencies of the 4 combinations of conditions and decisions
#' (i.e., the cells of the confusion table, or bottom row of boxes in the natural frequency tree)
#' reach or exceed a minimum value \code{min.freq} given the basic parameters
#' \code{prev}, \code{sens}, and \code{spec} (\code{spec = 1 - fart}).
#'
#' Using this function helps avoiding excessively small decimal values in categories
#' (esp. true positives, false negatives, false positives, and true negatives)
#' when expressing combinations of conditions and decisions as natural frequencies.
#' As values of zero (0) are tolerable, the function only increases  \code{\link{N}}
#' (in powers of 10) while the current value of any frequency (cell in confusion table or
#' leaf of tree) is positive but below \code{min.freq}.
#'
#' Note that \code{\link{comp_freq}} still needs to round to avoid decimal values
#' in frequencies \code{\link{freq}}.
#'
#' @param prev The condition's prevalence value \code{\link{prev}}
#' (i.e., the probability of condition being TRUE).
#' @param sens The decision's sensitivity value  \code{\link{sens}}
#' (i.e., the conditional probability
#' of a positive decision provided that the condition is TRUE).
#' @param spec The specificity value  \code{\link{spec}}
#' (i.e., the conditional probability
#' of a negative decision provided that the condition is FALSE).
#'
#' @param min.freq The minimum frequency of each combination of
#' a condition and a decision (i.e., hits, misses, false alarms, and correct rejections).
#' Default: \code{min.freq = 1}.
#'
#' @return An integer value \code{\link{N}} (as a power of 10).
#'
#' @examples
#' comp_min_N(0, 0, 0)  # => 1
#' comp_min_N(1, 1, 1)  # => 1
#'
#' comp_min_N(1, 1, 1, min.freq = 10)  # =>  10
#' comp_min_N(1, 1, 1, min.freq = 99)  # => 100
#'
#' comp_min_N(.1, .1, .1)        # =>       100 = 10^2
#' comp_min_N(.001, .1, .1)      # =>    10 000 = 10^4
#' comp_min_N(.001, .001, .1)    # => 1 000 000 = 10^6
#' comp_min_N(.001, .001, .001)  # => 1 000 000 = 10^6
#'
#' @family functions computing frequencies
#'
#' @seealso
#' population size \code{\link{N}};
#' \code{\link{freq}} contains current frequency information;
#' \code{\link{comp_freq}} computes frequencies from probabilities

comp_min_N <- function(prev, sens, spec,
                       min.freq = 1) {

  N <- 10^0 # initialize

  ## Compute frequency of 4 SDT cases:
  n.hi <- N * prev * sens
  n.mi <- N * prev * (1 - sens)
  n.cr <- N * (1 - prev) * spec
  n.fa <- N * (1 - prev) * (1 - spec)

  ## Freq of 4 SDT cases:
  while ((n.hi > 0 & n.hi < min.freq) |
         (n.mi > 0 & n.mi < min.freq) |
         (n.cr > 0 & n.cr < min.freq) |
         (n.fa > 0 & n.fa < min.freq)) {

    N <- (N * 10) # multiply N by 10

    ## Update frequency of 4 SDT cases:
    n.hi <- N * prev * sens
    n.mi <- N * prev * (1 - sens)
    n.cr <- N * (1 - prev) * spec
    n.fa <- N * (1 - prev) * (1 - spec)

  }

  ## Return number N:
  return(N)

}

## Check:
{
  # comp_min_N(0, 0, 0)  # => 1
  # comp_min_N(1, 1, 1)  # => 1
  # comp_min_N(1, 1, 1, min.freq = 10)  # =>  10
  # comp_min_N(1, 1, 1, min.freq = 99)  # => 100
  # comp_min_N(.1, .1, .1)        # =>       100 = 10^2
  # comp_min_N(.001, .1, .1)      # =>    10 000 = 10^4
  # comp_min_N(.001, .001, .1)    # => 1 000 000 = 10^6
  # comp_min_N(.001, .001, .001)  # => 1 000 000 = 10^6
}


## -----------------------------------------------

## -----------------------------------------------
## (+) ToDo:

# cond.true  cond.false
# hi mi fa cr
# dec.pos  dec.neg

## -----------------------------------------------
## eof.
