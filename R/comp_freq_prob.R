## comp_freq_prob.R | riskyr
## 2018 10 24
## Compute frequencies from probabilities:
## -----------------------------------------------

## Table of current terminology: -----------------

# Probabilities (13+):              Frequencies (11):
# -------------------               ------------------
# (A) by condition:

# non-conditional:                          N
# prev*                           cond.true | cond.false (columns)

# conditional:
# sens* = hit rate = TPR                hi* = TP
# mirt  = miss rate = FNR               mi* = FN
# fart  = false alarm rate = FPR        fa* = FP
# spec* = true negative rate = TNR      cr* = TN

# [Note: *...is essential]


# (B) by decision:                 Combined frequencies:

# non-conditional:
# ppod = proportion of dec.pos     dec.pos | dec.neg (rows)
#                                  dec.cor | dec.err (diagonal)

# conditional:
# PPV = precision
# FDR = false detection rate
# FOR = false omission rate
# NPV = neg. pred. value

# (C) by accuracy/correspondence of decision to condition (see accu):

# acc  = overall accuracy (probability/proportion correct decision)
# p_acc_hi = p(hi|acc)  # aka. acc-hi  "p(hi | dec.cor)"
# p_err_fa = p(fa|err)  # aka. err-fa  "p(fa | dec.err)"

# Other measures of accuracy (in accu):
# wacc = weighted accuracy
# mcc  = Matthews correlation coefficient
# f1s  = harmonic mean of PPV and sens

# err = error rate = (1 - acc)


## Data flow: Two basic directions: --------------

## (1) Probabilities ==> frequencies:
##     Bayesian: based on 3 essential probabilities:
##   - given:   prev;  sens, spec
##   - derived: all other values

## (2) Frequencies ==> probabilities:
##     Frequentist: based on 4 essential natural frequencies:
##   - given:   N = hi, mi, fa, cr
##   - derived: all other values


## 2 main functions convert between formats: ----------

## a. comp_freq_prob: Computes freq from prob (in comp_xxxx_prob.R)
## b. comp_prob_freq: Computes prob from freq (in comp_prob_freq.R)


## ad (1) from prob ==> to freq: ----------

## (1) Determine a suitable population size N: --------

## Criterion: All 4 SDT cells should have a minimal frequency of min.freq:

## comp_min_N: Compute suitable minimum population size value N ------

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
#' By default, \code{\link{{comp_freq_prob}} and \code{\link{comp_freq}}
#' round frequencies to nearest integers to avoid decimal values in
#' \code{\link{freq}} (i.e., \code{round = TRUE} by default).
#' Using the option \code{round = FALSE} turns off rounding.
#'
#' @param prev The condition's prevalence value \code{\link{prev}}
#' (i.e., the probability of condition being TRUE).
#'
#' @param sens The decision's sensitivity value  \code{\link{sens}}
#' (i.e., the conditional probability
#' of a positive decision provided that the condition is TRUE).
#'
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
#' \code{\link{num}} contains basic numeric parameters;
#' \code{\link{freq}} contains current frequency information;
#' \code{\link{comp_freq}} computes frequencies from probabilities;
#' \code{\link{prob}} contains current probability information;
#' \code{\link{comp_prob}} computes probabilities from probabilities;
#' \code{\link{comp_freq_freq}} computes current frequency information from (4 essential) frequencies;
#' \code{\link{comp_freq_prob}} computes current frequency information from (3 essential) probabilities;
#' \code{\link{comp_prob_freq}} computes current probability information from (4 essential) frequencies;
#' \code{\link{comp_prob_prob}} computes current probability information from (3 essential) probabilities.
#'
#' @export

comp_min_N <- function(prev, sens, spec,  # 3 essential probabilities
                       min.freq = 1) {

  ## (1) initialize:
  N <- 10^0

  ## (2) Only if triple of essential probabilities is valid:
  if (is_valid_prob_set(prev = prev, sens = sens, spec = spec)) {

    ## (3) Issue a warning if probabilities describe an extreme case:
    is_extreme_prob_set(prev = prev, sens = sens, spec = spec)  # prints a warning if TRUE

    ## (4) Compute frequency of 4 SDT cases (without rounding):
    n.hi <- N * prev * sens
    n.mi <- N * prev * (1 - sens)
    n.cr <- N * (1 - prev) * spec
    n.fa <- N * (1 - prev) * (1 - spec)

    ## (5) While freq of 4 SDT cases < min.freq:
    while ((n.hi > 0  &&  n.hi < min.freq) |
           (n.mi > 0  &&  n.mi < min.freq) |
           (n.cr > 0  &&  n.cr < min.freq) |
           (n.fa > 0  &&  n.fa < min.freq)) {

      ## (a) Multiply N by 10:
      N <- (N * 10)

      ## (b) Update frequency of 4 SDT cases for current N (in next loop):
      n.hi <- N * prev * sens
      n.mi <- N * prev * (1 - sens)
      n.cr <- N * (1 - prev) * spec
      n.fa <- N * (1 - prev) * (1 - spec)

    }
  }

  ## (6) Return number N:
  return(N)

}

## Check: -----
# comp_min_N(0, 0, 0)  # => 1
# comp_min_N(1, 1, 1)  # => 1
# comp_min_N(1, 1, 1, min.freq = 10)  # =>  10
# comp_min_N(1, 1, 1, min.freq = 99)  # => 100
# comp_min_N(.1, .1, .1)        # =>       100 = 10^2
# comp_min_N(.001, .1, .1)      # =>    10 000 = 10^4
# comp_min_N(.001, .001, .1)    # => 1 000 000 = 10^6
# comp_min_N(.001, .001, .001)  # => 1 000 000 = 10^6




## (*) Done: ----------

## - Clean up code.  [2018 08 30]


## (+) ToDo: ----------

# cond.true  cond.false
# hi mi fa cr
# dec.pos  dec.neg

## eof. ------------------------------------------
