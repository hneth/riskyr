## comp_min_N.R | riskyr
## 2018 12 20
## Compute minimum population size N (given prob)
## -----------------------------------------------

## Compute suitable minimum population size value N: ------

## Criterion: All 4 SDT cells should have a minimal frequency of min_freq:

## comp_min_N: Documentation ----

#' Compute a suitable minimum population size value N.
#'
#' \code{comp_min_N} computes a population size value \code{\link{N}} (an integer
#' as a power of 10) so that the frequencies of the 4 combinations of conditions and decisions
#' (i.e., the cells of the confusion table, or center row of boxes in the frequency prism)
#' reach or exceed a minimum value \code{min_freq} given the basic parameters
#' \code{\link{prev}}, \code{\link{sens}}, and \code{\link{spec}} (\code{spec = 1 - \link{fart}}).
#'
#' Using this function helps avoiding excessively small decimal values in categories
#' -- especially \code{\link{hi}}, \code{\link{mi}}, \code{\link{fa}}, \code{\link{cr}} --
#' when expressing combinations of conditions and decisions as natural frequencies.
#' As values of zero (0) are tolerable, the function only increases \code{\link{N}}
#' (in powers of 10) while the current value of any frequency (cell in confusion table or
#' leaf of a frequency tree) is positive but below \code{min_freq}.
#'
#' By default, \code{\link{comp_freq_prob}} and \code{\link{comp_freq}}
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
#' @param min_freq The minimum frequency of each combination of
#' a condition and a decision (i.e., hits, misses, false alarms, and correct rejections).
#' Default: \code{min_freq = 1}.
#'
#' @return An integer value \code{\link{N}} (as a power of 10).
#'
#' @examples
#' comp_min_N(0, 0, 0)  # => 1
#' comp_min_N(1, 1, 1)  # => 1
#'
#' comp_min_N(1, 1, 1, min_freq = 10)  # =>  10
#' comp_min_N(1, 1, 1, min_freq = 99)  # => 100
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

## comp_min_N: Definition ------

comp_min_N <- function(prev, sens, spec,  # 3 essential probabilities
                       min_freq = 1) {

  ## (1) initialize:
  N <- 10^0

  ## (2) Only if triple of essential probabilities is valid:
  if (is_valid_prob_set(prev = prev, sens = sens, spec = spec)) {

    ## (3) Issue a warning if probabilities describe an extreme case:
    is_extreme_prob_set(prev = prev, sens = sens, spec = spec)  # prints a warning if TRUE

    ## (4) Compute frequency of 4 SDT cases (without rounding):
    n_hi <- N * prev * sens
    n_mi <- N * prev * (1 - sens)
    n_cr <- N * (1 - prev) * spec
    n_fa <- N * (1 - prev) * (1 - spec)

    ## (5) While freq of 4 SDT cases < min_freq:
    while ((n_hi > 0  &&  n_hi < min_freq) |
           (n_mi > 0  &&  n_mi < min_freq) |
           (n_cr > 0  &&  n_cr < min_freq) |
           (n_fa > 0  &&  n_fa < min_freq)) {

      # (a) Multiply N by 10:
      N <- (N * 10)

      # (b) Update frequency of 4 SDT cases for current N (in next loop):
      n_hi <- N * prev * sens
      n_mi <- N * prev * (1 - sens)
      n_cr <- N * (1 - prev) * spec
      n_fa <- N * (1 - prev) * (1 - spec)

    }
  }

  ## (6) Return number N:
  return(N)

}

## Check:
# comp_min_N(0, 0, 0)  # => 1
# comp_min_N(1, 1, 1)  # => 1
# comp_min_N(1, 1, 1, min_freq = 10)  # =>  10
# comp_min_N(1, 1, 1, min_freq = 99)  # => 100
# comp_min_N(.1, .1, .1)        # =>       100 = 10^2
# comp_min_N(.001, .1, .1)      # =>    10 000 = 10^4
# comp_min_N(.001, .001, .1)    # => 1 000 000 = 10^6
# comp_min_N(.001, .001, .001)  # => 1 000 000 = 10^6


## (*) Done: ----------

## - Clean up code [2021 03 20]


## (+) ToDo: ----------

## eof. ------------------------------------------
