## init_num_freq.R | riskyr
## 2018 01 27
## -----------------------------------------------
## Compute all current frequencies (freq) based on num
## (using only the 4 necessary parameters of num):

## Note: Always use num (essential) rather than env (NON-essential)!

## -----------------------------------------------
## Table of current terminology:

# Probabilities (10):               Frequencies (9):
# -------------------               ------------------
# (A) by condition:

# non-conditional:                          N
# prev*                           cond.true | cond.false

# conditional:
# sens* = hit rate = TPR                hi* = TP
# mirt  = miss rate = FNR               mi* = FN
# fart  = false alarm rate = FPR        fa* = FP
# spec* = true negative rate = TNR      cr* = TN

# [Note: *...is essential]


# (B) by decision:                 Combined frequencies:

# non-conditional:
# ppod = proportion of dec.pos     dec.pos | dec.neg

# conditional:
# PPV = precision
# FDR = false detection rate
# FOR = false omission rate
# NPV = neg. pred. value


## -----------------------------------------------
## Data flow: Two basic directions:

## (1) Probabilities ==> frequencies:
##     Bayesian: based on 3 essential probabilities:
##   - given:   prev;  sens, spec
##   - derived: all other values

## (2) Frequencies ==> probabilities:
##     Frequentist: based on 4 essential natural frequencies:
##   - given:   N = hi, mi, fa, cr
##   - derived: all other values


## -----------------------------------------------
## (1) Initialize freq as a list (of NA values)
##     of 9 frequencies (4 essential ones):

init_freq <- function() {

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

  ## Return entire list freq:
  return(freq)

}

## Check:
# init_freq()          # initializes empty freq
# length(init_freq())  # =>  9 frequencies

## -----------------------------------------------
## (2) Compute 9 frequencies from 3 essential probabilities:

#' Compute frequencies from (3 essential) probabilities.
#'
#' \code{comp_freq} computes frequencies (typically
#' as rounded integers) given 3 basic probabilities --
#' \code{\link{prev}}, \code{\link{sens}}, and \code{\link{spec}} --
#' -- for a population of \code{\link{N}} individuals.
#' It returns a list of 9 frequencies \code{\link{freq}}
#' as its output.
#'
#' In addition to \code{\link{prev}}, both
#' \code{\link{sens}} and \code{\link{spec}} are necessary arguments.
#' If only their complements \code{\link{mirt}} or \code{\link{fart}}
#' are known, use the wrapper function \code{\link{comp_freq_prob}}
#' which also accepts \code{\link{mirt}} and \code{\link{fart}} as inputs
#' (but requires that the entire set of provided probabilities is
#' sufficient and consistent).
#' Alternatively, use \code{\link{comp_complement}},
#' \code{\link{comp_comp_pair}}, or \code{\link{comp_complete_prob_set}}
#' to obtain the 3 essential probabilities.
#'
#' \code{comp_freq} is the frequency counterpart to the
#' probability function \code{\link{comp_prob}}.
#'
#' By default, \code{\link{comp_freq}} rounds frequencies
#' to nearest integers to avoid decimal values in
#' \code{\link{freq}}. Use \code{round = FALSE}
#' to switch off rounding.
#'
#' Key relationships:
#'
#' \enumerate{
#'
#' \item to probabilities:
#' A population of \code{\link{N}} individuals can be split into 2 subsets
#' in 2 different ways:
#'
#' \enumerate{
#'   \item by condition:
#'   The frequency \code{\link{cond.true}} depends on the prevalence \code{\link{prev}}
#'   and
#'   the frequency \code{\link{cond.false}} depends on the prevalence's complement \code{1 - \link{prev}}.
#'
#'   \item by decision:
#'   The frequency \code{\link{dec.pos}} depends on the proportion of positive decisions \code{\link{ppod}}
#'   and
#'   the frequency \code{\link{dec.neg}} depends on the proportion of negative decisions \code{1 - \link{ppod}}.
#'
#' }
#'
#' The population size \code{\link{N}} is a free parameter (independent of the
#' essential probabilities \code{\link{prev}}, \code{\link{sens}}, and \code{\link{spec}}).
#'
#' If \code{\link{N}} is unknown, a suitable minimum value can be computed by \code{\link{comp_min_N}}.
#'
#'   \item to other frequencies:
#'   In a population of size \code{\link{N}}
#'   the following relationships hold:
#'
#'   \itemize{
#'
#'     \item \code{\link{N} = \link{cond.true} + \link{cond.false}} (by condition)
#'
#'     \item \code{\link{N} = \link{dec.pos} + \link{dec.neg}} (by decision)
#'
#'     \item \code{\link{N} = \link{hi} + \link{mi} + \link{fa} + \link{cr}} (by condition x decision)
#'   }
#'
#' }
#'
#' @param prev The condition's prevalence \code{\link{prev}}
#' (i.e., the probability of condition being \code{TRUE}).
#'
#' @param sens The decision's sensitivity \code{\link{sens}}
#' (i.e., the conditional probability of a positive decision
#' provided that the condition is \code{TRUE}).
#'
#' @param spec The decision's specificity value \code{\link{spec}}
#' (i.e., the conditional probability
#' of a negative decision provided that the condition is \code{FALSE}).
#'
#'
#' @param N The number of individuals in the population.
#' If \code{\link{N}} is unknown (\code{NA}),
#' a suitable minimum value is computed by \code{\link{comp_min_N}}.
#'
#' @param round A Boolean value that determines whether frequencies are
#' rounded to the nearest integer. Default: \code{round = TRUE}.
#'
#' @return A list \code{\link{freq}} containing 9 frequency values.
#'
#'
#' @examples
#' comp_freq()                  # => ok, using current defaults
#' length(comp_freq())          # => 9
#'
#' Ways to succeed:
#' comp_freq(prev = 1, sens = 1, spec = 1, 100)  # => ok, N hits (TP)
#' comp_freq(prev = 1, sens = 1, spec = 0, 100)  # => ok, N hits
#' comp_freq(prev = 1, sens = 0, spec = 1, 100)  # => ok, N misses (FN)
#' comp_freq(prev = 1, sens = 0, spec = 0, 100)  # => ok, N misses
#' comp_freq(prev = 0, sens = 1, spec = 1, 100)  # => ok, N correct rejections (TN)
#' comp_freq(prev = 0, sens = 1, spec = 0, 100)  # => ok, N false alarms (FP)
#'
#' # Watch out for:
#' comp_freq(prev = 1, sens = 1, spec = 1, N = NA)  # => ok, but warning that N = 1 was computed
#' comp_freq(prev = 1, sens = 1, spec = 1, N =  0)  # => ok, but all 0 + warning (extreme case: N hits)
#' comp_freq(prev = .5, sens = .5, spec = .5, N = 10, round = TRUE)  # => ok, but all rounded (increasing errors: mi and fa)
#' comp_freq(prev = .5, sens = .5, spec = .5, N = 10, round = FALSE)  # => ok, but not rounded
#'
#' # Ways to fail:
#' comp_freq(prev = NA,  sens = 1, spec = 1,  100)   # => NAs + warning (prev NA)
#' comp_freq(prev = 1,  sens = NA, spec = 1,  100)   # => NAs + warning (sens NA)
#' comp_freq(prev = 1,  sens = 1,  spec = NA, 100)  # => NAs + warning (spec NA)
#' comp_freq(prev = 8,  sens = 1,  spec = 1,  100)   # => NAs + warning (prev beyond range)
#' comp_freq(prev = 1,  sens = 8,  spec = 1,  100) # => NAs + warning (sens beyond range)
#'
#'
#' @family functions computing frequencies
#'
#' @seealso
#' \code{\link{num}} contains basic numeric variables;
#' \code{\link{init_num}} initializes basic numeric variables;
#' \code{\link{freq}} contains current frequency information;
#' \code{\link{prob}} contains current probability information;
#' \code{\link{comp_prob}} computes current probability information;
#' \code{\link{comp_complement}} computes a probability's complement;
#' \code{\link{comp_comp_pair}} computes pairs of complements;
#' \code{\link{comp_complete_prob_set}} completes valid sets of probabilities;
#' \code{\link{comp_min_N}} computes a suitable population size \code{\link{N}} (if missing).


comp_freq <- function(prev = num$prev, sens = num$sens, spec = num$spec, # 3 essential probabilities (NOT: mirt, fart)
                      N = num$N,
                      round = TRUE) {

  ## (0) Initialize freq:
  freq <- init_freq()  # initialize freq (containing only NA values)


  ## (1) Only if 3 essential probabilities are valid:
  if (is_valid_prob_set(prev = prev, sens = sens, spec = spec)) {
  # if (is_valid_prob_triple(prev = prev, sens = sens, spec = spec)) {

    ## (2) Compute missing fart or spec (4th argument) value (if applicable):
    # cur.spec.fart <- comp_comp_pair(spec, fart)  # (do only when needed)
    # spec <- cur.spec.fart[1]  # 1st argument
    # fart <- cur.spec.fart[2]  # 2nd argument

    ## (3) Issue a warning if essential probabilities describe an extreme case:
    is_extreme_prob_set(prev = prev, sens = sens, spec = spec)  # prints a warning if TRUE

    ## (4) Compute missing population size value N (if applicable):
    if (is.na(N)) {

      N <- comp_min_N(prev = prev, sens = sens, spec = spec, min.freq = 1)

      warning(paste0("Unknown population size N. A suitable minimum value of N = ", N, " was computed."))
    }

    ## (5) Set or compute all values of freq:
    freq$N <- N # copy N from argument OR num (input)

    ## (6) Number of true cases by condition:
    ##     (= 1st level of natural frequency tree):
    if (round) {
      freq$cond.true <- round((N * prev), 0)  # 1a. cond.true  = N x prev [rounded to nearest integer]
    } else {
      freq$cond.true <- (N * prev)            # 1b. cond.true  = N x prev [not rounded]
    }
    freq$cond.false <- (N - freq$cond.true)   # 2. cond.false = complement of cond.true (to N)

    ## (7) Number of SDT combinations:
    ##     (= 2nd level/leaves of natural frequency tree):
    if (round) {
      freq$hi <- round((sens * freq$cond.true), 0)   # a1. N of hi [rounded to nearest integer]
    } else {
      freq$hi <- (sens * freq$cond.true)             # a2. N of hi [not rounded]
    }
    freq$mi <- (freq$cond.true - freq$hi)            # b.  N of mi = complement of hi (to cond.true)

    if (round) {
      freq$cr <- round((spec * freq$cond.false), 0)  # c1. N of cr [rounded to nearest integer]
    } else {
      freq$cr <- (spec * freq$cond.false)            # c2. N of cr [not rounded]
    }
    freq$fa <- (freq$cond.false - freq$cr)           # d.  N of fa - complement of cr (to cond.false)

    ## (8) Number of decisions:
    freq$dec.pos <- freq$hi + freq$fa # 1. positive decisions (true & false positives)
    freq$dec.neg <- freq$mi + freq$cr # 2. negative decisions (false & true negatives)

    ## (9) Check for consistency:
    if ((freq$N != freq$hi + freq$mi + freq$fa + freq$cr) ||
        (freq$cond.true  != freq$hi + freq$mi)            ||
        (freq$cond.false != freq$fa + freq$cr)            ||
        (freq$dec.pos != freq$hi + freq$fa)               ||
        (freq$dec.neg != freq$mi + freq$cr)               ||
        (freq$N != freq$cond.true + freq$cond.false)      ||
        (freq$N != freq$dec.pos + freq$dec.neg)            ) {

      warning("Current frequencies do NOT add up.")
    }

  } # if (is_valid(prev, sens, spec, fart))

  ## (10) Return entire list freq:
  return(freq)

}

## Check:
{
  # comp_freq()                  # => ok, using current defaults
  # length(comp_freq())          # => 9
  #
  # # Ways to succeed:
  # comp_freq(prev = 1, sens = 1, spec = 1, 100)  # => ok, N hits (TP)
  # comp_freq(prev = 1, sens = 1, spec = 0, 100)  # => ok, N hits
  # comp_freq(prev = 1, sens = 0, spec = 1, 100)  # => ok, N misses (FN)
  # comp_freq(prev = 1, sens = 0, spec = 0, 100)  # => ok, N misses
  # comp_freq(prev = 0, sens = 1, spec = 1, 100)  # => ok, N correct rejections (TN)
  # comp_freq(prev = 0, sens = 1, spec = 0, 100)  # => ok, N false alarms (FP)
  #
  # # Watch out for:
  # comp_freq(prev = 1, sens = 1, spec = 1, N = NA)  # => ok, but warning that N = 1 was computed
  # comp_freq(prev = 1, sens = 1, spec = 1, N =  0)  # => ok, but all 0 + warning (extreme case: N hits)
  # comp_freq(prev = .5, sens = .5, spec = .5, N = 10, round = TRUE)  # => ok, but all rounded (increasing errors: mi and fa)
  # comp_freq(prev = .5, sens = .5, spec = .5, N = 10, round = FALSE)  # => ok, but not rounded
  #
  # # Ways to fail:
  # comp_freq(prev = NA,  sens = 1, spec = 1,  100)   # => NAs + warning (prev NA)
  # comp_freq(prev = 1,  sens = NA, spec = 1,  100)   # => NAs + warning (sens NA)
  # comp_freq(prev = 1,  sens = 1,  spec = NA, 100)  # => NAs + warning (spec NA)
  # comp_freq(prev = 8,  sens = 1,  spec = 1,  100)   # => NAs + warning (prev beyond range)
  # comp_freq(prev = 1,  sens = 8,  spec = 1,  100) # => NAs + warning (sens beyond range)
}

## -----------------------------------------------
## (3) Apply to initialize freq:

#' List current frequency information.
#'
#' \code{freq} is a list of named numeric variables
#' containing 9 (natural) frequencies:
#'
#' \enumerate{
#'
#'  \item the population size \code{\link{N}}
#'
#'  \item the number of cases for which \code{\link{cond.true}}
#'  \item the number of cases for which \code{\link{cond.false}}
#'
#'  \item the number of cases for which \code{\link{dec.pos}}
#'  \item the number of cases for which \code{\link{dec.neg}}
#'
#'  \item the number true positives, or hits \code{\link{hi}}
#'  \item the number false negatives, or misses \code{\link{mi}}
#'  \item the number false positives, or false alarms \code{\link{fa}}
#'  \item the number true negatives, or correct rejections \code{\link{cr}}
#'
#' }
#'
#' These frequencies are computed from basic probabilities
#' (contained in \code{\link{num}}) and computed by using
#' \code{\link{comp_freq}}.
#'
#' The list \code{freq} is the frequency counterpart
#' to the list containing probability information \code{\link{prob}}.
#'
#' Natural frequencies are always expressed in
#' relation to the current population of
#' size \code{\link{N}}.
#'
#' Key relationships:
#'
#' \enumerate{
#'
#' \item to probabilities:
#' A population of \code{\link{N}} individuals can be split into 2 subsets
#' in 2 different ways:
#'
#' \enumerate{
#'   \item by condition:
#'   The frequency \code{\link{cond.true}} depends on the prevalence \code{\link{prev}}
#'   and
#'   the frequency \code{\link{cond.false}} depends on the prevalence's complement \code{(1 - \link{prev})}.
#'
#'   \item by decision:
#'   The frequency \code{\link{dec.pos}} depends on the proportion of positive decisions \code{\link{ppod}}
#'   and
#'   the frequency \code{\link{dec.neg}} depends on the proportion of negative decisions \code{(1 - \link{ppod})}.
#'
#' }
#'
#' The population size \code{\link{N}} is a free parameter (independent of the
#' essential probabilities \code{\link{prev}}, \code{\link{sens}}, and \code{\link{spec}}).
#'
#' If \code{\link{N}} is unknown, a suitable minimum value can be computed by \code{\link{comp_min_N}}.
#'
#'   \item to other frequencies:
#'   In a population of size \code{\link{N}}
#'   the following relationships hold:
#'
#'   \itemize{
#'
#'     \item \code{\link{N} = \link{cond.true} + \link{cond.false}} (by condition)
#'
#'     \item \code{\link{N} = \link{dec.pos} + \link{dec.neg}} (by decision)
#'
#'     \item \code{\link{N} = \link{hi} + \link{mi} + \link{fa} + \link{cr}} (by condition x decision)
#'   }
#'
#' }
#'
#' Visualizations of the current frequency information
#' are provided by \code{\link{plot_tree}} and
#' \code{\link{plot_mosaic}}.
#'
#' @examples
#' freq <- comp_freq()  # => initialize freq to default parameters
#' freq                 # => show current values
#' length(freq)         # => 9
#'
#'
#' @family lists containing scenario settings
#'
#' @seealso
#' \code{\link{comp_freq}} computes current frequency information;
#' \code{\link{num}} contains basic numeric variables;
#' \code{\link{init_num}} initializes basic numeric variables;
#' \code{\link{prob}} contains current probability information

freq <- comp_freq()  # => initialize freq to default parameters
# freq               # => show current values
# length(freq)       # => 9

## -----------------------------------------------
## (+) ToDo:

## -----------------------------------------------
## eof.
