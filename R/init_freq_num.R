## init_freq_num.R | riskyr
## 2018 09 01
## Compute all current frequencies (freq) based on num
## (using only the 4 necessary parameters of num):
## -----------------------------------------------

## Note: Always use num (essential) rather than env (NON-essential)!

## Table of current terminology: -----------------

# Probabilities (10+):              Frequencies (11):
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

# acc  = overall accuracy (proportion correct)
# wacc = weighted accuracy
# mcc  = Matthews correlation coefficient
# f1s  = harmonic mean of PPV and sens


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



## (1) Initialize all frequencies as a list (of NA values) freq: ---------
##     Currently 11 frequencies (4 essential ones):

## init_freq Definition: ------

init_freq <- function() {

  ## (0) Initialize freq as a list:
  freq <- list(

    ## (1) Population size:
    "N" = NA, # Number of cases overall

    ## (2) Splits into 2 subgroups:
    ## (a) Number of true cases by condition
    ##     (= columns of confusion matrix):
    "cond.true"  = NA, # N of cond TRUE
    "cond.false" = NA, # N of cond FALSE

    ## (b) Number of decisions
    ##     (= rows of confusion matrix):
    "dec.pos" = NA, # N of dec POS [was: dec.true]
    "dec.neg" = NA, # N of dec NEG [was: dec.false]

    ## (c) Correspondence of decision to condition
    ##     (= diagonals of confusion matrix):
    "dec.cor" = NA, # N of correct decisions
    "dec.err" = NA, # N of erroneous decisions

    ## (3) Splits into 4 subgroups
    ##     (= 4 cells or SDT combinations of confusion matrix):
    "hi" = NA, # true positive
    "mi" = NA, # false negative
    "fa" = NA, # false positive
    "cr" = NA  # true negative
  )

  ## Return entire list of 11 freq:
  return(freq)

}

## Check: ------
# init_freq()          # initializes empty freq
# length(init_freq())  # =>  11 frequencies




## (2) Compute all frequencies from 3 essential probabilities: ----------

## comp_freq: Documentation --------

#' Compute frequencies from (3 essential) probabilities.
#'
#' \code{comp_freq} computes frequencies (typically
#' as rounded integers) given 3 basic probabilities --
#' \code{\link{prev}}, \code{\link{sens}}, and \code{\link{spec}} --
#' -- for a population of \code{\link{N}} individuals.
#' It returns a list of 11 frequencies \code{\link{freq}}
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
#' By default, \code{comp_freq} and its wrapper function
#' \code{\link{comp_freq_prob}}
#' round frequencies to nearest integers to avoid decimal values in
#' \code{\link{freq}} (i.e., \code{round = TRUE} by default).
#' Using the option \code{round = FALSE} turns off rounding.
#'
#' Key relationships between frequencies and probabilities:
#'
#' \itemize{
#'
#' \item Three \emph{perspectives} on a population:
#'
#' A population of \code{\link{N}} individuals can be split into 2 subsets in 3 different ways:
#'
#'    \enumerate{
#'
#'    \item by condition:
#'
#'    \code{\link{N} = \link{cond.true} + \link{cond.false}}
#'
#'    The frequency \code{\link{cond.true}} depends on the prevalence \code{\link{prev}}
#'    and
#'    the frequency \code{\link{cond.false}} depends on the prevalence's complement \code{1 - \link{prev}}.
#'
#'
#'    \item by decision:
#'
#'    \code{\link{N} = \link{dec.pos} + \link{dec.neg}}
#'
#'    The frequency \code{\link{dec.pos}} depends on the proportion of positive decisions \code{\link{ppod}}
#'    and
#'    the frequency \code{\link{dec.neg}} depends on the proportion of negative decisions \code{1 - \link{ppod}}.
#'
#'    \item by accuracy (i.e., correspondence of decision to condition):
#'
#'    \code{\link{N} = \link{dec.cor} + \link{dec.err}}
#'
#'    }
#'
#' Each perspective combines 2 pairs of the 4 essential probabilities (hi, mi, fa, cr).
#'
#' When providing probabilities, the population size \code{\link{N}}
#' is a free parameter (independent of the essential probabilities
#' \code{\link{prev}}, \code{\link{sens}}, and \code{\link{spec}}).
#'
#' If \code{\link{N}} is unknown (\code{NA}),
#' a suitable minimum value can be computed by \code{\link{comp_min_N}}.
#'
#'
#' \item Defining probabilities in terms of frequencies:
#'
#' Probabilities \emph{are} -- determine, describe, or are defined as --
#' the relationships between frequencies.
#' Thus, they can be computed as ratios between frequencies:
#'
#'   \enumerate{
#'
#'   \item prevalence \code{\link{prev}}:
#'
#'   \code{\link{prev} = \link{cond.true}/\link{N}  =  (\link{hi} + \link{mi}) / (\link{hi} + \link{mi} + \link{fa} + \link{cr})}
#'
#'
#'   \item sensitivity \code{\link{sens}}:
#'
#'   \code{\link{sens} = \link{hi}/\link{cond.true}  =  \link{hi} / (\link{hi} + \link{mi})  =  (1 - \link{mirt})}
#'
#'
#'   \item miss rate \code{\link{mirt}}:
#'
#'   \code{\link{mirt} = \link{mi}/\link{cond.true}  =  \link{mi} / (\link{hi} + \link{mi})  =  (1 - \link{sens})}
#'
#'
#'   \item specificity \code{\link{spec}}:
#'
#'   \code{\link{spec} = \link{cr}/\link{cond.false}  =  \link{cr} / (\link{fa} + \link{cr})  =  (1 - \link{fart})}
#'
#'
#'   \item false alarm rate \code{\link{fart}}:
#'
#'   \code{\link{fart} = \link{fa}/\link{cond.false}  =  \link{fa} / (\link{fa} + \link{cr})  =  (1 - \link{spec})}
#'
#'
#'   \item proportion of positive decisions \code{\link{ppod}}:
#'
#'   \code{\link{ppod} = \link{dec.pos}/\link{N}  =  (\link{hi} + \link{fa}) / (\link{hi} + \link{mi} + \link{fa} + \link{cr})}
#'
#'
#'   \item positive predictive value \code{\link{PPV}}:
#'
#'   \code{\link{PPV} = \link{hi}/\link{dec.pos}  =  \link{hi} / (\link{hi} + \link{fa})  =  (1 - \link{FDR})}
#'
#'
#'   \item negative predictive value \code{\link{NPV}}:
#'
#'   \code{\link{NPV} = \link{cr}/\link{dec.neg}  =  \link{cr} / (\link{mi} + \link{cr})  =  (1 - \link{FOR})}
#'
#'
#'   \item false detection rate \code{\link{FDR}}:
#'
#'   \code{\link{FDR} = \link{fa}/\link{dec.pos}  =  \link{fa} / (\link{hi} + \link{fa})  =  (1 - \link{PPV})}
#'
#'
#'   \item false omission rate \code{\link{FOR}}:
#'
#'   \code{\link{FOR} = \link{mi}/\link{dec.neg}  =  \link{mi} / (\link{mi} + \link{cr})  =  (1 - \link{NPV})}
#'
#'    }
#'
#' }
#'
#' Functions translating between representational formats:
#'
#' \enumerate{
#'
#'    \item \code{\link{comp_freq_prob}} is
#'    a wrapper function for \code{comp_freq} and
#'    an analog to 3 other format conversion functions:
#'
#'    \item \code{\link{comp_freq_freq}} computes
#'    current \emph{frequency} information contained in \code{\link{freq}}
#'    from 4 essential frequencies
#'    (\code{\link{hi}}, \code{\link{mi}}, \code{\link{fa}}, \code{\link{cr}}).
#'
#'    \item \code{\link{comp_prob_freq}} computes
#'    current \emph{probability} information contained in \code{\link{prob}}
#'    from 4 essential frequencies
#'    (\code{\link{hi}}, \code{\link{mi}}, \code{\link{fa}}, \code{\link{cr}}).
#'
#'    \item \code{\link{comp_prob_prob}} computes
#'    current \emph{probability} information contained in \code{\link{prob}}
#'    from 3 essential probabilities
#'    (\code{\link{prev}}, \code{\link{sens}}, \code{\link{spec}}).
#'
#' }
#'
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
#' @param N The number of individuals in the population.
#' If \code{\link{N}} is unknown (\code{NA}),
#' a suitable minimum value is computed by \code{\link{comp_min_N}}.
#'
#' @param round A Boolean value that determines whether frequencies are
#' rounded to the nearest integer. Default: \code{round = TRUE}.
#'
#' @return A list \code{\link{freq}} containing 9 frequency values.
#'
#' @examples
#' comp_freq()                  # => ok, using current defaults
#' length(comp_freq())          # => 11
#'
#' # Rounding effects:
#' comp_freq(prev = .1, sens = .9, spec = .8, N = 10)  # => 1 hit (TP, rounded)
#' comp_freq(prev = .1, sens = .9, spec = .8, N = 10, round = FALSE)    # => .9 hit
#' comp_freq(prev = 1/3, sens = 6/7, spec = 2/3, N = 1, round = FALSE)  # => 0.2857143 hit
#'
#' # Extreme cases:
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
#' comp_freq(prev = .5, sens = .5, spec = .5, N = 10, round = TRUE)   # => ok, rounded (see mi and fa)
#' comp_freq(prev = .5, sens = .5, spec = .5, N = 10, round = FALSE)  # => ok, not rounded
#'
#' # Ways to fail:
#' comp_freq(prev = NA,  sens = 1, spec = 1,  100)   # => NAs + warning (prev NA)
#' comp_freq(prev = 1,  sens = NA, spec = 1,  100)   # => NAs + warning (sens NA)
#' comp_freq(prev = 1,  sens = 1,  spec = NA, 100)   # => NAs + warning (spec NA)
#' comp_freq(prev = 8,  sens = 1,  spec = 1,  100)   # => NAs + warning (prev beyond range)
#' comp_freq(prev = 1,  sens = 8,  spec = 1,  100)   # => NAs + warning (sens beyond range)
#'
#' @family functions computing frequencies
#'
#' @seealso
#' \code{\link{comp_freq_prob}} corresponding wrapper function;
#' \code{\link{num}} contains basic numeric variables;
#' \code{\link{init_num}} initializes basic numeric variables;
#' \code{\link{freq}} contains current frequency information;
#' \code{\link{prob}} contains current probability information;
#' \code{\link{comp_prob}} computes current probability information;
#' \code{\link{comp_complement}} computes a probability's complement;
#' \code{\link{comp_comp_pair}} computes pairs of complements;
#' \code{\link{comp_complete_prob_set}} completes valid sets of probabilities;
#' \code{\link{comp_min_N}} computes a suitable population size \code{\link{N}} (if missing).
#'
#' @export

## comp_freq: Definition --------

comp_freq <- function(prev = num$prev, sens = num$sens, spec = num$spec, # 3 essential probabilities (NOT: mirt, fart)
                      N = num$N,    # default N
                      round = TRUE  # should freq be rounded to integers? (default: round = TRUE)
) {

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

    ## (a) Number of cond.true vs. cond.false cases (by condition):
    if (round) {
      freq$cond.true <- round((N * prev), 0)  # 1a. cond.true  = N x prev [rounded to nearest integer]
    } else {
      freq$cond.true <- (N * prev)            # 1b. cond.true  = N x prev [not rounded]
    }
    freq$cond.false <- (N - freq$cond.true)   # 2. cond.false = complement of cond.true (to N)

    ## (b) Number of 4 SDT combinations:
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

    ## (c) Number of positive vs. negative decisions (by decision):
    freq$dec.pos <- freq$hi + freq$fa  # 1. positive decisions (true & false positives)
    freq$dec.neg <- freq$mi + freq$cr  # 2. negative decisions (false & true negatives)

    ## (d) Accuracy/Correspondence of decision to condition (by correspondence):
    freq$dec.cor <- freq$hi + freq$cr  # N of correct decisions
    freq$dec.err <- freq$mi + freq$fa  # N of erroneous decisions

    ## (6) Check results for consistency:
    tol <- .0001  # tolerance threshold for mismatch of sums

    if ((abs(freq$N - (freq$hi + freq$mi + freq$fa + freq$cr)) > tol) ||
        (abs(freq$cond.true - (freq$hi + freq$mi)) > tol)             ||
        (abs(freq$cond.false - (freq$fa + freq$cr)) > tol)            ||
        # (freq$dec.pos != freq$hi + freq$fa)             ||  # (computed as such above)
        # (freq$dec.neg != freq$mi + freq$cr)             ||  # (computed as such above)
        # (freq$dec.cor != freq$hi + freq$cr)             ||  # (computed as such above)
        # (freq$dec.err != freq$mi + freq$fa)             ||  # (computed as such above)
        (abs(freq$N - (freq$cond.true + freq$cond.false)) > tol)      ||
        (abs(freq$N - (freq$dec.pos + freq$dec.neg)) > tol)           ||
        (abs(freq$N - (freq$dec.cor + freq$dec.err)) > tol)           ) {

      warning("Current frequencies do NOT add up.")
    }

  } # if (is_valid(prev, sens, spec, fart))

  ## (7) Return entire list freq:
  return(freq)

}

## Check: --------

# comp_freq()                  # => ok, using current defaults
# length(comp_freq())          # => 11
#
# # Rounding effects:
# comp_freq(prev = .1, sens = .9, spec = .8, N = 10)  # => 1 hit (TP, rounded)
# comp_freq(prev = .1, sens = .9, spec = .8, N = 10, round = FALSE)  # => .9 hit
# comp_freq(prev = 1/3, sens = 6/7, spec = 2/3, N = 1, round = FALSE)  # => 0.2857143 hit
#
# # Extreme cases:
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
# comp_freq(prev = .5, sens = .5, spec = .5, N = 10, round = TRUE)   # => ok, but rounded (increasing errors: mi and fa)
# comp_freq(prev = .5, sens = .5, spec = .5, N = 10, round = FALSE)  # => ok, but not rounded
#
# # Ways to fail:
# comp_freq(prev = NA,  sens = 1, spec = 1,  100)  # => NAs + warning (prev NA)
# comp_freq(prev = 1,  sens = NA, spec = 1,  100)  # => NAs + warning (sens NA)
# comp_freq(prev = 1,  sens = 1,  spec = NA, 100)  # => NAs + warning (spec NA)
# comp_freq(prev = 8,  sens = 1,  spec = 1,  100)  # => NAs + warning (prev beyond range)
# comp_freq(prev = 1,  sens = 8,  spec = 1,  100)  # => NAs + warning (sens beyond range)




## (3) Apply to initialize freq: -----------------

## freq: Documentation --------

#' List current frequency information.
#'
#' \code{freq} is a list of named numeric variables
#' containing 11 frequencies:
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
#'  \item the number of cases for which \code{\link{dec.cor}}
#'  \item the number of cases for which \code{\link{dec.err}}
#'
#'  \item the number of true positives, or hits \code{\link{hi}}
#'  \item the number of false negatives, or misses \code{\link{mi}}
#'  \item the number of false positives, or false alarms \code{\link{fa}}
#'  \item the number of true negatives, or correct rejections \code{\link{cr}}
#'
#' }
#'
#' These frequencies are computed from basic parameters
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
#'     \item \code{\link{N} = \link{dec.cor} + \link{dec.err}} (by correspondence of decision to condition)
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
#' length(freq)         # => 11 known frequencies
#' names(freq)          # => show names of known frequencies
#'
#' @family lists containing current scenario information
#'
#' @seealso
#' \code{\link{comp_freq}} computes current frequency information;
#' \code{\link{num}} contains basic numeric variables;
#' \code{\link{init_num}} initializes basic numeric variables;
#' \code{\link{prob}} contains current probability information;
#' \code{\link{num}} contains basic numeric parameters;
#' \code{\link{init_num}} initializes basic numeric parameters;
#' \code{\link{txt}} contains current text information;
#' \code{\link{init_txt}} initializes text information;
#' \code{\link{pal}} contains current color information;
#' \code{\link{init_pal}} initializes color information.
#'
#' @export

## freq: Definition --------

freq <- comp_freq()  # => initialize freq to default parameters

## Check: ------
# freq               # => show current values
# length(freq)       # => 11 known frequencies
# names(freq)        # => show names of known frequencies




## comp_freq_type: Determine the type of a named frequency (freq):  ----------

comp_freq_type <- function(fname) {

  f_type <- "typeless"  # initialize

  # (1) Define types of all known frequencies:

  # (a) Using basic names:
  # freq_types <- c("popu", rep("cond", 2), rep("dec", 2), rep("accu", 2), rep("cell", 4))

  # (b) Using labels defined in txt:
  freq_types <- c(txt$popu.lbl,
                  rep(txt$cond.lbl, 2),
                  rep(txt$dec.lbl, 2),
                  rep(txt$acc.lbl, 2),
                  rep(txt$sdt.lbl, 4))
  # freq_types

  # (2) Map freq to name in freq_types:
  if (fname %in% names(freq)) { # if freq corresponds to named frequency in freq:
    ix <- which(names(freq) == fname)  # index in freq
    # print(ix)
    f_type <- freq_types[ix]
  }

  return(f_type)  # return f_type (as character)

}

## Check: --------
# comp_freq_type("N")
# comp_freq_type("cond.false")
# comp_freq_type("dec.neg")
# comp_freq_type("dec.err")
# comp_freq_type("cr")

## Note:
# comp_freq_type(N)        # => typeless (as function requires name, NOT a value)
# comp_freq_type("false")  # => typeless (as full name is required)



## comp_freq_col: Determine the color of a named frequency (freq):  ----------

comp_freq_col <- function(fname) {

  # initialize:
  col_name <- NA
  f_col <- NA

  if (fname %in% names(freq)) { # if freq corresponds to named frequency in freq:

    ## Derive current values corresponding to freq:
    ix <- which(names(freq) == fname)  # index in freq

    ## (a) Value of frequency in freq:
    # f_val <- freq[ix]

    ## (b) Type of frequency:
    # f_type <- comp_freq_type(fname)  # see helper function (defined in init_freq_num.R)

    # (c) Color of frequency:
    # Note that names(freq) are sometimes longer than names(pal):
    if (any(grep(pattern = "\\.", x = fname))) {  # if fname contains a dot (.):

      nameparts <- unlist(strsplit(fname, split = "\\."))

      fname_1st <- nameparts[1]  # 1st part of fname
      fname_2nd <- nameparts[2]  # 2nd part of fname

      col_name <- fname_2nd  # 2nd part corresponds to name of color

      # if (col_name == "true") { col_name <- "cor" }

    } else {
      col_name <- fname
    }

    # print(col_name)

  }

  # Find color value of col_name in color pal:
  if (col_name %in% names(pal)) { # if col_name corresponds to a color name in pal
    f_col <- pal[col_name]     # use this color to fill box
  } else {
    f_col <- grey(.95, .50) # use some default color (e.g., "white")
  }

  # print(f_col)

  return(f_col)

}

## Check: --------
# comp_freq_col("N")
# comp_freq_col("hi")
# comp_freq_col("dec.err")
# comp_freq_col("dec.cor")
#
# comp_freq_col("default")  # default color



## (*) Done: -----------

## - Clean up code [2018 08 30].

## - Added help functions comp_freq_type and comp_freq_col
##   to classify freq into types and determine freq color
##   based on freq name.                             [2018 08 18]

## - Added 2 more frequencies for accuracy, i.e.,
##   "decision correctness" or correspondence of decision to condition:
##   "dec.cor" vs. "dec.err" (i.e., diagonal of confusion matrix)

## (+) ToDo: -----------

## - ...

## eof. ------------------------------------------
