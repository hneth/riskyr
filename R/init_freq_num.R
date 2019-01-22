## init_freq_num.R | riskyr
## 2019 01 22
## Compute all current frequencies (freq) based on num
## (using only the 4 necessary parameters of num):
## -----------------------------------------------

## Table of current terminology: -----------------

# Probabilities (13+):              Frequencies (11):
# -------------------               ------------------
# (A) by condition:

# non-conditional:                          N
# prev*                           cond_true | cond_false (columns)

# conditional:
# sens* = hit rate = TPR                hi* = TP
# mirt  = miss rate = FNR               mi* = FN
# fart  = false alarm rate = FPR        fa* = FP
# spec* = true negative rate = TNR      cr* = TN

# [Note: *...is essential]


# (B) by decision:                 Combined frequencies:

# non-conditional:
# ppod = proportion of dec_pos     dec_pos | dec_neg (rows)
#                                  dec_cor | dec_err (diagonal)

# conditional:
# PPV = precision
# FDR = false detection rate
# FOR = false omission rate
# NPV = neg. pred. value

# (C) by accuracy/correspondence of decision to condition (see accu):

# acc  = overall accuracy (probability/proportion correct decision)
# p_acc_hi = p(hi|acc)  # aka. acc-hi  "p(hi | dec_cor)"
# p_err_fa = p(fa|err)  # aka. err-fa  "p(fa | dec_err)"

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
    "cond_true"  = NA, # N of cond TRUE
    "cond_false" = NA, # N of cond FALSE

    ## (b) Number of decisions
    ##     (= rows of confusion matrix):
    "dec_pos" = NA, # N of dec POS [was: dec_true]
    "dec_neg" = NA, # N of dec NEG [was: dec_false]

    ## (c) Correspondence of decision to condition
    ##     (= diagonals of confusion matrix):
    "dec_cor" = NA, # N of correct decisions
    "dec_err" = NA, # N of erroneous decisions

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
#' for a population of \code{\link{N}} individuals.
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
#' When frequencies are rounded, probabilities computed from
#' \code{\link{freq}} may differ from exact probabilities.
#' Using the option \code{round = FALSE} turns off rounding.
#'
#'
#' Key relationships between probabilities and frequencies:
#'
#' \itemize{
#'
#' \item Three perspectives on a population:
#'
#' A population of \code{\link{N}} individuals can be split into 2 subsets of frequencies
#' in 3 different ways:
#'
#'    \enumerate{
#'
#'    \item by condition:
#'
#'    \code{\link{N} = \link{cond_true} + \link{cond_false}}
#'
#'    The frequency \code{\link{cond_true}} depends on the prevalence \code{\link{prev}}
#'    and
#'    the frequency \code{\link{cond_false}} depends on the prevalence's complement \code{1 - \link{prev}}.
#'
#'    \item by decision:
#'
#'    \code{\link{N} = \link{dec_pos} + \link{dec_neg}}
#'
#'    The frequency \code{\link{dec_pos}} depends on the proportion of positive decisions \code{\link{ppod}}
#'    and
#'    the frequency \code{\link{dec_neg}} depends on the proportion of negative decisions \code{1 - \link{ppod}}.
#'
#'    \item by accuracy (i.e., correspondence of decision to condition):
#'
#'    \code{\link{N} = \link{dec_cor} + \link{dec_err}}
#'
#'    }
#'
#' Each perspective combines 2 pairs of the 4 essential probabilities (hi, mi, fa, cr).
#'
#' When providing probabilities, the population size \code{\link{N}} is a free parameter (independent of the
#' essential probabilities \code{\link{prev}}, \code{\link{sens}}, and \code{\link{spec}}).
#'
#' If \code{\link{N}} is unknown (\code{NA}), a suitable minimum value can be computed by \code{\link{comp_min_N}}.
#'
#'
#' \item Defining probabilities in terms of frequencies:
#'
#' Probabilities \emph{are} -- determine, describe, or are defined as -- the relationships between frequencies.
#' Thus, they can be computed as ratios between frequencies:
#'
#'   \enumerate{
#'
#'   \item prevalence \code{\link{prev}}:
#'
#'   \code{\link{prev} = \link{cond_true}/\link{N}  =  (\link{hi} + \link{mi}) / (\link{hi} + \link{mi} + \link{fa} + \link{cr})}
#'
#'
#'   \item sensitivity \code{\link{sens}}:
#'
#'   \code{\link{sens} = \link{hi}/\link{cond_true}  =  \link{hi} / (\link{hi} + \link{mi})  =  (1 - \link{mirt})}
#'
#'
#'   \item miss rate \code{\link{mirt}}:
#'
#'   \code{\link{mirt} = \link{mi}/\link{cond_true}  =  \link{mi} / (\link{hi} + \link{mi})  =  (1 - \link{sens})}
#'
#'
#'   \item specificity \code{\link{spec}}:
#'
#'   \code{\link{spec} = \link{cr}/\link{cond_false}  =  \link{cr} / (\link{fa} + \link{cr})  =  (1 - \link{fart})}
#'
#'
#'   \item false alarm rate \code{\link{fart}}:
#'
#'   \code{\link{fart} = \link{fa}/\link{cond_false}  =  \link{fa} / (\link{fa} + \link{cr})  =  (1 - \link{spec})}
#'
#'
#'   \item proportion of positive decisions \code{\link{ppod}}:
#'
#'   \code{\link{ppod} = \link{dec_pos}/\link{N}  =  (\link{hi} + \link{fa}) / (\link{hi} + \link{mi} + \link{fa} + \link{cr})}
#'
#'
#'   \item positive predictive value \code{\link{PPV}}:
#'
#'   \code{\link{PPV} = \link{hi}/\link{dec_pos}  =  \link{hi} / (\link{hi} + \link{fa})  =  (1 - \link{FDR})}
#'
#'
#'   \item negative predictive value \code{\link{NPV}}:
#'
#'   \code{\link{NPV} = \link{cr}/\link{dec_neg}  =  \link{cr} / (\link{mi} + \link{cr})  =  (1 - \link{FOR})}
#'
#'
#'   \item false detection rate \code{\link{FDR}}:
#'
#'   \code{\link{FDR} = \link{fa}/\link{dec_pos}  =  \link{fa} / (\link{hi} + \link{fa})  =  (1 - \link{PPV})}
#'
#'
#'   \item false omission rate \code{\link{FOR}}:
#'
#'   \code{\link{FOR} = \link{mi}/\link{dec_neg}  =  \link{mi} / (\link{mi} + \link{cr})  =  (1 - \link{NPV})}
#'
#'
#'   \item accuracy \code{\link{acc}}:
#'
#'   \code{\link{acc} = \link{dec_cor}/\link{N}  =  (\link{hi} + \link{cr}) / (\link{hi} + \link{mi} + \link{fa} + \link{cr})}
#'
#'
#'   \item rate of hits, given accuracy \code{p_acc_hi}:
#'
#'   \code{p_acc_hi = \link{hi}/\link{dec_cor} = (1 - \link{cr}/\link{dec_cor})}
#'
#'
#'   \item rate of false alarms, given inaccuracy \code{p_err_fa}:
#'
#'   \code{p_err_fa = \link{fa}/\link{dec_err} = (1 - \link{mi}/\link{dec_err})}
#'
#'    }
#'
#'    Note: When frequencies are rounded (by \code{round = TRUE} in \code{\link{comp_freq}}),
#'    probabilities computed from \code{\link{freq}} may differ from exact probabilities.
#'
#' }
#'
#'
#' Functions translating between representational formats:
#' \code{\link{comp_prob_prob}}, \code{\link{comp_prob_freq}},
#' \code{\link{comp_freq_prob}}, \code{\link{comp_freq_freq}}
#' (see documentation of \code{\link{comp_prob_prob}} for details).
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
#' @param round  Boolean value that determines whether frequencies are
#' rounded to the nearest integer. Default: \code{round = TRUE}.
#'
#' Note: Removed \code{n_digits} parameter:  Number of digits to which frequency values
#' are to be rounded when \code{round = FALSE}.
#' Default: \code{n_digits = 5}.
#'
#'
#' @return A list \code{\link{freq}} containing 11 frequency values.
#'
#' @examples
#' comp_freq()                  # => ok, using current defaults
#' length(comp_freq())          # => 11
#'
#' # Rounding effects:
#' comp_freq(prev = .5, sens = .5, spec = .5, N = 1)   # => yields fa = 1 (see ?round for reason)
#' comp_freq(prev = .1, sens = .9, spec = .8, N = 10)  # => 1 hit (TP, rounded)
#' comp_freq(prev = .1, sens = .9, spec = .8, N = 10, round = FALSE)    # => hi = .9
#' comp_freq(prev = 1/3, sens = 6/7, spec = 2/3, N = 1, round = FALSE)  # => hi = 0.2857143
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
                      N = num$N,      # default N
                      round = TRUE    # should freq be rounded to integers? (default: round = TRUE)
                      # n_digits = 5  # digits to which non-rounded freq are rounded (REMOVED: only round values SHOWN, not computed!)
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

      N <- comp_min_N(prev = prev, sens = sens, spec = spec, min_freq = 1)

      warning(paste0("Unknown population size N. A suitable minimum value of N = ", N, " was computed."))
    }

    ## (5) Set or compute all values of freq:
    freq$N <- N # copy N from argument OR num (input)

    ## (a) Number of cond_true vs. cond_false cases (by condition):
    if (round) {
      freq$cond_true <- round((N * prev), 0)  # 1a. cond_true  = N x prev [rounded to nearest integer]
    } else {
      freq$cond_true <- (N * prev)            # 1b. cond_true  = N x prev [not rounded]
    }
    freq$cond_false <- (N - freq$cond_true)   # 2. cond_false = complement of cond_true (to N)

    ## (b) Number of 4 SDT combinations:
    if (round) {
      freq$hi <- round((sens * freq$cond_true), 0)   # a1. N of hi [rounded to nearest integer]
    } else {
      freq$hi <- (sens * freq$cond_true)             # a2. N of hi [not rounded]
    }
    freq$mi <- (freq$cond_true - freq$hi)            # b.  N of mi = complement of hi (to cond_true)

    if (round) {
      freq$cr <- round((spec * freq$cond_false), 0)  # c1. N of cr [rounded to nearest integer]
    } else {
      freq$cr <- (spec * freq$cond_false)            # c2. N of cr [not rounded]
    }
    freq$fa <- (freq$cond_false - freq$cr)           # d.  N of fa - complement of cr (to cond_false)

    ## (c) Number of positive vs. negative decisions (by decision):
    freq$dec_pos <- freq$hi + freq$fa  # 1. positive decisions (true & false positives)
    freq$dec_neg <- freq$mi + freq$cr  # 2. negative decisions (false & true negatives)

    ## (d) Accuracy/Correspondence of decision to condition (by correspondence):
    freq$dec_cor <- freq$hi + freq$cr  # N of correct decisions
    freq$dec_err <- freq$mi + freq$fa  # N of erroneous decisions

    ## (6) Check current frequencies for consistency:
    tol <- .0001  # tolerance threshold for mismatch of sums

    if (#isTRUE(all.equal(freq$N, (freq$hi + freq$mi + freq$fa + freq$cr), tolerance = tol)) &&
        (abs(freq$N - (freq$hi + freq$mi + freq$fa + freq$cr)) > tol) ||
        (abs(freq$cond_true - (freq$hi + freq$mi)) > tol)             ||
        (abs(freq$cond_false - (freq$fa + freq$cr)) > tol)            ||
        # (abs(dec_pos - (hi + fa)) > tol)               ||  # (computed as such above)
        # (abs(dec_neg - (mi + cr)) > tol)               ||  # (computed as such above)
        # (abs(dec_cor - (hi + cr)) > tol)               ||  # (computed as such above)
        # (abs(dec_err - (mi + fa)) > tol)               ||  # (computed as such above)
        (abs(freq$N - (freq$cond_true + freq$cond_false)) > tol)      ||
        (abs(freq$N - (freq$dec_pos + freq$dec_neg)) > tol)           ||
        (abs(freq$N - (freq$dec_cor + freq$dec_err)) > tol)           ) {

      warning("Current frequencies do NOT add up to N.")
    }

  } # if (is_valid(prev, sens, spec, fart))


  ## (7) Round non-rounded freq (to n_digits):
  ## if (!round) { freq <- lapply(X = freq, FUN = round, digits = n_digits) }
  ## (REMOVED: only round values SHOWN, not computed!)


  ## (8) Return entire list freq:
  return(freq)

}

## Check: --------

# comp_freq()                  # => ok, using current defaults
# length(comp_freq())          # => 11
#
# # Rounding effects:
# comp_freq(prev = .5, sens = .5, spec = .5, N = 1)  # => yields fa = 1 (see ?round for reason)
# comp_freq(prev = .1, sens = .9, spec = .8, N = 10)  # => 1 hit (TP, rounded)
# comp_freq(prev = .1, sens = .9, spec = .8, N = 10, round = FALSE)  # => .9 hit
# comp_freq(prev = 1/3, sens = 6/7, spec = 2/3, N = 1, round = FALSE)  # => hi = 0.2857143
# # comp_freq(prev = 1/3, sens = 6/7, spec = 2/3, N = 1, round = FALSE, n_digits = 3)  # => hi = 0.286 # Removed n_digits.
# # comp_freq(prev = 1/3, sens = 6/7, spec = 2/3, N = 1, round = FALSE, n_digits = 1)  # => hi = 0.3   # Removed n_digits.
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
#'  \item the number of cases for which \code{\link{cond_true}}
#'  \item the number of cases for which \code{\link{cond_false}}
#'
#'  \item the number of cases for which \code{\link{dec_pos}}
#'  \item the number of cases for which \code{\link{dec_neg}}
#'
#'  \item the number of cases for which \code{\link{dec_cor}}
#'  \item the number of cases for which \code{\link{dec_err}}
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
#' Key relationships between frequencies and probabilities
#' (see documentation of \code{\link{comp_freq}} or \code{\link{comp_prob}} for details):
#'
#' \itemize{
#'
#'   \item Three perspectives on a population:
#'
#'   by condition / by decision / by accuracy.
#'
#'   \item Defining probabilities in terms of frequencies:
#'
#'   Probabilities can be computed as ratios between frequencies, but beware of rounding issues.
#'
#' }
#'
#'
#' Functions translating between representational formats:
#' \code{\link{comp_prob_prob}}, \code{\link{comp_prob_freq}},
#' \code{\link{comp_freq_prob}}, \code{\link{comp_freq_freq}}
#' (see documentation of \code{\link{comp_prob_prob}} for details).
#'
#'
#' Visualizations of current frequency information
#' are provided by \code{\link{plot_prism}} and
#' \code{\link{plot_icons}}.
#'
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

comp_freq_type <- function(fname, lbl_txt = txt) {

  f_type <- "typeless"  # initialize

  # (1) Define types of all known frequencies:

  # (a) Using basic names:
  # freq_types <- c("popu", rep("cond", 2), rep("dec", 2), rep("accu", 2), rep("cell", 4))

  # (b) Using labels defined in lbl_txt:
  freq_types <- c(lbl_txt$popu_lbl,
                  rep(lbl_txt$cond_lbl, 2),
                  rep(lbl_txt$dec_lbl, 2),
                  rep(lbl_txt$acc_lbl, 2),
                  rep(lbl_txt$sdt_lbl, 4))
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
# comp_freq_type("cond_false")
# comp_freq_type("dec_neg")
# comp_freq_type("dec_err")
# comp_freq_type("cr")

## Using alternative text labels:
# comp_freq_type("cond_true", lbl_txt = txt_TF) # => "Truth"
# comp_freq_type("dec_pos", lbl_txt = txt_TF)   # => "Test"

## Note:
# comp_freq_type(N)        # => typeless (as function requires name, NOT a value)
# comp_freq_type("false")  # => typeless (as full name is required)




## comp_freq_col: Determine the color of a named frequency (freq) in current color palette (col_pal):  ----------

comp_freq_col <- function(fname,
                          col_pal = pal,
                          col = NA  # primary color
) {

  # initialize:
  col_name <- NA
  f_col <- NA

  if (!is.na(col)) { # if col is specified:

    f_col <- col  # use it!

  } else {  # figure out f_col from fname and col_pal:

    # (A) if freq corresponds to named frequency in freq:
    if (fname %in% names(freq)) {

      ## Derive current values corresponding to freq:
      ix <- which(names(freq) == fname)  # index in freq

      ## (a) Value of frequency in freq:
      # f_val <- freq[ix]

      ## (b) Type of frequency:
      # f_type <- comp_freq_type(fname)  # see helper function (defined in init_freq_num.R)

      # (c) Color of frequency:

      # Note that names(freq) were sometimes longer than names(pal):
      # If fname contains a dot (.):  Use only the name part after the dot:
      if (any(grep(pattern = "\\.", x = fname))) {

        nameparts <- unlist(strsplit(fname, split = "\\."))

        part_1 <- nameparts[1]  # 1st part of fname
        part_2 <- nameparts[2]  # 2nd part of fname
        col_name <- part_2  # 2nd part of fname corresponds to name of color

        # if (col_name == "true") { col_name <- "cor" }

      } else {
        col_name <- fname  # col_name corresponds to fname (for frequencies)
      }

      # print(col_name)

    }

    # (B) Find color value of col_name in current color col_pal:
    if (col_name %in% names(col_pal)) { # if col_name corresponds to a color name in col_pal
      f_col <- col_pal[col_name]        # use this color to fill box
    } else {
      f_col <- grey(.95, .50)  # use some default color (e.g., "white")
    }

  } # else.

  # print(f_col)
  return(f_col)

}

## Check: --------

# comp_freq_col("N")
# comp_freq_col("hi")
# comp_freq_col("cond_true")
# comp_freq_col("dec_pos")
# comp_freq_col("dec_cor")
#
# comp_freq_col("default")          # use default color
# comp_freq_col("N", col = "gold")  # "gold"
# comp_freq_col("nn", col = "gold") # "gold"



## (*) Done: -----------

## - Clean up code [2018 08 30].

## - Added help functions comp_freq_type and comp_freq_col
##   to classify freq into types and determine freq color
##   based on freq name.                             [2018 08 18]

## - Added 2 more frequencies for accuracy, i.e.,
##   "decision correctness" or correspondence of decision to condition:
##   "dec_cor" vs. "dec_err" (i.e., diagonal of confusion matrix)

## (+) ToDo: -----------

## - ...

## eof. ------------------------------------------
