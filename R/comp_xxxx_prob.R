## comp_xxxx_prob.R | riskyr
## 2018 08 30
## Add 2 wrapper functions (that use existing functions)
## to translate (back) from prob to freq and prob:
## -----------------------------------------------


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




## (A) Translate (back from 3 essential) prob to freq: --------

## comp_freq_prob: Documentation ------

## Note: comp_freq_prob is a WRAPPER function for the more basic
##       comp_freq(...) defined before!

#' Compute frequencies from (3 essential) probabilities.
#'
#' \code{comp_freq_prob} computes current frequency information
#' from a sufficient and valid set of 3 essential probabilities
#' (\code{\link{prev}}, and
#' \code{\link{sens}} or its complement \code{\link{mirt}}, and
#' \code{\link{spec}} or its complement \code{\link{fart}}).
#' It returns a list of 9 frequencies \code{\link{freq}}
#' for a population of \code{\link{N}} individuals
#' as its output.
#'
#' By default, the values of \code{\link{prev}}, \code{\link{sens}},
#' and \code{\link{spec}} are initialized to the probability information
#' currently contained in \code{\link{prob}}.
#'
#' \code{comp_freq_prob} is a wrapper function for the more basic
#' function \code{\link{comp_freq}}, which only accepts the
#' 3 essential probabilities
#' (\code{\link{prev}}, \code{\link{sens}}, \code{\link{spec}})
#' as inputs.
#'
#' When using \code{comp_freq_prob} with the arguments
#' \code{\link{mirt}} and \code{\link{fart}}, their complements
#' \code{\link{sens}} and \code{\link{spec}} must either be
#' valid complements (as in \code{\link{is_complement}}) or
#' \code{NA}.
#'
#' The value of \code{\link{N}} uses the frequency information
#' currently contained in \code{\link{freq}} as its default.
#' If \code{\link{N}} is unknown (\code{NA}),
#' a suitable minimum value can be computed by \code{\link{comp_min_N}}.
#'
#' In addition to \code{\link{prev}}, both
#' \code{\link{sens}} and \code{\link{spec}} are necessary arguments.
#' If only their complements \code{\link{mirt}} or \code{\link{fart}}
#' are known, first use \code{\link{comp_complement}},
#' \code{\link{comp_comp_pair}}, or \code{\link{comp_complete_prob_set}}
#' to obtain the 3 essential probabilities.
#'
#' By default, \code{comp_freq_prob} and \code{\link{comp_freq}}
#' round frequencies to nearest integers to avoid decimal values in
#' \code{\link{freq}} (i.e., \code{round = TRUE} by default).
#' Using the option \code{round = FALSE} turns off rounding.
#'
#' Key relationships:
#'
#' \itemize{
#'
#' \item Other functions translating between representational formats:
#'
#'    \enumerate{
#'
#'    \item \code{comp_freq_prob} (defined here) is
#'    a wrapper function for \code{\link{comp_freq}} and
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
#'    }
#'
#' \item Two perspectives:
#'
#' A population of \code{\link{N}} individuals can be split into 2 subsets
#' in 2 different ways:
#'
#'    \enumerate{
#'
#'    \item by condition:
#'
#'    The frequency \code{\link{cond.true}} depends on the prevalence \code{\link{prev}}
#'    and
#'    the frequency \code{\link{cond.false}} depends on the prevalence's complement \code{1 - \link{prev}}.
#'
#'    \item by decision:
#'
#'    The frequency \code{\link{dec.pos}} depends on the proportion of positive decisions \code{\link{ppod}}
#'    and
#'    the frequency \code{\link{dec.neg}} depends on the proportion of negative decisions \code{1 - \link{ppod}}.
#'
#'    }
#'
#' The population size \code{\link{N}} is a free parameter (independent of the
#' essential probabilities \code{\link{prev}}, \code{\link{sens}}, and \code{\link{spec}}).
#'
#' If \code{\link{N}} is unknown (\code{NA}), a suitable minimum value can be computed by \code{\link{comp_min_N}}.
#'
#' \item Combinations of frequencies:
#'
#'    In a population of size \code{\link{N}} the following relationships hold:
#'
#'    \enumerate{
#'
#'     \item \code{\link{N} = \link{cond.true} + \link{cond.false} = (\link{hi} + \link{mi}) + (\link{fa} + \link{cr})} (by condition)
#'
#'     \item \code{\link{N} = \link{dec.pos} + \link{dec.neg} = (\link{hi} + \link{fa}) + (\link{mi} + \link{cr})} (by decision)
#'
#'     \item \code{\link{N} = \link{hi} + \link{mi} + \link{fa} + \link{cr}} (by condition x decision)
#'
#'    }
#'
#'   The two perspectives (by condition vs. by decision) combine the 4 essential frequencies
#'   (i.e., \code{\link{hi}}, \code{\link{mi}}, \code{\link{fa}}, \code{\link{cr}})
#'   in 2 different ways.
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
#' }
#'
#' @param prev The condition's prevalence \code{\link{prev}}
#' (i.e., the probability of condition being \code{TRUE}).
#'
#' @param sens The decision's sensitivity \code{\link{sens}}
#' (i.e., the conditional probability of a positive decision
#' provided that the condition is \code{TRUE}).
#' \code{sens} is optional when its complement \code{mirt} is provided.
#'
#' @param mirt The decision's miss rate \code{\link{mirt}}
#' (i.e., the conditional probability of a negative decision
#' provided that the condition is \code{TRUE}).
#' \code{mirt} is optional when its complement \code{sens} is provided.
#'
#' @param spec The decision's specificity value \code{\link{spec}}
#' (i.e., the conditional probability
#' of a negative decision provided that the condition is \code{FALSE}).
#' \code{spec} is optional when its complement \code{fart} is provided.
#'
#' @param fart The decision's false alarm rate \code{\link{fart}}
#' (i.e., the conditional probability
#' of a positive decision provided that the condition is \code{FALSE}).
#' \code{fart} is optional when its complement \code{spec} is provided.
#'
#' @param tol A numeric tolerance value for \code{\link{is_complement}}.
#' Default: \code{tol = .01}.
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
#' # Basics:
#' comp_freq_prob(prev = .1, sens = .9, spec = .8, N = 100)           # => ok: hi = 9, ... cr = 72.
#' # Same case with complements (using NAs to prevent defaults):
#' comp_freq_prob(prev = .1, sens = NA, mirt = .1, spec = NA, fart = .2, N = 100)  # => same result
#'
#' comp_freq_prob()                   # => ok, using probability info currently contained in prob
#' length(comp_freq_prob())           # => a list containing 9 frequencies
#' all.equal(freq, comp_freq_prob())  # => TRUE, unless prob has been changed after computing freq
#' freq <- comp_freq_prob()           # => computes frequencies and stores them in freq
#'
#' # Ways to work:
#' comp_freq_prob(prev = 1, sens = 1, spec = 1, N = 101)  # => ok + warning: N hits (TP)
#'
#' # Same case with complements (using NAs to prevent defaults):
#' comp_freq_prob(prev = 1, sens = NA, mirt = 0, spec = NA, fart = 0, N = 101)
#'
#' comp_freq_prob(prev = 1, sens = 1, spec = 0, N = 102)  # => ok + warning: N hits (TP)
#' comp_freq_prob(prev = 1, sens = 0, spec = 1, N = 103)  # => ok + warning: N misses (FN)
#' comp_freq_prob(prev = 1, sens = 0, spec = 0, N = 104)  # => ok + warning: N misses (FN)
#' comp_freq_prob(prev = 0, sens = 1, spec = 1, N = 105)  # => ok + warning: N correct rejections (TN)
#'
#' comp_freq_prob(prev = 0, sens = 1, spec = 0, N = 106)  # => ok + warning: N false alarms (FP)
#'
#' # Same case with complements (using NAs to prevent defaults):
#' comp_freq_prob(prev = 0, sens = NA, mirt = 0,
#'                spec = NA, fart = 1, N = 106)  # => ok + warning: N false alarms (FP)
#'
#' # Watch out for:
#' comp_freq_prob(prev = 1, sens = 1, spec = 1, N = NA)  # => ok + warning: N = 1 computed
#' comp_freq_prob(prev = 1, sens = 1, spec = 1, N =  0)  # => ok, but all 0 + warning (NPV = NaN)
#' comp_freq_prob(prev = .5, sens = .5, spec = .5, N = 10, round = TRUE)  # => ok, but all rounded
#' comp_freq_prob(prev = .5, sens = .5, spec = .5, N = 10, round = FALSE) # => ok, but not rounded
#'
#' # Ways to fail:
#' comp_freq_prob(prev = NA, sens = 1, spec = 1, 100)  # => NAs + no warning (prev NA)
#' comp_freq_prob(prev = 1, sens = NA, spec = 1, 100)  # => NAs + no warning (sens NA)
#' comp_freq_prob(prev = 1, sens = 1, spec = NA, 100)  # => NAs + no warning (spec NA)
#' comp_freq_prob(prev = 8, sens = 1, spec = 1,  100)  # => NAs + warning (prev beyond range)
#' comp_freq_prob(prev = 1, sens = 8, spec = 1,  100)  # => NAs + warning (sens & spec beyond range)
#'
#' @family functions computing frequencies
#' @family format conversion functions
#'
#' @seealso
#' \code{\link{comp_freq_freq}} computes current frequency information from (4 essential) frequencies;
#' \code{\link{comp_prob_freq}} computes current probability information from (4 essential) frequencies;
#' \code{\link{comp_prob_prob}} computes current probability information from (3 essential) probabilities;
#' \code{\link{num}} contains basic numeric variables;
#' \code{\link{init_num}} initializes basic numeric variables;
#' \code{\link{freq}} contains current frequency information;
#' \code{\link{comp_freq}} computes current frequency information;
#' \code{\link{prob}} contains current probability information;
#' \code{\link{comp_prob}} computes current probability information;
#' \code{\link{comp_complement}} computes a probability's complement;
#' \code{\link{comp_comp_pair}} computes pairs of complements;
#' \code{\link{comp_complete_prob_set}} completes valid sets of probabilities;
#' \code{\link{comp_min_N}} computes a suitable population size \code{\link{N}} (if missing).
#'
#' @export

## comp_freq_prob: Definition ------

comp_freq_prob <- function(prev = prob$prev,  # 3 essential probabilities (removed: mirt & fart ):
                           sens = prob$sens, mirt = NA, # using current probability info contained in prob!
                           spec = prob$spec, fart = NA,
                           tol = .01,         # tolerance for is_complement
                           N = freq$N,        # using current freq info contained in freq!
                           round = TRUE) {

  ## (A) If a valid set of probabilities was provided:
  if (is_valid_prob_set(prev = prev, sens = sens, mirt = mirt, spec = spec, fart = fart, tol = tol)) {

    ## (1) Compute the complete quintet of probabilities:
    prob_quintet <- comp_complete_prob_set(prev, sens, mirt, spec, fart)
    sens <- prob_quintet[2] # gets sens (if not provided)
    mirt <- prob_quintet[3] # gets mirt (if not provided)
    spec <- prob_quintet[4] # gets spec (if not provided)
    fart <- prob_quintet[5] # gets fart (if not provided)

    ## (2) Pass on:
    ## Wrapper function: Delegate to existing and more basic function:
    freq <- comp_freq(prev, sens, spec,  # 3 essential probabilities; currently NOT SUPPORTED: 2 optional (mirt, fart)
                      N, round)

    ## Return entire list freq:
    return(freq)

  }

  else { # (B) NO valid set of probabilities was provided:

    warning("Please enter a valid set of essential probabilities.")

  }

}

## Check: ------

# # Basics:
# comp_freq_prob(prev = .1, sens = .9, spec = .8, N = 100)  # => ok: hi = 9, mi = 1, fa = 18, cr = 72.
# # Same case with complements (using NAs to prevent defaults):
# comp_freq_prob(prev = .1, sens = NA, mirt = .1, spec = NA, fart = .2, N = 100)  # => same result
#
# comp_freq_prob()                   # => ok, using probability info currently contained in prob
# length(comp_freq_prob())           # => a list containing 9 frequencies
# all.equal(freq, comp_freq_prob())  # => TRUE, unless prob has been changed after computing freq
# freq <- comp_freq_prob()           # => computes frequencies and stores them in freq
#
# # Ways to work:
# comp_freq_prob(prev = 1, sens = 1, spec = 1, N = 101)  # => ok + warning: N hits (TP)
# # Same case with complements (using NAs to prevent defaults):
# comp_freq_prob(prev = 1, sens = NA, mirt = 0, spec = NA, fart = 0, N = 101)
#
# comp_freq_prob(prev = 1, sens = 1, spec = 0, N = 102)  # => ok + warning: N hits (TP)
# comp_freq_prob(prev = 1, sens = 0, spec = 1, N = 103)  # => ok + warning: N misses (FN)
# comp_freq_prob(prev = 1, sens = 0, spec = 0, N = 104)  # => ok + warning: N misses (FN)
# comp_freq_prob(prev = 0, sens = 1, spec = 1, N = 105)  # => ok + warning: N correct rejections (TN)
#
# comp_freq_prob(prev = 0, sens = 1, spec = 0, N = 106)  # => ok + warning: N false alarms (FP)
# # Same case with complements (using NAs to prevent defaults):
# comp_freq_prob(prev = 0, sens = NA, mirt = 0, spec = NA, fart = 1, N = 106)  # => ok + warning: N false alarms (FP)
#
# # Watch out for:
# comp_freq_prob(prev = 1, sens = 1, spec = 1, N = NA)  # => ok + additional warning that N = 1 was computed
# comp_freq_prob(prev = 1, sens = 1, spec = 1, N =  0)  # => ok, but all 0 + warning (extreme case: N hits, NPV = NaN)
# comp_freq_prob(prev = .5, sens = .5, spec = .5, N = 10, round = TRUE)   # => ok, but all rounded (increasing errors: mi and fa)
# comp_freq_prob(prev = .5, sens = .5, spec = .5, N = 10, round = FALSE)  # => ok, but not rounded
#
# # Ways to fail:
# comp_freq_prob(prev = NA,  sens = 1, spec = 1,  100)   # => NAs + no warning (prev NA)
# comp_freq_prob(prev = 1,  sens = NA, spec = 1,  100)   # => NAs + no warning (sens NA)
# comp_freq_prob(prev = 1,  sens = 1,  spec = NA, 100)   # => NAs + no warning (spec NA)
# comp_freq_prob(prev = 8,  sens = 1,  spec = 1,  100)   # => NAs + warning (prev beyond range)
# comp_freq_prob(prev = 1,  sens = 8,  spec = 1,  100)   # => NAs + warning (sens & spec beyond range)



## (B) Translate (back from 3 essential) prob to prob: ----------

## Note: comp_prob_prob is a WRAPPER function for the more basic
##       comp_prob(...) defined before!


## comp_prob_prob: Documentation ------

#' Compute probabilities from (3 essential) probabilities.
#'
#' \code{comp_prob_prob} computes current probability information
#' from 3 essential probabilities
#' (\code{\link{prev}},
#' \code{\link{sens}} or \code{\link{mirt}},
#' \code{\link{spec}} or \code{\link{fart}}).
#' It returns a list of 10 probabilities \code{\link{prob}}
#' as its output.
#'
#' \code{comp_prob} assumes that a sufficient and
#' consistent set of essential probabilities
#' (i.e., \code{\link{prev}} and
#' either \code{\link{sens}} or its complement \code{\link{mirt}}, and
#' either \code{\link{spec}} or its complement \code{\link{fart}})
#' is provided.
#'
#' \code{comp_prob} computes and returns a full set of basic and
#' various derived probabilities (e.g.,
#' the probability of a positive decision \code{\link{ppod}},
#' the predictive values \code{\link{PPV}} and \code{\link{NPV}}, as well
#' as their complements \code{\link{FDR}} and \code{\link{FOR}})
#' in its output of a list \code{\link{prob}}.
#'
#' Extreme probabilities (sets containing two or more
#' probabilities of 0 or 1) may yield unexpected values
#' (e.g., predictive values \code{\link{PPV}} or \code{\link{NPV}}
#' turning \code{NaN} when \code{\link{is_extreme_prob_set}}
#' evaluates to \code{TRUE}).
#'
#' Key relationships:
#'
#' \itemize{
#'
#' \item Other functions translating between representational formats:
#'
#'    \enumerate{
#'
#'    \item \code{comp_prob_prob} (defined here) is
#'    a wrapper function for \code{\link{comp_prob}} and
#'    an analog to 3 other format conversion functions:
#'
#'    \item \code{\link{comp_prob_freq}} computes
#'    current \emph{probability} information contained in \code{\link{prob}}
#'    from 4 essential frequencies
#'    (\code{\link{hi}}, \code{\link{mi}}, \code{\link{fa}}, \code{\link{cr}}).
#'
#'    \item \code{\link{comp_freq_prob}} computes
#'    current \emph{frequency} information contained in \code{\link{freq}}
#'    from 3 essential probabilities
#'    (\code{\link{prev}}, \code{\link{sens}}, \code{\link{spec}}).
#'
#'    \item \code{\link{comp_freq_freq}} computes
#'    current \emph{frequency} information contained in \code{\link{freq}}
#'    from 4 essential frequencies
#'    (\code{\link{hi}}, \code{\link{mi}}, \code{\link{fa}}, \code{\link{cr}}).
#'
#'    }
#'
#' \item Two perspectives:
#'
#' A population of \code{\link{N}} individuals can be split into 2 subsets
#' in 2 different ways:
#'
#'    \enumerate{
#'
#'    \item by condition:
#'
#'    The frequency \code{\link{cond.true}} depends on the prevalence \code{\link{prev}}
#'    and
#'    the frequency \code{\link{cond.false}} depends on the prevalence's complement \code{1 - \link{prev}}.
#'
#'    \item by decision:
#'
#'    The frequency \code{\link{dec.pos}} depends on the proportion of positive decisions \code{\link{ppod}}
#'    and
#'    the frequency \code{\link{dec.neg}} depends on the proportion of negative decisions \code{1 - \link{ppod}}.
#'
#'    }
#'
#' The population size \code{\link{N}} is a free parameter (independent of the
#' essential probabilities \code{\link{prev}}, \code{\link{sens}}, and \code{\link{spec}}).
#'
#' If \code{\link{N}} is unknown (\code{NA}), a suitable minimum value can be computed by \code{\link{comp_min_N}}.
#'
#' \item Combinations of frequencies:
#'
#'    In a population of size \code{\link{N}} the following relationships hold:
#'
#'    \enumerate{
#'
#'     \item \code{\link{N} = \link{cond.true} + \link{cond.false} = (\link{hi} + \link{mi}) + (\link{fa} + \link{cr})} (by condition)
#'
#'     \item \code{\link{N} = \link{dec.pos} + \link{dec.neg} = (\link{hi} + \link{fa}) + (\link{mi} + \link{cr})} (by decision)
#'
#'     \item \code{\link{N} = \link{hi} + \link{mi} + \link{fa} + \link{cr}} (by condition x decision)
#'
#'    }
#'
#'   The two perspectives (by condition vs. by decision) combine the 4 essential frequencies
#'   (i.e., \code{\link{hi}}, \code{\link{mi}}, \code{\link{fa}}, \code{\link{cr}})
#'   in 2 different ways.
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
#' }
#'
#' @param prev The condition's prevalence value \code{\link{prev}}
#' (i.e., the probability of condition being \code{TRUE}).
#'
#' @param sens The decision's sensitivity value \code{\link{sens}}
#' (i.e., the conditional probability of a positive decision
#' provided that the condition is \code{TRUE}).
#' \code{sens} is optional when its complement \code{\link{mirt}} is provided.
#'
#' @param mirt The decision's miss rate value \code{\link{mirt}}
#' (i.e., the conditional probability of a negative decision
#' provided that the condition is \code{TRUE}).
#' \code{mirt} is optional when its complement \code{\link{sens}} is provided.
#'
#' @param spec The decision's specificity value \code{\link{spec}}
#' (i.e., the conditional probability
#' of a negative decision provided that the condition is \code{FALSE}).
#' \code{spec} is optional when its complement \code{\link{fart}} is provided.
#'
#' @param fart The decision's false alarm rate \code{\link{fart}}
#' (i.e., the conditional probability
#' of a positive decision provided that the condition is \code{FALSE}).
#' \code{fart} is optional when its complement \code{\link{spec}} is provided.
#'
#' @param tol A numeric tolerance value for \code{\link{is_complement}}.
#' Default: \code{tol = .01}.
#'
#' @return A list \code{\link{prob}} containing 10 probability values.
#'
#' @examples
#' # Basics:
#' comp_prob_prob(prev = .11, sens = .88, spec = .77)                        # => ok: PPV = 0.3210614
#' comp_prob_prob(prev = .11, sens = NA, mirt = .12, spec = NA, fart = .23)  # => ok: PPV = 0.3210614
#' comp_prob_prob()          # => ok, using current defaults
#' length(comp_prob_prob())  # => 10 probabilities
#'
#' # Ways to work:
#' comp_prob_prob(.99, sens = .99, spec = .99)              # => ok: PPV = 0.999898
#' comp_prob_prob(.99, sens = .90, spec = NA, fart = .10)   # => ok: PPV = 0.9988789
#'
#' # Watch out for extreme cases:
#' comp_prob_prob(1, sens = 0, spec = 1)      # => ok, but with warnings (as PPV & FDR are NaN)
#' comp_prob_prob(1, sens = 0, spec = 0)      # => ok, but with warnings (as PPV & FDR are NaN)
#' comp_prob_prob(1, sens = 0, spec = NA, fart = 0)  # => ok, but with warnings (as PPV & FDR are NaN)
#' comp_prob_prob(1, sens = 0, spec = NA, fart = 1)  # => ok, but with warnings (as PPV & FDR are NaN)
#'
#' comp_prob_prob(1, sens = 1, spec = 0)      # => ok, but with warnings (as NPV & FOR are NaN)
#' comp_prob_prob(1, sens = 1, spec = 1)      # => ok, but with warnings (as NPV & FOR are NaN)
#' comp_prob_prob(1, sens = 1, spec = NA, fart = 0)  # => ok, but with warnings (as NPV & FOR are NaN)
#' comp_prob_prob(1, sens = 1, spec = NA, fart = 1)  # => ok, but with warnings (as NPV & FOR are NaN)
#'
#' # Ways to fail:
#' comp_prob_prob(NA, 1, 1, NA)  # => only warning: invalid set (prev not numeric)
#' comp_prob_prob(8,  1, 1, NA)  # => only warning: prev no probability
#' comp_prob_prob(1,  8, 1, NA)  # => only warning: sens no probability
#' comp_prob_prob(1,  1, 1,  1)  # => only warning: is_complement not in tolerated range
#'
#' @family functions computing frequencies
#' @family format conversion functions
#'
#' @seealso
#' \code{\link{comp_freq_prob}} computes current frequency information from (3 essential) probabilities;
#' \code{\link{comp_freq_freq}} computes current frequency information from (4 essential) frequencies;
#' \code{\link{comp_prob_freq}} computes current probability information from (4 essential) frequencies;
#' \code{\link{num}} contains basic numeric variables;
#' \code{\link{init_num}} initializes basic numeric variables;
#' \code{\link{freq}} contains current frequency information;
#' \code{\link{comp_freq}} computes current frequency information;
#' \code{\link{prob}} contains current probability information;
#' \code{\link{comp_prob}} computes current probability information;
#' \code{\link{comp_complement}} computes a probability's complement;
#' \code{\link{comp_comp_pair}} computes pairs of complements;
#' \code{\link{comp_complete_prob_set}} completes valid sets of probabilities;
#' \code{\link{comp_min_N}} computes a suitable population size \code{\link{N}} (if missing).
#'
#' @export

## comp_prob_prob: Definition ------

comp_prob_prob <- function(prev = prob$prev,             # probabilities: 3 essential (prev; sens, spec)
                           sens = prob$sens, mirt = NA,  #                2 optional  (      mirt, fart)
                           spec = prob$spec, fart = NA,  # Defaults: Values currently contained in prob!
                           tol = .01                     # tolerance for is_complement
) {

  ## Pass on:
  ## Wrapper function: Delegate to existing and more basic function:
  prob <- comp_prob(prev,        # probabilities:
                    sens, mirt,  # 3 essential (prev, sens, spec)
                    spec, fart,  # 2 optional  (      mirt, fart)
                    tol = tol    # tolerance for is_complement
  )

  ## Return entire list prob:
  return(prob)

}


## Check: ------
# Basics:
# comp_prob_prob(prev = .11, sens = .88, spec = .77)                        # => ok: PPV = 0.3210614
# comp_prob_prob(prev = .11, sens = NA, mirt = .12, spec = NA, fart = .23)  # => ok: PPV = 0.3210614
# comp_prob_prob()          # => ok, using current defaults
# length(comp_prob_prob())  # => 10 probabilities
#
# # Ways to work:
# comp_prob_prob(.99, sens = .99, spec = .99)              # => ok: PPV = 0.999898
# comp_prob_prob(.99, sens = .90, spec = NA, fart = .10)   # => ok: PPV = 0.9988789
#
# # Watch out for extreme cases:
# comp_prob_prob(1, sens = 0, spec = 1)      # => ok, but with warnings (as PPV & FDR are NaN)
# comp_prob_prob(1, sens = 0, spec = 0)      # => ok, but with warnings (as PPV & FDR are NaN)
# comp_prob_prob(1, sens = 0, spec = NA, fart = 0)  # => ok, but with warnings (as PPV & FDR are NaN)
# comp_prob_prob(1, sens = 0, spec = NA, fart = 1)  # => ok, but with warnings (as PPV & FDR are NaN)
#
# comp_prob_prob(1, sens = 1, spec = 0)      # => ok, but with warnings (as NPV & FOR are NaN)
# comp_prob_prob(1, sens = 1, spec = 1)      # => ok, but with warnings (as NPV & FOR are NaN)
# comp_prob_prob(1, sens = 1, spec = NA, fart = 0)  # => ok, but with warnings (as NPV & FOR are NaN)
# comp_prob_prob(1, sens = 1, spec = NA, fart = 1)  # => ok, but with warnings (as NPV & FOR are NaN)
#
# # Ways to fail:
# comp_prob_prob(NA, 1, 1, NA)  # => only warning: invalid set (prev not numeric)
# comp_prob_prob(8,  1, 1, NA)  # => only warning: prev no probability
# comp_prob_prob(1,  8, 1, NA)  # => only warning: sens no probability
# comp_prob_prob(1,  1, 1,  1)  # => only warning: is_complement not in tolerated range



## (*) Done: -----------

## - Clean up code [2018 08 30].

## (+) ToDo: ----------

## - ...

## eof. ------------------------------------------
