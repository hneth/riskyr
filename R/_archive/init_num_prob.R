## init_num_prob.R | riskyR
## 2018 01 27
## -----------------------------------------------
## Define and initialize probability information prob
## by using basic parameter values of num:

## -----------------------------------------------

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

## -----------------------------------------------
## (1) Initialize prob as a list (of NA values)
##     of 10 probabilities (3 essential ones):

init_prob <- function() {

  ## (0) Initialize prob as a list:
  prob <- list(

    ## (a) By condition: 3 essential + 2 optional probabilities:

    "prev" = NA,  # simple p of cond.true

    "sens" = NA,  # conditional p
    "mirt" = NA,  # conditional p: 1 - sens
    "spec" = NA,  # conditional p:
    "fart" = NA,  # conditional p: 1 - spec

    ## (b) By decision: 5 derived probabilities and predictive values (PVs):

    "ppod" = NA,  # simple p of dec.pos

    "PPV" = NA,   # conditional p: reversal of sens
    "NPV" = NA,   # conditional p: reversal of spec
    "FDR" = NA,   # conditional p: 1 - PPV
    "FOR" = NA    # conditional p: 1 - NPV
  )

  ## Return entire list prob:
  return(prob)

}


## Check:
# init_prob()          # initializes empty prob
# length(init_prob())  # => 10 probabilities


## -----------------------------------------------
## (2) Compute the set of ALL current probabilities:
##     So far: Compute current values of PPV and NPV
##     as functions of prev, sens, and spec (using Bayes):


#' Compute probabilities from (3 essential) probabilities.
#'
#' \code{comp_prob} computes current probability information
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
#' turning \code{NaN} when \code{\link{is_extreme}}
#' evaluates to \code{TRUE}).
#'
#' \code{comp_prob} is the probability counterpart to the
#' frequency function \code{\link{comp_freq}}.
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
#'
#'
#' @param prev The condition's prevalence value \code{\link{prev}}
#' (i.e., the probability of condition being \code{TRUE}).
#'
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
#'
#' @return A list \code{\link{prob}} containing 10 probability values.
#'
#'
#' @examples
#' # Basics:
#' comp_prob()             # => ok, using current defaults
#' length(comp_prob())     # => 8
#'
#' # Ways to work:
#' comp_prob(.999, 1, 1)   # => ok
#' comp_prob(1, .999, 1)   # => ok
#'
#' # Watch out for extreme cases:
#' comp_prob(1, 0, 1)      # => ok, but with warnings (as PPV & FDR are NaN)
#' comp_prob(1, 0, 0)      # => ok, but with warnings (as PPV & FDR are NaN)
#' comp_prob(1, 0, NA, 0)  # => ok, but with warnings (as PPV & FDR are NaN)
#' comp_prob(1, 0, NA, 1)  # => ok, but with warnings (as PPV & FDR are NaN)
#'
#' comp_prob(1, 1, 0)      # => ok, but with warnings (as NPV & FOR are NaN)
#' comp_prob(1, 1, 1)      # => ok, but with warnings (as NPV & FOR are NaN)
#' comp_prob(1, 1, 1, NA)  # => ok, but with warnings (as NPV & FOR are NaN)
#' comp_prob(1, 1, NA, 1)  # => ok, but with warnings (as NPV & FOR are NaN)
#'
#' # Ways to fail:
#' comp_prob(NA, 1, 1, NA)  # => NAs + warning: prev not numeric
#' comp_prob(1, NA, 1, NA)  # => NAs + warning: sens not numeric
#' comp_prob(8,  1, 1, NA)  # => NAs + warning: prev no probability
#' comp_prob(1,  8, 1, NA)  # => NAs + warning: sens no probability
#' comp_prob(1,  1, 1,  1)  # => NAs and warning: is_complement not in tolerated range
#'
#' @family functions computing probabilities
#' @family format conversion functions
#'
#' @seealso
#' \code{\link{prob}} contains current probability information;
#' \code{\link{num}} contains basic parameter values;
#' \code{\link{init_num}} initializes basic numeric variables;
#' \code{\link{pal}} contains current color settings;
#' \code{\link{txt}} contains current text settings;
#' \code{\link{freq}} contains current frequency information;
#' \code{\link{comp_freq}} computes frequencies from probabilities;
#' \code{\link{is_valid_prob_set}} verifies sets of probability inputs;
#' \code{\link{is_extreme_prob_set}} verifies sets of extreme probabilities;
#' \code{\link{comp_min_N}} computes a suitable minimum population size \code{\link{N}}.


comp_prob <- function(prev = num$prev,             # probabilities:
                      sens = num$sens, mirt = NA,  # 3 essential (prev, sens, spec)
                      spec = num$spec, fart = NA   # 2 optional  (      mirt, fart)
                      ) {

  ## (0) Initialize prob as a list:
  # prob <- list(
  #
  #   ## (a) By condition: 3 essential + 2 optional probabilities:
  #   "prev" = NA,  # simple p of cond.true
  #
  #   "sens" = NA,  # conditional p
  #   "mirt" = NA,  # conditional p: 1 - sens
  #   "spec" = NA,  # conditional p:
  #   "fart" = NA,  # conditional p: 1 - spec
  #
  #   ## (b) By decision: 5 derived probabilities and predictive values (PVs):
  #   "ppod" = NA,  # simple p of dec.pos
  #
  #   "PPV" = NA,   # conditional p: reversal of sens
  #   "NPV" = NA,   # conditional p: reversal of spec
  #   "FDR" = NA,   # conditional p: 1 - PPV
  #   "FOR" = NA    # conditional p: 1 - NPV
  # )

  prob <- init_prob()  # initialize prob (containing only NA values)


  ## (1) Only if basic quadruple of probabilities is valid:
  if (is_valid_prob_set(prev = prev, sens = sens, mirt = mirt, spec = spec, fart = fart, tol = .01)) { # provided probabilities are valid:

    ## (2) Compute the complete quintet of probabilities:
    prob_quintet <- comp_complete_prob_set(prev, sens, mirt, spec, fart)
    sens <- prob_quintet[2] # gets sens (if not provided)
    mirt <- prob_quintet[3] # gets mirt (if not provided)
    spec <- prob_quintet[4] # gets spec (if not provided)
    fart <- prob_quintet[5] # gets fart (if not provided)

    ## (3) Issue a warning if probabilities describe an extreme case:
    is_extreme_prob_set(prev = prev, sens = sens, spec = spec)  # prints a warning if TRUE

    ## (4) Assign all values of prob based on current parameter values:

    ## (a) By condition: basic probability parameters:
    prob$prev <- prev

    prob$sens <- sens
    prob$mirt <- mirt
    prob$spec <- spec
    prob$fart <- fart

    ## (b) By decision: derived probabilities and predictive values (PVs):
    prob$ppod <- comp_ppod(prev, sens, spec)  # Note: using probabilistic versions (Bayes)

    prob$PPV  <- comp_PPV(prev, sens, spec)
    prob$NPV  <- comp_NPV(prev, sens, spec)
    prob$FDR  <- comp_FDR(prev, sens, spec)
    prob$FOR  <- comp_FOR(prev, sens, spec)

    ## (5) Check derived PVs:
    if ( is.na(prob$ppod) | is.nan(prob$ppod) | !is_prob(prob$ppod) |
         is.na(prob$PPV)  | is.nan(prob$PPV)  | !is_prob(prob$PPV)  |
         is.na(prob$NPV)  | is.nan(prob$NPV)  | !is_prob(prob$NPV)  |
         is.na(prob$FDR)  | is.nan(prob$FDR)  | !is_prob(prob$FDR)  |
         is.na(prob$FOR)  | is.nan(prob$FOR)  | !is_prob(prob$FOR) ) {

      warning( "Some PVs are peculiar. Check for extreme probabilities!" )

    }

  } # if (is_valid_prob_set(...

  ## (6) Return the entire list prob:
  return(prob)

}

## Check:
{
  # comp_prob()          # => ok, using current defaults
  # length(comp_prob())  # => 8
  #
  # # Ways to succeed:
  # comp_prob(.999, 1, 1)   # => ok
  # comp_prob(1, .999, 1)   # => ok
  #
  # # Watch out for extreme cases:
  # comp_prob(1, 0, 1)      # => ok, but with warnings (as PPV & FDR are NaN)
  # comp_prob(1, 0, 0)      # => ok, but with warnings (as PPV & FDR are NaN)
  # comp_prob(1, 0, NA, 0)  # => ok, but with warnings (as PPV & FDR are NaN)
  # comp_prob(1, 0, NA, 1)  # => ok, but with warnings (as PPV & FDR are NaN)
  #
  # comp_prob(1, 1, 0)      # => ok, but with warnings (as NPV & FOR are NaN)
  # comp_prob(1, 1, 1)      # => ok, but with warnings (as NPV & FOR are NaN)
  # comp_prob(1, 1, 1, NA)  # => ok, but with warnings (as NPV & FOR are NaN)
  # comp_prob(1, 1, NA, 1)  # => ok, but with warnings (as NPV & FOR are NaN)
  #
  # # Ways to fail:
  # comp_prob(NA, 1, 1, NA)  # => NAs + warning: prev not numeric
  # comp_prob(1, NA, 1, NA)  # => NAs + warning: sens not numeric
  # comp_prob(8,  1, 1, NA)  # => NAs + warning: prev no probability
  # comp_prob(1,  8, 1, NA)  # => NAs + warning: sens no probability
  # comp_prob(1,  1, 1,  1)  # => NAs and warning: is_complement not in tolerated range
}

## -----------------------------------------------
## (3) Apply to initialize prob:


#' List current probability information.
#'
#' \code{prob} is a list of named numeric variables
#' containing 3 essential (1 non-conditional and 2 conditional) probabilities
#' and 7 derived (1 non-conditional and 6 conditional) probabilities:
#'
#' \enumerate{
#'
#'  \item the condition's prevalence \code{\link{prev}}
#'  (i.e., the probability of condition being \code{TRUE}):
#'  \code{prev = \link{cond.true}/\link{N}}.
#'
#'  \item the decision's sensitivity \code{\link{sens}}
#'  (i.e., the conditional probability of a positive decision
#'  provided that the condition is \code{TRUE}).
#'
#'  \item the decision's miss rate \code{\link{mirt}}
#'  (i.e., the conditional probability of a negative decision
#'  provided that the condition is \code{TRUE}).
#'
#'  \item the decision's specificity \code{\link{spec}}
#'  (i.e., the conditional probability
#'  of a negative decision provided that the condition is \code{FALSE}).
#'
#'  \item the decision's false alarm rate \code{\link{fart}}
#'  (i.e., the conditional probability
#'  of a positive decision provided that the condition is \code{FALSE}).
#'
#'
#'  \item the proportion (baseline probability or rate)
#'  of positive decisions \code{\link{ppod}}
#'  (but not necessarily true decisions):
#'  \code{ppod = \link{dec.pos}/\link{N}}.
#'
#'  \item the decision's positive predictive value \code{\link{PPV}}
#' (i.e., the conditional probability of the condition being \code{TRUE}
#' provided that the decision is positive)
#'
#'  \item the decision's negative predictive value \code{\link{NPV}}
#' (i.e., the conditional probability of the condition being \code{FALSE}
#' provided that the decision is negative)
#'
#'  \item the decision's false discovery or false detection rate \code{\link{FDR}}
#' (i.e., the conditional probability of the condition being \code{FALSE}
#' provided that the decision is positive)
#'
#'  \item the decision's false omission rate \code{\link{FOR}}
#' (i.e., the conditional probability of the condition being \code{TRUE}
#' provided that the decision is negative)
#'
#' }
#'
#' These probabilities are computed from basic probabilities
#' (contained in \code{\link{num}}) and computed by using
#' \code{\link{comp_prob}}.
#'
#' The list \code{prob} is the probability counterpart
#' to the list containing frequency information \code{\link{freq}}.
#'
#' Note that inputs of extreme probabilities (of 0 or 1)
#' may yield unexpected values (e.g., an \code{\link{NPV}}
#' value of NaN when \code{\link{is_perfect}}
#' evaluates to \code{TRUE}).
#'
#' @examples
#' prob <- comp_prob()  # => initialize prob to default parameters
#' prob                 # => show current values
#' length(prob)         # => 8
#'
#' @family lists containing scenario settings
#'
#' @seealso
#' \code{\link{comp_freq}} computes current frequency information;
#' \code{\link{num}} contains basic numeric variables;
#' \code{\link{init_num}} initializes basic numeric variables;
#' \code{\link{prob}} contains current probability information


prob <- comp_prob()  # => initialize prob to default parameters
# prob               # => show current values
# length(prob)       # => 8


## -----------------------------------------------
## (+) ToDo:

## - Document comp_PPV, comp_NPV, ... etc.
##
## - Allow using fart instead of spec in all functions
##   (defined above)
##
## - Add alternative ways to compute probabilities
##   from frequencies (based on various elements of freq)!
##
## - Compute alternative prob from freq with
##   a. N of dec.pos (rather than N of fa) and
##   b. N of dec.neg (rather than N of mi) provided.
##
## - Compute basic parameters (probabilities and frequencies)
##   from MIX of existing probabilities and frequencies!

## -----------------------------------------------
## eof.
