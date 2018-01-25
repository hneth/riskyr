## init_num_prob.R | riskyR
## 2018 01 25
## -----------------------------------------------
## Define and initialize probability information prob
## by using basic parameter values of num:

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
## Two basic directions:

## 1: Bayesian: starting with 3 basic probabilities:
## - given:   prev;  sens, spec
## - derived: all other values

## 2: Natural frequencies:
## - given:   N = hi, mi, fa, cr
## - derived: all other values

## -----------------------------------------------


## -----------------------------------------------
## (1) Compute the set of ALL current probabilities:
##     So far: Compute current values of PPV and NPV
##     as functions of prev, sens, and spec (using Bayes):

#' Compute derived probabilities from basic probabilities.
#'
#' \code{comp_prob} is a function that computes derived probabilities
#' (typically conditional probabilities) from basic probabilities --
#' \code{\link{prev}} and \code{\link{sens}}, and
#' \code{\link{spec}} or \code{\link{fart}} (\code{spec = 1 - fart}).
#'
#' By default, \code{comp_prob} assumes that sufficient
#' basic probabilities (e.g., \code{\link{prev}}, \code{\link{sens}},
#' and either \code{\link{spec}} or \code{\link{fart}}) are provided
#' as inputs and then computes and returns derived probabilities (e.g., the
#' predictive values \code{\link{PPV}} and \code{\link{NPV}}, as well
#' as their complements \code{\link{FDR}} and \code{\link{FOR}})
#' as its output (a list of probabilities \code{\link{prob}}).
#'
#' \code{comp_prob} is the probability counterpart to the
#' frequency function \code{\link{comp_freq}}.
#'
#' Extreme probabilities (sets containing two or more
#' probabilities of 0 or 1) may yield unexpected values
#' (e.g., predictive values \code{\link{PPV}} or \code{\link{NPV}}
#' turning \code{NaN} when \code{\link{is_extreme}}
#' evaluates to \code{TRUE}).
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
#' @return A list \code{\link{prob}} containing 8 probability values.
#'
#' @examples
#' comp_prob()             # => ok, using current defaults
#' length(comp_prob())     # => 8
#'
#' # Ways to succeed:
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
  prob <- list(

    ## (a) By condition: basic probability parameters:
    "prev" = NA,  # simple p of cond.true

    "sens" = NA,  # conditional p
    "mirt" = NA,  # conditional p: 1 - sens
    "spec" = NA,  # conditional p:
    "fart" = NA,  # conditional p: 1 - spec

    ## (b) By decision: derived probabilities and predictive values (PVs):
    "ppod" = NA,  # simple p of dec.pos

    "PPV" = NA,   # conditional p: reversal of sens
    "NPV" = NA,   # conditional p: reversal of spec
    "FDR" = NA,   # conditional p: 1 - PPV
    "FOR" = NA    # conditional p: 1 - NPV
  )

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
## (E) Apply to initialize prob:

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
