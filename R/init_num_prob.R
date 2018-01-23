## init_num_prob.R | riskyR
## 2018 01 22
## -----------------------------------------------
## Define and initialize prob by using num:

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
#' @param sens The decision's sensitivity value \code{\link{sens}}
#' (i.e., the conditional probability of a positive decision
#' provided that the condition is \code{TRUE}).
#' @param spec The decision's specificity value \code{\link{spec}}
#' (i.e., the conditional probability
#' of a negative decision provided that the condition is \code{FALSE}).
#' \code{spec} is optional when is complement \code{fart} is provided.
#' @param fart The decision's false alarm rate \code{\link{fart}}
#' (i.e., the conditional probability
#' of a positive decision provided that the condition is \code{FALSE}).
#' \code{fart} is optional when its complement \code{spec} is provided.
#'
#' @return A list \code{prob} containing 8 probability values.
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
#' \code{\link{num}} contains basic numeric variables;
#' \code{\link{init_num}} initializes basic numeric variables;
#' \code{\link{freq}} contains current frequency information;
#' \code{\link{prob}} contains current probability information;
#' \code{\link{comp_prob}} computes current probability information;
#' \code{\link{is_valid}} verifies the validity of probability inputs;
#' \code{\link{comp_prob_comp}} computes a probability's complement;
#' \code{\link{comp_comp_pair}} computes pairs of complements;
#' \code{\link{comp_min_N}} computes a suitable population size \code{\link{N}} (if missing).

comp_prob <- function(prev = num$prev, sens = num$sens,
                      spec = num$spec, fart = NA) {

  ## (0) Initialize prob as a list:
  prob <- list(

    ## (a) basic probability parameters:
    "prev" = NA,  # simple p
    "sens" = NA,  # conditional p
    "spec" = NA,  # conditional p: 1 - fart
    "fart" = NA,  # conditional p: 1 - spec

    ## (b) derived predictive values (PVs):
    "PPV" = NA,   # conditional p: reversal of sens
    "NPV" = NA,   # conditional p: reversal of spec
    "FDR" = NA,   # conditional p: 1 - PPV
    "FOR" = NA    # conditional p: 1 - NPV
  )

  ## (1) Only if basic quadruple of probabilities is valid:
  if (is_valid(prev, sens, spec, fart)) {

    ## (2) Compute missing fart or spec (4th argument) value (if applicable):
    cur.spec.fart <- comp_comp_pair(spec, fart)
    spec <- cur.spec.fart[1] # 1st argument
    fart <- cur.spec.fart[2] # 2nd argument

    ## (3) Issue a warning if probabilities describe an extreme case:
    is_extreme(prev, sens, spec, fart)  # prints a warning if TRUE

    ## (4) Assign all values of prob based on current parameter values:
    ## (a) basic probability parameters:
    prob$prev <- prev
    prob$sens <- sens
    prob$spec <- spec
    prob$fart <- fart
    ## (b) derived predictive values (PVs):
    prob$PPV <- comp_PPV(prev, sens, spec) # Note: using probabilistic version (Bayes)
    prob$NPV <- comp_NPV(prev, sens, spec)
    prob$FDR <- comp_FDR(prev, sens, spec)
    prob$FOR <- comp_FOR(prev, sens, spec)

    ## (5) Check derived PVs:
    if ( is.na(prob$PPV) | is.nan(prob$PPV) | !is_prob(prob$PPV) |
         is.na(prob$NPV) | is.nan(prob$NPV) | !is_prob(prob$NPV) |
         is.na(prob$FDR) | is.nan(prob$FDR) | !is_prob(prob$FDR) |
         is.na(prob$FOR) | is.nan(prob$FOR) | !is_prob(prob$FOR) ) {

      warning( "Some PVs are peculiar. Check for extreme probabilities!" )

    }

  } # if (is_valid(prev, sens, spec, fart))

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
#' containing 4 basic (1 non-conditional and 3 conditional) probabilities
#' and 4 derived (conditional) probabilities:
#'
#' \enumerate{
#'
#'  \item the condition's prevalence value \code{\link{prev}}
#'  (i.e., the probability of condition being \code{TRUE}).
#'
#'  \item the decision's sensitivity value \code{\link{sens}}
#'  (i.e., the conditional probability of a positive decision
#'  provided that the condition is \code{TRUE}).
#'
#'  \item the decision's specificity value \code{\link{spec}}
#'  (i.e., the conditional probability
#'  of a negative decision provided that the condition is \code{FALSE}).
#'
#'  \item the decision's false alarm rate \code{\link{fart}}
#'  (i.e., the conditional probability
#'  of a positive decision provided that the condition is \code{FALSE}).
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
