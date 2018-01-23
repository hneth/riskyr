## comp_prob.R | riskyR
## 2018 01 22
## -----------------------------------------------
## Compute probabilities (prob) from basic probabilities
## based on num (using only the necessary parameters of num):

## Note: Always use num (essential) rather than env (NON-essential)!

## -----------------------------------------------
## (A) ToDo: Compute basic parameters (prev; sens + spec, mirt + fart)
##           from existing frequencies (N;   hi + cr,     mi   + fa)

## -----------------------------------------------
## Table of current terminology:

# 9 probabilities:                  9 frequencies:
# ----------------                  ------------------
# (A) basic:
#                                          N
# prev                              n.true | n.false

# sens = hit rate = TPR             hi = TP
# mirt = miss rate = FNR            mi = FN
# fart = false alarm rate = FPR     fa = FP
# spec = true negative rate = TNR   cr = TN


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
## ad 1: Bayesian computations with probabilities:
## -----------------------------------------------

## -----------------------------------------------
## (A) Compute basic probabilities
##     from probabilities:

## -----------------------------------------------
## Computing complementary probabilities:
## -----------------------------------------------
## (a) mirt from sens:
## -----------------------------------------------
## (b) sens from mirt:
## -----------------------------------------------
## (c) fart from spec:

#' Compute a decision's false alarm rate from its specificity.
#'
#' \code{comp_fart} is a conversion function that takes a specificity \code{\link{spec}}
#' -- given as a probability (i.e., a numeric value in the range from 0 to 1) --
#' as its input, and returns the corresponding false alarm rate \code{\link{fart}}
#' -- also as a probability -- as its output.
#'
#' Specificity and false alarm rate are both features of the decision process
#' (e.g., a diagnostic test).
#' The false alarm rate and specificity are complements (i.e., \code{fart = 1 - spec}).
#'
#' \code{comp_fart} is complementary to the conversion function
#' \code{\link{comp_spec}}.
#'
#' @param spec The decision's specificity value \code{\link{spec}}
#' (i.e., the conditional probability
#' of a negative decision provided that the condition is FALSE).
#'
#' @return A false alarm rate \code{\link{fart}} as a probability
#' (i.e., a numeric value in the range from 0 to 1).
#'
#' @examples
#' comp_fart(2)                 # => NA + Warning that 2 is not in 0 to 1 range
#' comp_fart(1/3)               # => 0.6666667
#' comp_fart(comp_spec(0.123))  # => 0.123
#'
#' @family functions computing probabilities
#'
#' @seealso
#' \code{\link{prob}} contains current probability information;
#' \code{\link{comp_prob}} computes current probability information;
#' \code{\link{is_complement}} verifies numeric complements;
#' \code{\link{as_pc}} displays a probability as a percentage;

comp_fart <- function(spec) {

  fart <- NA # initialize

  if (is_prob(spec)) {
    fart <- 1 - spec # compute complement
  }

  return(fart)
}

## -----------------------------------------------
## (d) spec from fart:

#' Compute a decision's specificity from its false alarm rate.
#'
#' \code{comp_spec} is a function that takes a false alarm rate \code{fart}
#' -- given as a probability (i.e., a numeric value in the range from 0 to 1) --
#' as its input, and returns the corresponding specificity \code{spec}
#' -- also as a probability -- as its output.
#'
#' False alarm rate and specificity are both features of the decision process
#' (e.g., a diagnostic test).
#' The specificity and false alarm rate are complements (i.e., \code{spec = 1 - fart}).
#'
#' \code{comp_spec} is complementary to the conversion function
#' \code{\link{comp_fart}}.
#'
#' @param fart The decision's false alarm rate \code{\link{fart}}
#' (i.e., the conditional probability
#' of a positive decision provided that the condition is FALSE).
#'
#' @return A specificity as a probability
#' (i.e., a numeric value in the range from 0 to 1).
#'
#' @examples
#' comp_spec(2)                 # => NA + Warning that 2 is not in 0 to 1 range
#' comp_spec(2/3)               # => 0.3333333
#' comp_spec(comp_fart(0.123))  # => 0.123
#'
#' @family functions computing probabilities
#'
#' @seealso
#' \code{\link{is_complement}} verifies numeric complements;
#' \code{\link{as_pc}} displays a probability as a percentage;
#' \code{\link{comp_prob}} computes derived probabilities

comp_spec <- function(fart) {

  spec <- NA # initialize

  if (is_prob(fart)) {
    spec <- 1 - fart # compute complement
  }

  return(spec)
}

## Check:
{
  # comp_fart(2)
  # comp_fart(1/3)
  # comp_spec(2)
  # comp_spec("one third")
  # comp_spec(comp_fart(2/3))
}

## -----------------------------------------------
## (+) More general approach:

## ToDo: Generalize comp_complement function to 2 types of pairs:
## a. sens + mirt
## b. spec + fart

## -----------------------------------------------

#' Compute a probability's complement (if missing).
#'
#' \code{comp_complement} is a function that takes one or two probabilities
#' that are complements
#' -- either a sensitivity \code{\link{sens}} and miss rate \code{\link{mirt}}
#' -- or a specificity \code{\link{spec}} and false alarm rate \code{\link{fart}} --
#' as inputs. If either of them is missing (\code{NA}), it computes the complement
#' of the other one and returns both probabilities.
#'
#' This function does nothing when both arguments are provided
#' (i.e., \code{!is.na(spec) & !is.na(fart)}) and only issues
#' a warning if both arguments are missing
#' (i.e., \code{is.na(spec) & is.na(fart)}).
#' Use \code{\link{is_complement}} to verify that
#' two provided values actually are complements.
#'
#' @param spec The decision's specificity value \code{\link{spec}}
#' (i.e., the conditional probability
#' of a negative decision provided that the condition is FALSE).
#' \code{spec} is optional when is complement \code{fart} is provided.
#' @param fart The decision's false alarm rate \code{\link{fart}}
#' (i.e., the conditional probability
#' of a positive decision provided that the condition is FALSE).
#' \code{fart} is optional when its complement \code{spec} is provided.
#'
#' @return A vector \code{v} containing two scalars \code{c(spec, fart)}
#' with \code{spec <- v[1]} and \code{fart <- v[2]}).
#'
#' @examples
#' comp_complement(1, 0)   # => 1 0
#' comp_complement(0, 1)   # => 0 1
#' comp_complement(1, NA)  # => 1 0
#' comp_complement(NA, 1)  # => 0 1
#'
#' comp_complement(NA, NA) # => NA NA + warning
#' comp_complement(1, 1)   # => 1 1 + NO warning (as is_complement is not applied here)
#' comp_complement(8, 8)   # => 8 8 + NO warning (as is_prob or is_valid are not applied here)
#'
#' @family functions computing probabilities
#'
#' @seealso
#' \code{\link{is_complement}} verifies numeric complements;
#' \code{\link{is_valid}} verifies valid quadruples of probabilities;
#' \code{\link{comp_prob}} computes derived probabilities

comp_complement <- function(spec, fart){

  pair <- c(NULL, NULL) # initialize
  missing <- NA

  if (is.na(spec) & is.na(fart)) {
    warning("One argument (either spec or fart) is necessary.")
    pair <- c(NA, NA)                      # - set to NA NA
  } else if (!is.na(spec) & is.na(fart)) { # only spec is provided:
    missing <- comp_fart(spec)             # - compute fart
    pair <- c(spec, missing)               # - define pair
  } else if (!is.na(fart) & is.na(spec)) { # only fart is provided:
    missing <- comp_spec(fart)             # - compute spec
    pair <- c(missing, fart)               # - define pair
  } else {
    pair <- c(spec, fart)                  # - set to inputs

  }

  return(pair)  # always return pair c(spec, fart)

}

## Check:
{
  # comp_complement(1, 0)   # => 1 0
  # comp_complement(0, 1)   # => 0 1
  # comp_complement(1, NA)  # => 1 0
  # comp_complement(NA, 1)  # => 0 1
  #
  # comp_complement(NA, NA) # => NA NA + warning
  # comp_complement(1, 1)   # => 1 1 + NO warning (as is_complement is not applied here)
  # comp_complement(8, 8)   # => 8 8 + NO warning (as is_prob or is_valid are not applied here)
}

## -----------------------------------------------

#' Compute a probability's complement (if missing).
#'
#' \code{comp_complement_obs} is a *obsolete* function that
#' takes one or two probabilities that are complements
#' -- either a sensitivity \code{\link{sens}} and miss rate \code{\link{mirt}}
#' -- or a specificity \code{\link{spec}} and false alarm rate \code{\link{fart}} --
#' as inputs. If either of them is missing (\code{NA}), it computes the complement
#' of the other one and returns both probabilities.
#'
#' This function is *obsolete* and *replaced* by \code{\link{comp_comp_pair}}!
#'
#' This function does nothing when both arguments are provided
#' (i.e., \code{!is.na(spec) & !is.na(fart)}) and only issues
#' a warning if both arguments are missing
#' (i.e., \code{is.na(spec) & is.na(fart)}).
#' Use \code{\link{is_complement}} to verify that
#' two provided values actually are complements.
#'
#' @param spec The decision's specificity value \code{\link{spec}}
#' (i.e., the conditional probability
#' of a negative decision provided that the condition is FALSE).
#' \code{spec} is optional when is complement \code{fart} is provided.
#' @param fart The decision's false alarm rate \code{\link{fart}}
#' (i.e., the conditional probability
#' of a positive decision provided that the condition is FALSE).
#' \code{fart} is optional when its complement \code{spec} is provided.
#'
#' @return A vector \code{v} containing two scalars \code{c(spec, fart)}
#' with \code{spec <- v[1]} and \code{fart <- v[2]}).
#'
#' @examples
#' comp_complement(1, 0)   # => 1 0
#' comp_complement(0, 1)   # => 0 1
#' comp_complement(1, NA)  # => 1 0
#' comp_complement(NA, 1)  # => 0 1
#'
#' comp_complement(NA, NA) # => NA NA + warning
#' comp_complement(1, 1)   # => 1 1 + NO warning (as is_complement is not applied here)
#' comp_complement(8, 8)   # => 8 8 + NO warning (as is_prob or is_valid are not applied here)
#'
#' @family functions computing probabilities
#'
#' @seealso
#' \code{\link{is_complement}} verifies numeric complements;
#' \code{\link{is_valid}} verifies valid quadruples of probabilities;
#' \code{\link{comp_prob}} computes derived probabilities

comp_complement_obs <- function(spec, fart){

  pair <- c(NULL, NULL) # initialize
  missing <- NA

  if (is.na(spec) & is.na(fart)) {
    warning("One argument (either spec or fart) is necessary.")
    pair <- c(NA, NA)                      # - set to NA NA
  } else if (!is.na(spec) & is.na(fart)) { # only spec is provided:
    missing <- comp_fart(spec)             # - compute fart
    pair <- c(spec, missing)               # - define pair
  } else if (!is.na(fart) & is.na(spec)) { # only fart is provided:
    missing <- comp_spec(fart)             # - compute spec
    pair <- c(missing, fart)               # - define pair
  } else {
    pair <- c(spec, fart)                  # - set to inputs

  }

  return(pair)  # always return pair c(spec, fart)

}

## Check:
{
  # comp_complement(1, 0)   # => 1 0
  # comp_complement(0, 1)   # => 0 1
  # comp_complement(1, NA)  # => 1 0
  # comp_complement(NA, 1)  # => 0 1
  #
  # comp_complement(NA, NA) # => NA NA + warning
  # comp_complement(1, 1)   # => 1 1 + NO warning (as is_complement is not applied here)
  # comp_complement(8, 8)   # => 8 8 + NO warning (as is_prob or is_valid are not applied here)
}


## -----------------------------------------------
## (B) Compute derived probabilities (predictive values PVs)
##     from probabilities:
## -----------------------------------------------
## 1. Positive predictive value (PPV) from probabilities:

comp_PPV <- function(prev = num$prev, sens = num$sens, spec = num$spec) {

  PPV <- NA # initialize

  ## ToDo: Add condition
  ## if (is_valid(prev, sens, spec, fart)) { ... }

  ## PPV = hits / positive decision = hits / (hits + false alarms):
  hi <- (prev * sens)
  fa <- (1 - prev) * (1 - spec)

  PPV <- hi / (hi + fa)

  ## Check:
  if (is.nan(PPV)) {
    warning("PPV is NaN.")
  }

  return(PPV)
}

## Check:
## for extreme values:
## comp_PPV(0, 0, 1)  # => NaN, as hi = 0 and fa = 0:  0/0

## -----------------------------------------------
## 2. False discovery/detection rate (FDR = complement of PPV):

comp_FDR <- function(prev = num$prev, sens = num$sens, spec = num$spec) {

  PPV <- NA # initialize
  FDR <- NA

  ## ToDo: Add condition
  ## if (is_valid(prev, sens, spec, fart)) { ... }

  PPV <- comp_PPV(prev, sens, spec)
  FDR <- (1 - PPV) # FDR is the complement of PPV

  return(FDR)
}

## -----------------------------------------------
## 3. Negative predictive value (NPV) from probabilities:

comp_NPV <- function(prev = num$prev, sens = num$sens, spec = num$spec) {

  NPV <- NA # initialize

  ## ToDo: Add condition
  ## if (is_valid(prev, sens, spec, fart)) { ... }

  ## NPV = cr / negative decision = cr / (cr + mi):
  cr <- (1 - prev) * spec
  mi <- prev * (1 - sens)

  NPV <- cr / (cr + mi)

  ## Check:
  if (is.nan(NPV)) {
    warning("NPV is NaN.")
  }

  return(NPV)
}

## Check:
## for extreme values:
## comp_NPV(1, 1, 1)  # => NaN, as cr = 0 and mi = 0: 0/0
## comp_NPV(1, 1, 0)  # => NaN, as cr = 0 and mi = 0: 0/0

## -----------------------------------------------
## 4. False omission rate (FOR = complement of NPV):

comp_FOR <- function(prev = num$prev, sens = num$sens, spec = num$spec) {

  NPV <- NA # initialize
  FOR <- NA

  ## ToDo: Add condition
  ## if (is_valid(prev, sens, spec, fart)) { ... }

  NPV <- comp_NPV(prev, sens, spec)
  FOR <- (1 - NPV) # FOR is the complement of NPV

  return(FOR)
}

## -----------------------------------------------
## (C) Compute predictive values
##     from frequencies (various versions):

## ToDo: Add alternative ways to compute probabilities
##       from frequencies (based on different elements of freq)!
##
## Moved to separate file: comp_prob_freq.R !!!

## -----------------------------------------------
## (D) Compute the set of ALL current probabilities:
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
#' \code{\link{comp_complement}} computes a complementary probability (if missing);
#' \code{\link{comp_min_N}} computes a suitable population size \code{\link{N}} (if missing)

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
    cur.spec.fart <- comp_complement(spec, fart)
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
## Compute either PPV or NPV for an entire matrix of values
## (when sens and spec are given as vectors):

comp_PV_matrix <- function(prev, sens, spec, metric = "PPV") {

  # Initialize matrix as df:
  n.rows <- length(sens)
  n.cols <- length(spec)
  matrix <- as.data.frame(matrix(NA,
                                 nrow = n.rows,
                                 ncol = n.cols))
  names(matrix) <- sens

  ## Loop through rows and columns of matrix:
  for (row in 1:n.rows) {
    for (col in 1:n.cols) {

      cell.val <- NA # initialize current cell value

      if (metric == "PPV") {cell.val <- comp_PPV(prev, sens[row], spec[col])} # compute PPV
      if (metric == "NPV") {cell.val <- comp_NPV(prev, sens[row], spec[col])} # compute NPV

      matrix[row, col] <- cell.val # store result in matrix

    }
  }

  return(matrix)

}



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
