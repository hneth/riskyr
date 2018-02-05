## comp_prob_prob.R | riskyR
## 2018 01 31
## -----------------------------------------------
## Compute other probabilities from probabilities:

## Note: For computing ALL prob from 3 basic probabilities
##       see comp_prob in file "init_num_prob.R".

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
## 2 functions convert between formats:

## a. comp_freq_prob: Computes freq from prob
## b. comp_prob_freq: Computes prob from freq

## -----------------------------------------------


## -----------------------------------------------
## ad 1: Bayesian computations with probabilities:
## -----------------------------------------------

## -----------------------------------------------
## (A) Compute basic probabilities
##     from probabilities:

## -----------------------------------------------
## Computing complementary probabilities:
## -----------------------------------------------
## A general approach:

#' Compute a probability's complement probability.
#'
#' \code{comp_complement} computes the
#' probability complement of a
#' given probability \code{prob}.
#'
#' The type and range of \code{prob} is
#' verified with \code{\link{is_prob}}.
#'
#'
#' @param prob A numeric probability value
#' (in range from 0 to 1).
#'
#' @return A numeric probability value
#' (in range from 0 to 1).
#'
#'
#' @examples
#' comp_complement(0)    # => 1
#' comp_complement(1)    # => 0
#'
#' comp_complement(2)    # => NA + warning (beyond range)
#' comp_complement("p")  # => NA + warning (non-numeric)
#'
#'
#' @family functions computing probabilities
#'
#' @seealso
#' \code{\link{is_complement}} verifies numeric complements;
#' \code{\link{comp_comp_pair}} returns a probability and its complement;
#' \code{\link{is_prob}} verifies probabilities.
#'
#' @export
#'

comp_complement <- function(prob) {

  comp <- NA

  if (is_prob(prob)) {  # verify
    comp <- 1 - prob    # complement
  }

  return(comp)
}

## Check:
{
  # comp_complement(0)   # => 1
  # comp_complement(1)   # => 0
  #
  # comp_complement(2)   # => NA + warning (beyond range)
  # comp_complement("")  # => NA + warning (non-numeric)
}

## -----------------------------------------------
## (a) mirt from sens:

#' Compute a decision's miss rate from its sensitivity.
#'
#' \code{comp_mirt} is a conversion function that takes a sensitivity \code{\link{sens}}
#' -- given as a probability (i.e., a numeric value in the range from 0 to 1) --
#' as its input, and returns the corresponding miss rate \code{\link{mirt}}
#' -- also as a probability -- as its output.
#'
#' The miss rate \code{\link{mirt}} and sensitivity \code{\link{sens}}
#' are complements (\code{mirt = (1 - sens)}) and both features of
#' the decision process (e.g., a diagnostic test).
#'
#' The function \code{comp_mirt} is complementary to the conversion function
#' \code{\link{comp_sens}} and uses the generic function
#' \code{\link{comp_complement}}.
#'
#' @param sens The decision's sensitivity \code{\link{sens}} as a probability.
#'
#' @return The decision's miss rate \code{\link{mirt}} as a probability.
#'
#' @examples
#' comp_mirt(2)                      # => NA + warning (beyond range)
#' comp_mirt(1/3)                    # => 0.6666667
#' comp_mirt(comp_complement(0.123))  # => 0.123
#'
#' @family functions computing probabilities
#'
#' @seealso
#' \code{\link{comp_complement}} computes a probability's complement;
#' \code{\link{is_complement}} verifies probability complements;
#' \code{\link{comp_prob}} computes current probability information;
#' \code{\link{prob}} contains current probability information;
#' \code{\link{is_prob}} verifies probabilities.

comp_mirt <- function(sens) {

  mirt <- comp_complement(sens)  # use generic function

  return(mirt)
}


## -----------------------------------------------
## (b) sens from mirt:

#' Compute a decision's sensitivity from its miss rate.
#'
#' \code{comp_sens} is a conversion function that takes a miss rate \code{\link{mirt}}
#' -- given as a probability (i.e., a numeric value in the range from 0 to 1) --
#' as its input, and returns the corresponding sensitivity \code{\link{sens}}
#' -- also as a probability -- as its output.
#'
#' The sensitivity \code{\link{sens}} and miss rate \code{\link{mirt}}
#' are complements (\code{sens = (1 - mirt)}) and both features of
#' the decision process (e.g., a diagnostic test).
#'
#' The function \code{comp_sens} is complementary to the conversion function
#' \code{\link{comp_mirt}} and uses the generic function
#' \code{\link{comp_complement}}.
#'
#' @param mirt The decision's miss rate \code{\link{mirt}} as a probability.
#'
#' @return The decision's sensitivity \code{\link{sens}} as a probability.
#'
#' @examples
#' comp_sens(2)                      # => NA + warning (beyond range)
#' comp_sens(1/3)                    # => 0.6666667
#' comp_sens(comp_complement(0.123))  # => 0.123
#'
#' @family functions computing probabilities
#'
#' @seealso
#' \code{\link{comp_complement}} computes a probability's complement;
#' \code{\link{is_complement}} verifies probability complements;
#' \code{\link{comp_prob}} computes current probability information;
#' \code{\link{prob}} contains current probability information;
#' \code{\link{is_prob}} verifies probabilities.

comp_sens <- function(mirt) {

  sens <- comp_complement(mirt)  # use generic function

  return(sens)
}

## -----------------------------------------------
## (c) fart from spec:

#' Compute a decision's false alarm rate from its specificity.
#'
#' \code{comp_fart} is a conversion function that takes a specificity \code{\link{spec}}
#' -- given as a probability (i.e., a numeric value in the range from 0 to 1) --
#' as its input, and returns the corresponding false alarm rate \code{\link{fart}}
#' -- also as a probability -- as its output.
#'
#' The false alarm rate \code{\link{fart}} and specificity \code{\link{spec}}
#' are complements (\code{fart = (1 - spec)}) and both features of
#' the decision process (e.g., a diagnostic test).
#'
#' The function \code{comp_fart} is complementary to the conversion function
#' \code{\link{comp_spec}} and uses the generic function
#' \code{\link{comp_complement}}.
#'
#' @param spec The decision's specificity value \code{\link{spec}} as a probability.
#'
#' @return The decision's false alarm rate \code{\link{fart}} as a probability.
#'
#' @examples
#' comp_fart(2)                       # => NA + warning (beyond range)
#' comp_fart(1/3)                     # => 0.6666667
#' comp_fart(comp_complement(0.123))  # => 0.123
#'
#' @family functions computing probabilities
#'
#' @seealso
#' \code{\link{comp_complement}} computes a probability's complement;
#' \code{\link{is_complement}} verifies probability complements;
#' \code{\link{comp_prob}} computes current probability information;
#' \code{\link{prob}} contains current probability information;
#' \code{\link{is_prob}} verifies probabilities.

comp_fart <- function(spec) {

  fart <- comp_complement(spec)  # use generic function

  return(fart)
}

## -----------------------------------------------
## (d) spec from fart:

#' Compute a decision's specificity from its false alarm rate.
#'
#' \code{comp_spec} is a conversion function that takes a false alarm rate \code{\link{fart}}
#' -- given as a probability (i.e., a numeric value in the range from 0 to 1) --
#' as its input, and returns the corresponding specificity \code{\link{spec}}
#' -- also as a probability -- as its output.
#'
#' The specificity \code{\link{spec}} and the false alarm rate \code{\link{fart}}
#' are complements (\code{spec = (1 - fart)}) and both features of
#' the decision process (e.g., a diagnostic test).
#'
#' The function \code{comp_spec} is complementary to the conversion function
#' \code{\link{comp_fart}} and uses the generic function
#' \code{\link{comp_complement}}.
#'
#' @param fart The decision's false alarm rate \code{\link{fart}} as a probability.
#'
#' @return The decision's specificity \code{\link{spec}} as a probability.
#'
#' @examples
#' comp_spec(2)                      # => NA + warning (beyond range)
#' comp_spec(1/3)                    # => 0.6666667
#' comp_spec(comp_complement(0.123))  # => 0.123
#'
#' @family functions computing probabilities
#'
#' @seealso
#' \code{\link{comp_complement}} computes a probability's complement;
#' \code{\link{is_complement}} verifies probability complements;
#' \code{\link{comp_prob}} computes current probability information;
#' \code{\link{prob}} contains current probability information;
#' \code{\link{is_prob}} verifies probabilities.

comp_spec <- function(fart) {

  spec <- comp_complement(fart)  # use generic function

  return(spec)
}


## -----------------------------------------------
## Pairs of complements:

#' Compute a probability's (missing) complement and return both.
#'
#' \code{comp_comp_pair} is a function that takes 0, 1, or 2
#' probabilities (\code{p1} and \code{p2}) as inputs.
#' If either of them is missing (\code{NA}), it computes the complement
#' of the other one and returns both probabilities.
#'
#' \code{comp_comp_pair} does \emph{nothing} when both arguments are provided
#' (i.e., \code{!is.na(p1) & !is.na(p2)}) and only issues
#' a warning if both arguments are missing
#' (i.e., \code{is.na(p1) & is.na(p2)}).
#'
#' Inputs are \emph{not} verified:
#' Use \code{\link{is_prob}} to verify that an input is
#' a probability and \code{\link{is_complement}} to verify
#' that two provided values actually are complements.
#'
#' @param p1 A numeric probability value
#' (in range from 0 to 1).
#' \code{p1} is optional when \code{p2} is provided.
#'
#' @param p2 A numeric probability value
#' (in range from 0 to 1).
#' \code{p2} is optional when \code{p1} is provided.
#'
#' @return A vector \code{v} containing 2 numeric probability values
#' (in range from 0 to 1): \code{v = c(p1, p2)}.
#'
#' @examples
#' # ways to work:
#' comp_comp_pair(1, 0)   # => 1 0
#' comp_comp_pair(0, 1)   # => 0 1
#' comp_comp_pair(1, NA)  # => 1 0
#' comp_comp_pair(NA, 1)  # => 0 1
#'
#' # watch out for:
#' comp_comp_pair(NA, NA) # => NA NA + warning
#' comp_comp_pair(8, 8)   # => 8 8 + NO warning (as is_prob is not verified)
#' comp_comp_pair(1, 1)   # => 1 1 + NO warning (as is_complement is not verified)
#'
#' @family functions computing probabilities
#'
#' @seealso
#' \code{\link{is_complement}} verifies numeric complements;
#' \code{\link{is_valid_prob_set}} verifies sets of probabilities;
#' \code{\link{comp_complete_prob_set}} completes valid sets of probabilities;
#' \code{\link{is_extreme_prob_set}} verifies extreme cases;
#' \code{\link{comp_prob}} computes current probability information;
#' \code{\link{prob}} contains current probability information;
#' \code{\link{is_prob}} verifies probabilities.

comp_comp_pair <- function(p1 = NA, p2 = NA){

  pair <- c(NULL, NULL) # initialize
  missing <- NA

  if (is.na(p1) & is.na(p2)) {

    warning("One argument (either p1 or p2) is necessary.")
    pair <- c(NA, NA)

  } else if (!is.na(p1) & is.na(p2)) {  # 1: only p1 provided:

    missing <- comp_complement(p1)       #    - compute its comp
    pair <- c(p1, missing)              #    - define pair (leaving input order intact)

  } else if (!is.na(p2) & is.na(p1)) {  # 2: only p2 is provided:

    missing <- comp_complement(p2)       #    - compute spec
    pair <- c(missing, p2)              #    - define pair (leaving input order intact)

  } else {                              # 3: both are provided
    pair <- c(p1, p2)                   #    - leave inputs intact
  }

  return(pair)  # always return pair in order c(p1, p2)

}

## Check:
{
  # # ways to work:
  # comp_comp_pair(1, 0)   # => 1 0
  # comp_comp_pair(0, 1)   # => 0 1
  # comp_comp_pair(1, NA)  # => 1 0
  # comp_comp_pair(NA, 1)  # => 0 1
  #
  # # watch out for:
  # comp_comp_pair(NA, NA) # => NA NA + warning
  # comp_comp_pair(8, 8)   # => 8 8 + NO warning (as is_prob is not verified)
  # comp_comp_pair(1, 1)   # => 1 1 + NO warning (as is_complement is not verified)
}

## -----------------------------------------------
# Complete a valid set of probability inputs:

#' Compute a complete set of probabilities from valid probability inputs.
#'
#' \code{comp_complete_prob_set} is a function takes a
#' valid set of (3 to 5) probabilities as inputs (as a vector)
#' and returns the complete set of
#' (3 essential and 2 optional) probabilities.
#'
#' Assuming that \code{\link{is_valid_prob_set} = TRUE}
#' this function uses \code{\link{comp_comp_pair}} on the
#' two optional pairs (i.e.,
#' \code{\link{sens}} and \code{\link{mirt}}, and
#' \code{\link{spec}} and \code{\link{fart}}) and
#' returns the complete set of 5 probabilities.
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
#' @return A vector of 5 probabilities:
#' \code{c(\link{prev}, \link{sens}, \link{mirt}, \link{spec}, \link{fart})}.
#'
#' @examples
#' comp_complete_prob_set(1, .8, NA, .7, NA)  # => 1.0 0.8 0.2 0.7 0.3
#' comp_complete_prob_set(1, NA, .8, NA, .4)  # => 1.0 0.2 0.8 0.6 0.4
#'
#' # Watch out for:
#' comp_complete_prob_set(8)                  # => 8 NA NA NA NA       + warnings that comp_comp_pair needs 1 argument
#' comp_complete_prob_set(8, 7, 6, 5, 4)      # => 8 7 6 5 4           + no warning (as valid set assumed)!
#' comp_complete_prob_set(8, .8, NA, .7, NA)  # => 8.0 0.8 0.2 0.7 0.3 + no warning (as valid set assumed)!
#' comp_complete_prob_set(8, 2, NA, 3, NA)    # => 8 2 NA 3 NA         + no warning (as valid set assumed)!
#'
#' @family functions computing probabilities
#'
#' @seealso
#' \code{\link{is_valid_prob_set}} verifies a set of probability inputs;
#' \code{\link{is_extreme_prob_set}} verifies extreme cases;
#' \code{\link{comp_comp_pair}} computes pairs of complements;
#' \code{\link{is_complement}} verifies numeric complements;
#' \code{\link{is_prob}} verifies probabilities;
#' \code{\link{comp_prob}} computes current probability information;
#' \code{\link{prob}} contains current probability information;
#' \code{\link{init_num}} initializes basic numeric variables;
#' \code{\link{num}} contains basic numeric variables.
#'
#' @export

comp_complete_prob_set <- function(prev,
                                   sens = NA, mirt = NA,
                                   spec = NA, fart = NA
) {

  # (1) initialize:
  prob_quintet <- rep(NA, 5)

  ## (2) Compute missing sens or mirt (if applicable):
  cur.sens.mirt <- comp_comp_pair(sens, mirt)
  sens <- cur.sens.mirt[1]  # 1st argument
  mirt <- cur.sens.mirt[2]  # 2nd argument

  ## (3) Compute missing spec or fart (if applicable):
  cur.spec.fart <- comp_comp_pair(spec, fart)
  spec <- cur.spec.fart[1]  # 1st argument
  fart <- cur.spec.fart[2]  # 2nd argument

  ## (4) Assemble all probabilities:
  prob_quintet <- c(prev, sens, mirt, spec, fart)

  ## (5) return vector:
  return(prob_quintet)

}

## Check:
{
  #   comp_complete_prob_set(1, .8, NA, .7, NA)  # => 1.0 0.8 0.2 0.7 0.3
  #   comp_complete_prob_set(1, NA, .8, NA, .4)  # => 1.0 0.2 0.8 0.6 0.4
  #
  #   # Watch out for:
  #   comp_complete_prob_set(8)                  # => 8 NA NA NA NA       + warnings that comp_comp_pair needs 1 argument
  #   comp_complete_prob_set(8, 7, 6, 5, 4)      # => 8 7 6 5 4           + no warning (as valid set assumed)!
  #   comp_complete_prob_set(8, .8, NA, .7, NA)  # => 8.0 0.8 0.2 0.7 0.3 + no warning (as valid set assumed)!
  #   comp_complete_prob_set(8, 2, NA, 3, NA)    # => 8 2 NA 3 NA         + no warning (as valid set assumed)!
}

## -----------------------------------------------
## Compute derived probabilities:
## -----------------------------------------------

## -----------------------------------------------
## B: Perspective: by decision:
## -----------------------------------------------

## -----------------------------------------------
## (0) ppod = base rate of decisions being positive (PR):

#' Compute the proportion of positive decisions (ppod) from probabilities.
#'
#' \code{comp_ppod} computes the proportion of positive decisions \code{\link{ppod}}
#' from 3 essential probabilities
#' \code{\link{prev}}, \code{\link{sens}}, and \code{\link{spec}}.
#'
#' \code{comp_ppod} uses probabilities (not frequencies)
#' and does not round results.
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
#'
#' @return The proportion of positive decisions \code{\link{ppod}} as a probability.
#' A warning is provided for NaN values.
#'
#'
#' @examples
#' # (1) Ways to work:
#' comp_ppod(.10, .200, .300)  # => ppod = 0.65
#' comp_ppod(.50, .333, .666)  # => ppod = 0.3335
#'
#' # (2) Watch out for vectors:
#' prev <- seq(0, 1, .1)
#' comp_ppod(prev, .8, .5)  # => 0.50 0.53 0.56 0.59 0.62 0.65 0.68 0.71 0.74 0.77 0.80
#' comp_ppod(prev,  0,  1)  # => 0 0 0 0 0 0 0 0 0 0 0
#'
#' # (3) Watch out for extreme values:
#'
#' comp_ppod(1, 1, 1)  #  => 1
#' comp_ppod(1, 1, 0)  #  => 1
#'
#' comp_ppod(1, 0, 1)  #  => 0
#' comp_ppod(1, 0, 0)  #  => 0
#'
#' comp_ppod(0, 1, 1)  #  => 0
#' comp_ppod(0, 1, 0)  #  => 1
#'
#' comp_ppod(0, 0, 1)  #  => 0
#' comp_ppod(0, 0, 0)  #  => 1
#'
#'
#' @family functions computing probabilities
#'
#' @seealso
#' \code{\link{comp_sens}} and \code{\link{comp_NPV}} compute related probabilities;
#' \code{\link{is_extreme_prob_set}} verifies extreme cases;
#' \code{\link{comp_complement}} computes a probability's complement;
#' \code{\link{is_complement}} verifies probability complements;
#' \code{\link{comp_prob}} computes current probability information;
#' \code{\link{prob}} contains current probability information;
#' \code{\link{is_prob}} verifies probabilities.
#'
#' @export


comp_ppod <- function(prev, sens, spec) {

  ppod <- NA # initialize

  ## ToDo: Add condition
  ## if (is_valid_prob_set(prev, sens, mirt, spec, fart)) { ... }

  ## Definition: ppod = dec.pos / N  =  (hi + fa) / (hi + mi + fa + cr)

  ## Computation:
  hi <- prev * sens
  mi <- prev * (1 - sens)
  cr <- (1 - prev) * spec
  fa <- (1 - prev) * (1 - spec)

  ppod <- (hi + fa) / (hi + mi + fa + cr)

  ## Print a warning if NaN:
  if (any(is.nan(ppod))) {
    warning("ppod is NaN.")
  }

  return(ppod)
}

## Check:
{
  # comp_ppod(1, 1, 1)  #  => 1
  # comp_ppod(1, 1, 0)  #  => 1
  #
  # comp_ppod(1, 0, 1)  #  => 0
  # comp_ppod(1, 0, 0)  #  => 0
  #
  # comp_ppod(0, 1, 1)  #  => 0
  # comp_ppod(0, 1, 0)  #  => 1
  #
  # comp_ppod(0, 0, 1)  #  => 0
  # comp_ppod(0, 0, 0)  #  => 1
}

## for extreme values:
## \code{\link{is_extreme_prob_set}} verifies extreme cases;

## -----------------------------------------------
## 1. Positive predictive value (PPV) from probabilities:


#' Compute a decision's positive predictive value (PPV) from probabilities.
#'
#' \code{comp_PPV} computes the positive predictive value \code{\link{PPV}}
#' from 3 essential probabilities
#' \code{\link{prev}}, \code{\link{sens}}, and \code{\link{spec}}.
#'
#' \code{comp_PPV} uses probabilities (not frequencies)
#' and does not round results.
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
#'
#' @return The positive predictive value \code{\link{PPV}} as a probability.
#' A warning is provided for NaN values.
#'
#'
#' @examples
#' # (1) Ways to work:
#' comp_PPV(.50, .500, .500)  # => PPV = 0.5
#' comp_PPV(.50, .333, .666)  # => PPV = 0.499
#'
#' # (2) Watch out for vectors:
#' prev <- seq(0, 1, .1)
#' comp_PPV(prev, .5, .5)  # => without NaN values
#' comp_PPV(prev,  0,  1)  # => with NaN values
#'
#' # (3) Watch out for extreme values:
#' comp_PPV(prev = 1, sens = 0, spec = .5)             # => NaN, only mi ==> hi = 0 and fa = 0:  PPV = 0/0 = NaN
#' is_extreme_prob_set(prev = 1, sens = 0, spec = .5)  # => verifies extreme cases
#'
#' comp_PPV(prev = 0, sens = .5, spec = 1)             # => NaN, only cr ==> hi = 0 and fa = 0:  PPV = 0/0 = NaN
#' is_extreme_prob_set(prev = 0, sens = .5, spec = 1)  # => verifies extreme cases
#'
#' comp_PPV(prev = .5, sens = 0, spec = 1)             # => NaN, only cr ==> hi = 0 and fa = 0:  PPV = 0/0 = NaN
#' is_extreme_prob_set(prev = .5, sens = 0, spec = 1)  # => verifies extreme cases
#'
#'
#' @family functions computing probabilities
#'
#' @seealso
#' \code{\link{comp_sens}} and \code{\link{comp_NPV}} compute related probabilities;
#' \code{\link{is_extreme_prob_set}} verifies extreme cases;
#' \code{\link{comp_complement}} computes a probability's complement;
#' \code{\link{is_complement}} verifies probability complements;
#' \code{\link{comp_prob}} computes current probability information;
#' \code{\link{prob}} contains current probability information;
#' \code{\link{is_prob}} verifies probabilities.
#'
#' @export


comp_PPV <- function(prev, sens, spec) {

  PPV <- NA # initialize

  ## ToDo: Verify (is_valid_prob_set(prev, sens, mirt, spec, fart)) { ... }

  ## Definition: PPV = hi / dec.pos  =  hi / (hi + fa)

  ## Computation:
  hi <- prev * sens
  fa <- (1 - prev) * (1 - spec)

  PPV <- hi / (hi + fa)

  ## Print a warning if NaN:
  if (any(is.nan(PPV))) {
    warning("PPV is NaN.")
  }

  return(PPV)
}

## Check:

{
  ## (1) Ways to work:
  # comp_PPV(.50, .500, .500)  # => PPV = 0.5
  # comp_PPV(.50, .333, .666)  # => PPV = 0.499

  ## (2) Watch out for vectors:
  # prev <- seq(0, 1, .1)
  # comp_PPV(prev, .5, .5)  # => without NaN values
  # comp_PPV(prev,  0,  1)  # => with NaN values

  ## (3) Watch out for extreme values:
  # comp_PPV(prev = 1, sens = 0, spec = .5)             # => NaN, only mi ==> hi = 0 and fa = 0:  PPV = 0/0 = NaN
  # is_extreme_prob_set(prev = 1, sens = 0, spec = .5)  # => verifies extreme cases
  #
  # comp_PPV(prev = 0, sens = .5, spec = 1)             # => NaN, only cr ==> hi = 0 and fa = 0:  PPV = 0/0 = NaN
  # is_extreme_prob_set(prev = 0, sens = .5, spec = 1)  # => verifies extreme cases
  #
  # comp_PPV(prev = .5, sens = 0, spec = 1)             # => NaN, only cr ==> hi = 0 and fa = 0:  PPV = 0/0 = NaN
  # is_extreme_prob_set(prev = .5, sens = 0, spec = 1)  # => verifies extreme cases
}



## -----------------------------------------------
## 2. False discovery/detection rate (FDR = complement of PPV):

#' Compute a decision's false detection rate (FDR) from probabilities.
#'
#' \code{comp_FDR} computes the false detection rate \code{\link{FDR}}
#' from 3 essential probabilities
#' \code{\link{prev}}, \code{\link{sens}}, and \code{\link{spec}}.
#'
#' \code{comp_FDR} uses probabilities (not frequencies)
#' and does not round results.
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
#'
#' @return The false detection rate \code{\link{FDR}} as a probability.
#' A warning is provided for NaN values.
#'
#'
#' @examples
#' # (1) Ways to work:
#' comp_FDR(.50, .500, .500)  # => FDR = 0.5    = (1 - PPV)
#' comp_FDR(.50, .333, .666)  # => FDR = 0.5007 = (1 - PPV)
#'
#'
#' @family functions computing probabilities
#'
#' @seealso
#' \code{\link{comp_sens}} and \code{\link{comp_PPV}} compute related probabilities;
#' \code{\link{is_extreme_prob_set}} verifies extreme cases;
#' \code{\link{comp_complement}} computes a probability's complement;
#' \code{\link{is_complement}} verifies probability complements;
#' \code{\link{comp_prob}} computes current probability information;
#' \code{\link{prob}} contains current probability information;
#' \code{\link{is_prob}} verifies probabilities.
#'
#' @export


## (a) from basic probabilities:
comp_FDR <- function(prev, sens, spec) {

  FDR <- NA # initialize
  # PPV <- NA

  ## ToDo: Verify (is_valid_prob_set(prev, sens, mirt, spec, fart)) { ... }

  ## Definition: FDR = fa / dec.pos  =  fa / (hi + fa)

  ## Computation:
  hi <- prev * sens
  fa <- (1 - prev) * (1 - spec)

  FDR <- fa / (hi + fa)

  ## Print a warning if NaN:
  if (any(is.nan(FDR))) {
    warning("FDR is NaN.")
  }

  return(FDR)
}


## (b) FDR = 1 - PPV:
comp_FDR_PPV <- function(PPV) {

  FDR <- NA  # initialize

  FDR <- comp_complement(PPV)  # FDR is the complement of PPV

  return(FDR)
}


## -----------------------------------------------
## 3. Negative predictive value (NPV) from probabilities:

#' Compute a decision's negative predictive value (NPV) from probabilities.
#'
#' \code{comp_NPV} computes the negative predictive value \code{\link{NPV}}
#' from 3 essential probabilities
#' \code{\link{prev}}, \code{\link{sens}}, and \code{\link{spec}}.
#'
#' \code{comp_NPV} uses probabilities (not frequencies)
#' and does not round results.
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
#'
#' @return The negative predictive value \code{\link{NPV}} as a probability.
#' A warning is provided for NaN values.
#'
#'
#' @examples
#' # (1) Ways to work:
#' comp_NPV(.50, .500, .500)  # => NPV = 0.5
#' comp_NPV(.50, .333, .666)  # => NPV = 0.4996
#'
#' # (2) Watch out for vectors:
#' prev <- seq(0, 1, .1)
#' comp_NPV(prev, .5, .5)  # => without NaN values
#' comp_NPV(prev,  1,  0)  # => with NaN values
#'
#' # (3) Watch out for extreme values:
#' comp_NPV(1, 1, 1)    # => NaN, as cr = 0 and mi = 0: 0/0
#' comp_NPV(1, 1, 0)    # => NaN, as cr = 0 and mi = 0: 0/0
#' comp_NPV(.5, sens = 1, spec = 0)              # => NaN, no dec.neg cases:  NPV = 0/0 = NaN
#' is_extreme_prob_set(.5, sens = 1, spec = 0)   # => verifies extreme cases
#'
#'
#' @family functions computing probabilities
#'
#' @seealso
#' \code{\link{comp_spec}} and \code{\link{comp_PPV}} compute related probabilities;
#' \code{\link{is_extreme_prob_set}} verifies extreme cases;
#' \code{\link{comp_complement}} computes a probability's complement;
#' \code{\link{is_complement}} verifies probability complements;
#' \code{\link{comp_prob}} computes current probability information;
#' \code{\link{prob}} contains current probability information;
#' \code{\link{is_prob}} verifies probabilities.
#'
#' @export

comp_NPV <- function(prev, sens, spec) {

  NPV <- NA # initialize

  ## ToDo: Verify (is_valid_prob_set(prev, sens, mirt, spec, fart)) { ... }

  ## Definition: NPV = cr / dec.neg  =  cr / (mi + cr)

  ## Computation:
  mi <- prev * (1 - sens)
  cr <- (1 - prev) * spec

  NPV <- cr / (mi + cr)

  ## Print a warning if NaN:
  if (any(is.nan(NPV))) {
    warning("NPV is NaN.")
  }

  return(NPV)
}

## Check:
{
  # # (1) Ways to work:
  # comp_NPV(.50, .500, .500)  # => PPV = 0.5
  # comp_NPV(.50, .333, .666)  # => PPV = 0.4996

  ## (2) Watch out for vectors:
  # prev <- seq(0, 1, .1)
  # comp_NPV(prev, .5, .5)  # => without NaN values
  # comp_NPV(prev,  1,  0)  # => with NaN values

  ## (3) Watch out for extreme values:
  # comp_NPV(1, 1, 1)    # => NaN, as cr = 0 and mi = 0: 0/0
  # comp_NPV(1, 1, 0)    # => NaN, as cr = 0 and mi = 0: 0/0
  # comp_NPV(.5, sens = 1, spec = 0)               # => NaN, no dec.neg cases:  NPV = 0/0 = NaN
  # is_extreme_prob_set(.5, sens = 1, spec = 0)    # => verifies extreme cases

  ## \code{\link{is_extreme_prob_set}} verifies extreme cases
}


## -----------------------------------------------
## 4. False omission rate (FOR = complement of NPV):

#' Compute a decision's false omission rate (FOR) from probabilities.
#'
#' \code{comp_FOR} computes the false omission rate \code{\link{FOR}}
#' from 3 essential probabilities
#' \code{\link{prev}}, \code{\link{sens}}, and \code{\link{spec}}.
#'
#' \code{comp_FOR} uses probabilities (not frequencies)
#' and does not round results.
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
#'
#' @return The false omission rate \code{\link{FOR}} as a probability.
#' A warning is provided for NaN values.
#'
#'
#' @examples
#' # (1) Ways to work:
#' comp_FOR(.50, .500, .500)  # => FOR = 0.5    = (1 - NPV)
#' comp_FOR(.50, .333, .666)  # => FOR = 0.5004 = (1 - NPV)
#'
#'
#' @family functions computing probabilities
#'
#' @seealso
#' \code{\link{comp_spec}} and \code{\link{comp_NPV}} compute related probabilities;
#' \code{\link{is_extreme_prob_set}} verifies extreme cases;
#' \code{\link{comp_complement}} computes a probability's complement;
#' \code{\link{is_complement}} verifies probability complements;
#' \code{\link{comp_prob}} computes current probability information;
#' \code{\link{prob}} contains current probability information;
#' \code{\link{is_prob}} verifies probabilities.
#'
#' @export

comp_FOR <- function(prev, sens, spec) {

  FOR <- NA  # initialize
  # NPV <- NA

  ## ToDo: Verify (is_valid_prob_set(prev, sens, mirt, spec, fart)) { ... }

  ## Definition: FOR =  mi / dec.neg  =  mi / (cr + mi)

  ## Computation:
  mi <- prev * (1 - sens)
  cr <- (1 - prev) * spec

  FOR <- mi / (mi + cr)

  ## Print a warning if NaN:
  if (any(is.nan(FOR))) {
    warning("FOR is NaN.")
  }

  ## Alternative computation:
  # NPV <- comp_NPV(prev, sens, spec)
  # FOR <- comp_complement(NPV)   # FDR = 1 - NPV

  return(FOR)
}

## Check:
## for extreme values:
# comp_FOR(1, 1, 1)  # => NaN, as cr = 0 and mi = 0: 0/0
# comp_FOR(1, 1, 0)  # => NaN, as cr = 0 and mi = 0: 0/0
# comp_FOR(.5, sens = 1, spec = 0)                    # => NaN, no dec.neg cases:  NPV = 0/0 = NaN
# is_extreme_prob_set(prev = .5, sens = 1, spec = 0)  # => verifies extreme cases

## (b) FOR = 1 - NPV:
comp_FOR_NPV <- function(NPV) {

  FOR <- NA  # initialize

  FOR <- comp_complement(NPV)  # FOR is the complement of NPV

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


## -----------------------------------------------
## Compute either PPV or NPV for an entire matrix of values
## (when sens and spec are given as vectors):

comp_PV_matrix <- function(prev, sens, spec,
                           metric = "PPV",
                           nan.adjust = FALSE) {

  # Initialize matrix as df:
  n.rows <- length(sens)
  n.cols <- length(spec)
  matrix <- as.data.frame(matrix(NA,
                                 nrow = n.rows,
                                 ncol = n.cols))
  names(matrix) <- sens

  ## Loop through rows and columns of matrix:
  for (row in 1:n.rows) {    # row = sens
    for (col in 1:n.cols) {  # col = spec

      cell.val <- NA  # initialize

      cur.sens <- sens[row]
      cur.spec <- spec[col]

      ## (A) metric == PPV:
      if (metric == "PPV") {

        ## Beware of cases in which PPV or NPV are NaN:

        ## (1) PPV is NaN if:
        ##     (a)  (prev = 1) & (sens = 0)
        ##     (b)  (prev = 0) & (spec = 1)
        ##     (c)  (sens = 0) & (spec = 1)

        if (nan.adjust) {  ## Hack fix:

          eps <- 10^-9    # some very small value

          ## Catch and adjust in 3 cases:

          ## (1a) (prev = 1) & (sens = 0): Only mi ==> hi = 0 and fa = 0:  PPV = 0/0 = NaN
          if ((prev == 1) && (cur.sens == 0)) {

            warning("Adjusting for extreme case (PPV:a): (prev = 1) & (sens = 0)!")

            ## Adjustment (to prevent NaN):
            cur.sens <- (cur.sens + eps)  # adjust upwards

          }

          ## (1b) (prev = 0) & (spec = 1): Only cr ==> hi = 0 and fa = 0:  PPV = 0/0 = NaN
          if ((prev == 0) && (cur.spec == 1)) {

            warning("Adjusting for extreme case (PPV:b): (prev = 0) & (spec = 1)!")

            ## Adjustment (to prevent NaN):
            cur.spec <- (cur.spec - eps)  # adjust downwards

          }

          ## (1c) (sens = 0) & (spec = 1): Only mi + cr ==> hi = 0 and fa = 0:  PPV = 0/0 = NaN
          if ((cur.sens == 0) && (cur.spec == 1)) {

            warning("Adjusting for extreme case (PPV:c): (sens = 0) & (spec = 1)!")

            ## Adjustment (to prevent NaN):
            cur.sens <- (cur.sens + eps)  # adjust upwards
            cur.spec <- (cur.spec - eps)  # adjust downwards

          }

        }

        ## (2) Compute PPV:
        cell.val <- comp_PPV(prev, cur.sens, cur.spec)

      }

      ## (B) metric == NPV:
      if (metric == "NPV") {

        ## Beware of cases in which PPV or NPV are NaN:

        ## (2) NPV is NaN if:
        ##     (a)  (prev = 1) & (sens = 1)
        ##     (b)  (prev = 1) & (sens = 0)
        ##     (c)  (sens = 1) & (spec = 0)

        if (nan.adjust) {  ## Hack fix:

          eps <- 10^-9    # some very small value

          ## Catch and adjust in 3 cases:

          ## (2a) (prev = 1) & (sens = 1): NPV = 0/0 = NaN
          if ((prev == 1) && (cur.sens == 1)) {

            warning("Adjusting for extreme case (NPV:a): (prev = 1) & (sens = 1)!")

            ## Adjustment (to prevent NaN):
            cur.sens <- (cur.sens - eps)  # adjust downwards

          }

          ## (2b) (prev = 1) & (spec = 0): NPV = 0/0 = NaN
          if ((prev == 1) && (cur.spec == 0)) {

            warning("Adjusting for extreme case (NPV:b): (prev = 1) & (spec = 0)!")

            ## Adjustment (to prevent NaN):
            cur.spec <- (cur.spec + eps)  # adjust upwards

          }

          ## (2c) (sens = 1) & (spec = 0): NPV = 0/0 = NaN
          if ((cur.sens == 1) && (cur.spec == 0)) {

            warning("Adjusting for extreme case (NPV:c): (sens = 1) & (spec = 0)!")

            ## Adjustment (to prevent NaN):
            cur.sens <- (cur.sens - eps)  # adjust downwards
            cur.spec <- (cur.spec + eps)  # adjust upwards

          }
        }

        ## (2) Compute NPV:
        cell.val <- comp_NPV(prev, cur.sens, cur.spec)  # compute NPV

      }

      matrix[row, col] <- cell.val # store result in matrix

    }
  }

  return(matrix)

}

## Check:
{
  # sens.seq <- seq(0, 1, by = .10)
  # spec.seq <- seq(0, 1, by = .10)
  #
  # # Contrast without and with NaN adjustment:
  # comp_PV_matrix(prev = .33, sens.seq, spec.seq, metric = "PPV", nan.adjust = FALSE)
  # comp_PV_matrix(prev = .33, sens.seq, spec.seq, metric = "PPV", nan.adjust = TRUE)
  #
  # # Contrast without and with NaN adjustment:
  # comp_PV_matrix(prev = .33, sens.seq, spec.seq, metric = "NPV", nan.adjust = FALSE)
  # comp_PV_matrix(prev = .33, sens.seq, spec.seq, metric = "NPV", nan.adjust = TRUE)
}

## -----------------------------------------------
## (+) ToDo:

## - Add documentation.
##   Document comp_PPV, comp_NPV, ... etc.

## - Allow using fart & mirt in addition to sens & spec in all functions
##   defined above

## - Add alternative ways to compute probabilities
##   from frequencies (based on various elements of freq)!
##   - Compute alternative prob from freq with
##     a. N of dec.pos (rather than N of fa) and
##     b. N of dec.neg (rather than N of mi) provided.

## - Compute basic parameters (probabilities and frequencies)
##   from MIX of existing probabilities and frequencies!

## -----------------------------------------------
## eof.
