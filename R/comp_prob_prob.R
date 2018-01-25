## comp_prob_prob.R | riskyR
## 2018 01 25
## -----------------------------------------------
## Compute probabilities from probabilities:

## -----------------------------------------------
## (A) ToDo: Compute basic parameters (prev; sens + spec, mirt + fart)
##           from existing frequencies (N;   hi + cr,     mi   + fa)

## -----------------------------------------------
## Table of current terminology:

# probabilities (9):                frequencies (9):
# ------------------                ------------------
# (A) basic:
#                                          N
# prev*                             n.true | n.false

# sens* = hit rate = TPR              hi* = TP
# mirt  = miss rate = FNR             mi* = FN
# fart  = false alarm rate = FPR      fa* = FP
# spec* = true negative rate = TNR    cr* = TN

# [Note: *...is essential]

# (B) derived:
#                                 dec.pos | dec.neg

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
## - given:   N = hi, mi, fa, cr
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
## A general approach:

#' Compute a probability's complement probability.
#'
#' \code{comp_prob_comp} computes the
#' probability complement of a
#' given probability \code{prob}.
#'
#' The type and range of \code{prob} is
#' verified with \code{\link{is_prob}}.
#'
#' @param prob A numeric probability value
#' (in range from 0 to 1).
#'
#' @return A numeric probability value
#' (in range from 0 to 1).
#'
#' @examples
#' comp_prob_comp(0)    # => 1
#' comp_prob_comp(1)    # => 0
#'
#' comp_prob_comp(2)    # => NA + warning (beyond range)
#' comp_prob_comp("p")  # => NA + warning (non-numeric)
#'
#' @family functions computing probabilities
#'
#' @seealso
#' \code{\link{is_prob}} verifies probabilities;
#' \code{\link{is_complement}} verifies numeric complements;
#' \code{\link{comp_comp_pair}} returns a probability and its complement.

comp_prob_comp <- function(prob) {

  comp <- NA

  if (is_prob(prob)) {  # verify
    comp <- 1 - prob    # complement
  }

  return(comp)
}

## Check:
{
  # comp_prob_comp(0)   # => 1
  # comp_prob_comp(1)   # => 0
  #
  # comp_prob_comp(2)   # => NA + warning (beyond range)
  # comp_prob_comp("")  # => NA + warning (non-numeric)
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
#' are complements (\code{mirt = 1 - sens}) and both features of
#' the decision process (e.g., a diagnostic test).
#'
#' The function \code{comp_mirt} is complementary to the conversion function
#' \code{\link{comp_sens}} and uses the generic function
#' \code{\link{comp_prob_comp}}.
#'
#' @param sens The decision's sensitivity \code{\link{sens}}.
#'
#' @return The decision's miss rate \code{\link{mirt}}.
#'
#' @examples
#' comp_mirt(2)                      # => NA + warning (beyond range)
#' comp_mirt(1/3)                    # => 0.6666667
#' comp_mirt(comp_prob_comp(0.123))  # => 0.123
#'
#' @family functions computing probabilities
#'
#' @seealso
#' \code{\link{comp_prob_comp}} computes a probability's complement;
#' \code{\link{is_complement}} verifies probability complements;
#' \code{\link{comp_prob}} computes current probability information;
#' \code{\link{prob}} contains current probability information.

comp_mirt <- function(sens) {

  mirt <- comp_prob_comp(sens)  # use generic function

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
#' are complements (\code{sens = 1 - mirt}) and both features of
#' the decision process (e.g., a diagnostic test).
#'
#' The function \code{comp_sens} is complementary to the conversion function
#' \code{\link{comp_mirt}} and uses the generic function
#' \code{\link{comp_prob_comp}}.
#'
#' @param mirt The decision's miss rate \code{\link{mirt}}.
#'
#' @return The decision's sensitivity \code{\link{sens}}.
#'
#' @examples
#' comp_sens(2)                      # => NA + warning (beyond range)
#' comp_sens(1/3)                    # => 0.6666667
#' comp_sens(comp_prob_comp(0.123))  # => 0.123
#'
#' @family functions computing probabilities
#'
#' @seealso
#' \code{\link{comp_prob_comp}} computes a probability's complement;
#' \code{\link{is_complement}} verifies probability complements;
#' \code{\link{comp_prob}} computes current probability information;
#' \code{\link{prob}} contains current probability information.

comp_sens <- function(mirt) {

  sens <- comp_prob_comp(mirt)  # use generic function

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
#' are complements (\code{fart = 1 - spec}) and both features of
#' the decision process (e.g., a diagnostic test).
#'
#' The function \code{comp_fart} is complementary to the conversion function
#' \code{\link{comp_spec}} and uses the generic function
#' \code{\link{comp_prob_comp}}.
#'
#' @param spec The decision's specificity value \code{\link{spec}}.
#'
#' @return The decision's false alarm rate \code{\link{fart}}.
#'
#' @examples
#' comp_fart(2)                      # => NA + warning (beyond range)
#' comp_fart(1/3)                    # => 0.6666667
#' comp_fart(comp_prob_comp(0.123))  # => 0.123
#'
#' @family functions computing probabilities
#'
#' @seealso
#' \code{\link{comp_prob_comp}} computes a probability's complement;
#' \code{\link{is_complement}} verifies probability complements;
#' \code{\link{comp_prob}} computes current probability information;
#' \code{\link{prob}} contains current probability information.

comp_fart <- function(spec) {

  fart <- comp_prob_comp(spec)  # use generic function

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
#' are complements (\code{spec = 1 - fart}) and both features of
#' the decision process (e.g., a diagnostic test).
#'
#' The function \code{comp_spec} is complementary to the conversion function
#' \code{\link{comp_fart}} and uses the generic function
#' \code{\link{comp_prob_comp}}.
#'
#' @param fart The decision's false alarm rate \code{\link{fart}}.
#'
#' @return The decision's specificity \code{\link{spec}}.
#'
#' @examples
#' comp_spec(2)                      # => NA + warning (beyond range)
#' comp_spec(1/3)                    # => 0.6666667
#' comp_spec(comp_prob_comp(0.123))  # => 0.123
#'
#' @family functions computing probabilities
#'
#' @seealso
#' \code{\link{comp_prob_comp}} computes a probability's complement;
#' \code{\link{is_complement}} verifies probability complements;
#' \code{\link{comp_prob}} computes current probability information;
#' \code{\link{prob}} contains current probability information.

comp_spec <- function(fart) {

  spec <- comp_prob_comp(fart)  # use generic function

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
#' \code{\link{prob}} contains current probability information.

comp_comp_pair <- function(p1 = NA, p2 = NA){

  pair <- c(NULL, NULL) # initialize
  missing <- NA

  if (is.na(p1) & is.na(p2)) {

    warning("One argument (either p1 or p2) is necessary.")
    pair <- c(NA, NA)

  } else if (!is.na(p1) & is.na(p2)) {  # 1: only p1 provided:

    missing <- comp_prob_comp(p1)       #    - compute its comp
    pair <- c(p1, missing)              #    - define pair (leaving input order intact)

  } else if (!is.na(p2) & is.na(p1)) {  # 2: only p2 is provided:

    missing <- comp_prob_comp(p2)       #    - compute spec
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
## (B) Compute derived probabilities (predictive values PVs)
##     from probabilities:
## -----------------------------------------------
## 1. Positive predictive value (PPV) from probabilities:

comp_PPV <- function(prev, sens, spec) {

  PPV <- NA # initialize

  ## ToDo: Add condition
  ## if (is_valid_prob_set(prev, sens, spec, fart)) { ... }

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

## \code{\link{is_extreme_prob_set}} verifies extreme cases;

## -----------------------------------------------
## 2. False discovery/detection rate (FDR = complement of PPV):

## (a) from basic probabilities:

comp_FDR <- function(prev, sens, spec) {

  PPV <- NA # initialize
  FDR <- NA

  ## ToDo: Add condition
  ## if (is_valid_prob_set(prev, sens, spec, fart)) { ... }

  PPV <- comp_PPV(prev, sens, spec)
  FDR <- comp_prob_comp(PPV)  # FDR is the complement of PPV

  return(FDR)
}

## (b) FDR = 1 - PPV:

comp_FDR_PPV <- function(PPV) {

  FDR <- comp_prob_comp(PPV)  # FDR is the complement of PPV

  return(FDR)
}

## -----------------------------------------------
## 3. Negative predictive value (NPV) from probabilities:

comp_NPV <- function(prev, sens, spec) {

  NPV <- NA # initialize

  ## ToDo: Add condition
  ## if (is_valid_prob_set(prev, sens, spec, fart)) { ... }

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

## \code{\link{is_extreme_prob_set}} verifies extreme cases;

## -----------------------------------------------
## 4. False omission rate (FOR = complement of NPV):

comp_FOR <- function(prev, sens, spec) {

  NPV <- NA # initialize
  FOR <- NA

  ## ToDo: Add condition
  ## if (is_valid_prob_set(prev, sens, spec, fart)) { ... }

  NPV <- comp_NPV(prev, sens, spec)
  FOR <- comp_prob_comp(NPV)  # FDR is the complement of NPV

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
