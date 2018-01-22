## comp_prob_prob.R | riskyR
## 2018 01 22
## -----------------------------------------------
## Compute probabilities from probabilities
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
## (B) Compute derived probabilities (predictive values PVs)
##     from probabilities:
## -----------------------------------------------
## 1. Positive predictive value (PPV) from probabilities:

comp_PPV <- function(prev, sens, spec) {

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

comp_FDR <- function(prev, sens, spec) {

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

comp_NPV <- function(prev, sens, spec) {

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

comp_FOR <- function(prev, sens, spec) {

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
