## comp_util.R | riskyR
## 2018 01 22
## -----------------------------------------------
## Generic utility functions:

## -----------------------------------------------
## (1) Verification functions:
## 1. is_prob
## 2. is_perc
## 3. is_sufficient
## 4. is_complement
## 5. is_extreme
## 6. is_valid

#' Verify that a numeric input is a probability (from 0 to 1).
#'
#' \code{is_prob} is a function that checks whether its argument \code{prob}
#' is a probability (i.e., a numeric value in the range from 0 to 1).
#'
#' @param prob A numeric argument (scalar or vector).
#'
#' @return A Boolean value: \code{TRUE} if \code{prob} is a probability, otherwise \code{FALSE}.
#'
#' @examples
#' # ways to succeed:
#' is_prob(1/2)            # => TRUE
#' p.seq <- seq(0, 1, by = .1)
#' is_prob(p.seq)          # => TRUE (for vector)
#'
#' # ways to fail:
#' is_prob(8)              # => FALSE + warning (outside range)
#' is_prob(c(.5, 8))       # => FALSE + warning (for vector)
#' is_prob(NA)             # => FALSE + warning (NA values)
#' is_prob(0/0)            # => FALSE + warning (NA + NaN values)
#' is_prob("Laplace")      # => FALSE + warning (non-numeric values)
#' is_prob(c(8, NA, NaN))  # => FALSE + warning (NA + NaN values)
#'
#' @family verification functions
#'
#' @seealso
#' \code{\link{num}} contains basic numeric variables;
#' \code{\link{init_num}} initializes basic numeric variables;
#' \code{\link{prob}} contains current probability information;
#' \code{\link{comp_prob}} computes current probability information;
#' \code{\link{freq}} contains current frequency information;
#' \code{\link{comp_freq}} computes current frequency information;
#' \code{\link{is_valid}} verifies the validity of probability inputs;
#' \code{\link{as_pc}} displays a probability as a percentage;
#' \code{\link{as_pb}} displays a percentage as probability

is_prob <- function(prob) {

  val <- NA # initialize

  ## many ways to fail:
  if (sum(is.na(prob)) > 0) {
    val <- FALSE
    warning(paste0(prob, " contains NA values. "))
  }
  if (sum(is.nan(prob)) > 0) {
    val <- FALSE
    warning(paste0(prob, " contains NaN values. "))
  }
  else if (sum(!is.numeric(prob)) > 0) {
    val <- FALSE
    warning(paste0(prob, " contains non-numeric values. "))
  }
  else if (sum((prob < 0) | (prob > 1)) > 0) {
    val <- FALSE
    warning(paste0(prob, " contains values beyond the range from 0 to 1. "))
  }
  else { ## one way to succeed:
    val <- TRUE
  }

  return(val)

}

## Checks:
{

  ## ways to succeed:
  # is_prob(1/2)            # => TRUE
  # prob.seq <- seq(0, 1, by = .1)
  # is_prob(prob.seq)       # => TRUE (for vector)

  ## ways to fail:
  # is_prob(8)              # => FALSE + warning (outside range)
  # is_prob(c(.5, 8))       # => FALSE + warning (for vector)
  # is_prob(NA)             # => FALSE + warning (NA values)
  # is_prob(0/0)            # => FALSE + warning (NA + NaN values)
  # is_prob("Laplace")      # => FALSE + warning (non-numeric values)
  # is_prob(c(8, NA, NaN))  # => FALSE + warning (NA + NaN values)
}

## -----------------------------------------------
#' Verify that a numeric input is a percentage (from 0 to 100).
#'
#' \code{is_perc} is a function that checks whether its single argument \code{perc}
#' is a percentage (proportion, i.e., a numeric value in the range from 0 to 100).
#'
#' @param perc A single (typically numeric) argument.
#'
#' @return A Boolean value: \code{TRUE} if \code{perc} is a percentage (proportion),
#' otherwise \code{FALSE}.
#'
#' @examples
#' # ways to succeed:
#' is_perc(2)           # => TRUE, but does NOT return the percentage 2.
#' is_perc(1/2)         # => TRUE, but does NOT return the percentage 0.5.
#' pc.sq <- seq(0, 100, by = 10)
#' is_perc(pc.sq)       # => TRUE (for vector)
#'
#' # ways to fail:
#' is_perc(NA)          # => FALSE + warning (NA values)
#' is_perc(NaN)         # => FALSE + warning (NaN values)
#' is_perc("Bernoulli") # => FALSE + warning (non-numeric values)
#' is_perc(101)         # => FALSE + warning (beyond range)
#'
#' @family verification functions
#'
#' @seealso
#' \code{\link{num}} contains basic numeric variables;
#' \code{\link{init_num}} initializes basic numeric variables;
#' \code{\link{prob}} contains current probability information;
#' \code{\link{comp_prob}} computes current probability information;
#' \code{\link{freq}} contains current frequency information;
#' \code{\link{comp_freq}} computes current frequency information;
#' \code{\link{is_valid}} verifies the validity of probability inputs;
#' \code{\link{as_pc}} displays a probability as a percentage;
#' \code{\link{as_pb}} displays a percentage as probability

is_perc <- function(perc) {

  val <- NA # initialize

  if (sum(is.na(perc)) > 0) {
    val <- FALSE
    warning(paste0(perc, " contains NA values. "))
  }
  else if (sum(is.nan(perc)) > 0) {
    val <- FALSE
    warning(paste0(perc, " contains NaN values. "))
  }
  else if (sum(!is.numeric(perc)) > 0) {
    val <- FALSE
    warning(paste0(perc, " contains non-numeric values. "))
  }
  else if (sum((perc < 0) | (perc > 100)) > 0) {
    val <- FALSE
    warning(paste0(perc, " contains values beyond the range from 0 to 100. "))
  }
  else {
    val <- TRUE
  }

  return(val)

}

## -----------------------------------------------
#' Verify that a numeric input is a freqency (positive integer value).
#'
#' \code{is_freq} is a function that checks whether its single argument \code{freq}
#' is a frequency (i.e., a positive numeric integer value).
#'
#' @param freq A single (typically numeric) argument.
#'
#' @return A Boolean value: \code{TRUE} if \code{freq} is a frequency (positive integer),
#' otherwise \code{FALSE}.
#'
#' @examples
#' # ways to succeed:
#' is_freq(2)    # => TRUE, but does NOT return the frequency 2.
#' is_freq(1:3)  # => TRUE (for vector)
#'
#' # ways to fail:
#' is_freq(-1)    # => FALSE + warning (negative values)
#' is_freq(1:-1)  # => FALSE (for vector) + warning (negative values)
#' is_freq(c(1, 1.5, 2))  # => FALSE (for vector) + warning (non-integer values)
#'
#' ## Note that:
#' is.integer(2)  # => FALSE!
#'
#' @family verification functions
#'
#' @seealso
#' \code{\link{num}} contains basic numeric variables;
#' \code{\link{init_num}} initializes basic numeric variables;
#' \code{\link{prob}} contains current probability information;
#' \code{\link{comp_prob}} computes current probability information;
#' \code{\link{freq}} contains current frequency information;
#' \code{\link{comp_freq}} computes current frequency information;
#' \code{\link{is_valid}} verifies the validity of probability inputs;
#' \code{\link{as_pc}} displays a probability as a percentage;
#' \code{\link{as_pb}} displays a percentage as probability

is_freq <- function(freq) {

  val <- NA # initialize

  if (sum(is.na(freq)) > 0) {
    val <- FALSE
    warning(paste0(freq, " contains NA values. "))
  }
  else if (sum(is.nan(freq)) > 0) {
    val <- FALSE
    warning(paste0(freq, " contains NaN values. "))
  }
  else if (sum(!is.numeric(freq)) > 0) {
    val <- FALSE
    warning(paste0(freq, " contains non-numeric values. "))
  }
  else if (sum((freq < 0)) > 0) {
    val <- FALSE
    warning(paste0(freq, " contains negative values (< 0). "))
  }
  # else if (!all.equal(freq, as.integer(freq))) {
  else if (sum( freq %% 1 != 0) > 0) {
    val <- FALSE
    warning(paste0(freq, " contains non-integer values. "))
  }
  else {
    val <- TRUE
  }

  return(val)

}

## -----------------------------------------------
# Verify that 3 out of 4 arguments are provided:

#' Verify that a sufficient number of probabilities are provided.
#'
#' \code{is_sufficient} is a function that
#' takes 3 or 4 arguments (typically probabilities) as inputs and
#' verifies that they are sufficient to compute the
#' frequencies and conditional probabilities
#' for a population of N individuals.
#'
#' While no alternative input option for frequencies is provided,
#' specification of \code{\link{prev}} and \code{\link{sens}} are always
#' necessary. One additional probability parameter is necessary:
#' If \code{\link{spec}} is provided, its complement \code{\link{fart}} is optional.
#' If \code{\link{fart}} is provided, its complement \code{\link{spec}} is optional.
#'
#' Note that this function does not verify the type, range or
#' consistency of its arguments. See \code{\link{is_prob}} and
#' \code{\link{is_complement}} for this purpose.
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
#' @return A Boolean value: \code{TRUE} if the parameters provided are sufficient,
#' otherwise \code{FALSE}.
#'
#' @examples
#' is_sufficient()          # => FALSE + Warning
#' is_sufficient(prev = 1)  # => FALSE + Warning
#' is_sufficient(prev = 1, sens = 2)  # => FALSE + Warning
#'
#' is_sufficient(prev = 1, sens = 2, spec = 3)  # => TRUE, but is_prob would be FALSE for 2 and 3
#' is_sufficient(prev = 1, sens = 2, fart = 4)  # => TRUE, but is_prob would be FALSE for 2 and 4
#'
#' is_sufficient(prev = 1, sens = 2, spec = 3, fart = 4)  # => TRUE, but is_prob would be FALSE for 2, 3, and 4
#'
#' @family verification functions
#' @seealso
#' \code{\link{num}} contains basic numeric variables;
#' \code{\link{init_num}} initializes basic numeric variables;
#' \code{\link{prob}} contains current probability information;
#' \code{\link{comp_prob}} computes current probability information;
#' \code{\link{freq}} contains current frequency information;
#' \code{\link{comp_freq}} computes current frequency information;
#' \code{\link{is_valid}} verifies the validity of probability inputs;
#' \code{\link{as_pc}} displays a probability as a percentage;
#' \code{\link{as_pb}} displays a percentage as probability

is_sufficient <- function(prev, sens, spec = NA, fart = NA) {

  val <- FALSE # initialize

  ## Many ways to fail:
  if (is.na(prev)) {
    warning("A prevalence value (prev) is missing but necessary.")}
  else if (is.na(sens)) {
    warning("A sensitivity value (sens) is missing but necessary.")}
  else if (is.na(spec) & is.na(fart)) {
    warning("Either a specificity value (spec) OR a false alarm rate (fart) is necessary.")
  } else {  ## one way to succeed:
    val <- TRUE
  }

  return(val)

}

## Checks:
{
  # is_sufficient()          # => FALSE + Warning
  # is_sufficient(prev = 1)  # => FALSE + Warning
  # is_sufficient(prev = 1, sens = 2)  # => FALSE + Warning
  # is_sufficient(prev = 1, sens = 2, spec = 3)  # => TRUE, but is_prob would be FALSE for 2 and 3
  # is_sufficient(prev = 1, sens = 2, fart = 4)  # => TRUE, but is_prob would be FALSE for 2 and 4
  # is_sufficient(prev = 1, sens = 2, spec = 3, fart = 4)  # => TRUE, but is_prob would be FALSE for 2, 3, and 4
}

## -----------------------------------------------
# Verify that 2 arguments are complements of each other:

#' Verify that two arguments are numeric complements.
#'
#' \code{is_complement} is a function that
#' takes 2 numeric arguments (probabilities) as inputs and
#' verifies that they are complements (i.e., add up to 1).
#'
#' Both \code{spec} and \code{fart} are necessary arguments.
#'
#' The argument \code{tol} is optional (with a default value of .01)
#' Complements differing by less than this
#' value are still considered to be complements.
#'
#' This function does not verify the type, range or sufficiency
#' of the inputs provided. See \code{\link{is_prob}} and
#' \code{\link{is_sufficient}} for this purpose.
#'
#' @param spec The decision's specificity value \code{\link{spec}}
#' (i.e., the conditional probability
#' of a negative decision provided that the condition is FALSE).
#' \code{spec} is optional when is complement \code{fart} is provided.
#'
#' @param fart The decision's false alarm rate \code{\link{fart}}
#' (i.e., the conditional probability
#' of a positive decision provided that the condition is FALSE).
#' \code{fart} is optional when its complement \code{spec} is provided.
#'
#' @param tol A numeric tolerance value.
#'
#' @return A Boolean value: \code{TRUE} if the arguments are complements,
#' otherwise \code{FALSE}.
#'
#' @examples
#' is_complement(0, 0)              # => FALSE + Warning that difference exceeds tolerance
#'
#' is_complement(0, 1)              # => TRUE
#' is_complement(1/3, 2/3)          # => TRUE
#' is_complement(.33, .66)          # => TRUE
#'
#' is_complement(.3, .6)            # => FALSE + Warning that difference exceeds tolerance
#' is_complement(.3, .6, tol = .1)  # => TRUE (due to increased tolerance)
#'
#' @family verification functions
#'
#' @seealso
#' \code{\link{comp_prob_comp}} computes a probability's complement;
#' \code{\link{comp_comp_pair}} computes pairs of complements;
#' \code{\link{num}} contains basic numeric variables;
#' \code{\link{init_num}} initializes basic numeric variables;
#' \code{\link{prob}} contains current probability information;
#' \code{\link{comp_prob}} computes current probability information;
#' \code{\link{freq}} contains current frequency information;
#' \code{\link{comp_freq}} computes current frequency information;
#' \code{\link{is_valid}} verifies the validity of probability inputs;
#' \code{\link{as_pc}} displays a probability as a percentage;
#' \code{\link{as_pb}} displays a percentage as probability

is_complement <- function(spec, fart, tol = .01) {

  val <- NA   # initialize

  # if (isTRUE(all.equal(spec, (1 - fart), tolerance = tol))) {
  #   val <- TRUE
  # } else {
  #   warning("Specificity (spec) and false alarm rate (fart) are not complements (in tolerated range).")
  #   warning(paste0("spec = ", spec, "; 1 - fart = ", 1 - fart, ", difference = ", abs(spec - (1 - fart))))
  #   val <- FALSE
  # }

  cur.spec <- 1 - fart  # compute spec as complement of fart
  eps <- 10^-9          # some very small value

  if (abs(spec - cur.spec) > (tol + eps)) {
    warning("Specificity (spec) and false alarm rate (fart) are not complements (in tolerated range).")
    # warning(paste0("spec = ", spec, "; 1 - fart = ", 1 - fart, ", difference = ", abs(spec - (1 - fart))))
    val <- FALSE
  } else {
    val <- TRUE
  }

  return(val)

}

## Checks:
{
  # is_complement(0, 0)      # => FALSE + Warning that difference exceeds tolerance.
  # is_complement(0, 1)      # => TRUE
  # is_complement(1/3, 2/3)  # => TRUE
  # is_complement(.33, .66)  # => TRUE
  # is_complement(.3, .6)    # => FALSE + Warning that difference exceeds tolerance.
  # is_complement(.3, .6, tol = .10) # => TRUE (due to increased tolerance)
}

## -----------------------------------------------
## (E) Beware of extreme cases:
##     Verify if the current set of (sufficient) probabilities
##     describe an extreme case:

#' Verify that probabilities provided describe an extreme case.
#'
#' \code{is_extreme} verifies that the current combination
#' of probabilities provided (i.e., \code{\link{prev}}
#' and \code{\link{sens}}, and \code{\link{spec}} or
#' \code{\link{fart}}) describe an extreme case.
#'
#' If \code{TRUE}, a warning message describing the
#' nature of the extreme case is printed to allow
#' anticipating peculiar effects (e.g., that
#' PPV or NPV values cannot be computed or are \code{NaN}).
#'
#' This function does not verify the type, range, sufficiency,
#' or consistency of its arguments. See \code{\link{is_prob}},
#' \code{\link{is_sufficient}}, and \code{\link{is_complement}}
#' for this purpose.
#'
#' @param prev The condition's prevalence value \code{\link{prev}}
#' (i.e., the probability of condition being \code{TRUE}).
#' @param sens The decision's sensitivity value \code{\link{sens}}
#' (i.e., the conditional probability of a positive decision
#' provided that the condition is \code{TRUE}).
#' @param spec The decision's specificity value \code{\link{spec}}
#' (i.e., the conditional probability of a negative decision
#' provided that the condition is \code{FALSE}).
#' \code{spec} is optional when is complement \code{fart} is provided.
#' @param fart The decision's false alarm rate \code{\link{fart}}
#' (i.e., the conditional probability of a positive decision
#' provided that the condition is \code{FALSE}).
#' \code{fart} is optional when its complement \code{spec} is provided.
#'
#' @return A Boolean value: \code{TRUE} if an extreme case is identified,
#' otherwise \code{FALSE}.
#'
#' @examples
#' # Identify 4 extreme cases (+2 variants):
#' is_extreme(1, 1)           # => TRUE + warning: N true positives
#' plot_tree(1, 1, N = 100)   # => illustrates this case
#'
#' is_extreme(1, 0)           # => TRUE + warning: N false negatives
#' plot_tree(1, 0, N = 100)   # => illustrates this case
#'
#' sens <- .50
#' is_extreme(0, sens, 0)              # => TRUE + warning: N false positives
#' plot_tree(0, sens, 0, N = 100)      # => illustrates this case
#' # Variant:
#' is_extreme(0, sens, NA, 1)          # => TRUE + warning: N false positives
#' plot_tree(0, sens, NA, 1, N = 100)  # => illustrates this case
#'
#' is_extreme(0, sens, 1)              # => TRUE + warning: N true negatives
#' plot_tree(0, sens, 1, N = 100)      # => illustrates this case
#' # Variant:
#' is_extreme(0, sens, NA, 0)          # => TRUE + warning: N true negatives
#' plot_tree(0, sens, NA, 0, N = 100)  # => illustrates this case
#'
#' @family verification functions
#'
#' @seealso
#' \code{\link{is_valid}} uses \code{is_extreme}
#' to verify the validity of probability inputs;
#' \code{\link{num}} contains basic numeric variables;
#' \code{\link{init_num}} initializes basic numeric variables;
#' \code{\link{prob}} contains current probability information;
#' \code{\link{comp_prob}} computes current probability information;
#' \code{\link{freq}} contains current frequency information;
#' \code{\link{comp_freq}} computes current frequency information;
#' \code{\link{as_pc}} displays a probability as a percentage;
#' \code{\link{as_pb}} displays a percentage as probability

is_extreme <- function(prev, sens, spec = NA, fart = NA) {

  ## (1) Initialize:
  val <- NA

  ## (2) Compute missing fart or spec (4th argument) value (if applicable):
  cur.spec.fart <- comp_comp_pair(spec, fart)
  spec <- cur.spec.fart[1] # 1st argument
  fart <- cur.spec.fart[2] # 2nd argument

  ## (3) Check cases:
  if ((prev == 1) & (sens == 1)) {         # 1. prev and sens are both perfect:

    warning("Extreme case (prev = 1 & sens = 1):\n  N true positives; no cond.false or dec.false cases; NPV = NaN.")
    val <- TRUE

  } else if ((prev == 1) & (sens == 0)) {  # 2. prev perfect and sens zero:

    warning("Extreme case (prev = 1 & sens = 0):\n  N false negatives; no cond.false or dec.true cases; PPV = NaN.")
    val <- TRUE

  } else if ((prev == 0) & (spec == 0)) {  # 3a. prev and spec are both zero:

    warning("Extreme case (prev = 0 & spec = 0):\n  N false positives; no cond.true or dec.true cases; PPV = NaN.")
    val <- TRUE

  } else if ((prev == 0) & (fart == 1)) {  # 3b. prev zero and fart perfect (i.e., spec zero):

    warning("Extreme case (prev = 0 & fart = 1):\n  N false positives; no cond.true or dec.true cases; PPV = NaN.")
    val <- TRUE

  } else if ((prev == 0) & (spec == 1)) {  # 4a. prev zero and spec perfect:

    warning("Extreme case (prev = 0 & spec = 1):\n  N true negatives; no cond.true or dec.false cases; NPV = NaN.")
    val <- TRUE

  } else if ((prev == 0) & (fart == 0)) {  # 4b. prev zero and fart zero (i.e., spec perfect):

    warning("Extreme case (prev = 0 & fart = 0):\n  N true negatives; no cond.true or dec.false cases; NPV = NaN.")
    val <- TRUE

  } else {  # not an extreme case:

    val <- FALSE

  }

  ## (4) Return value:
  return(val)

}

## Check:
{
  # # Identify 4 extreme cases (+2 variants):
  # is_extreme(1, 1)           # => TRUE + warning: N true positives
  # plot_tree(1, 1, N = 100)   # => illustrates this case
  #
  # is_extreme(1, 0)           # => TRUE + warning: N false negatives
  # plot_tree(1, 0, N = 100)   # => illustrates this case
  #
  # sens <- .50
  # is_extreme(0, sens, 0)              # => TRUE + warning: N false positives
  # plot_tree(0, sens, 0, N = 100)      # => illustrates this case
  # # Variant:
  # is_extreme(0, sens, NA, 1)          # => TRUE + warning: N false positives
  # plot_tree(0, sens, NA, 1, N = 100)  # => illustrates this case
  #
  # is_extreme(0, sens, 1)              # => TRUE + warning: N true negatives
  # plot_tree(0, sens, 1, N = 100)      # => illustrates this case
  # # Variant:
  # is_extreme(0, sens, NA, 0)          # => TRUE + warning: N true negatives
  # plot_tree(0, sens, NA, 0, N = 100)  # => illustrates this case
}


## -----------------------------------------------
# Verify that 3 or 4 probabilities are valid inputs:

#' Verify that basic probabilities are valid inputs.
#'
#' \code{is_valid} is a function that takes
#' 3 or 4 numeric arguments as inputs and
#' verifies that they can be interpreted as a
#' valid quadruple of probabilities.
#'
#' \code{is_valid} is a wrapper function that combines
#' \code{\link{is_prob}}, \code{\link{is_sufficient}},
#' and \code{\link{is_complement}} in one function.
#'
#' Note that \code{is_valid} only verifies the validity of inputs,
#' but does not compute or return numeric variables.
#' Use \code{\link{is_extreme}} to verify sets of probabilities
#' that describe extreme cases and \code{\link{init_num}}
#' for initializing basic parameters.
#'
#' Both \code{prev} and \code{sens} and
#' either \code{spec} or \code{fart} are necessary arguments.
#' The argument \code{tol} is optional (with a default value of .01)
#' and used as the tolerance value of \code{\link{is_complement}}.
#'
#' @param prev The condition's prevalence value \code{\link{prev}}
#' (i.e., the probability of condition being TRUE).
#' @param sens The decision's sensitivity value \code{\link{sens}}
#' (i.e., the conditional probability of a positive decision
#' provided that the condition is TRUE).
#' @param spec The decision's specificity value \code{\link{spec}}
#' (i.e., the conditional probability
#' of a negative decision provided that the condition is FALSE).
#' \code{spec} is optional when is complement \code{fart} is provided.
#' @param fart The decision's false alarm rate \code{\link{fart}}
#' (i.e., the conditional probability
#' of a positive decision provided that the condition is FALSE).
#' \code{fart} is optional when its complement \code{spec} is provided.
#'
#' @param tol A numeric tolerance value used by \code{\link{is_complement}}.
#'
#' @return A Boolean value: \code{TRUE} if the parameters provided are valid,
#' otherwise \code{FALSE}.
#'
#' @examples
#' # ways to succeed:
#' is_valid(.1, .9, .8, .2)      # => TRUE
#' is_valid(.1, .9, NA, .2)      # => TRUE
#' is_valid(.1, .9, .8, NA)      # => TRUE
#' is_valid(.1, .9, .8, .3, .1)  # => TRUE
#'
#' # watch out for:
#' is_valid(1, 1, 1, NA, 0)  # => TRUE, but NO warning about extreme case!
#' is_valid(1, 1, 1, 0)      # => TRUE, but NO warning about extreme case!
#' is_valid(1, 1, 0, 1)      # => TRUE, but NO warning about extreme case!
#' is_valid(1, 1, 1, NA)     # => TRUE, but NO warning about extreme case!
#' is_valid(1, 1, NA, 1)     # => TRUE, but NO warning about extreme case!
#'
#' # ways to fail:
#' is_valid(1, 1, 1, 1)   # => FALSE + warning that is_complement fails
#' is_valid(1, 1, NA, NA) # => FALSE + warning that is_sufficient fails
#' is_valid(8, 1, 1, 0)   # => FALSE + warning that is_prob(prev) fails
#' is_valid(1, 8, 1, 0)   # => FALSE + warning that is_prob(sens) fails
#' is_valid(1, 1, 8, NA)  # => FALSE + warning that is_prob(spec) fails
#' is_valid(1, 1, NA, 8)  # => FALSE + warning that is_prob(fart) fails
#'
#' @family verification functions
#'
#' @seealso
#' \code{\link{num}} contains basic numeric variables;
#' \code{\link{init_num}} initializes basic numeric variables;
#' \code{\link{prob}} contains current probability information;
#' \code{\link{comp_prob}} computes current probability information;
#' \code{\link{freq}} contains current frequency information;
#' \code{\link{comp_freq}} computes current frequency information;
#' \code{\link{is_valid}} verifies the validity of probability inputs;
#' \code{\link{as_pc}} displays a probability as a percentage;
#' \code{\link{as_pb}} displays a percentage as probability.

is_valid <- function(prev, sens, spec = NA, fart = NA, tol = .01) {

  val <- NA  # initialize

  ## many ways to fail:
  if (!is_prob(prev))      { val <- FALSE }                          # 1. prev is a probability

  else if (!is_prob(sens)) { val <- FALSE }                          # 2. sens is a probability

  else if (!is_sufficient(prev, sens, spec, fart)) { val <- FALSE }  # 3. sufficient (3 of 4 parameters)

  else if (is.na(fart)) {                                            # 4a. if only spec is provided:
    if (!is_prob(spec)) { val <- FALSE } else { val <- TRUE }        #     spec is a probability
  }

  else if (is.na(spec)) {                                            # 4b. if only fart is provided:
    if (!is_prob(fart)) { val <- FALSE } else { val <- TRUE }        #     fart is a probability
  }

  else if (!is.na(spec) & !is.na(fart)) {                            # 5.  if both spec + fart are provided:

    if (!is_complement(spec, fart, tol)) {                           # 5a. spec and fart are NOT complements (within tol)
      val <- FALSE
    } else {                                                         # 5b. spec and fart ARE complements (within tol)
      val <- TRUE
    }
  }

  ## Issue a warning if probabilities describe an extreme case:
  # is_extreme(prev, sens, spec, fart)  # prints a warning if TRUE # NOT ALWAYS done (to avoid multiple messages)!

  return(val)
}

## Check:
{
  ## ways to succeed:
  # is_valid(.1, .9, .8, .2)      # => TRUE
  # is_valid(.1, .9, NA, .2)      # => TRUE
  # is_valid(.1, .9, .8, NA)      # => TRUE
  # is_valid(.1, .9, .8, .3, .1)  # => TRUE

  ## watch out for:
  # is_valid(1, 1, 1, NA, 0)  # => TRUE, but NO warning about extreme case!
  # is_valid(1, 1, 1, 0)      # => TRUE, but NO warning about extreme case!
  # is_valid(1, 1, 0, 1)      # => TRUE, but NO warning about extreme case!
  # is_valid(1, 1, 1, NA)     # => TRUE, but NO warning about extreme case!
  # is_valid(1, 1, NA, 1)     # => TRUE, but NO warning about extreme case!

  ## ways to fail:
  # is_valid(1, 1, 1, 1)   # => FALSE + warning that is_complement fails
  # is_valid(1, 1, NA, NA) # => FALSE + warning that is_sufficient fails
  # is_valid(8, 1, 1, 0)   # => FALSE + warning that is_prob(prev) fails
  # is_valid(1, 8, 1, 0)   # => FALSE + warning that is_prob(sens) fails
  # is_valid(1, 1, 8, NA)  # => FALSE + warning that is_prob(spec) fails
  # is_valid(1, 1, NA, 8)  # => FALSE + warning that is_prob(fart) fails
}

## -----------------------------------------------
## Toggle between showing probabilities and percentages:

#' Display a probability as a (rounded) percentage.
#'
#' \code{as_pc} is a function that displays a probability \code{prob}
#' as a percentage (rounded to \code{n.digits} decimals).
#'
#' \code{as_pc} and its complement function \code{\link{as_pb}} allow
#' toggling the display of numeric values between percentages and probabilities.
#'
#' @param prob A probability (as a scalar or vector of numeric values from 0 to 1).
#' @param n.digits Number of decimal places to which percentage is rounded.
#' Default: \code{n.digits = 2}.
#'
#' @return A percentage (as a numeric value).
#'
#' @examples
#' as_pc(.50)                # =>  50
#' as_pc(1/3)                # =>  33.33
#' as_pc(1/3, n.digits = 0)  # =>  33
#'
#' as_pc(pi)                 # => 314.16 + Warning that prob is not in range.
#' as_pc(as_pb(12.3))        # =>  12.3
#'
#' prob.seq <- seq(0, 1, by = 1/10)
#' perc.seq <- seq(0, 100, by = 10)
#'
#' as_pc(prob.seq)  # =>   0  10  20  30  40  50  60  70  80  90 100
#' as_pb(perc.seq)  # => 0.0 0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9 1.0
#'
#' perc.seq == as_pc(as_pb(perc.seq))  # all TRUE
#' prob.seq == as_pb(as_pc(prob.seq))  # some FALSE due to rounding errors!
#' round(prob.seq, 4) == as_pb(as_pc(prob.seq))  # all TRUE (both rounded to 4 decimals)
#'
#' @family utility functions
#'
#' @seealso
#' \code{\link{is_prob}} verifies a probability;
#' \code{\link{is_perc}} verifies a percentage;
#' \code{\link{is_valid}} verifies the validity of probability inputs;
#' \code{\link{num}} contains basic numeric variables;
#' \code{\link{init_num}} initializes basic numeric variables;
#' \code{\link{prob}} contains current probability information;
#' \code{\link{comp_prob}} computes current probability information;
#' \code{\link{freq}} contains current frequency information;
#' \code{\link{comp_freq}} computes current frequency information;
#' \code{\link{comp_prob_comp}} computes a probability's complement;
#' \code{\link{comp_comp_pair}} computes pairs of complements.

## Probability as percentage (2 decimals):

as_pc <- function(prob, n.digits = 2) {

  perc <- NA # initialize

  if (is_prob(prob)) {

    perc <- round(prob * 100, n.digits) # compute

  } else {
    warning("Probability (prob) is not in range 0 to 1.")
    perc <- round(prob * 100, n.digits) # still compute
  }

  return(perc)
}

# Check:
{
  # as_pc(1/2)                # =>  50
  # as_pc(1/3)                # =>  33.33
  # as_pc(1/3, n.digits = 0)  # =>  33
  # as_pc(pi)                 # => 314.16 + Warning that prob is not in range.
  # as_pc(as_pb(12.3))        # =>  12.3
}

## -----------------------------------------------
## Percentage as probability (4 decimals):

#' Display a percentage as a (rounded) probability.
#'
#' \code{as_pb} is a function that displays a percentage \code{perc}
#' as a probability (rounded to \code{n.digits} decimals).
#'
#' \code{as_pb} and its complement function \code{\link{as_pc}} allow
#' toggling the display of numeric values between percentages and probabilities.
#'
#' @param perc A percentage (as a scalar or vector of numeric values from 0 to 100).
#' @param n.digits Number of decimal places to which percentage is rounded.
#' Default: \code{n.digits = 4}.
#'
#' @return A probability (as a numeric value).
#'
#' @examples
#' as_pb(1/3)          # => 0.0033
#' as_pb(as_pc(2/3))   # => 0.6667 (rounded to 4 decimals)
#'
#' prob.seq <- seq(0, 1, by = 1/10)
#' perc.seq <- seq(0, 100, by = 10)
#'
#' as_pc(prob.seq)  # =>   0  10  20  30  40  50  60  70  80  90 100
#' as_pb(perc.seq)  # => 0.0 0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9 1.0
#'
#' perc.seq == as_pc(as_pb(perc.seq))  # all TRUE
#' prob.seq == as_pb(as_pc(prob.seq))  # some FALSE due to rounding errors!
#' round(prob.seq, 4) == as_pb(as_pc(prob.seq))  # all TRUE (both rounded to 4 decimals)
#'
#' @family utility functions
#'
#' @seealso
#' \code{\link{is_perc}} verifies a percentage;
#' \code{\link{is_prob}} verifies a probability;
#' \code{\link{is_valid}} verifies the validity of probability inputs;
#' \code{\link{num}} contains basic numeric variables;
#' \code{\link{init_num}} initializes basic numeric variables;
#' \code{\link{prob}} contains current probability information;
#' \code{\link{comp_prob}} computes current probability information;
#' \code{\link{freq}} contains current frequency information;
#' \code{\link{comp_freq}} computes current frequency information;
#' \code{\link{comp_prob_comp}} computes a probability's complement;
#' \code{\link{comp_comp_pair}} computes pairs of complements.

as_pb <- function(perc, n.digits = 4) {

  prob <- NA # initialize

  if (is_perc(perc)) {

    prob <- round(perc/100, n.digits) # compute

  } else {
    warning("Percentage (perc) is not in range 0 to 100.")
    prob <- round(perc/100, n.digits) # still compute
  }

  return(prob)
}

## Check:
{
  # as_pb(1/3)          # => 0.0033
  # as_pb(as_pc(2/3))   # => 0.6667 (rounded to 4 decimals)
  #
  # prob.seq <- seq(0, 1, by = 1/10)
  # perc.seq <- seq(0, 100, by = 10)
  #
  # as_pc(prob.seq)  # =>   0  10  20  30  40  50  60  70  80  90 100
  # as_pb(perc.seq)  # => 0.0 0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9 1.0
  #
  # perc.seq == as_pc(as_pb(perc.seq)) # all TRUE
  # prob.seq == as_pb(as_pc(prob.seq)) # some FALSE due to rounding errors!
  # round(prob.seq, 4) == as_pb(as_pc(prob.seq)) # all TRUE (as both rounded to 4 decimals)
}

## -----------------------------------------------
## Reformat the plotting area to allow placing legend outside of a plot:

add_legend <- function(...) {
  ## Reformat the plotting area to allow placing legend outside of a plot
  ## Source: https://stackoverflow.com/questions/3932038/plot-a-legend-outside-of-the-plotting-area-in-base-graphics

  opar <- par(fig = c(0, 1, 0, 1),
              oma = c(0, 0, 0, 0),
              mar = c(0, 0, 0, 0),
              new = TRUE)

  on.exit(par(opar))

  plot(0, 0, type = 'n', bty = 'n', xaxt = 'n', yaxt = 'n')

  legend(...)
}

## -----------------------------------------------
## Making colors transparent:

makeTransparent = function(..., alpha = .50) {

  if (alpha < 0 | alpha > 1) {
    stop("The value for alpha must be between 0 and 1")
  }

  alpha <- floor(255 * alpha)
  newColor <- col2rgb(col = unlist(list(...)), alpha = FALSE)

  .makeTransparent <- function(col, alpha) {
    rgb(red = col[1], green = col[2], blue = col[3],
        alpha = alpha, maxColorValue = 255)
  }

  newColor <- apply(newColor, 2, .makeTransparent, alpha = alpha)

  return(newColor)

}

## Note also:
# adjustcolor(col = "green", alpha.f = .50)

## -----------------------------------------------
## (+) ToDo:

## (e+) ToDo: Generalize is_perfect to
##      is_extreme to incorporate other extreme cases:
##      [see (e1) and (e+) above]:
##
## - prev = 0, spec = 0: only fa cases
## - prev = 0, spec = 1: only cr cases

## -----------------------------------------------
## eof.
