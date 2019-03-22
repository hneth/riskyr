## comp_util.R | riskyr
## 2018 03 22
## Generic utility functions:
## -----------------------------------------------

## (A) Verification functions
## (B) Beware of extreme cases
## (C) Conversion functions
## (D) Color and plotting functions (mostly moved to plot_util.R)
## (E) Text functions

## (A) Verification functions: ----------

#  1. is_prob               (exported)
#  2. is_perc               (exported)
#  3. is_freq               (exported)
#  4. is_suff_prob_set      (exported)
#  +. is_suff_freq_set      (ToDo)
#  5. is_complement         (exported)
#  6. is_prob_range         (NOT exported)
#  7. is_extreme_prob_set   (exported)
#  8. is_valid_prob_pair    (exported)
#  9. is_valid_prob_set     (exported)
# 10. is_valid_prob_triple  [exported, but deprecated]


## is_prob: Verify that input is a probability ----------

#' Verify that input is a probability (numeric value from 0 to 1).
#'
#' \code{is_prob} is a function that checks whether its argument \code{prob}
#' (a scalar or a vector) is a probability
#' (i.e., a numeric value in the range from 0 to 1).
#'
#' @param prob A numeric argument (scalar or vector) that is to be checked.
#'
#' @param NA_warn Boolean value determining whether a warning is shown
#' for \code{NA} values.
#' Default: \code{NA_warn = FALSE}.
#'
#' @return A Boolean value:
#' \code{TRUE} if \code{prob} is a probability,
#' otherwise \code{FALSE}.
#'
#' @examples
#' is_prob(1/2)                  # TRUE
#' is_prob(2)                    # FALSE
#'
#' # vectors:
#' p_seq <- seq(0, 1, by = .1)   # Vector of probabilities
#' is_prob(p_seq)                # TRUE (as scalar, not: TRUE TRUE etc.)
#' is_prob(c(.1, 2, .9))         # FALSE (as scalar, not: TRUE FALSE etc.)
#'
#' ## watch out for:
#' # is_prob(NA)                   # => FALSE + NO warning!
#  # is_prob(NA, NA_warn = TRUE)   # => FALSE + warning (NA values)
#' # is_prob(0/0)                  # => FALSE + NO warning (NA + NaN values)
#' # is_prob(0/0, NA_warn = TRUE)  # => FALSE + warning (NA values)
#'
#' ## ways to fail:
#' # is_prob(8, NA_warn = TRUE)         # => FALSE + warning (outside range element)
#' # is_prob(c(.5, 8), NA_warn = TRUE)  # => FALSE + warning (outside range vector element)
#' # is_prob("Laplace", NA_warn = TRUE) # => FALSE + warning (non-numeric values)
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
#' \code{\link{is_valid_prob_set}} verifies the validity of probability inputs;
#' \code{\link{as_pc}} displays a probability as a percentage;
#' \code{\link{as_pb}} displays a percentage as probability.
#'
#' @export

is_prob <- function(prob, NA_warn = FALSE) {

  val <- NA  # initialize

  ## many ways to fail:
  if (any(is.na(prob))) {

    val <- FALSE

    if (NA_warn) {
      warning(paste0(prob, " contains NA values. "))
    }
  }

  # else if (any(is.nan(prob))) {  ## NOTE: is.nan not implemented for lists!
  #
  #   val <- FALSE
  #
  #   if (NA_warn) {
  #     warning(paste0(prob, " contains NaN values. "))
  #   }
  # }

  else if (any(!is.numeric(prob))) {

    val <- FALSE

    if (NA_warn) {
      warning(paste0(prob, " contains non-numeric values. "))
    }
  }

  else if (any(prob < 0) || any(prob > 1)) {

    val <- FALSE

    if (NA_warn) {
      warning(paste0(prob, " contains values beyond the range from 0 to 1. "))
    }
  }

  else {  ## one way to succeed:

    val <- TRUE
  }

  return(val)

}

## Checks:
# ## ways to succeed:
# is_prob(1/2)                  # => TRUE
# p.seq <- seq(0, 1, by = .1)   # Define vector of probabilities.
# is_prob(p.seq)                # => TRUE (for vector)
#
# ## watch out for:
# is_prob(NA)                   # => FALSE + NO warning!
# is_prob(NA, NA_warn = TRUE)   # => FALSE + warning (NA values)
# is_prob(0/0)                  # => FALSE + NO warning (NA + NaN values)
# is_prob(0/0, NA_warn = TRUE)  # => FALSE + warning (NA values)
#
# ## ways to fail:
# is_prob(8, NA_warn = TRUE)         # => FALSE + warning (outside range element)
# is_prob(c(.5, 8), NA_warn = TRUE)  # => FALSE + warning (outside range vector element)
# is_prob("Laplace", NA_warn = TRUE) # => FALSE + warning (non-numeric values)


## is_perc: Verify that input is a percentage --------------------

#' Verify that input is a percentage (numeric value from 0 to 100).
#'
#' \code{is_perc} is a function that checks whether its single argument \code{perc}
#' is a percentage (proportion, i.e., a numeric value in the range from 0 to 100).
#'
#' @param perc A single (typically numeric) argument.
#'
#' @return A Boolean value:
#' \code{TRUE} if \code{perc} is a percentage (proportion),
#' otherwise \code{FALSE}.
#'
#' @examples
#' # ways to succeed:
#' is_perc(2)    # => TRUE, but does NOT return the percentage 2.
#' is_perc(1/2)  # => TRUE, but does NOT return the percentage 0.5.
#'
#' ## note:
#' # pc_sq <- seq(0, 100, by = 10)
#' # is_perc(pc_sq)       # => TRUE (for vector)
#'
#' ## ways to fail:
#' # is_perc(NA)          # => FALSE + warning (NA values)
#' # is_perc(NaN)         # => FALSE + warning (NaN values)
#' # is_perc("Bernoulli") # => FALSE + warning (non-numeric values)
#' # is_perc(101)         # => FALSE + warning (beyond range)
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
#' \code{\link{is_valid_prob_set}} verifies the validity of probability inputs;
#' \code{\link{as_pc}} displays a probability as a percentage;
#' \code{\link{as_pb}} displays a percentage as probability.
#'
#' @export

is_perc <- function(perc) {

  val <- NA  # initialize

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

  else if (sum((perc < 0) || (perc > 100)) > 0) {

    val <- FALSE

    warning(paste0(perc, " contains values beyond the range from 0 to 100. "))
  }

  else {  # one way to succeed:

    val <- TRUE
  }

  return(val)

}

## is_freq: Verify that input is a frequency -----------

#' Verify that input is a frequency (positive integer value).
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
#' is_freq(0:3)  # => TRUE (for vector)
#'
#' ## ways to fail:
#' # is_freq(-1)            # => FALSE + warning (negative values)
#' # is_freq(1:-1)          # => FALSE (for vector) + warning (negative values)
#' # is_freq(c(1, 1.5, 2))  # => FALSE (for vector) + warning (non-integer values)
#'
#' ## note:
#' # is.integer(2)          # => FALSE!
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
#' \code{\link{is_valid_prob_set}} verifies the validity of probability inputs;
#' \code{\link{as_pc}} displays a probability as a percentage;
#' \code{\link{as_pb}} displays a percentage as probability.
#'
#' @export

is_freq <- function(freq) {

  val <- NA  # initialize

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

  else {  # one way to succeed:

    val <- TRUE
  }

  return(val)

}


## is_suff_prob_set: Verify that sufficient set of probabilities is provided ----------

#' Verify a sufficient set of probability inputs.
#'
#' \code{is_suff_prob_set} is a function that
#' takes 3 to 5 probabilities as inputs and
#' verifies that they are sufficient to compute
#' all derived probabilities and combined frequencies
#' for a population of \code{\link{N}} individuals.
#'
#' While no alternative input option for frequencies is provided,
#' specification of the essential probability \code{\link{prev}}
#' is always necessary.
#'
#' However, for 2 other essential probabilities there is a choice:
#'
#' \enumerate{
#'
#' \item either \code{\link{sens}} or \code{\link{mirt}} is necessary
#' (as both are complements).
#'
#' \item either \code{\link{spec}} or \code{\link{fart}} is necessary
#' (as both are complements).
#'
#' }
#'
#' \code{is_suff_prob_set} does not verify the type, range, or
#' consistency of its arguments. See \code{\link{is_prob}} and
#' \code{\link{is_complement}} for this purpose.
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
#' @return A Boolean value:
#' \code{TRUE} if the probabilities provided are sufficient,
#' otherwise \code{FALSE}.
#'
#' @examples
#' # ways to work:
#' is_suff_prob_set(prev = 1, sens = 1, spec = 1)  # => TRUE
#' is_suff_prob_set(prev = 1, mirt = 1, spec = 1)  # => TRUE
#' is_suff_prob_set(prev = 1, sens = 1, fart = 1)  # => TRUE
#' is_suff_prob_set(prev = 1, mirt = 1, fart = 1)  # => TRUE
#'
#' # watch out for:
#' is_suff_prob_set(prev = 1, sens = 2, spec = 3)  # => TRUE, but is_prob is FALSE
#' is_suff_prob_set(prev = 1, mirt = 2, fart = 4)  # => TRUE, but is_prob is FALSE
#' is_suff_prob_set(prev = 1, sens = 2, spec = 3, fart = 4)  # => TRUE, but is_prob is FALSE
#'
#' ## ways to fail:
#' # is_suff_prob_set()                    # => FALSE + warning (prev missing)
#' # is_suff_prob_set(prev = 1)            # => FALSE + warning (sens or mirt missing)
#' # is_suff_prob_set(prev = 1, sens = 1)  # => FALSE + warning (spec or fart missing)
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
#' \code{\link{is_valid_prob_set}} verifies the validity of probability inputs;
#' \code{\link{as_pc}} displays a probability as a percentage;
#' \code{\link{as_pb}} displays a percentage as probability.
#'
#' @export

is_suff_prob_set <- function(prev,
                             sens = NA, mirt = NA,
                             spec = NA, fart = NA) {

  val <- NA  # initialize

  ## Many ways to fail:
  if (is.na(prev)) {

    val <- FALSE

    warning("A prevalence (prev) is missing but necessary.")}

  else if (is.na(sens) & is.na(mirt)) {

    val <- FALSE

    warning("Either a sensitivity (sens) OR a miss rate (mirt) is necessary.")

  } else if (is.na(spec) & is.na(fart)) {

    val <- FALSE

    warning("Either a specificity (spec) OR a false alarm rate (fart) is necessary.")

  } else {  # one way to succeed:

    val <- TRUE

  }

  return(val)

}

## Checks:
# # ways to work:
# is_suff_prob_set(prev = 1, sens = 1, spec = 1)  # => TRUE
# is_suff_prob_set(prev = 1, mirt = 1, spec = 1)  # => TRUE
# is_suff_prob_set(prev = 1, sens = 1, fart = 1)  # => TRUE
# is_suff_prob_set(prev = 1, mirt = 1, fart = 1)  # => TRUE
#
# # watch out for:
# is_suff_prob_set(prev = 1, sens = 2, spec = 3)            # => TRUE, but is_prob would be FALSE for 2 and 3
# is_suff_prob_set(prev = 1, mirt = 2, fart = 4)            # => TRUE, but is_prob would be FALSE for 2 and 4
# is_suff_prob_set(prev = 1, sens = 2, spec = 3, fart = 4)  # => TRUE, but is_prob would be FALSE for 2, 3, and 4
#
# ## ways to fail:
# # is_suff_prob_set()                    # => FALSE + warning (prev missing)
# # is_suff_prob_set(prev = 1)            # => FALSE + warning (sens or mirt missing)
# # is_suff_prob_set(prev = 1, sens = 1)  # => FALSE + warning (spec or fart missing)


## ToDo: Analog fn for freq: is_suff_freq_set ----------

## Analog function: is_suff_freq_set
## that verifies an input for sufficient number of frequencies



## is_complement: Verify that 2 numbers are complements -----------------

#' Verify that two numbers are complements.
#'
#' \code{is_complement} is a function that
#' takes 2 numeric arguments (typically probabilities) as inputs and
#' verifies that they are \emph{complements} (i.e., add up to 1,
#' within some tolerance range \code{tol}).
#'
#' Both \code{p1} and \code{p2} are necessary arguments.
#' If one or both arguments are \code{NA}, \code{is_complement}
#' returns \code{NA} (i.e., neither \code{TRUE} nor \code{FALSE}).
#'
#' The argument \code{tol} is optional (with a default value of .01)
#' Numeric near-complements that differ by less than this
#' value are still considered to be complements.
#'
#' This function does not verify the type, range, or sufficiency
#' of the inputs provided. See \code{\link{is_prob}} and
#' \code{\link{is_suff_prob_set}} for this purpose.
#'
#' @param p1 A numeric argument (typically probability in range from 0 to 1).
#'
#' @param p2 A numeric argument (typically probability in range from 0 to 1).
#'
#' @param tol A numeric tolerance value.
#' Default: \code{tol = .01}.
#'
#' @return \code{NA} or a Boolean value:
#' \code{NA} if one or both arguments are \code{NA};
#' \code{TRUE} if both arguments are provided
#' and complements (in \code{tol} range);
#' otherwise \code{FALSE}.
#'
#' @examples
#' # Basics:
#' is_complement(0, 1)           # => TRUE
#' is_complement(1/3, 2/3)       # => TRUE
#' is_complement(.33, .66)       # => TRUE  (as within default tol = .01)
#' is_complement(.33, .65)       # => FALSE (as beyond default tol = .01)
#'
#' # watch out for:
#' is_complement(NA, NA)            # => NA (but not FALSE)
#' is_complement(1, NA)             # => NA (but not FALSE)
#' is_complement(2, -1)             # => TRUE + warnings (p1 and p2 beyond range)
#' is_complement(8, -7)             # => TRUE + warnings (p1 and p2 beyond range)
#' is_complement(.3, .6)            # => FALSE + warning (beyond tolerance)
#' is_complement(.3, .6, tol = .1)  # => TRUE (due to increased tolerance)
#'
#' # ways to fail:
#' # is_complement(0, 0)            # => FALSE + warning (beyond tolerance)
#' # is_complement(1, 1)            # => FALSE + warning (beyond tolerance)
#' # is_complement(8, 8)            # => FALSE + warning (beyond tolerance)
#'
#' @family verification functions
#'
#' @seealso
#' \code{\link{comp_complement}} computes a probability's complement;
#' \code{\link{comp_comp_pair}} computes pairs of complements;
#' \code{\link{num}} contains basic numeric variables;
#' \code{\link{init_num}} initializes basic numeric variables;
#' \code{\link{prob}} contains current probability information;
#' \code{\link{comp_prob}} computes current probability information;
#' \code{\link{freq}} contains current frequency information;
#' \code{\link{comp_freq}} computes current frequency information;
#' \code{\link{is_valid_prob_set}} verifies the validity of probability inputs;
#' \code{\link{as_pc}} displays a probability as a percentage;
#' \code{\link{as_pb}} displays a percentage as probability.
#'
#' @export

is_complement <- function(p1, p2, tol = .01) {

  val <- NA     # initialize
  eps <- 10^-9  # some very small value

  ## Issue warnings for non-probability arguments:
  if ( !is_prob(p1) ) { NULL }
  if ( !is_prob(p2) ) { NULL }

  if ( !is.na(p1) & !is.na(p2) ) {  # only ask if both are not NA:

    # Condition p1 and p2 being complements:
    # p1 + p2 = 1
    # p1      = 1 - p2

    differ  <- abs(p1 - (1 - p2))

    if (differ > (tol + eps)) {

      warning("Probabilities (p1 and p2) are not complements (in tolerated range).")
      # warning(paste0("p1 = ", p1, "; (1 - p2) = ", (1 - p2), ", difference = ", differ))

      val <- FALSE

    } else {

      val <- TRUE
    }

  }

  return(val)

}

## Checks:

## Removed from documentation (to avoid ERRORS):

# # ways to succeed:
# is_complement(0, 1)           # => TRUE
# is_complement(1/3, 2/3)       # => TRUE
# is_complement(.33, .66)       # => TRUE
#
# # watch out for:
# is_complement(2, -1)             # => TRUE + warnings (p1 and p2 beyond range)
# is_complement(8, -7)             # => TRUE + warnings (p1 and p2 beyond range)
# is_complement(1, NA)             # => NA (but not FALSE)
# is_complement(.3, .6)            # => FALSE + warning (beyond tolerance)
# is_complement(.3, .6, tol = .1)  # => TRUE (due to increased tolerance)
#
# # ways to fail:
# # is_complement(0, 0)            # => FALSE + warning (beyond tolerance)
# # is_complement(1, 1)            # => FALSE + warning (beyond tolerance)
# # is_complement(8, 8)            # => FALSE + warning (beyond tolerance)

## is_prob_range: Verify that some_range includes exactly 2 numeric prob values (from 0 to 1): ------

is_prob_range <- function(some_range) {

  val <- NA

  if (!is.numeric(some_range)) {
    message(paste0("Range must be numeric."))
    val <- FALSE
  } else if (length(some_range) != 2) {
    message(paste0("Range requires exactly 2 values."))
    val <- FALSE
  } else if (!is_prob(some_range)) {
    message(paste0("Range requires probability values (from 0 to 1)."))
    val <- FALSE
  } else {
    val <- TRUE
  }

  return(val)

}

# ## Check:
# # succeeds:
# is_prob_range(c(0, 1))   # TRUE
# is_prob_range(c(0, 0))   # TRUE
# # fails:
# is_prob_range(c("a", 1))  # FALSE: not numeric
# is_prob_range(c(0, 0, 1)) # FALSE: not 2 values
# is_prob_range(c(0, 2))    # FALSE: not prob
# is_prob_range(c(0, NA))   # FALSE: not prob


## (B) Beware of extreme cases: ----------
##     Verify if the current set of (sufficient) probabilities
##     describe an extreme case:

## is_extreme_prob_set: Verify that a prob set is an extreme case ----------

#' Verify that a set of probabilities describes an extreme case.
#'
#' \code{is_extreme_prob_set} verifies that a set
#' of probabilities (i.e., \code{\link{prev}},
#' and \code{\link{sens}} or \code{\link{mirt}},
#' and \code{\link{spec}} or \code{\link{fart}})
#' describe an extreme case.
#'
#' If \code{TRUE}, a warning message describing the
#' nature of the extreme case is printed to allow
#' anticipating peculiar effects (e.g., that
#' \code{\link{PPV}} or \code{\link{NPV}} values
#' cannot be computed or are \code{NaN}).
#'
#' This function does not verify the type, range, sufficiency,
#' or consistency of its arguments. See \code{\link{is_prob}},
#' \code{\link{is_suff_prob_set}}, \code{\link{is_complement}},
#' \code{\link{is_valid_prob_pair}} and
#' \code{\link{is_valid_prob_set}} for these purposes.
#'
#' @param prev The condition's prevalence value \code{\link{prev}}
#' (i.e., the probability of condition being \code{TRUE}).
#'
#' @param sens The decision's sensitivity \code{\link{sens}}
#' (i.e., the conditional probability of a positive decision
#' provided that the condition is \code{TRUE}).
#' \code{sens} is optional when is complement \code{mirt} is provided.
#'
#' @param mirt The decision's miss rate \code{\link{mirt}}
#' (i.e., the conditional probability of a negative decision
#' provided that the condition is \code{TRUE}).
#' \code{mirt} is optional when is complement \code{sens} is provided.
#'
#' @param spec The decision's specificity \code{\link{spec}}
#' (i.e., the conditional probability of a negative decision
#' provided that the condition is \code{FALSE}).
#' \code{spec} is optional when is complement \code{fart} is provided.
#'
#' @param fart The decision's false alarm rate \code{\link{fart}}
#' (i.e., the conditional probability of a positive decision
#' provided that the condition is \code{FALSE}).
#' \code{fart} is optional when its complement \code{spec} is provided.
#'
#' @return A Boolean value:
#' \code{TRUE} if an extreme case is identified;
#' otherwise \code{FALSE}.
#'
#' @examples
#' # Identify 6 extreme cases (+ 4 variants):
#' is_extreme_prob_set(1, 1, NA, 1, NA)       # => TRUE + warning: N true positives
#' plot_tree(1, 1, NA, 1, NA, N = 100)        # => illustrates this case
#'
#' is_extreme_prob_set(1, 0, NA, 1, NA)       # => TRUE + warning: N false negatives
#' plot_tree(1, 0, NA, 1, NA, N = 200)        # => illustrates this case
#'
#' sens <- .50
#' is_extreme_prob_set(0, sens, NA, 0, NA)    # => TRUE + warning: N false positives
#' plot_tree(0, sens, NA, 0, N = 300)         # => illustrates this case
#' # Variant:
#' is_extreme_prob_set(0, sens, NA, NA, 1)    # => TRUE + warning: N false positives
#' plot_tree(0, sens, NA, NA, 1, N = 350)     # => illustrates this case
#'
#' sens <- .50
#' is_extreme_prob_set(0, sens, NA, 1)        # => TRUE + warning: N true negatives
#' plot_tree(0, sens, NA, NA, 1, N = 400)     # => illustrates this case
#' # Variant:
#' is_extreme_prob_set(0, sens, NA, NA, 0)    # => TRUE + warning: N true negatives
#' plot_tree(0, sens, NA, NA, 0, N = 450)     # => illustrates this case
#'
#' prev <- .50
#' is_extreme_prob_set(prev, 0, NA, 1, NA)    # => TRUE + warning: 0 hi and 0 fa (0 dec_pos cases)
#' plot_tree(prev, 0, NA, 1, NA, N = 500)     # => illustrates this case
#' # # Variant:
#' is_extreme_prob_set(prev, 0, 0, NA, 0)     # => TRUE + warning: 0 hi and 0 fa (0 dec_pos cases)
#' plot_tree(prev, 0, NA, 1, NA, N = 550)     # => illustrates this case
#'
#' prev <- .50
#' is_extreme_prob_set(prev, 1, NA, 0, NA)    # => TRUE + warning: 0 mi and 0 cr (0 dec_neg cases)
#' plot_tree(prev, 1, NA, 0, NA, N = 600)     # => illustrates this case
#' # # Variant:
#' is_extreme_prob_set(prev, 1, NA, 0, NA)    # => TRUE + warning: 0 mi and 0 cr (0 dec_neg cases)
#' plot_tree(prev, 1, NA, 0, NA, N = 650)     # => illustrates this case
#'
#' @family verification functions
#'
#' @seealso
#' \code{\link{is_valid_prob_pair}} verifies that a pair of probabilities can be complements;
#' \code{\link{is_valid_prob_set}} verifies the validity of a set of probability inputs;
#' \code{\link{num}} contains basic numeric variables;
#' \code{\link{init_num}} initializes basic numeric variables;
#' \code{\link{prob}} contains current probability information;
#' \code{\link{comp_prob}} computes current probability information;
#' \code{\link{freq}} contains current frequency information;
#' \code{\link{comp_freq}} computes current frequency information;
#' \code{\link{as_pc}} displays a probability as a percentage;
#' \code{\link{as_pb}} displays a percentage as probability
#'
#' @export

is_extreme_prob_set <- function(prev,
                                sens = NA, mirt = NA,
                                spec = NA, fart = NA) {

  ## (1) Initialize:
  val <- NA


  ## (2) Compute the complete quintet of probabilities:
  # prob_quintet <- comp_complete_prob_set(prev, sens, mirt, spec, fart)
  # sens <- prob_quintet[2] # gets sens (if not provided)
  # mirt <- prob_quintet[3] # gets mirt (if not provided)
  # spec <- prob_quintet[4] # gets spec (if not provided)
  # fart <- prob_quintet[5] # gets fart (if not provided)

  ## Problem: This does not work yet (as comp_complete_prob_set is only defined later)
  ## Hack fix: 4 possible ways to complete an NA value:
  if (is.na(sens) && is_prob(mirt)) { sens <- 1 - mirt }  # 1. compute sens if only mirt is provided.
  if (is.na(mirt) && is_prob(sens)) { mirt <- 1 - sens }  # 2. compute mirt if only sens is provided.
  if (is.na(spec) && is_prob(fart)) { spec <- 1 - fart }  # 3. compute spec if only fart is provided.
  if (is.na(fart) && is_prob(spec)) { fart <- 1 - spec }  # 4. compute fart if only spec is provided.
  ## Note: This does NOT check for consistency of complements (e.g., inputs of both spec = 1 & fart = 1)


  ## Beware of cases in which PPV or NPV are NaN:

  ## (1) PPV is NaN if:
  ##     (a)  (prev = 1) & (sens = 0)
  ##     (b)  (prev = 0) & (spec = 1)
  ##     (c)  (sens = 0) & (spec = 1)

  ## (2) NPV is NaN if:
  ##     (a)  (prev = 1) & (sens = 1)
  ##     (b)  (prev = 1) & (sens = 0)
  ##     (c)  (sens = 1) & (spec = 0)


  ## (3) Check cases (specific combinations of prev, sens, and spec/fart):

  if ((prev == 1) & (sens == 1)) {        # 1. prev and sens are both perfect:

    warning("Extreme case (prev = 1 & sens = 1):\n  N hi (TP) cases; 0 cond_false or dec_false cases; NPV = NaN.")
    val <- TRUE

  }

  else if ((prev == 1) & (sens == 0)) {   # 2. prev perfect and sens zero:

    warning("Extreme case (prev = 1 & sens = 0):\n  N mi (FN) cases; 0 cond_false or dec_true cases; PPV = NaN.")
    val <- TRUE

  }

  else if ((prev == 0) & (spec == 0)) {   # 3a. prev and spec are both zero:

    warning("Extreme case (prev = 0 & spec = 0):\n  N fa (FP) cases; 0 cond_true or dec_true cases; PPV = NaN.")
    val <- TRUE

  } else if ((prev == 0) & (fart == 1)) {  # 3b. prev zero and fart perfect (i.e., spec zero):

    warning("Extreme case (prev = 0 & fart = 1):\n  N fa (FP) cases; 0 cond_true or dec_true cases; PPV = NaN.")
    val <- TRUE

  }

  else if ((prev == 0) & (spec == 1)) {   # 4a. prev zero and spec perfect:

    warning("Extreme case (prev = 0 & spec = 1):\n  N cr (TN) cases; 0 cond_true or dec_false cases; NPV = NaN.")
    val <- TRUE

  } else if ((prev == 0) & (fart == 0)) {  # 4b. prev zero and fart zero (i.e., spec perfect):

    warning("Extreme case (prev = 0 & fart = 0):\n  N cr (TN) cases; 0 cond_true or dec_false cases; NPV = NaN.")
    val <- TRUE

  }

  else if ((sens == 0) & (spec == 1)) {   # 5a. sens zero and spec perfect (i.e., fart zero):

    warning("Extreme case (sens = 0 & spec = 1):\n  0 hi (TP) and 0 fa (FP) cases; 0 dec_pos cases; PPV = NaN.")
    val <- TRUE

  } else if ((sens == 0) & (fart == 0)) {  # 5b. sens zero and fart zero (i.e., spec perfect):

    warning("Extreme case (sens = 0 & fart = 0):\n  0 hi (TP) and 0 fa (FP) cases; 0 dec_pos cases; PPV = NaN.")
    val <- TRUE

  }


  else if ((sens == 1) & (spec == 0)) {   # 6a. sens perfect and spec zero (i.e., fart perfect):

    warning("Extreme case (sens = 1 & spec = 0):\n  0 mi (FN) and 0 cr (TN) cases; 0 dec_neg cases; NPV = NaN.")
    val <- TRUE

  } else if ((sens == 1) & (fart == 1)) {  # 6b. sens perfect and fart perfect (i.e., spec zero):

    warning("Extreme case (sens = 1 & fart = 1):\n  0 mi (FN) and 0 cr (TN) cases; 0 dec_neg cases; NPV = NaN.")
    val <- TRUE

  }

  else {  # not (detected as) an extreme case:

    val <- FALSE

  }

  ## (4) Return value:
  return(val)

} # is_extreme_prob_set end.

## Check:
#
# # Identify 6 extreme cases (+ 4 variants):
# is_extreme_prob_set(1, 1, NA, 1, NA)       # => TRUE + warning: N true positives
# plot_tree(1, 1, NA, 1, NA, N = 100)        # => illustrates this case
#
# is_extreme_prob_set(1, 0, NA, 1, NA)       # => TRUE + warning: N false negatives
# plot_tree(1, 0, NA, 1, NA, N = 200)        # => illustrates this case
#
# sens <- .50
# is_extreme_prob_set(0, sens, NA, 0, NA)    # => TRUE + warning: N false positives
# plot_tree(0, sens, NA, 0, N = 300)         # => illustrates this case
# # Variant:
# is_extreme_prob_set(0, sens, NA, NA, 1)    # => TRUE + warning: N false positives
# plot_tree(0, sens, NA, NA, 1, N = 350)     # => illustrates this case
#
# sens <- .50
# is_extreme_prob_set(0, sens, NA, 1)        # => TRUE + warning: N true negatives
# plot_tree(0, sens, NA, NA, 1, N = 400)     # => illustrates this case
# # Variant:
# is_extreme_prob_set(0, sens, NA, NA, 0)    # => TRUE + warning: N true negatives
# plot_tree(0, sens, NA, NA, 0, N = 450)     # => illustrates this case
#
# prev <- .50
# is_extreme_prob_set(prev, 0, NA, 1, NA)    # => TRUE + warning: 0 hi and 0 fa (0 dec_pos cases)
# plot_tree(prev, 0, NA, 1, NA, N = 500)     # => illustrates this case
# # # Variant:
# is_extreme_prob_set(prev, 0, 0, NA, 0)     # => TRUE + warning: 0 hi and 0 fa (0 dec_pos cases)
# plot_tree(prev, 0, NA, 1, NA, N = 550)     # => illustrates this case
#
# prev <- .50
# is_extreme_prob_set(prev, 1, NA, 0, NA)    # => TRUE + warning: 0 mi and 0 cr (0 dec_neg cases)
# plot_tree(prev, 1, NA, 0, NA, N = 600)     # => illustrates this case
# # # Variant:
# is_extreme_prob_set(prev, 1, NA, 0, NA)    # => TRUE + warning: 0 mi and 0 cr (0 dec_neg cases)
# plot_tree(prev, 1, NA, 0, NA, N = 650)     # => illustrates this case


## is_valid_prob_pair: Verify a pair of probability inputs -------------

# Verify that 2 probabilities are valid inputs
# for a pair of complementary probabilities:

#' Verify that a pair of probability inputs
#' can be a pair of complementary probabilities.
#'
#' \code{is_valid_prob_pair} is a function that verifies that
#' a pair of 2 numeric inputs \code{p1} and \code{p2}
#' can be interpreted as a valid pair of probabilities.
#'
#' \code{is_valid_prob_pair} is a wrapper function
#' that combines \code{\link{is_prob}} and
#' \code{\link{is_complement}} in one function.
#'
#' Either \code{p1} or \code{p2} must be a probability
#' (verified via \code{\link{is_prob}}).
#' If both arguments are provided they must be
#' probabilities and complements
#' (verified via \code{\link{is_complement}}).
#'
#' The argument \code{tol} is optional (with a default value of .01)
#' Numeric near-complements that differ by less than this
#' value are still considered to be complements.
#'
#' @param p1 A numeric argument
#' (typically probability in range from 0 to 1).
#'
#' @param p2 A numeric argument
#' (typically probability in range from 0 to 1).
#'
#' @param tol A numeric tolerance value.
#'
#' @return A Boolean value:
#' \code{TRUE} if exactly one argument is a probability,
#' if both arguments are probabilities and complements,
#' otherwise \code{FALSE}.
#'
#' @examples
#' # ways to succeed:
#' is_valid_prob_pair(1, 0)      # => TRUE
#' is_valid_prob_pair(0, 1)      # => TRUE
#' is_valid_prob_pair(1, NA)     # => TRUE + warning (NA)
#' is_valid_prob_pair(NA, 1)     # => TRUE + warning (NA)
#' is_valid_prob_pair(.50, .51)  # => TRUE (as within tol)
#'
#' # ways to fail:
#' is_valid_prob_pair(.50, .52)  # => FALSE (as beyond tol)
#' is_valid_prob_pair(1, 2)      # => FALSE + warning (beyond range)
#' is_valid_prob_pair(NA, NA)    # => FALSE + warning (NA)
#'
#' @family verification functions
#'
#' @seealso
#' \code{\link{is_valid_prob_set}} uses this function to verify sets of probability inputs;
#' \code{\link{is_complement}} verifies numeric complements;
#' \code{\link{is_prob}} verifies probabilities;
#' \code{\link{num}} contains basic numeric variables;
#' \code{\link{init_num}} initializes basic numeric variables;
#' \code{\link{prob}} contains current probability information;
#' \code{\link{comp_prob}} computes current probability information;
#' \code{\link{freq}} contains current frequency information;
#' \code{\link{comp_freq}} computes current frequency information;
#' \code{\link{as_pc}} displays a probability as a percentage;
#' \code{\link{as_pb}} displays a percentage as probability.
#'
#' @export

is_valid_prob_pair <- function(p1, p2, tol = .01) {

  val <- FALSE

  if ( ( is.na(p1) && !is.na(p2) && is_prob(p2) ) |  # only p2 is provided and is_prob

       ( is.na(p2) && !is.na(p1) && is_prob(p1) ) |  # only p1 is provided and is_prob

       ( # !is.na(p1)  && !is.na(p2)  &&  # commented out to suppress NA warning messages
         is_prob(p1) && is_prob(p2) &&               # both p1 and p2 are provided
         is_complement(p1, p2, tol) ) ) {            # and both are complements

    val <- TRUE
  }

  return(val)

} # is_valid_prob_pair end.

## Check:
# # ways to succeed:
# is_valid_prob_pair(1, 0)      # => TRUE
# is_valid_prob_pair(0, 1)      # => TRUE
# is_valid_prob_pair(1, NA)     # => TRUE + warning (NA)
# is_valid_prob_pair(NA, 1)     # => TRUE + warning (NA)
# is_valid_prob_pair(.50, .51)  # => TRUE (as within tol)
#
# # ways to fail:
# is_valid_prob_pair(.50, .52)  # => FALSE (as beyond tol)
# is_valid_prob_pair(1, 2)      # => FALSE + warning (beyond range)
# is_valid_prob_pair(NA, NA)    # => FALSE + warning (NA)


## is_valid_prob_set: Verify a set of probability inputs ------------

# Verify that a set of up to 5 probabilities can
# be interpreted as valid probability inputs:

#' Verify that a set of probability inputs is valid.
#'
#' \code{is_valid_prob_set} is a function that verifies that
#' a set of (3 to 5) numeric inputs can be interpreted as a
#' valid set of (3 essential and 2 optional) probabilities.
#'
#' \code{is_valid_prob_set} is a wrapper function that combines
#' \code{\link{is_prob}}, \code{\link{is_suff_prob_set}},
#' and \code{\link{is_complement}} in one function.
#'
#' While no alternative input option for frequencies is provided,
#' specification of the essential probability \code{\link{prev}}
#' is always necessary. However, for 2 other essential
#' probabilities there is a choice:
#'
#' \enumerate{
#'   \item Either \code{\link{sens}} or \code{\link{mirt}} is necessary
#'         (as both are complements).
#'
#'   \item Either \code{\link{spec}} or \code{\link{fart}} is necessary
#'        (as both are complements).
#' }
#'
#' The argument \code{tol} is optional (with a default value of .01)
#' and used as the tolerance value of \code{\link{is_complement}}.
#'
#' \code{is_valid_prob_set} verifies the validity of inputs,
#' but does not compute or return numeric variables.
#' Use \code{\link{is_extreme_prob_set}} to verify sets of probabilities
#' that describe extreme cases and \code{\link{init_num}}
#' for initializing basic parameters.
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
#' @param tol A numeric tolerance value used by \code{\link{is_complement}}.
#'
#' @return A Boolean value:
#' \code{TRUE} if the probabilities provided are valid;
#' otherwise \code{FALSE}.
#'
#' @examples
#' # ways to succeed:
#' is_valid_prob_set(1, 1, 0, 1, 0)                 # => TRUE
#' is_valid_prob_set(.3, .9, .1, .8, .2)            # => TRUE
#' is_valid_prob_set(.3, .9, .1, .8, NA)            # => TRUE + warning (NA)
#' is_valid_prob_set(.3, .9, NA, .8, NA)            # => TRUE + warning (NAs)
#' is_valid_prob_set(.3, .9, NA, NA, .8)            # => TRUE + warning (NAs)
#' is_valid_prob_set(.3, .8, .1, .7, .2, tol = .1)  # => TRUE (due to increased tol)
#'
#' # watch out for:
#' is_valid_prob_set(1, 0, 1, 0, 1)    # => TRUE, but NO warning about extreme case!
#' is_valid_prob_set(1, 1, 0, 1, 0)    # => TRUE, but NO warning about extreme case!
#' is_valid_prob_set(1, 1, 0, 1, NA)   # => TRUE, but NO warning about extreme case!
#' is_valid_prob_set(1, 1, 0, NA, 1)   # => TRUE, but NO warning about extreme case!
#' is_valid_prob_set(1, 1, 0, NA, 0)   # => TRUE, but NO warning about extreme case!
#'
#' # ways to fail:
#' is_valid_prob_set(8, 1, 0, 1, 0)      # => FALSE + warning (is_prob fails)
#' is_valid_prob_set(1, 1, 8, 1, 0)      # => FALSE + warning (is_prob fails)
#' is_valid_prob_set(2, 1, 3, 1, 4)      # => FALSE + warning (is_prob fails)
#' is_valid_prob_set(1, .8, .2, .7, .2)  # => FALSE + warning (beyond complement range)
#' is_valid_prob_set(1, .8, .3, .7, .3)  # => FALSE + warning (beyond complement range)
#' is_valid_prob_set(1, 1, 1, 1, 1)      # => FALSE + warning (beyond complement range)
#' is_valid_prob_set(1, 1, 0, 1, 1)      # => FALSE + warning (beyond complement range)
#'
#' @family verification functions
#'
#' @seealso
#' \code{\link{is_valid_prob_pair}} verifies that probability pairs are complements;
#' \code{\link{is_prob}} verifies probabilities;
#' \code{\link{prob}} contains current probability information;
#' \code{\link{num}} contains basic numeric variables;
#' \code{\link{init_num}} initializes basic numeric variables;
#' \code{\link{comp_prob}} computes current probability information;
#' \code{\link{freq}} contains current frequency information;
#' \code{\link{comp_freq}} computes current frequency information;
#' \code{\link{as_pc}} displays a probability as a percentage;
#' \code{\link{as_pb}} displays a percentage as probability.
#'
#' @export

is_valid_prob_set <- function(prev,
                              sens = NA, mirt = NA,
                              spec = NA, fart = NA,
                              tol = .01) {

  val <- FALSE  # initialize

  if ( is_prob(prev) &&
       is_valid_prob_pair(sens, mirt, tol) &&
       is_valid_prob_pair(spec, fart, tol) ) {
    val <- TRUE
  }

  return(val)

} # is_valid_prob_set end.

## Check:
#
# # ways to succeed:
# is_valid_prob_set(1, 1, 0, 1, 0)                 # => TRUE
# is_valid_prob_set(.3, .9, .1, .8, .2)            # => TRUE
# is_valid_prob_set(.3, .9, .1, .8, NA)            # => TRUE + warning (NA)
# is_valid_prob_set(.3, .9, NA, .8, NA)            # => TRUE + warning (NAs)
# is_valid_prob_set(.3, .9, NA, NA, .8)            # => TRUE + warning (NAs)
# is_valid_prob_set(.3, .8, .1, .7, .2, tol = .1)  # => TRUE (due to increased tol)
#
# # watch out for:
# is_valid_prob_set(1, 0, 1, 0, 1)    # => TRUE, but NO warning about extreme case!
# is_valid_prob_set(1, 1, 0, 1, 0)    # => TRUE, but NO warning about extreme case!
# is_valid_prob_set(1, 1, 0, 1, NA)   # => TRUE, but NO warning about extreme case!
# is_valid_prob_set(1, 1, 0, NA, 1)   # => TRUE, but NO warning about extreme case!
# is_valid_prob_set(1, 1, 0, NA, 0)   # => TRUE, but NO warning about extreme case!
#
# # ways to fail:
# is_valid_prob_set(8, 1, 0, 1, 0)      # => FALSE + warning (is_prob fails)
# is_valid_prob_set(1, 1, 8, 1, 0)      # => FALSE + warning (is_prob fails)
# is_valid_prob_set(2, 1, 3, 1, 4)      # => FALSE + warning (is_prob fails)
# is_valid_prob_set(1, .8, .2, .7, .2)  # => FALSE + warning (beyond complement range)
# is_valid_prob_set(1, .8, .3, .7, .3)  # => FALSE + warning (beyond complement range)
# is_valid_prob_set(1, 1, 1, 1, 1)      # => FALSE + warning (beyond complement range)
# is_valid_prob_set(1, 1, 0, 1, 1)      # => FALSE + warning (beyond complement range)


## is_valid_prob_triple: Verify a triple of essential probability inputs ---------------

# Verify that a triple of inputs can
# be interpreted as valid set of 3 essential probabilites:

#' Verify that a triple of essential probability inputs is valid.
#'
#' \code{is_valid_prob_triple} is a \strong{deprecated} function that verifies that
#' a set of 3 numeric inputs can be interpreted as a
#' valid set of 3 probabilities.
#'
#' \code{is_valid_prob_triple} is a simplified version
#' of \code{\link{is_valid_prob_set}}.
#' It is a quick wrapper function that only verifies
#' \code{\link{is_prob}} for all of its 3 arguments.
#'
#' \code{is_valid_prob_triple} does not compute or return numeric variables.
#' Use \code{\link{is_extreme_prob_set}} to verify extreme cases and
#' \code{\link{comp_complete_prob_set}} to complete sets of valid probabilities.
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
#' @return A Boolean value:
#' \code{TRUE} if the probabilities provided are valid;
#' otherwise \code{FALSE}.
#'
#' @examples
#' # ways to work:
#' is_valid_prob_triple(0, 0, 0)    # => TRUE
#' is_valid_prob_triple(1, 1, 1)    # => TRUE
#'
#' ## ways to fail:
#' # is_valid_prob_triple(0, 0)       # => ERROR (as no triple)
#' # is_valid_prob_triple(0, 0, 7)    # => FALSE + warning (beyond range)
#' # is_valid_prob_triple(0, NA, 0)   # => FALSE + warning (NA)
#' # is_valid_prob_triple("p", 0, 0)  # => FALSE + warning (non-numeric)
#'
#' @family verification functions
#'
#' @seealso
#' \code{\link{is_extreme_prob_set}} verifies extreme cases;
#' \code{\link{is_valid_prob_set}} verifies sets of probability inputs;
#' \code{\link{is_valid_prob_pair}} verifies that probability pairs are complements;
#' \code{\link{num}} contains basic numeric variables;
#' \code{\link{init_num}} initializes basic numeric variables;
#' \code{\link{prob}} contains current probability information;
#' \code{\link{comp_prob}} computes current probability information;
#' \code{\link{freq}} contains current frequency information;
#' \code{\link{comp_freq}} computes current frequency information;
#' \code{\link{as_pc}} displays a probability as a percentage;
#' \code{\link{as_pb}} displays a percentage as probability.
#'
#' @export

is_valid_prob_triple <- function(prev, sens, spec) {

  val <- FALSE  # initialize

  if ( is_prob(prev) && is_prob(sens) && is_prob(spec) ) {
    val <- TRUE
  }

  return(val)

}

## Check:
# is_valid_prob_triple(0, 0, 0)
# is_valid_prob_triple(1, 1, 1)
#
# ## ways to fail:
# # is_valid_prob_triple(0, 0)       # => ERROR (as no triple)
# # is_valid_prob_triple(0, 0, 7)    # => FALSE + warning (beyond range)
# # is_valid_prob_triple(0, NA, 0)   # => FALSE + warning (NA)
# # is_valid_prob_triple("p", 0, 0)  # => FALSE + warning (non-numeric)





## (C) Conversion functions: ------------------------

## Toggle between showing probabilities and percentages:

## as_pc: Show a probability as a (numeric and rounded) percentage ----------

#' Display a probability as a (numeric and rounded) percentage.
#'
#' \code{as_pc} is a function that displays a probability \code{prob}
#' as a percentage (rounded to \code{n_digits} decimals).
#'
#' \code{as_pc} and its complement function \code{\link{as_pb}} allow
#' toggling the display of numeric values between percentages and probabilities.
#'
#' @param prob  A probability (as a scalar or vector of numeric values from 0 to 1).
#'
#' @param n_digits  Number of decimal places to which percentage is rounded.
#' Default: \code{n_digits = 2}.
#'
#' @return A percentage (as a numeric value).
#'
#' @examples
#' as_pc(.50)                # 50
#' as_pc(1/3)                # 33.33
#' as_pc(1/3, n_digits = 0)  # 33
#' as_pc(as_pb(12.3))        # 12.3
#'
#' @family utility functions
#' @family display functions
#'
#' @seealso
#' \code{\link{is_prob}} verifies a probability;
#' \code{\link{is_perc}} verifies a percentage;
#' \code{\link{is_valid_prob_set}} verifies the validity of probability inputs;
#' \code{\link{num}} contains basic numeric variables;
#' \code{\link{init_num}} initializes basic numeric variables;
#' \code{\link{prob}} contains current probability information;
#' \code{\link{comp_prob}} computes current probability information;
#' \code{\link{freq}} contains current frequency information;
#' \code{\link{comp_freq}} computes current frequency information;
#' \code{\link{comp_complement}} computes a probability's complement;
#' \code{\link{comp_comp_pair}} computes pairs of complements.
#'
#' @export

## Probability as percentage (2 decimals):

as_pc <- function(prob, n_digits = 2) {

  perc <- NA # initialize

  if (is_prob(prob)) {

    perc <- round(prob * 100, n_digits)  # compute percentage

  }

  else {

    warning("Argument (prob) is no probability.")

    perc <- round(prob * 100, n_digits)  # still try to compute

  }

  return(perc)  # return (numeric)
}

## Check:
# as_pc(1/2)                # =>  50
# as_pc(1/3)                # =>  33.33
# as_pc(1/3, n_digits = 0)  # =>  33
# as_pc(pi)                 # => 314.16 + Warning that prob is not in range.
# as_pc(as_pb(12.3))        # =>  12.3
# as_pc(NA)
# as_pc(0/0)

## Removed from documentation (to avoid ERRORS):
#
# ## ways to fail:
# # as_pc(pi)               # => 314.16 + WARNING that prob is no probability
#
# ## Check (not run):
# # prob_seq <- seq(0, 1, by = 1/10)
# # perc_seq <- seq(0, 100, by = 10)
#
# # as_pc(prob_seq)  # =>   0  10  20  30  40  50  60  70  80  90 100
# # as_pb(perc_seq)  # => 0.0 0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9 1.0
#
# # perc_seq == as_pc(as_pb(perc_seq))            # => all TRUE
# # prob_seq == as_pb(as_pc(prob_seq))            # => some FALSE due to rounding errors!
# # round(prob_seq, 4) == as_pb(as_pc(prob_seq))  # => all TRUE (both rounded to 4 decimals)


## Percentage as probability (4 decimals):

## as_pb: Show a percentage as a (numeric and rounded) probability ----------

#' Display a percentage as a (numeric and rounded) probability.
#'
#' \code{as_pb} is a function that displays a percentage \code{perc}
#' as a probability (rounded to \code{n_digits} decimals).
#'
#' \code{as_pb} and its complement function \code{\link{as_pc}} allow
#' toggling the display of numeric values between percentages and probabilities.
#'
#' @param perc A percentage (as a scalar or vector of numeric values from 0 to 100).
#'
#' @param n_digits Number of decimal places to which percentage is rounded.
#' Default: \code{n_digits = 4}.
#'
#' @return A probability (as a numeric value).
#'
#' @examples
#' as_pb(1/3)          # => 0.0033
#' as_pb(as_pc(2/3))   # => 0.6667 (rounded to 4 decimals)
#'
#' @family utility functions
#' @family display functions
#'
#' @seealso
#' \code{\link{is_perc}} verifies a percentage;
#' \code{\link{is_prob}} verifies a probability;
#' \code{\link{is_valid_prob_set}} verifies the validity of probability inputs;
#' \code{\link{num}} contains basic numeric variables;
#' \code{\link{init_num}} initializes basic numeric variables;
#' \code{\link{prob}} contains current probability information;
#' \code{\link{comp_prob}} computes current probability information;
#' \code{\link{freq}} contains current frequency information;
#' \code{\link{comp_freq}} computes current frequency information;
#' \code{\link{comp_complement}} computes a probability's complement;
#' \code{\link{comp_comp_pair}} computes pairs of complements.
#'
#' @export

as_pb <- function(perc, n_digits = 4) {

  prob <- NA # initialize

  if (is_perc(perc)) {

    prob <- round(perc/100, n_digits) # compute
  } else {
    warning("Percentage (perc) is not in range 0 to 100.")
    prob <- round(perc/100, n_digits) # still compute
  }

  return(prob)  # numeric value

}

## Check:
# as_pb(1/3)          # => 0.0033
# as_pb(as_pc(2/3))   # => 0.6667 (rounded to 4 decimals)
#
# prob_seq <- seq(0, 1, by = 1/10)
# perc_seq <- seq(0, 100, by = 10)
#
# as_pc(prob_seq)  # =>   0  10  20  30  40  50  60  70  80  90 100
# as_pb(perc_seq)  # => 0.0 0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9 1.0
#
# perc_seq == as_pc(as_pb(perc_seq)) # all TRUE
# prob_seq == as_pb(as_pc(prob_seq)) # some FALSE due to rounding errors!
# round(prob_seq, 4) == as_pb(as_pc(prob_seq)) # all TRUE (as both rounded to 4 decimals)

## Removed from documentation (to avoid ERRORS):
#
# ## Check (not run):
# # prob_seq <- seq(0, 1, by = 1/10)
# # perc_seq <- seq(0, 100, by = 10)
#
# # as_pc(prob_seq)  # =>   0  10  20  30  40  50  60  70  80  90 100
# # as_pb(perc_seq)  # => 0.0 0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9 1.0
#
# # perc_seq == as_pc(as_pb(perc_seq))            # => all TRUE
# # prob_seq == as_pb(as_pc(prob_seq))            # => some FALSE due to rounding errors!
# # round(prob_seq, 4) == as_pb(as_pc(prob_seq))  # => all TRUE (both rounded to 4 decimals)



## (D) Color and plotting functions: ----------

# Note:
# - Moved plotting help functions to file "plot_util.R".
# - Use unikn pkg or functions for color settings.

## make_transparent: Make colors transparent ------

make_transparent <- function(..., alpha = .50) {

  if (alpha < 0 | alpha > 1) {
    stop("alpha value must be in range from 0 to 1")
  }

  alpha <- floor(255 * alpha)
  newColor <- col2rgb(col = unlist(list(...)), alpha = FALSE)

  .make_transparent <- function(col, alpha) {
    rgb(red = col[1], green = col[2], blue = col[3],
        alpha = alpha, maxColorValue = 255)
  }

  newColor <- apply(newColor, 2, .make_transparent, alpha = alpha)

  return(newColor)

}

## Check:
# make_transparent("black")

## See also:
# adjustcolor(col = "green", alpha.f = .50)

## (E) Text functions: ----------

capitalise_1st <- function(string) {
  String <- ""
  String <- paste0(toupper(substr(string, 1, 1)), substr(string, 2, nchar(string)))
  return(String)
}

## Check:
# capitalise_1st("the end.") # "The end."
# capitalise_1st("")         # ""
# capitalise_1st(123)        # "123"

## (F) Miscellaneous: ----------

# kill_all: Kill all objects in current environment (without warning): ------

kill_all <- function(){

  rm(list = ls())

}

# Check: ----
# kill_all()

## (*) Done: ----------

## - Moved graphical help functions to file "plot_util.R" [2018 08 27].
## - Clean up code                                        [2018 09 22].
## - Export utility functions again,
##   as they are used in some examples                    [2018 11 08].


## (+) ToDo: ----------

## (e+) ToDo: Generalize is_perfect to
##      is_extreme_prob_set to incorporate other extreme cases:
##      [see (e1) and (e+) above]:
##
## - prev = 0, spec = 0: only fa cases
## - prev = 0, spec = 1: only cr cases
## - Also: Deal with 0/0 in probability computations
##   (especially from rounded freq with low N).

## eof. ------------------------------------------
