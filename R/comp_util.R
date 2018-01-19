## comp_util.R | riskyR
## 2018 01 19
## -----------------------------------------------
## Generic utility functions:

## -----------------------------------------------
## (1) Verification functions:
## 1. is_prob
## 2. is_perc
## 3. is_sufficient
## 4. is_complement

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
#' is_prob("Laplace")  # => FALSE + warning that prob is not numeric
#' is_prob(2)          # => FALSE + warning that prob is not in 0 to 1 range
#'
#' is_prob(1/2)        # => TRUE, but does NOT return the probability .5
#'
#' prob.seq <- seq(0, 1, by = 1/10)
#' is_prob(prob.seq)   # => TRUE (for vector)
#'
#' @family verification functions
#' @seealso \code{\link{as_pc}} to display a probability as a percentage;
#' \code{\link{as_pb}} to display a percentage as probability

is_prob <- function(prob) {

  val <- FALSE # initialize

  if (all(!is.numeric(prob))) {
    warning(paste0(prob, " is not numeric."))
  }
  else if (all((prob < 0) | (prob > 1))) {
    warning(paste0(prob, " is no probability (range from 0 to 1)."))
  }
  else {
    val <- TRUE
  }

  return(val)

}

## Checks:
{
  # is_prob("Laplace")  # => FALSE + Warning
  # is_prob(2)          # => FALSE + Warning
  # is_prob(1/2)        # => TRUE
  # is_prob(c(.3, .9))  # => TRUE (for vector)
  # prob.seq <- seq(0, 1, by = 1/10)
  # is_prob(prob.seq)   # => TRUE (for vector)
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
#' is_perc("Bernoulli") # => FALSE + warning that perc is not numeric
#'
#' is_perc(2)           # => TRUE, but does NOT return the percentage 2.
#' is_perc(1/2)         # => TRUE, but does NOT return the percentage 0.5.
#'
#' perc.seq <- seq(0, 100, by = 10)
#' is_perc(perc.seq)    # => TRUE (for vector)

#' @family verification functions
#' @seealso \code{\link{as_pc}} to display a probability as a percentage;
#' \code{\link{as_pb}} to display a percentage as probability

is_perc <- function(perc) {

  val <- FALSE # initialize

  if (all(!is.numeric(perc))) {
    warning(paste0(perc, " is not numeric."))
  }
  else if (all((perc < 0) | (perc > 100))) {
    warning(paste0(perc, " is no percentage in the range from 0 to 100."))
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
#' takes 3 or 4 arguments (probabilities) as inputs and
#' verifies that they are sufficient to compute the
#' frequencies and conditional probabilities
#' for a population of N individuals.
#'
#' While no alternative input option for frequencies is provided,
#' specification of \code{prev} and \code{sens} are always
#' necessary. One additional probability parameter is necessary:
#' If \code{spec} is provided, its complement \code{fart} is optional.
#' If \code{fart} is provided, its complement \code{spec} is optional.
#'
#' Note that this function does not verify the type, range or
#' consistency of its arguments. See \code{\link{is_prob}} and
#' \code{\link{is_consistent}} for this purpose.
#'
#' @param prev The condition's prevalence value (i.e., the probability of condition being TRUE).
#' @param sens A decision's sensitivity value (i.e., the conditional probability
#' of a positive decision provided that the condition is TRUE).
#' @param spec A specificity value (i.e., the conditional probability
#' of a negative decision provided that the condition is FALSE).
#' \code{spec} is optional when is complement \code{fart} is provided.
#' @param fart A false alarm rate (i.e., the conditional probability
#' of a positive decision provided that the condition is FALSE).
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
#' @seealso \code{\link{as_pc}} to display a probability as a percentage

is_sufficient <- function(prev, sens, spec = NA, fart = NA) {

  val <- FALSE # initialize

  if (is.na(prev)) {
    warning("A prevalence value (prev) is missing but necessary.")}
  else if (is.na(sens)) {
    warning("A sensitivity value (sens) is missing but necessary.")}
  else if (is.na(spec) & is.na(fart)) {
    warning("Either a specificity value (spec) OR a false alarm rate (fart) is necessary.")
  } else {
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
#' @param spec A specificity value (i.e., the conditional probability
#' of a negative decision provided that the condition is FALSE).
#' @param fart A false alarm rate (i.e., the conditional probability
#' of a positive decision provided that the condition is FALSE).
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
#' @seealso \code{\link{as_pc}} to display a probability as a percentage

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
#' @seealso \code{\link{is_perc}} to verify a percentage;
#' \code{\link{is_prob}} to verify a probability

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
#' @seealso \code{\link{is_perc}} to verify a percentage;
#' \code{\link{is_prob}} to verify a probability

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

## - ...

## -----------------------------------------------
## eof.
