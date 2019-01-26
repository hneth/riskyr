## comp_prob_prob.R | riskyr
## 2018 12 20
## Compute probabilities from probabilities:
## -----------------------------------------------

## Note: For computing ALL prob from 3 basic probabilities
##       see comp_prob in file "init_num_prob.R".


## Computing probabilities from probabilities (Bayes etc.): ----------

## (1) by condition: Compute basic probabilities of conditions/cases from probabilities: ----------

## Computing complementary probabilities: --------


## A general approach:

## comp_complement: Compute complement probability ------

#' Compute a probability's complement probability.
#'
#' \code{comp_complement} computes the
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

comp_complement <- function(prob) {

  comp <- NA

  if (is_prob(prob)) {  # verify
    comp <- 1 - prob    # complement
  }

  return(comp)
}

## Check:

# comp_complement(0)   # => 1
# comp_complement(1)   # => 0
#
# comp_complement(2)   # => NA + warning (beyond range)
# comp_complement("")  # => NA + warning (non-numeric)


## (a) comp_mirt: mirt from sens: ------

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
#' comp_mirt(2)                       # => NA + warning (beyond range)
#' comp_mirt(1/3)                     # => 0.6666667
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
#'
#' @export

comp_mirt <- function(sens) {

  mirt <- comp_complement(sens)  # use generic function

  return(mirt)
}


## (b) comp_sens: sens from mirt: ------

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
#' comp_sens(2)                       # => NA + warning (beyond range)
#' comp_sens(1/3)                     # => 0.6666667
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
#'
#' @export

comp_sens <- function(mirt) {

  sens <- comp_complement(mirt)  # use generic function

  return(sens)

}


## (c) comp_fart: fart from spec: ------

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
#'
#' @export

comp_fart <- function(spec) {

  fart <- comp_complement(spec)  # use generic function

  return(fart)

}


## (d) comp_spec: spec from fart: ------

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
#' comp_spec(2)                       # => NA + warning (beyond range)
#' comp_spec(1/3)                     # => 0.6666667
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
#'
#' @export

comp_spec <- function(fart) {

  spec <- comp_complement(fart)  # use generic function

  return(spec)

}


## comp_comp_pair: Pairs of complements: ------

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
#'
#' @export

comp_comp_pair <- function(p1 = NA, p2 = NA){

  pair <- c(NULL, NULL) # initialize
  missing <- NA

  if (is.na(p1) & is.na(p2)) {

    warning("One argument (either p1 or p2) is necessary.")
    pair <- c(NA, NA)

  } else if (!is.na(p1) & is.na(p2)) {  # 1: only p1 provided:

    missing <- comp_complement(p1)      #    - compute its comp
    pair <- c(p1, missing)              #    - define pair (leaving input order intact)

  } else if (!is.na(p2) & is.na(p1)) {  # 2: only p2 is provided:

    missing <- comp_complement(p2)      #    - compute spec
    pair <- c(missing, p2)              #    - define pair (leaving input order intact)

  } else {                              # 3: both are provided
    pair <- c(p1, p2)                   #    - leave inputs intact
  }

  return(pair)  # always return pair in order c(p1, p2)

}

## Check:

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


## comp_complete_prob_set: Complete a valid set of probability inputs: ------

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
#' # ways to work:
#' comp_complete_prob_set(1, .8, NA, .7, NA) # => 1.0 0.8 0.2 0.7 0.3
#' comp_complete_prob_set(1, NA, .8, NA, .4) # => 1.0 0.2 0.8 0.6 0.4
#'
#' # watch out for:
#' comp_complete_prob_set(8)                  # => 8 NA NA NA NA + warnings
#' comp_complete_prob_set(8, 7, 6, 5, 4)      # => 8 7 6 5 4 + no warning (valid set assumed)
#' comp_complete_prob_set(8, .8, NA, .7, NA)  # => 8.0 0.8 0.2 0.7 0.3 + no warning (sic)
#' comp_complete_prob_set(8, 2, NA, 3, NA)    # => 8 2 NA 3 NA + no warning (sic)
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
  cur_sens.mirt <- comp_comp_pair(sens, mirt)
  sens <- cur_sens.mirt[1]  # 1st argument
  mirt <- cur_sens.mirt[2]  # 2nd argument

  ## (3) Compute missing spec or fart (if applicable):
  cur_spec.fart <- comp_comp_pair(spec, fart)
  spec <- cur_spec.fart[1]  # 1st argument
  fart <- cur_spec.fart[2]  # 2nd argument

  ## (4) Assemble all probabilities:
  prob_quintet <- c(prev, sens, mirt, spec, fart)

  ## (5) return vector:
  return(prob_quintet)

}

## Check:

#   comp_complete_prob_set(1, .8, NA, .7, NA)  # => 1.0 0.8 0.2 0.7 0.3
#   comp_complete_prob_set(1, NA, .8, NA, .4)  # => 1.0 0.2 0.8 0.6 0.4
#
#   # Watch out for:
#   comp_complete_prob_set(8)                  # => 8 NA NA NA NA       + warnings that comp_comp_pair needs 1 argument
#   comp_complete_prob_set(8, 7, 6, 5, 4)      # => 8 7 6 5 4           + no warning (as valid set assumed)!
#   comp_complete_prob_set(8, .8, NA, .7, NA)  # => 8.0 0.8 0.2 0.7 0.3 + no warning (as valid set assumed)!
#   comp_complete_prob_set(8, 2, NA, 3, NA)    # => 8 2 NA 3 NA         + no warning (as valid set assumed)!



## Compute derived probabilities: ----------------






## (2) by decision: Compute probabilities of decisions/cases from probabilities: ------------------


## (a) ppod = proportion of positive decisions (PR) from probabilities: ------

#' Compute the proportion of positive decisions (ppod) from probabilities.
#'
#' \code{comp_ppod} computes the proportion of positive decisions \code{\link{ppod}}
#' from 3 essential probabilities
#' \code{\link{prev}}, \code{\link{sens}}, and \code{\link{spec}}.
#'
#' \code{comp_ppod} uses probabilities (not frequencies) as
#' inputs and returns a proportion (probability)
#' without rounding.
#'
#' Definition: \code{ppod} is
#' proportion (or probability) of positive decisions:
#'
#' \code{ppod = dec_pos/N = (hi + fa)/(hi + mi + fa + cr)}
#'
#' Values range from 0 (only negative decisions)
#' to 1 (only positive decisions).
#'
#' Importantly, positive decisions \code{\link{dec_pos}}
#' are not necessarily correct decisions \code{\link{dec_cor}}.
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
#' @return The proportion of positive decisions \code{\link{ppod}} as a probability.
#' A warning is provided for NaN values.
#'
#' @examples
#' # (1) ways to work:
#' comp_ppod(.10, .200, .300)  # => ppod = 0.65
#' comp_ppod(.50, .333, .666)  # => ppod = 0.3335
#'
#' # (2) watch out for vectors:
#' prev <- seq(0, 1, .1)
#' comp_ppod(prev, .8, .5)  # => 0.50 0.53 0.56 0.59 0.62 0.65 0.68 0.71 0.74 0.77 0.80
#' comp_ppod(prev,  0,  1)  # => 0 0 0 0 0 0 0 0 0 0 0
#'
#' # (3) watch out for extreme values:
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

  ## Definition: ppod = dec_pos / N  =  (hi + fa) / (hi + mi + fa + cr)

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


## for extreme values:
## \code{\link{is_extreme_prob_set}} verifies extreme cases;

## (b) Positive predictive value (PPV) ------------

## comp_PPV      (a) PPV from probabilities: ------

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
#' comp_PPV(prev = 1, sens = 0, spec = .5)  # => NaN, only mi: hi = 0 and fa = 0: PPV = 0/0 = NaN
#' is_extreme_prob_set(prev = 1, sens = 0, spec = .5)  # => verifies extreme cases
#'
#' comp_PPV(prev = 0, sens = .5, spec = 1)  # => NaN, only cr: hi = 0 and fa = 0: PPV = 0/0 = NaN
#' is_extreme_prob_set(prev = 0, sens = .5, spec = 1)  # => verifies extreme cases
#'
#' comp_PPV(prev = .5, sens = 0, spec = 1)  # => NaN, only cr: hi = 0 and fa = 0: PPV = 0/0 = NaN
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

  ## Definition: PPV = hi / dec_pos  =  hi / (hi + fa)

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

## (1) Ways to work:
# comp_PPV(.50, .500, .500)  # => PPV = 0.5
# comp_PPV(.50, .333, .666)  # => PPV = 0.499

## (2) Watch out for vectors:
# prev <- seq(0, 1, .1)
# comp_PPV(prev, .5, .5)  # => without NaN values
# comp_PPV(prev,  0,  1)  # => with NaN values

## (3) Watch out for extreme values:
# comp_PPV(prev = 1, sens = 0, spec = .5)  # => NaN, only mi: hi = 0 and fa = 0: PPV = 0/0 = NaN
# is_extreme_prob_set(prev = 1, sens = 0, spec = .5)  # => verifies extreme cases
#
# comp_PPV(prev = 0, sens = .5, spec = 1)  # => NaN, only cr: hi = 0 and fa = 0: PPV = 0/0 = NaN
# is_extreme_prob_set(prev = 0, sens = .5, spec = 1)  # => verifies extreme cases
#
# comp_PPV(prev = .5, sens = 0, spec = 1)  # => NaN, only cr: hi = 0 and fa = 0: PPV = 0/0 = NaN
# is_extreme_prob_set(prev = .5, sens = 0, spec = 1)  # => verifies extreme cases




## (+) False discovery/detection rate (FDR = complement of PPV): ------

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


## comp_FDR:     (a) from basic probabilities ------

comp_FDR <- function(prev, sens, spec) {

  FDR <- NA # initialize
  # PPV <- NA

  ## ToDo: Verify (is_valid_prob_set(prev, sens, mirt, spec, fart)) { ... }

  ## Definition: FDR = fa / dec_pos  =  fa / (hi + fa)

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


## comp_FDR_PPV: (b) as complement FDR = 1 - PPV ------

comp_FDR_PPV <- function(PPV) {

  FDR <- NA  # initialize

  FDR <- comp_complement(PPV)  # FDR is the complement of PPV

  return(FDR)
}


## (c) Negative predictive value (NPV) ------------

## comp_NPV:     (a) from probabilities ------

#' Compute a decision's negative predictive value (NPV) from probabilities.
#'
#' \code{comp_NPV} computes the negative predictive value \code{\link{NPV}}
#' from 3 essential probabilities
#' \code{\link{prev}}, \code{\link{sens}}, and \code{\link{spec}}.
#'
#' \code{comp_NPV} uses probabilities (not frequencies)
#' and does not round results.
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
#' @return The negative predictive value \code{\link{NPV}} as a probability.
#' A warning is provided for NaN values.
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
#' comp_NPV(1, 1, 1)   # => NaN, as cr = 0 and mi = 0: 0/0
#' comp_NPV(1, 1, 0)   # => NaN, as cr = 0 and mi = 0: 0/0
#' comp_NPV(.5, sens = 1, spec = 0)  # => NaN, no dec_neg cases:  NPV = 0/0 = NaN
#' is_extreme_prob_set(.5, sens = 1, spec = 0)  # => verifies extreme cases
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

  ## Definition: NPV = cr / dec_neg  =  cr / (mi + cr)

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
# comp_NPV(.5, sens = 1, spec = 0)  # => NaN, no dec_neg cases:  NPV = 0/0 = NaN
# is_extreme_prob_set(.5, sens = 1, spec = 0)  # => verifies extreme cases

## \code{\link{is_extreme_prob_set}} verifies extreme cases



## (+) False omission rate (FOR = complement of NPV) ------------------------

## comp_FOR:     (a) from basic probabilities -------

#' Compute a decision's false omission rate (FOR) from probabilities.
#'
#' \code{comp_FOR} computes the false omission rate \code{\link{FOR}}
#' from 3 essential probabilities
#' \code{\link{prev}}, \code{\link{sens}}, and \code{\link{spec}}.
#'
#' \code{comp_FOR} uses probabilities (not frequencies)
#' and does not round results.
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
#' @return The false omission rate \code{\link{FOR}} as a probability.
#' A warning is provided for NaN values.
#'
#' @examples
#' # (1) Ways to work:
#' comp_FOR(.50, .500, .500)  # => FOR = 0.5    = (1 - NPV)
#' comp_FOR(.50, .333, .666)  # => FOR = 0.5004 = (1 - NPV)
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

  ## Definition: FOR =  mi / dec_neg  =  mi / (cr + mi)

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
# comp_FOR(.5, sens = 1, spec = 0)  # => NaN, no dec_neg cases:  NPV = 0/0 = NaN
# is_extreme_prob_set(prev = .5, sens = 1, spec = 0)  # => verifies extreme cases


## comp_FOR_NPV: (b) FOR = 1 - NPV ------

comp_FOR_NPV <- function(NPV) {

  FOR <- NA  # initialize

  FOR <- comp_complement(NPV)  # FOR is the complement of NPV

  return(FOR)

}







## (3) by accuracy: Compute probability of correct decisions from probabilities: ----------


## comp_acc: Documentation --------

#' Compute overall accuracy (acc) from probabilities.
#'
#' \code{comp_acc} computes overall accuracy \code{\link{acc}}
#' from 3 essential probabilities
#' \code{\link{prev}}, \code{\link{sens}}, and \code{\link{spec}}.
#'
#' \code{comp_acc} uses probabilities (not frequencies) as
#' inputs and returns an exact probability (proportion)
#' without rounding.
#'
#' Understanding the probability \code{\link{acc}}:
#'
#' \itemize{
#'
#'   \item Definition:
#'   \code{\link{acc}} is the (non-conditional) probability:
#'
#'   \code{acc = p(dec_cor) = dec_cor/N}
#'
#'   or the base rate (or baseline probability)
#'   of a decision being correct, but not necessarily positive.
#'
#'   \code{\link{acc}} values range
#'   from 0 (no correct decision/prediction)
#'   to 1 (perfect decision/prediction).
#'
#'   \item Computation: \code{\link{acc}} can be computed in 2 ways:
#'
#'    (a) from \code{\link{prob}}: \code{acc = (prev x sens) + [(1 - prev) x spec]}
#'
#'    (b) from \code{\link{freq}}: \code{acc = dec_cor/N = (hi + cr)/(hi + mi + fa + cr)}
#'
#'    When frequencies in \code{\link{freq}} are not rounded, (b) coincides with (a).
#'
#'   \item Perspective:
#'   \code{\link{acc}} classifies a population of \code{\link{N}} individuals
#'   by accuracy/correspondence (\code{acc = dec_cor/N}).
#'
#'   \code{\link{acc}} is the "by accuracy" or "by correspondence" counterpart
#'   to \code{\link{prev}} (which adopts a "by condition" perspective) and
#'   to \code{\link{ppod}} (which adopts a "by decision" perspective).
#'
#'   \item Alternative names of \code{\link{acc}}:
#'   base rate of correct decisions,
#'   non-erroneous cases
#'
#'   \item In terms of frequencies,
#'   \code{\link{acc}} is the ratio of
#'   \code{\link{dec_cor}} (i.e., \code{\link{hi} + \link{cr}})
#'   divided by \code{\link{N}} (i.e.,
#'   \code{\link{hi} + \link{mi}} + \code{\link{fa} + \link{cr}}):
#'
#'   \code{acc = dec_cor/N = (hi + cr)/(hi + mi + fa + cr)}
#'
#'   \item Dependencies:
#'   \code{\link{acc}} is a feature of both the environment (true condition) and
#'   of the decision process or diagnostic procedure. It reflects the
#'   correspondence of decisions to conditions.
#'
#' }
#'
#' See \code{\link{accu}} for other accuracy metrics
#' and several possible interpretations of accuracy.
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
#' @return Overall accuracy \code{\link{acc}} as a probability (proportion).
#' A warning is provided for NaN values.
#'
#' See \code{\link{acc}} for definition
#' and \code{\link{accu}} for other accuracy metrics.
#' \code{\link{comp_accu_freq}} and \code{\link{comp_accu_prob}}
#' compute accuracy metrics from frequencies and probabilities.
#'
#' @examples
#' # ways to work:
#' comp_acc(.10, .200, .300)  # => acc = 0.29
#' comp_acc(.50, .333, .666)  # => acc = 0.4995
#'
#' # watch out for vectors:
#' prev.range <- seq(0, 1, by = .1)
#' comp_acc(prev.range, .5, .5)  # => 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5
#'
#' # watch out for extreme values:
#' comp_acc(1, 1, 1)  #  => 1
#' comp_acc(1, 1, 0)  #  => 1
#'
#' comp_acc(1, 0, 1)  #  => 0
#' comp_acc(1, 0, 0)  #  => 0
#'
#' comp_acc(0, 1, 1)  #  => 1
#' comp_acc(0, 1, 0)  #  => 0
#'
#' comp_acc(0, 0, 1)  #  => 1
#' comp_acc(0, 0, 0)  #  => 0
#'
#' @family functions computing probabilities
#' @family metrics
#'
#' @seealso
#' \code{\link{acc}} defines accuracy as a probability;
#' \code{\link{accu}} lists all accuracy metrics;
#' \code{\link{comp_accu_prob}} computes exact accuracy metrics from probabilities;
#' \code{\link{comp_accu_freq}} computes accuracy metrics from frequencies;
#' \code{\link{comp_sens}} and \code{\link{comp_PPV}} compute related probabilities;
#' \code{\link{is_extreme_prob_set}} verifies extreme cases;
#' \code{\link{comp_complement}} computes a probability's complement;
#' \code{\link{is_complement}} verifies probability complements;
#' \code{\link{comp_prob}} computes current probability information;
#' \code{\link{prob}} contains current probability information;
#' \code{\link{is_prob}} verifies probabilities.
#'
#' @export

## comp_acc: Definition --------

comp_acc <- function(prev, sens, spec) {

  acc <- NA  # initialize

  ## ToDo: Add condition
  ## if (is_valid_prob_set(prev, sens, mirt, spec, fart)) { ... }

  ## Definition: acc = dec_cor / N  =  (hi + cr) / (hi + mi + fa + cr)
  ##             but from exact (not rounded) frequencies!

  ## Computation of 4 freq (from prob, without rounding):
  hi <- prev * sens
  mi <- prev * (1 - sens)
  cr <- (1 - prev) * spec
  fa <- (1 - prev) * (1 - spec)

  acc <- (hi + cr) / (hi + mi + fa + cr)

  ## Print a warning if NaN:
  if (any(is.nan(acc))) {
    warning("acc is NaN.")
  }

  return(acc)
}

## Check: ----

# # Basics:
# comp_acc(1, 1, 1)  #  => 1
# comp_acc(1, 1, 0)  #  => 1
#
# comp_acc(1, 0, 1)  #  => 0
# comp_acc(1, 0, 0)  #  => 0
#
# comp_acc(0, 1, 1)  #  => 1
# comp_acc(0, 1, 0)  #  => 0
#
# comp_acc(0, 0, 1)  #  => 1
# comp_acc(0, 0, 0)  #  => 0
#
# # Vectors:
# prev.range <- seq(0, 1, by = .1)
# comp_acc(prev.range, .5, .5)

## for extreme values:
## \code{\link{is_extreme_prob_set}} verifies extreme cases;



## comp_err: Documentation --------

#' Compute overall error rate (err) from probabilities.
#'
#' \code{comp_err} computes overall error rate \code{\link{err}}
#' from 3 essential probabilities
#' \code{\link{prev}}, \code{\link{sens}}, and \code{\link{spec}}.
#'
#' \code{comp_err} uses \code{\link{comp_acc}} to
#' compute \code{\link{err}} as the
#' complement of \code{\link{acc}}:
#'
#' \code{err = 1 - acc}
#'
#' See \code{\link{comp_acc}} and \code{\link{acc}}
#' for further details and
#' \code{\link{accu}} for other accuracy metrics
#' and several possible interpretations of accuracy.
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
#' @return Overall error rate \code{\link{err}} as a probability (proportion).
#' A warning is provided for NaN values.
#'
#' @examples
#' # ways to work:
#' comp_err(.10, .200, .300)  # => err = 0.71
#' comp_err(.50, .333, .666)  # => err = 0.5005
#'
#' # watch out for vectors:
#' prev.range <- seq(0, 1, by = .1)
#' comp_err(prev.range, .5, .5)  # => 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5
#'
#' # watch out for extreme values:
#' comp_err(1, 1, 1)  #  => 0
#' comp_err(1, 1, 0)  #  => 0
#'
#' comp_err(1, 0, 1)  #  => 1
#' comp_err(1, 0, 0)  #  => 1
#'
#' comp_err(0, 1, 1)  #  => 0
#' comp_err(0, 1, 0)  #  => 1
#'
#' comp_err(0, 0, 1)  #  => 0
#' comp_err(0, 0, 0)  #  => 1
#'
#'
#' @family functions computing probabilities
#' @family metrics
#'
#' @seealso
#' \code{\link{comp_acc}} computes overall accuracy \code{\link{acc}} from probabilities;
#' \code{\link{accu}} lists all accuracy metrics;
#' \code{\link{comp_accu_prob}} computes exact accuracy metrics from probabilities;
#' \code{\link{comp_accu_freq}} computes accuracy metrics from frequencies;
#' \code{\link{comp_sens}} and \code{\link{comp_PPV}} compute related probabilities;
#' \code{\link{is_extreme_prob_set}} verifies extreme cases;
#' \code{\link{comp_complement}} computes a probability's complement;
#' \code{\link{is_complement}} verifies probability complements;
#' \code{\link{comp_prob}} computes current probability information;
#' \code{\link{prob}} contains current probability information;
#' \code{\link{is_prob}} verifies probabilities.
#'
#' @export

## comp_err: Definition --------

comp_err <- function(prev, sens, spec) {

  err <- NA  # initialize
  acc <- NA

  ## ToDo: Add condition
  ## if (is_valid_prob_set(prev, sens, mirt, spec, fart)) { ... }

  ## Use comp_acc to compute accuracy:
  acc <- comp_acc(prev, sens, spec)

  ## err is the complement of acc:
  err <- (1 - acc)

  ## Print a warning if NaN:
  if (any(is.nan(err))) {
    warning("err is NaN.")
  }

  return(err)
}

## Check: ------

# # ways to work:
# comp_err(.10, .200, .300)  # => err = 0.71
# comp_err(.50, .333, .666)  # => err = 0.5005
#
# # watch out for vectors:
# prev.range <- seq(0, 1, by = .1)
# comp_err(prev.range, .5, .5)  # => 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5
#
# # watch out for extreme values:
# comp_err(1, 1, 1)  #  => 0
# comp_err(1, 1, 0)  #  => 0
#
# comp_err(1, 0, 1)  #  => 1
# comp_err(1, 0, 0)  #  => 1
#
# comp_err(0, 1, 1)  #  => 0
# comp_err(0, 1, 0)  #  => 1
#
# comp_err(0, 0, 1)  #  => 0
# comp_err(0, 0, 0)  #  => 1



## (4) Compute prob of freq/prob by name (fname/pname) from prob: ----------

## comp_prob_fname: Compute exact p value of a freq "fname" from prob: ------

comp_prob_fname <- function(fname, cur_prob = prob) {

  # Compute exact probability value p of a frequency (named by fname)
  # from the current prob values cur_prob:

  n <- length(fname)  # fname can be a vector of n freq names
  p <- rep(NA, n)  # initialize as vector

  for (i in 1:n) {  # loop through n elements of fname:

    # Consider fname for all frequencies in freq:
    if (tolower(fname[i]) == "n") { p[i] <- 1 }

    if (tolower(fname[i]) == "cond_true")  { p[i] <- cur_prob$prev }
    if (tolower(fname[i]) == "cond_false") { p[i] <- (1 - cur_prob$prev) }

    if (tolower(fname[i]) == "dec_pos") { p[i] <- cur_prob$ppod }
    if (tolower(fname[i]) == "dec_neg") { p[i] <- (1 - cur_prob$ppod) }

    if (tolower(fname[i]) == "dec_cor") { p[i] <- cur_prob$acc }
    if (tolower(fname[i]) == "dec_err") { p[i] <- (1 - cur_prob$acc) }

    if (tolower(fname[i]) == "hi")  { p[i] <- cur_prob$prev * cur_prob$sens }
    if (tolower(fname[i]) == "mi")  { p[i] <- cur_prob$prev * (1 - cur_prob$sens) }
    if (tolower(fname[i]) == "cr")  { p[i] <- (1 - cur_prob$prev) * cur_prob$spec }
    if (tolower(fname[i]) == "fa")  { p[i] <- (1 - cur_prob$prev) * (1 - cur_prob$spec) }

  } # for loop

  return(as.numeric(p))

}

## Check: ----
# comp_prob_fname("hi")
#
## Multiple freq names (fname as vector):
# comp_prob_fname(c("hi", "mi", "fa", "cr"))
# comp_prob_fname(c("hi", "mi", "cond_true"))
#
## Verify that (sum == 1) are TRUE:
# sum(comp_prob_fname(c("cond_true", "cond_false"))) == 1
# sum(comp_prob_fname(c("dec_pos", "dec_neg"))) == 1
# sum(comp_prob_fname(c("dec_cor", "dec_err"))) == 1
# sum(comp_prob_fname(c("hi", "mi", "fa", "cr"))) == 1



## comp_prob_pname: Get or compute the exact probability by name "pname" from current prob: ------

comp_prob_pname <- function(pname, cur_prob = prob) {

  n <- length(pname)  # pname can be a vector of n prob names
  p <- rep(NA, n)     # initialize as vector

  for (i in 1:n) {  # loop through n elements of pname:

    if (is.null(pname[i]) || is.na(pname[i])) {

      p[i] <- NA

    } else {  # 2 main cases:

      # (A) pname corresponds to named prob in cur_prob:
      if (tolower(pname[i]) %in% tolower(names(cur_prob))) {

        # p_lbl <- i  # initialize to pname

        # Derive current value corresponding to cur_prob:
        ix <- which(tolower(names(cur_prob)) == tolower(pname[i]))  # index in cur_prob

        # Value of probability in cur_prob:
        p[i] <- cur_prob[ix]

        # Type of probability:
        # p_type <- comp_prob_type(pname)  # toDo: helper function (to be defined in init_prob_num.R)

      } # if (i-th pname %in% (names(cur_prob)))...

      # (B) Special cases:
      if (tolower(pname[i]) == "cprev") {  # if complement of prevalence:
        p[i] <- (1 - cur_prob$prev)
      }

      if (tolower(pname[i]) == "cppod" || tolower(pname[i]) == "pned") {  # if complement of ppod:
        p[i] <- (1 - cur_prob$ppod)
      }

      # Accuracy (as probability):
      # 2 unconditional probabilities: overall accuracy acc + error rate err:
      if (tolower(pname[i]) == "acc") { p[i] <- cur_prob$acc }  # OR: accu$acc
      if (tolower(pname[i]) == "cor") { p[i] <- cur_prob$acc }  # OR: accu$acc
      if (tolower(pname[i]) == "err") { p[i] <- (1 - cur_prob$acc) }  # OR: (1 - accu$acc)

      # 4 conditional probabilities:
      if (tolower(pname[i]) == "acc_hi") { p[i] <- (cur_prob$prev * cur_prob$sens)/(cur_prob$acc) }          # prob of hi/dec_cor
      # if (tolower(pname[i]) == "acc_cr") { p[i] <- ((1 - cur_prob$prev) * cur_prob$spec)/(cur_prob$acc) }  # prob of cr/dec_cor computed from scratch OR:
      if (tolower(pname[i]) == "acc_cr") { p[i] <- (1 - (cur_prob$prev * cur_prob$sens)/(cur_prob$acc)) }    # prob of cr/dec_cor as complement of hi/dec_cor

      if (tolower(pname[i]) == "err_mi") { p[i] <- (cur_prob$prev * (1 - cur_prob$sens))/(1 - cur_prob$acc) }          # prob of mi/dec_err
      # if (tolower(pname[i]) == "err_fa") { p[i] <- ((1 - cur_prob$prev) * (1 - cur_prob$spec))/(1 - cur_prob$acc) }  # prob of fa/dec_err computed from scratch OR:
      if (tolower(pname[i]) == "err_fa") { p[i] <- (1 - (cur_prob$prev * (1 - cur_prob$sens))/(1 - cur_prob$acc)) }    # prob of fa/dec_err computed as complement of mi/dec_err

    } # if (is.na(pname[i]))...

  } # loop

  return(as.numeric(p))

}

## Check: ----

# comp_prob_pname("sens") # => OK
# comp_prob_pname("hi")   # => NA (as "hi" is freq)
#
## Multiple prob names (pname as vector):
# comp_prob_pname(c("prev", "sens", "spec"))
# comp_prob_pname(c("prev", "prev", "prev"))
#
## Missing values:
# comp_prob_pname(NA)
# comp_prob_pname(NULL)  # => NA
# comp_prob_pname(c("prev", "sens", NA, "spec", NULL))  # => drops final NULL
#
## Verify that (sum == 1) are TRUE:
# sum(comp_prob_pname(c("prev", "cprev"))) == 1
# sum(comp_prob_pname(c("sens", "mirt"))) == 1
# sum(comp_prob_pname(c("spec", "fart"))) == 1
#
# sum(comp_prob_pname(c("ppod", "pned"))) == 1
# sum(comp_prob_pname(c("PPV", "FDR"))) == 1
# sum(comp_prob_pname(c("NPV", "FOR"))) == 1
#
# sum(comp_prob_pname(c("acc", "err"))) == 1
# # if computed as complements:
# sum(comp_prob_pname(c("acc_hi", "acc_cr"))) == 1
# sum(comp_prob_pname(c("err_mi", "err_fa"))) == 1
# # if computed from scratch:
# all.equal(sum(comp_prob_pname(c("acc_hi", "acc_cr"))), 1)
# all.equal(sum(comp_prob_pname(c("err_mi", "err_fa"))), 1)




## (5): Compute predictive values from frequencies (various versions): ------

## ToDo: Add alternative ways to compute probabilities
##       from frequencies (based on different elements of freq)!

## Moved to a separate file: comp_prob_freq.R !!!


## comp_prob_matrix: Compute some metric for an entire matrix of values ------
## (given sens and spec are given as vectors)

## Compute some metric for an entire matrix of values
## (when sens and spec are given as vectors)

## Metrics currently accepted:
##   1 - PPV
##   2 - NPV
##   3 - ppod
##   4 - acc

comp_prob_matrix <- function(prev, sens, spec,
                             metric = "PPV",  # metric to be computed: "PPV", "NPV", "ppod", "acc".
                             nan_adjust = FALSE) {

  # Initialize matrix (as df):
  n_rows <- length(sens)
  n_cols <- length(spec)
  matrix <- as.data.frame(matrix(NA,
                                 nrow = n_rows,
                                 ncol = n_cols))
  names(matrix) <- spec  # column names

  ## Loop through rows and columns of matrix:
  for (row in 1:n_rows) {    # row = sens
    for (col in 1:n_cols) {  # col = spec

      cell_val <- NA  # initialize

      cur_sens <- sens[row]
      cur_spec <- spec[col]

      ## (A) metric == PPV: ----------
      if (metric == "PPV") {

        ## Beware of cases in which PPV or NPV are NaN:

        ## (1) PPV is NaN if:
        ##     (a)  (prev = 1) & (sens = 0)
        ##     (b)  (prev = 0) & (spec = 1)
        ##     (c)  (sens = 0) & (spec = 1)

        if (nan_adjust) {  ## Hack fix:

          eps <- 10^-9    # some very small value

          ## Catch and adjust in 3 cases:

          ## (1a) (prev = 1) & (sens = 0): Only mi ==> hi = 0 and fa = 0:  PPV = 0/0 = NaN
          if ((prev == 1) && (cur_sens == 0)) {

            warning("Adjusting for extreme case (PPV:a): (prev = 1) & (sens = 0)!")

            ## Adjustment (to prevent NaN):
            cur_sens <- (cur_sens + eps)  # adjust upwards

          }

          ## (1b) (prev = 0) & (spec = 1): Only cr ==> hi = 0 and fa = 0:  PPV = 0/0 = NaN
          if ((prev == 0) && (cur_spec == 1)) {

            warning("Adjusting for extreme case (PPV:b): (prev = 0) & (spec = 1)!")

            ## Adjustment (to prevent NaN):
            cur_spec <- (cur_spec - eps)  # adjust downwards

          }

          ## (1c) (sens = 0) & (spec = 1): Only mi + cr ==> hi = 0 and fa = 0:  PPV = 0/0 = NaN
          if ((cur_sens == 0) && (cur_spec == 1)) {

            warning("Adjusting for extreme case (PPV:c): (sens = 0) & (spec = 1)!")

            ## Adjustment (to prevent NaN):
            cur_sens <- (cur_sens + eps)  # adjust upwards
            cur_spec <- (cur_spec - eps)  # adjust downwards

          }

        } # if (nan_adjust)...

        ## (2) Compute PPV:
        cell_val <- comp_PPV(prev, cur_sens, cur_spec)

      } # if (metric == "PPV")...

      ## (B) metric == NPV: ----------
      if (metric == "NPV") {

        ## Beware of cases in which PPV or NPV are NaN:

        ## (2) NPV is NaN if:
        ##     (a)  (prev = 1) & (sens = 1)
        ##     (b)  (prev = 1) & (sens = 0)
        ##     (c)  (sens = 1) & (spec = 0)

        if (nan_adjust) {  ## Hack fix:

          eps <- 10^-9    # some very small value

          ## Catch and adjust in 3 cases:

          ## (2a) (prev = 1) & (sens = 1): NPV = 0/0 = NaN
          if ((prev == 1) && (cur_sens == 1)) {

            message("Adjusting for extreme case (NPV:a): (prev = 1) & (sens = 1)!")

            ## Adjustment (to prevent NaN):
            cur_sens <- (cur_sens - eps)  # adjust downwards

          }

          ## (2b) (prev = 1) & (spec = 0): NPV = 0/0 = NaN
          if ((prev == 1) && (cur_spec == 0)) {

            message("Adjusting for extreme case (NPV:b): (prev = 1) & (spec = 0)!")

            ## Adjustment (to prevent NaN):
            cur_spec <- (cur_spec + eps)  # adjust upwards

          }

          ## (2c) (sens = 1) & (spec = 0): NPV = 0/0 = NaN
          if ((cur_sens == 1) && (cur_spec == 0)) {

            message("Adjusting for extreme case (NPV:c): (sens = 1) & (spec = 0)!")

            ## Adjustment (to prevent NaN):
            cur_sens <- (cur_sens - eps)  # adjust downwards
            cur_spec <- (cur_spec + eps)  # adjust upwards

          }
        } #  if (nan_adjust)...

        ## (2) Compute NPV:
        cell_val <- comp_NPV(prev, cur_sens, cur_spec)  # compute NPV

      } # if (metric == "NPV")...

      ## (C) metric == ppod: ----------

      if (metric == "ppod") {

        ## Beware of cases in which ppod is NaN:

        ## (1) ppod is NaN if:
        ##     (a)  (prev = 0) & (N = 0) ???

        ## no adjustments made for ppod

        ## (2) Compute ppod:
        cell_val <- comp_ppod(prev, cur_sens, cur_spec)

      } # if (metric == "ppod")...

      ## (D) metric == acc: ----------

      if (metric == "acc") {

        ## Beware of cases in which acc is NaN:

        ## (1) acc is NaN if:
        ##     (a)  (N = 0) ???

        ## no adjustments made for acc

        ## (2) Compute acc:
        cell_val <- comp_acc(prev, cur_sens, cur_spec)

      } # if (metric == "acc")...


      ## (*) Store current value in matrix:
      matrix[row, col] <- cell_val

    } # for (col ...)
  } # for (row ...)

  return(matrix)

}


## Check:

# ## (a) Square matrices:
# sens_seq <- seq(0, 1, by = .10)  # sens range from 0 to 1
# spec_seq <- seq(0, 1, by = .10)  # spec range from 0 to 1
# #
# ## Contrast PPV without and with NaN adjustment:
# comp_prob_matrix(prev = .33, sens_seq, spec_seq, metric = "PPV", nan_adjust = FALSE)
# comp_prob_matrix(prev = .33, sens_seq, spec_seq, metric = "PPV", nan_adjust = TRUE)
# #
# ## Contrast NPV without and with NaN adjustment:
# comp_prob_matrix(prev = .33, sens_seq, spec_seq, metric = "NPV", nan_adjust = FALSE)
# comp_prob_matrix(prev = .33, sens_seq, spec_seq, metric = "NPV", nan_adjust = TRUE)
# #
# ## Other metrics:
# # ppod:
# comp_prob_matrix(prev = 0, sens_seq, spec_seq, metric = "ppod")
# comp_prob_matrix(prev = 1, sens_seq, spec_seq, metric = "ppod")
# # acc:
# comp_prob_matrix(prev = 0, sens_seq, spec_seq, metric = "acc")
# comp_prob_matrix(prev = 1, sens_seq, spec_seq, metric = "acc")
#
# ## (b) Non-square matrices:
# sens_seq <- seq(.1, .5, by = .10)
# spec_seq <- seq(.6, .9, by = .10)
#
# ## Contrast PPV without and with NaN adjustment:
# comp_prob_matrix(prev = .33, sens_seq, spec_seq, metric = "PPV", nan_adjust = FALSE)
# comp_prob_matrix(prev = .33, sens_seq, spec_seq, metric = "PPV", nan_adjust = TRUE)
# #
# ## Contrast NPV without and with NaN adjustment:
# comp_prob_matrix(prev = .33, sens_seq, spec_seq, metric = "NPV", nan_adjust = FALSE)
# comp_prob_matrix(prev = .33, sens_seq, spec_seq, metric = "NPV", nan_adjust = TRUE)


## (*) Done: ----------

## - Moved comp_acc from comp_accu.R to comp_prob_prob.R
##   (as it computes a probability from 3 essential prob).   [2018 09 04]

## - Clean up code.  [2018 08 28]

## (+) ToDo: ----------

## - Add documentation.
##   Document comp_PPV, comp_NPV, ... etc.

## - Allow using fart & mirt in addition to sens & spec in all functions
##   defined above

## - Add alternative ways to compute probabilities
##   from frequencies (based on various elements of freq)!
##   - Compute alternative prob from freq with
##     a. N of dec_pos (rather than N of fa) and
##     b. N of dec_neg (rather than N of mi) provided.

## - Compute basic parameters (probabilities and frequencies)
##   from MIX of existing probabilities and frequencies!

## eof. ------------------------------------------
