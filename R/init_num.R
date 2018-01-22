## init_num.R | riskyR
## 2018 01 22
## -----------------------------------------------
## Initialize a list of basic input parameters (num)
## that contains all numeric user inputs:
## -----------------------------------------------

## (1) Basic parameters:
## -----------------------------------------------
## (a) prev:

#' The prevalence (baseline probability) of a condition.
#'
#' \code{prev} defines a condition's prevalence value:
#' The baseline probability of the condition being \code{TRUE}.
#'
#' Understanding or obtaining the prevalence value \code{prev}:
#'
#' \itemize{
#'
#'   \item Definition:
#'   \code{prev} is the (non-conditional) probability:
#'
#'   \code{prev = p(condition = TRUE)}
#'
#'   or the (baseline) probability of the condition's occurrence.
#'
#'   \item Alternative names:
#'   \code{baseline}, proportion affected;
#'   to be distinguished from \emph{incidence rate} (i.e., new cases within a certain time period)
#'
#'   \item In natural frequencies,
#'   \code{prev} is the ratio of individuals for which
#'   \code{condition = TRUE} divided by the
#'   number \code{\link{N}} of all individuals
#'   in the population:
#'
#'   \code{prev = n(condition = TRUE) / \link{N}}
#'
#'   \item \code{prev} is a feature of the population
#'   and condition, but independent of the decision process
#'   or diagnostic procedure.
#'
#'   \item The value of \code{prev} does not depend
#'   on features of the decision process or diagnostic procedure.
#'   However, \code{prev} must be taken into account when
#'   computing the conditional probabilities
#'   \code{\link{sens}}, \code{\link{spec}}, \code{\link{fart}},
#'   \code{\link{ppv}}, and \code{\link{npv}}
#'   (as derived parameters).
#'
#' }
#'
#' @aliases
#' baseline
#'
#' @examples
#' prev <- .10     # => sets a prevalence value of 10%
#' prev <- 10/100  # => (condition = TRUE) for 10 out of 100 individuals
#' is_prob(prev)   # => TRUE (as prev is a probability)
#'
#' @family basic parameters
#'
#' @seealso
#' \code{\link{num}} contains basic numeric variables;
#' \code{\link{init_num}} initializes basic numeric variables;
#' \code{\link{is_prob}} verifies probability inputs;
#' \code{\link{comp_prob}} computes derived probabilities;
#' \code{\link{comp_freq}} computes natural frequencies from probabilities
#'
#' @references
#' Consult \href{https://en.wikipedia.org/wiki/Prevalence}{Wikipedia} for additional information.

prev <- .10 # default prevalence

## -----------------------------------------------
## (b) sens:

#' The sensitivity of a decision process or diagnostic procedure.
#'
#' \code{sens} defines a decision's sensitivity value:
#' The conditional probability of the decision being positive
#' if the condition is \code{TRUE}.
#'
#' Understanding or obtaining the sensitivity value \code{sens}:
#'
#' \itemize{
#'
#'   \item Definition: \code{sens} is the conditional probability
#'   for a (correct) positive decision given that
#'   the condition is \code{TRUE}:
#'
#'   \code{sens = p(decision = positive | condition = TRUE)}
#'
#'   or the probability of correctly detecting true cases
#'   (\code{condition = TRUE}).
#'
#'   \item Alternative names:
#'   \code{recall}, true positive rate (\code{TPR}), probability of detection
#'
#'   \item Relationships:
#'
#'   a. \code{sens} is the complement of the miss rate or
#'   false negative rate (\code{FNR}):
#'
#'   \code{sens = 1 - miss rate = 1 - FNR}
#'
#'   b. \code{sens} is the opposite conditional probability
#'   -- but not the complement --
#'   of the positive predictive value \code{\link{PPV}}:
#'
#'   \code{PPV = p(condition = TRUE | decision = positive)}
#'
#'   \item In natural frequencies,
#'   \code{sens} is the ratio of individuals for which
#'   \code{decision = positive} divided by the number of
#'   individuals for which \code{condition = TRUE}:
#'
#'   \code{sens = n(decision = positive) / n(condition = TRUE)}
#'
#'   \item \code{sens} is a feature of a decision process
#'   or diagnostic procedure and a measure of
#'   correct decisions (true positives).
#'
#'   However, due to being a conditional probability,
#'   the value of \code{sens} also depends on the
#'   condition's prevalence value \code{\link{prev}}.
#'
#' }
#'
#' @aliases
#' recall
#' TPR
#'
#' @references
#' Consult \href{https://en.wikipedia.org/wiki/Sensitivity_and_specificity}{Wikipedia} for additional information.
#'
#' @family basic parameters
#'
#' @seealso
#' \code{\link{num}} contains basic numeric variables;
#' \code{\link{init_num}} initializes basic numeric variables;
#' \code{\link{is_prob}} verifies probability inputs;
#' \code{\link{comp_prob}} computes derived probabilities;
#' \code{\link{comp_freq}} computes natural frequencies from probabilities
#'
#' @examples
#' sens <- .85     # => sets a sensitivity value of 85%
#' sens <- 85/100  # => (decision = positive) for 85 people out of 100 people for which (condition = TRUE)
#' is_prob(sens)   # => TRUE (as sens is a probability)

sens <- .85 # default sensitivity

## -----------------------------------------------
## (c) spec:

#' The specificity of a decision process or diagnostic procedure.
#'
#' \code{spec} defines a decision's specificity value:
#' The conditional probability of the decision being negative
#' if the condition is FALSE.
#'
#' Understanding or obtaining the specificity value \code{spec}:
#'
#' \itemize{
#'
#'   \item Definition:
#'   \code{spec} is the conditional probability
#'   for a (correct) negative decision given that
#'   the condition is \code{FALSE}:
#'
#'   \code{spec = p(decision = negative | condition = FALSE)}
#'
#'   or the probability of correctly detecting false cases
#'   (\code{condition = FALSE}).
#'
#'   \item Alternative names:
#'   true negative rate (\code{TNR})
#'
#'   \item Relationships:
#'
#'   a. \code{spec} is the complement of the
#'   false alarm rate \code{\link{fart}}:
#'
#'   \code{spec = 1 - fart}
#'
#'   b. \code{spec} is the opposite conditional probability
#'   -- but not the complement --
#'   of the negative predictive value \code{\link{NPV}}:
#'
#'   \code{NPV = p(condition = FALSE | decision = negative)}
#'
#'   \item In natural frequencies,
#'   \code{spec} is the ratio of individuals for which
#'   \code{decision = negative} divided by the number of
#'   individuals for which \code{condition = FALSE}:
#'
#'   \code{spec = n(decision = negative) / n(condition = FALSE)}
#'
#'   \item \code{spec} is a feature of a decision process
#'   or diagnostic procedure and a measure of
#'   correct decisions (true negatives).
#'
#'   However, due to being a conditional probability,
#'   the value of \code{spec} also depends on the
#'   condition's prevalence value \code{\link{prev}}.
#'
#' }
#'
#' @aliases
#' TNR
#'
#' @references
#' Consult \href{https://en.wikipedia.org/wiki/Sensitivity_and_specificity}{Wikipedia} for additional information.
#'
#' @family basic parameters
#'
#' @seealso
#' \code{\link{num}} contains basic numeric variables;
#' \code{\link{init_num}} initializes basic numeric variables;
#' \code{\link{is_prob}} verifies probability inputs;
#' \code{\link{comp_prob}} computes derived probabilities;
#' \code{\link{comp_freq}} computes natural frequencies from probabilities;
#' \code{\link{comp_spec}} computes \code{spec}
#' as the complement of \code{\link{fart}}
#'
#' @examples
#' spec <- .75     # => sets a specificity value of 75%
#' spec <- 75/100  # => (decision = negative) for 75 people out of 100 people for which (condition = FALSE)
#' is_prob(spec)   # => TRUE (as spec is a probability)

spec <- .75 # default specificity

## -----------------------------------------------
## (d) fart:

#' The false alarm rate of a decision process or diagnostic procedure.
#'
#' \code{fart} defines a decision's false alarm rate
#' or the rate of false positive decisions: The conditional probability
#' of the decision being positive if the condition is FALSE.
#'
#' Understanding or obtaining the false alarm rate \code{fart}:
#'
#' \itemize{
#'
#'   \item Definition:
#'   \code{fart} is the conditional probability
#'   for an (incorrect) positive decision given that
#'   the condition is \code{FALSE}:
#'
#'   \code{fart = p(decision = positive | condition = FALSE)}
#'
#'   or the probability of a false alarm.
#'
#'   \item Alternative names:
#'   \code{fallout}, false positive rate (\code{FPR})
#'
#'   \item Relationships:
#'
#'   \code{fart} is the complement of the
#'   specificity \code{\link{spec}}:
#'
#'   \code{fart = 1 - spec}
#'
#'   \item In natural frequencies,
#'   \code{fart} is the ratio of individuals for which
#'   \code{decision = positive} divided by the number of
#'   individuals for which \code{condition = FALSE}:
#'
#'   \code{fart = n(decision = positive) / n(condition = FALSE)}
#'
#'   \item \code{fart} is a feature of a decision process
#'   or diagnostic procedure and a measure of
#'   incorrect decisions (false positives).
#'
#'   However, due to being a conditional probability,
#'   the value of \code{fart} also depends on the
#'   condition's prevalence value \code{\link{prev}}.
#'
#' }
#'
#' @aliases
#' fallout
#' FPR
#'
#' @references
#' Consult \href{https://en.wikipedia.org/wiki/Sensitivity_and_specificity}{Wikipedia} for additional information.
#'
#' @family basic parameters
#'
#' @seealso
#' \code{\link{num}} contains basic numeric variables;
#' \code{\link{init_num}} initializes basic numeric variables;
#' \code{\link{is_prob}} verifies probability inputs;
#' \code{\link{comp_prob}} computes derived probabilities;
#' \code{\link{comp_freq}} computes natural frequencies from probabilities;
#' \code{\link{comp_fart}} computes \code{fart}
#' as the complement of \code{\link{spec}}
#'
#' @examples
#' fart <- .25     # => sets a false alarm rate of 25%
#' fart <- 25/100  # => (decision = positive) for 25 people out of 100 people for which (condition = FALSE)
#' is_prob(fart)   # => TRUE (as fart is a probability)

fart <- NA # default false alarm rate

## -----------------------------------------------
## (e) N:

#' The number of individuals in the population.
#'
#' \code{N} defines how many individuals make up
#' the current population (i.e., the overall
#' number of cases considered).
#'
#' A frequency (integer) value.
#'
#' The following relationships hold for natural
#' frequencies in a population:
#'
#' \itemize{
#'
#'   \item N = n(condition true) + n(condition false)
#'
#'   \item n(positive decisions) + n(negative decisions)
#'
#'   \item N = n(hi) + n(mi) + n(fa) + n(cr)
#' }
#'
#' The current frequency information is computed by
#' \code{\link{comp_freq}} and contained in a list
#' \code{\link{freq}}.
#'
#' If \code{N} is unknown, a suitable minimum value
#' can be computed by \code{\link{comp_min_N}}.
#'
#' @examples
#' N <- 1000   # => sets a population size of 1000
#' is_freq(N)  # => TRUE
#' is_prob(N)  # => FALSE (as N is no probability)
#'
#' @family basic parameters
#'
#' @seealso
#' \code{\link{comp_min_N}} computes a suitable
#' minimum value of \code{N} for given probabilities;
#' \code{\link{num}} contains basic numeric variables;
#' \code{\link{init_num}} initializes basic numeric variables;
#' \code{\link{freq}} contains basic frequency variables;
#' \code{\link{comp_freq}} computes natural frequencies from probabilities;
#' \code{\link{comp_prob}} computes derived probabilities

N <- 1000 # default population

## -----------------------------------------------
## (2) Basic functions on probabilities:
## Specificity (spec) is the complement of the false alarm rate (fart):

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

  # if ((spec < 0) | (spec > 1)) {
  #   warning( "Warning: spec is no probability (range from 0 to 1)." )
  # }

  fart <- NA # initialize

  if (is_prob(spec)) {
    fart <- 1 - spec # compute complement
  }

  return(fart)
}

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

  # if ((fart < 0) | (fart > 1)) {
  #   warning( "Warning: fart is no probability (range from 0 to 1)." )
  # }

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

#' Compute a probability's complement (if missing).
#'
#' \code{comp_complement} is a function that takes one or two probabilities
#' --  a specificity \code{spec} and a false alarm rate \code{fart} --
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
## (3) Determine a good number for population size N:
##     Criterion: All 4 SDT cells should have a minimal frequency of min.freq:

#' Compute a suitable minimum population size value N.
#'
#' \code{comp_min_N} is a function that computes a population size value \code{N} (an integer
#' as a power of 10) so that the frequencies of the 4 combinations of conditions and decisions
#' (i.e., the cells of the confusion table, or bottom row of boxes in the natural frequency tree)
#' reach or exceed a minimum value \code{min.freq} given the basic parameters
#' \code{prev}, \code{sens}, and \code{spec} (\code{spec = 1 - fart}).
#'
#' Using this function helps avoiding excessively small decimal values in categories
#' (esp. true positives, false negatives, false positives, and true negatives)
#' when expressing combinations of conditions and decisions as natural frequencies.
#' As values of zero (0) are tolerable, the function only increases \code{N}
#' (in powers of 10) while the current value of any frequency (cell in confusion table or
#' leaf of tree) is positive but below \code{min.freq}.
#'
#' Note that \code{\link{comp_freq}} still needs to round to avoid decimal values
#' in frequencies \code{\link{freq}}.
#'
#' @param prev The condition's prevalence value (i.e., the probability of condition being TRUE).
#' @param sens A decision's sensitivity value (i.e., the conditional probability
#' of a positive decision provided that the condition is TRUE).
#' @param spec A specificity value (i.e., the conditional probability
#' of a negative decision provided that the condition is FALSE).
#' @param min.freq The minimum frequency of each combination of
#' a condition and a decision (i.e., hits, misses, false alarms, and correct rejections).
#' Default: \code{min.freq = 1}.
#'
#' @return An integer value \code{N} (as a power of 10).
#'
#' @examples
#' comp_min_N(0, 0, 0)  # => 1
#' comp_min_N(1, 1, 1)  # => 1
#'
#' comp_min_N(1, 1, 1, min.freq = 10)  # =>  10
#' comp_min_N(1, 1, 1, min.freq = 99)  # => 100
#'
#' comp_min_N(.1, .1, .1)        # =>       100 = 10^2
#' comp_min_N(.001, .1, .1)      # =>    10 000 = 10^4
#' comp_min_N(.001, .001, .1)    # => 1 000 000 = 10^6
#' comp_min_N(.001, .001, .001)  # => 1 000 000 = 10^6
#'
#' @family functions computing frequencies
#'
#' @seealso
#' population size \code{\link{N}};
#' \code{\link{freq}} contains current frequency information;
#' \code{\link{comp_freq}} computes frequencies from probabilities

comp_min_N <- function(prev, sens, spec,
                       min.freq = 1) {

  N <- 10^0 # initialize

  ## Compute frequency of 4 SDT cases:
  n.hi <- N * prev * sens
  n.mi <- N * prev * (1 - sens)
  n.cr <- N * (1 - prev) * spec
  n.fa <- N * (1 - prev) * (1 - spec)

  ## Freq of 4 SDT cases:
  while ((n.hi > 0 & n.hi < min.freq) |
         (n.mi > 0 & n.mi < min.freq) |
         (n.cr > 0 & n.cr < min.freq) |
         (n.fa > 0 & n.fa < min.freq)) {

    N <- (N * 10) # multiply N by 10

    ## Update frequency of 4 SDT cases:
    n.hi <- N * prev * sens
    n.mi <- N * prev * (1 - sens)
    n.cr <- N * (1 - prev) * spec
    n.fa <- N * (1 - prev) * (1 - spec)

  }

  ## Return number N:
  return(N)

}

## Check:
{
  # comp_min_N(0, 0, 0)  # => 1
  # comp_min_N(1, 1, 1)  # => 1
  # comp_min_N(1, 1, 1, min.freq = 10)  # =>  10
  # comp_min_N(1, 1, 1, min.freq = 99)  # => 100
  # comp_min_N(.1, .1, .1)        # =>       100 = 10^2
  # comp_min_N(.001, .1, .1)      # =>    10 000 = 10^4
  # comp_min_N(.001, .001, .1)    # => 1 000 000 = 10^6
  # comp_min_N(.001, .001, .001)  # => 1 000 000 = 10^6
}

## -----------------------------------------------
## (4) Define and initialize num:

## The minimal set of numeric input parameters num
## consists of 3 probabilities (+ 1 complement):

## Define defaults for num:     # Description:                                     # Type of input:
num.def <- list("prev" = prev,  # prevalence in target population = p(condition TRUE)     [basic p]
                "sens" = sens,  # sensitivity = p(decision POS | condition TRUE)    [conditional p]
                "spec" = spec,  # specificity = p(decision NEG | condition FALSE)   [conditional p]
                "fart" =   NA,  # false alarm rate = 1 - spec        [optional, complement of spec]
                "N"    =  1000  # population size (N of individuals in population)  [optional freq]
                )

#' Initialize basic numeric variables.
#'
#' \code{init_num} initializes basic numeric variables to define \code{\link{num}}
#' as a list of named elements containing four basic probabilities
#' (\code{\link{prev}}, \code{\link{sens}}, \code{\link{spec}}, and \code{\link{fart}})
#' and one frequency parameter (the population size \code{\link{N}}).
#'
#' If \code{\link{spec}} is provided, its complement \code{\link{fart}} is optional.
#' If \code{\link{fart}} is provided, its complement \code{\link{spec}} is optional.
#' If no \code{\link{N}} is provided, a suitable minimum value is
#' computed by \code{\link{comp_min_N}}.
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
#' @return A list containing a valid quadruple of probabilities
#' (\code{\link{prev}}, \code{\link{sens}},
#' \code{\link{spec}}, and \code{\link{fart}})
#' and one frequency (population size \code{\link{N}}).
#'
#' @examples
#' init_num(1, 1, 1, 0, 100)  # => succeeds
#' init_num(1, 1, 0, 1, 100)  # => succeeds
#'
#' init_num(1, 1, 0, 1)           # => succeeds (with N computed)
#' init_num(1, 1, NA, 1, 100)     # => succeeds (with spec computed)
#' init_num(1, 1, 0, NA, 100)     # => succeeds (with fart computed)
#' init_num(1, 1, NA, 1)          # => succeeds (with spec and N computed)
#' init_num(1, 1, 0, NA)          # => succeeds (with fart and N computed)
#' init_num(1, 1, .51, .50, 100)  # => succeeds (as spec and fart are within tolarated range)
#'
#' init_num(prev = NA)                   # => NAs + warning that prev is not numeric
#' init_num(prev = 88)                   # => NAs + warning that prev is no probability
#' init_num(prev =  1, sens = NA)        # => NAs + warning that sens is not numeric
#' init_num(1, 1, spec = NA, fart = NA)  # => NAs + warning that spec or fart is necessary
#' init_num(1, 1, 1, 1)                  # => NAs + warning that spec and fart are not complements (in tolerated range)
#' init_num(1, 1, .52, .50, 100)         # => NAs + warning that spec and fart are not complements (in tolerated range)
#'
#' @family functions to initialize scenario settings
#'
#' @seealso
#' \code{\link{num}} contains basic parameter values;
#' \code{\link{comp_min_N}} to compute a suitable minimum population size \code{\link{N}}

init_num <- function(prev = num.def$prev, sens = num.def$sens,
                     spec = num.def$spec, fart = num.def$fart,
                     N = num.def$N) {

  ## (0) Initialize num:    # Description:                                       # Type of input:
  num <- list("prev"  = NA, # prevalence in target population = p(condition TRUE)       [basic p]
              "sens"  = NA, # sensitivity = p(decision POS | condition TRUE)      [conditional p]
              "spec"  = NA, # specificity = p(decision NEG | condition FALSE)     [conditional p]
              "fart"  = NA, # false alarm rate = 1 - spec          [optional, complement of spec]
              "N"     = NA  # population size (N of individuals in population)    [optional freq]
  )


  ## (1) Verify validity of basic input parameters:
  if (is_valid(prev, sens, spec, fart, tol = .01)){

    ## (2) Compute missing fart or spec (4th argument) value (if applicable):
    cur.spec.fart <- comp_complement(spec, fart)
    spec <- cur.spec.fart[1] # 1st argument
    fart <- cur.spec.fart[2] # 2nd argument

    ## (3) Issue a warning if probabilities describe an extreme case:
    is_extreme(prev, sens, spec, fart)  # prints a warning if TRUE

    ## (4) Compute a missing value for N (5th argument) value (if applicable):
    if (is.na(N)) {
      N <- comp_min_N(prev, sens, spec, min.freq = 1)
      warning(paste0("Unknown population size N. A suitable minimum value of N = ", N, " was computed."))
      }

    ## (5) Initialize num with current arguments:
    num$prev <- prev
    num$sens <- sens
    num$spec <- spec
    num$fart <- fart
    num$N    <- N

  } ## if(...)

  ## (6) Return the entire list num:
  return(num)

}

## Check:
{
  # init_num(1, 1, 1, 0, 100)  # => succeeds
  # init_num(1, 1, 0, 1, 100)  # => succeeds
  #
  # init_num(1, 1, 0, 1)           # => succeeds (with N computed)
  # init_num(1, 1, NA, 1, 100)     # => succeeds (with spec computed)
  # init_num(1, 1, 0, NA, 100)     # => succeeds (with fart computed)
  # init_num(1, 1, NA, 1)          # => succeeds (with spec and N computed)
  # init_num(1, 1, 0, NA)          # => succeeds (with fart and N computed)
  # init_num(1, 1, .51, .50, 100)  # => succeeds (as spec and fart are within tolarated range)
  #
  # init_num(prev = NA)                                  # => NAs + warning that prev is not numeric
  # init_num(prev = 88)                                  # => NAs + warning that prev is no probability
  # init_num(prev =  1, sens = NA)                       # => NAs + warning that sens is not numeric
  # init_num(prev =  1, sens = 1, spec = NA, fart = NA)  # => NAs + warning that spec or fart is necessary
  # init_num(1, 1, 1, 1)                                 # => NAs + warning that spec and fart are not complements (in tolerated range)
  # init_num(1, 1, .52, .50, 100)                        # => NAs + warning that spec and fart are not complements (in tolerated range)
}

## -----------------------------------------------
## (5) Apply to initialize num:

#' List current values of basic numeric variables.
#'
#' \code{num} is a list of named numeric variables containing
#' 4 basic probabilities (\code{\link{prev}}, \code{\link{sens}},
#' \code{\link{spec}}, and \code{\link{fart}})
#' and 1 frequency parameter (the population size \code{\link{N}}).
#'
#' @family lists containing scenario settings
#'
#' @examples
#' num <- init_num()  # => initialize num to default parameters
#' num                # => show defaults
#' length(num)        # => 5
#'
#' @seealso
#' \code{\link{init_num}} to initialize basic parameter values;
#' \code{\link{freq}} contains basic frequency information;
#' \code{\link{comp_freq}} computes current frequency information;
#' \code{\link{prob}} contains current probability information;
#' \code{\link{comp_prob}} computes current probability information

num <- init_num()  # => initialize num to default parameters
# num              # => show current values
# length(num)      # => 5

## -----------------------------------------------
## (6) Compute fart (4th parameter) of num (if NA):
##     (moved to init_num() above)

{
  # if (is.na(num$fart)) {
  #   num$fart <- comp_fart(num$spec)
  # }
  #
  # if (is.na(num$spec)) {
  #   num$spec <- comp_spec(num$fart)
  # }
}

## -----------------------------------------------
## (+) ToDo:

## - re-organize "scenarios.xls" according to data structure of env.
## - read in pre-defined datasets ("scenarios.csv") from "/data".
##
## - [init_num]: Verify that input parameters are in the correct range [0; 1].
## - [init_num]: If both spec and fart values are provided,
##   make sure that they are complements of each other.

## -----------------------------------------------
## eof.
