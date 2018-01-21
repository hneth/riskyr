## init_num.R | riskyR
## 2018 01 21
## -----------------------------------------------
## Initialize a list of basic input parameters (num)
## that contains all numeric user inputs:
## -----------------------------------------------

## (1) Basic parameters:
## -----------------------------------------------
## (a) prev:

#' The prevalence (baseline probability) of some condition.
#'
#' \code{prev} defines some condition's prevalence value:
#' The baseline probability of the condition being \code{TRUE}.
#'
#' Understanding or obtaining the prevalence value \code{prev}:
#'
#' \itemize{
#'
#'   \item \code{prev} is the (non-conditional) probability:
#'
#'   \code{prev = p(condition = TRUE)}
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

prev <- .10 # default prevalence

## -----------------------------------------------
## (b) sens:

#' The sensitivity of a decision process or diagnostic procedure.
#'
#' \code{sens} defines some decision's sensitivity value:
#' The conditional probability of the decision being positive
#' if the condition is \code{TRUE}.
#'
#' Understanding or obtaining the sensitivity value \code{sens}:
#'
#' \itemize{
#'
#'   \item \code{sens} is the conditional probability
#'   for a (correct) positive decision given that
#'   the condition is \code{TRUE}:
#'
#'   \code{sens = p(decision = positive | condition = TRUE)}
#'
#'   \code{sens} is the opposite conditional probability
#'   -- but not the complement --
#'   of the positive predictive value \code{\link{ppv}}:
#'
#'   \code{ppv = p(condition = TRUE | decision = positive)}
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
#' @examples
#' sens <- .85     # => sets a sensitivity value of 85%
#' sens <- 85/100  # => (decision = positive) for 85 people out of 100 people for which (condition = TRUE)
#' is_prob(sens)   # => TRUE (as sens is a probability)
#'
#' @family basic parameters
#'
#' @seealso
#' \code{\link{num}} contains basic numeric variables;
#' \code{\link{init_num}} initializes basic numeric variables;
#' \code{\link{is_prob}} verifies probability inputs;
#' \code{\link{comp_prob}} computes derived probabilities;
#' \code{\link{comp_freq}} computes natural frequencies from probabilities

sens <- .85 # default sensitivity

## -----------------------------------------------
## (c) spec:

#' The specificity of a decision process or diagnostic procedure.
#'
#' \code{spec} defines some decision's specificity value:
#' The conditional probability of the decision being negative
#' if the condition is FALSE.
#'
#' Understanding or obtaining the specificity value \code{spec}:
#'
#' \itemize{
#'
#'   \item \code{spec} is the conditional probability
#'   for a (correct) negative decision given that
#'   the condition is \code{FALSE}:
#'
#'   \code{spec = p(decision = negative | condition = FALSE)}
#'
#'   \code{spec} is the opposite conditional probability
#'   -- but not the complement --
#'   of the negative predictive value \code{\link{npv}}:
#'
#'   \code{npv = p(condition = FALSE | decision = negative)}
#'
#'   \item \code{spec} is the complement of the
#'   false alarm rate \code{\link{fart}}:
#'
#'   \code{spec = 1 - fart}
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
#' @examples
#' spec <- .75     # => sets a specificity value of 75%
#' spec <- 75/100  # => (decision = negative) for 75 people out of 100 people for which (condition = FALSE)
#' is_prob(spec)   # => TRUE (as spec is a probability)
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

spec <- .75 # default specificity

## -----------------------------------------------
## (d) fart:

#' The false alarm rate of a decision process or diagnostic procedure.
#'
#' \code{fart} defines some decision's false alarm rate
#' or the rate of false positive decisions: The conditional probability
#' of the decision being positive if the condition is FALSE.
#'
#' Understanding or obtaining the false alarm rate \code{fart}:
#'
#' \itemize{
#'
#'   \item \code{fart} is the conditional probability
#'   for an (incorrect) positive decision given that
#'   the condition is \code{FALSE}:
#'
#'   \code{fart = p(decision = positive | condition = FALSE)}
#'
#'   \item \code{fart} is the complement of the
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
#' @examples
#' fart <- .25     # => sets a false alarm rate of 25%
#' fart <- 25/100  # => (decision = positive) for 25 people out of 100 people for which (condition = FALSE)
#' is_prob(fart)   # => TRUE (as fart is a probability)
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

fart <- NA # default false alarm rate

## -----------------------------------------------
## (e) N:

#' The number of individuals in the population.
#'
#' \code{N} defines how many individuals make up
#' some specific (current) population.
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
#' @examples
#' N <- 1000   # => sets a population size of 1000
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
#' \code{comp_fart} is a conversion function that takes a specificity \code{spec}
#' -- given as a probability (i.e., a numeric value in the range from 0 to 1) --
#' as its input, and returns the corresponding false alarm rate \code{fart}
#' -- also as a probability -- as its output.
#'
#' Specificity and false alarm rate are both features of the decision process
#' (e.g., a diagnostic test).
#' The false alarm rate and specificity are complements (i.e., \code{fart = 1 - spec}).
#'
#' \code{comp_fart} is complementary to the conversion function
#' \code{\link{comp_spec}}.
#'
#' @param spec A specificity given as a probability
#' (i.e., a numeric value in the range from 0 to 1).
#'
#' @return A false alarm rate as a probability
#' (i.e., a numeric value in the range from 0 to 1).
#'
#' @examples
#' comp_fart(2)                 # => NA + Warning that 2 is not in 0 to 1 range
#' comp_fart(1/3)               # => 0.6666667
#' comp_fart(comp_spec(0.123))  # => 0.123
#'
#' @family conversion functions
#'
#' @seealso
#' \code{\link{is_complement}} verifies numeric complements;
#' \code{\link{as_pc}} displays a probability as a percentage;
#' \code{\link{comp_prob}} computes derived probabilities

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
#' @param fart A false alarm rate given as a probability
#' (i.e., a numeric value in the range from 0 to 1).
#'
#' @return A specificity as a probability
#' (i.e., a numeric value in the range from 0 to 1).
#'
#' @examples
#' comp_spec(2)                 # => NA + Warning that 2 is not in 0 to 1 range
#' comp_spec(2/3)               # => 0.3333333
#' comp_spec(comp_fart(0.123))  # => 0.123
#'
#' @family conversion functions
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
#' @param spec A specificity value (i.e., the conditional probability
#' of a negative decision provided that the condition is FALSE).
#' \code{spec} is optional when is complement \code{fart} is provided.
#' @param fart A false alarm rate (i.e., the conditional probability
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
#' @family conversion functions
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
#' \code{init_num} initializes basic numeric variables to define \code{num}
#' as a list of named elements containing four basic probabilities
#' (\code{prev}, \code{sens}, \code{spec}, and \code{fart})
#' and one frequency parameter (the population size \code{N}).
#'
#' If \code{spec} is provided, its complement \code{fart} is optional.
#' If \code{fart} is provided, its complement \code{spec} is optional.
#' If no \code{N} is provided, it is computed by \code{\link{comp_min_N}}.
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
#' @return A list containing a valid quadruple of probabilities
#' (\code{prev}, \code{sens}, \code{spec}, and \code{fart})
#' and one frequency (population size \code{N}).
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
#' @seealso \code{\link{num}} to store basic parameter values;
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


  ## (1) Verify basic input parameters are valid:
  if (is_valid(prev, sens, spec, fart, tol = .01)){

    ## (2) Compute missing fart or spec (4th argument) value (if applicable):
    cur.spec.fart <- comp_complement(spec, fart)
    spec <- cur.spec.fart[1] # 1st argument
    fart <- cur.spec.fart[2] # 2nd argument

    ## (3) Compute a missing value for N (5th argument) value (if applicable):
    if (is.na(N)) {
      N <- comp_min_N(prev, sens, spec, min.freq = 1)
      warning(paste0("Unknown population size N. A minimum N = ", N, " was computed."))
      }

    ## (4) Initialize num with current arguments:
    num$prev <- prev
    num$sens <- sens
    num$spec <- spec
    num$fart <- fart
    num$N    <- N

  } ## if(...)

  ## (5) Return the entire list num:
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

#' List current values of basic numeric variables.
#'
#' \code{num} is initialized to a list of named numeric variables containing
#' four basic probabilities (\code{prev}, \code{sens}, \code{spec}, and \code{fart})
#' and one frequency parameter (the population size \code{N}).
#'
#' @family lists containing basic scenario settings
#' @seealso \code{\link{init_num}} to initialize basic parameter values

## -----------------------------------------------
## (5) Apply to initialize num:

num <- init_num()
# num

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
