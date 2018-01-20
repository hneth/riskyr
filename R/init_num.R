## init_num.R | riskyR
## 2018 01 20
## -----------------------------------------------
## Initialize a list of basic input parameters (num)
## that contains all numeric user inputs:

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

#' @family conversion functions
#' @seealso \code{\link{is_complement}} to verify numeric complements;
#' \code{\link{as_pc}} to display a probability as a percentage

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

#' @family conversion functions
#' @seealso \code{\link{is_complement}} to verify numeric complements;
#' \code{\link{as_pc}} to display a probability as a percentage

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
#' comp_min_N(0, 0, 0) # => 1
#' comp_min_N(1, 1, 1) # => 1
#'
#' comp_min_N(1, 1, 1, min.freq = 10) # =>  10
#' comp_min_N(1, 1, 1, min.freq = 99) # => 100
#'
#' comp_min_N(.1, .1, .1)             # => 100       = 10^2
#' comp_min_N(.001, .1, .1)           # =>    10 000 = 10^4
#' comp_min_N(.001, .001, .1)         # => 1 000 000 = 10^6
#' comp_min_N(.001, .001, .001)       # => 1 000 000 = 10^6
#'
#' @family functions turning probabilities into frequencies
#' @seealso \code{\link{comp_freq}} computes frequencies from probabilities

comp_min_N <- function(prev, sens, spec, min.freq = 1) {

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
  # comp_min_N(0, 0, 0) # => 1
  # comp_min_N(1, 1, 1) # => 1
  # comp_min_N(1, 1, 1, min.freq = 10) # =>  10
  # comp_min_N(1, 1, 1, min.freq = 99) # => 100
  # comp_min_N(.1, .1, .1)       # => 100       = 10^2
  # comp_min_N(.001, .1, .1)     # => 10 000    = 10^4
  # comp_min_N(.001, .001, .1)   # => 1 000 000 = 10^6
  # comp_min_N(.001, .001, .001) # => 1 000 000 = 10^6
}

## -----------------------------------------------
## (4) Define and initialize num:

## The minimal set of numeric input parameters num
## consists of 3 probabilities (+ 1 complement):

## Define defaults for num:     # Description:                                     # Type of input:
num.def <- list("prev" = .15,   # prevalence in target population = p(condition TRUE)     [basic p]
                "sens" = .85,   # sensitivity = p(decision POS | condition TRUE)    [conditional p]
                "spec" = .75,   # specificity = p(decision NEG | condition FALSE)   [conditional p]
                "fart" =  NA,   # false alarm rate = 1 - spec        [optional, complement of spec]
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
#' \code{\link{comp_min_N}} to get a minimum value of population size N

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
  if (is_valid(prev, sens, spec, fart, tol = .01))
  {

    ## (2) Compute missing fart or spec (4th argument) value (if applicable):
    if (is.na(fart) & !is.na(spec)) { fart <- comp_fart(spec) }
    if (is.na(spec) & !is.na(fart)) { spec <- comp_spec(fart) }

    ## (3) Ensure that spec and fart are complements:
    # if (!is_complement(spec, fart)) {
    #   warning("Specificity (spec) and false alarm rate (fart) are NOT complements.")
    # }

    ## (4) Compute a missing value for N (5th argument) value (if applicable):
    if (is.na(N)) {
      N <- comp_min_N(prev, sens, spec, min.freq = 1)
      message(paste0("Computed a suitable minimal population size value N = ", N))
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
