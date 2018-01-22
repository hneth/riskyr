## init_num.R | riskyR
## 2018 01 22
## -----------------------------------------------
## Define and initialize a list of basic input parameters (num)
## that contains numeric user inputs:
## -----------------------------------------------

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

## 1: Bayesian computations: starting with 3 basic probabilities:
## - given:   prev;  sens, spec
## - derived: all other values

## 2: Natural frequencies:
## - given:   N;  hi, mi, fa, cr
## - derived: all other values

## -----------------------------------------------
## (1) Define and initialize num:

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
## (4) Apply to initialize num:

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
## (+) ToDo:

## 1. Add comp_mirt (1a) and comp_sens (1b) above!

## 2. Generalize comp_complement function to 2 types of pairs:
##    a. sens + mirt
##    b. spec + fart

## 3. re-organize "scenarios.xls" according to data structure of env.
##    and read in pre-defined datasets ("scenarios.csv") from "/data".

## - [init_num]: Verify that input parameters are in the correct range [0; 1].
## - [init_num]: If both spec and fart values are provided,
##       make sure that they are complements of each other.

## -----------------------------------------------
## eof.
