## init_num.R | riskyR
## 2018 01 18
## -----------------------------------------------
## Initialize a list of basic input parameters (num)
## that contains all numeric user inputs:

.onAttach <- function(libname, pkgname) {
  packageStartupMessage("Welcome to riskyr!")
}

## -----------------------------------------------
## Documentation:

#' Check whether an input is a probability.
#'
#' \code{is_prob} is a function that checks whether its single argument \code{x}
#' is a probability: A number in the range from 0 to 1.
#'
#' @param x A single (typically numeric) argument.
#'
#' @return A Boolean value: \code{TRUE} if \code{x} is a probability, otherwise \code{FALSE}.
#'
#' @examples
#' is_prob("Laplace") # => FALSE + Warning that x is not numeric
#' is_prob(2)         # => FALSE + Warning that x is not in 0 to 1 range
#' is_prob(1/2)       # => TRUE, but does NOT return the probability .5
#'
#' @family checking functions
#' @seealso \code{\link{as_pc}} to display a probability as a percentage

is_prob <- function(x) {

  val <- FALSE # initialize

  if (!is.numeric(x)) {
    warning(paste0(x, " is not numeric."))
  }
  else if ((x < 0) | (x > 1)) {
    warning(paste0(x, " is no probability (range from 0 to 1)."))
  }
  else {
    val <- TRUE
  }

  return(val)

}

## Checks:
# is_prob("Laplace") # => FALSE + Warning
# is_prob(2)         # => FALSE + Warning
# is_prob(1/2)       # => TRUE

## -----------------------------------------------
## (1) Basic functions on probabilities:
## Specificity (spec) is the complement of the false alarm rate (fart):

#' Compute a decision's false alarm rate from its specificity.
#'
#' \code{comp_fart} is a function that takes a \emph{specificity} \code{spec}
#' -- given as a probability (i.e., a number in the range from 0 to 1) --
#' as its input, and returns the corresponding \emph{false alarm rate} \code{fart}
#' -- also as a probability -- as its output.
#'
#' Specificity and false alarm rate are both features of the decision process
#' (e.g., a diagnostic test).
#' The false alarm rate and specificity are complements (i.e., \code{fart = 1 - spec}).
#'
#' @param spec A \emph{specificity} given as a probability
#' (i.e., a number in the range from 0 to 1).
#'
#' @return A \emph{false alarm rate} given as a probability
#' (i.e., a number in the range from 0 to 1).
#'
#' @examples
#' comp_fart(2)   # NA + Warning that 2 is not in 0 to 1 range
#' comp_fart(1/3) # 0.6666667
#' comp_fart(comp_spec(0.123)) # 0.123

#' @family conversion functions
#' @seealso \code{\link{as_pc}} to display a probability as a percentage

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
#' \code{comp_spec} is a function that takes a \emph{false alarm rate} \code{fart}
#' -- given as a probability (i.e., a number in the range from 0 to 1) --
#' as its input, and returns the corresponding \emph{specificity} \code{spec}
#' -- also as a probability -- as its output.
#'
#' False alarm rate and specificity are both features of the decision process
#' (e.g., a diagnostic test).
#' The specificity and false alarm rate are complements (i.e., \code{spec = 1 - fart}).
#'
#' @param fart A \emph{false alarm rate} given as a probability
#' (i.e., a number in the range from 0 to 1).
#'
#' @return A \emph{specificity} given as a probability
#' (i.e., a number in the range from 0 to 1).
#'
#' @examples
#' comp_spec(2)   # NA + Warning that 2 is not in 0 to 1 range
#' comp_spec(2/3) # 0.3333333
#' comp_spec(comp_fart(0.123)) # 0.123

#' @family conversion functions
#' @seealso \code{\link{as_pc}} to display a probability as a percentage

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
## (2) Determine a good number for population size N:
##     Criterion: All 4 SDT cells should have a minimal frequency of min.freq:

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
## (3) Define and initialize num:

## The minimal set of numeric input parameters num
## consists of 3 probabilities (+ 1 complement):

## Define defaults for num:   # Description:                                     # Type of input:
num.def <- list("prev" = .15, # prevalence in target population = p(condition TRUE)     [basic p]
                "sens" = .85, # sensitivity = p(decision POS | condition TRUE)    [conditional p]
                "spec" = .75, # specificity = p(decision NEG | condition FALSE)   [conditional p]
                "fart" =  NA, # false alarm rate = 1 - spec        [optional, complement of spec]
                "N"    =  NA  # population size (N of individuals in population)  [optional freq]
                )

init_num <- function(prev = num.def$prev, sens = num.def$sens, spec = num.def$spec,
                     fart = num.def$fart, N = num.def$N) {

  ## (a) Verify that 3 essential probabilities are provided:
  if (is.na(prev)) {stop("A prevalence value (prev) is missing but necessary [init_num()].")}
  if (is.na(sens)) {stop("A sensitivity value (sens) is missing but necessary [init_num()].")}
  if (is.na(spec) & is.na(fart)) {
    stop("Either a specificity value (spec) OR a false alarm rate (fart) is necessary [init_num()].")}

  ## (+) ToDo: Verify that input parameters are in the correct range [0; 1].

  ## (+) ToDo: If both spec and fart values are provided,
  ##           make sure that they are complements of each other.

  ## (b) Compute missing fart (4th argument) value (if applicable):
  if (is.na(fart)) {fart <- comp_fart(spec)}
  if (is.na(spec)) {spec <- comp_spec(fart)}

  ## (c) Compute missing N (5th argument) value (if applicable):
  if (is.na(N)) {N <- comp_min_N(prev, sens, spec, min.freq = 1)}

  ## (d) Initialize num:   # Description:                                       # Type of input:
  num <- list("prev"  = NA, # prevalence in target population = p(condition TRUE)       [basic p]
              "sens"  = NA, # sensitivity = p(decision POS | condition TRUE)      [conditional p]
              "spec"  = NA, # specificity = p(decision NEG | condition FALSE)     [conditional p]
              "fart"  = NA, # false alarm rate = 1 - spec          [optional, complement of spec]
              "N"     = NA  # population size (N of individuals in population)    [optional freq]
              )

  ## (e) Initialize num with current arguments:
  num$prev <- prev
  num$sens <- sens
  num$spec <- spec
  num$fart <- fart
  num$N    <- N

  ## (f) Return the entire list num:
  return(num)

}

## Check:
{
  # init_num(prev = NA) # => fails
  # init_num(prev = .1, sens = NA) # => fails
  # init_num(prev = .1, sens = .1, spec = NA, fart = NA) # => fails
  # init_num(.5, .5, 1/3) # => succeeds
  # init_num(.5, .5, 1/3, NA, 999) # => succeeds
  # init_num(11, 22, 1/3, NA, 999) # => succeeds, but should not (as prev and sens are not in correct range)
}

## Apply:
num <- init_num()
# num

## -----------------------------------------------
## (4) Compute fart (4th parameter) of num (if NA):
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
## Import ready-made and worked out example data
## (in both ui.R and server.R):

# datasets <- read.csv2("./data/scenarios.csv", stringsAsFactors = FALSE)

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
