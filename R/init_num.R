## init_num.R | riskyr
## 2018 10 16
## Define and initialize a list of basic parameters (num)
## that collects and contains numeric user inputs:
## -----------------------------------------------

## Table of current terminology: -----------------

# Probabilities (13+):              Frequencies (11):
# -------------------               ------------------
# (A) by condition:

# non-conditional:                          N
# prev*                           cond.true | cond.false (columns)

# conditional:
# sens* = hit rate = TPR                hi* = TP
# mirt  = miss rate = FNR               mi* = FN
# fart  = false alarm rate = FPR        fa* = FP
# spec* = true negative rate = TNR      cr* = TN

# [Note: *...is essential]


# (B) by decision:                 Combined frequencies:

# non-conditional:
# ppod = proportion of dec.pos     dec.pos | dec.neg (rows)
#                                  dec.cor | dec.err (diagonal)

# conditional:
# PPV = precision
# FDR = false detection rate
# FOR = false omission rate
# NPV = neg. pred. value

# (C) by accuracy/correspondence of decision to condition (see accu):

# acc  = overall accuracy (probability/proportion correct decision)
# p_acc_hi = p(hi|acc)  # aka. acc-hi  "p(hi | dec.cor)"
# p_err_fa = p(fa|err)  # aka. err-fa  "p(fa | dec.err)"

# Other measures of accuracy (in accu):
# wacc = weighted accuracy
# mcc  = Matthews correlation coefficient
# f1s  = harmonic mean of PPV and sens

# err = error rate = (1 - acc)


## Data flow: Two basic directions: --------------

## (1) Probabilities ==> frequencies:
##     Bayesian: based on 3 essential probabilities:
##   - given:   prev;  sens, spec
##   - derived: all other values

## (2) Frequencies ==> probabilities:
##     Frequentist: based on 4 essential natural frequencies:
##   - given:   N = hi, mi, fa, cr
##   - derived: all other values


## (1) Define and initialize num: ----------------

## The minimal set of numeric input parameters num
## consists of 3 probabilities (+ 1 complement):

## Define defaults for num:  # random:                  # fix: # Description:                                                              # Type of input:
num.def <- list("prev" = round(runif(1, .01, .99), 2),  # .5   # prevalence in target population = p(condition TRUE)     [basic p]
                "sens" = round(runif(1, .01, .99), 2),  # .5   # sensitivity = p(decision POS | condition TRUE)    [conditional p]
                "spec" = round(runif(1, .01, .99), 2),  # .5   # specificity = p(decision NEG | condition FALSE)   [conditional p]
                "fart" = NA,                            # NA   # false alarm rate = 1 - spec        [optional, complement of spec]
                "N"    = round(runif(1, 5, 15), 0)      # 100  # population size N                                 [optional freq]
                )


## Define some fixed stimuli (for MSc Joachim): ----------

# scen <- tribble(
#   ~name,           ~N,	  ~prev,	  ~sens,	 ~spec,
#   "Beschr",		     1000,	  .01,	    .80,	   .904,
#   "Task1 Part2",   1000,	  .09,	    .49,     .95,
#   "Task2 Part2",	 1000,	  .90,	    .95,	   .50,
#   "Task3 Part2",	 1000,	  .80,	    .85,     .75,
#   "Taskx Part3",   1000,	  .02,	    .70,	   .90,
#   "Task1 Part3",	 1000,	  .11,	    .51,	   .939,
#   "Task2 Part3",	 1000,	  .95,	    .90,	   .50,
#   "Task3 Part3",	 1000,    .75,	    .70,	   .85)

# num.def <- list("prev" = .01, "sens" = .80, "spec" = .904, "fart" = NA, "N" = 1000)  # Scenario 1: "Beschr"



## init_num: ----------

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
#'
#' @param sens The decision's sensitivity value \code{\link{sens}}
#' (i.e., the conditional probability of a positive decision
#' provided that the condition is \code{TRUE}).
#'
#' @param spec The decision's specificity value \code{\link{spec}}
#' (i.e., the conditional probability
#' of a negative decision provided that the condition is \code{FALSE}).
#' \code{spec} is optional when is complement \code{\link{fart}} is provided.
#'
#' @param fart The decision's false alarm rate \code{\link{fart}}
#' (i.e., the conditional probability
#' of a positive decision provided that the condition is \code{FALSE}).
#' \code{fart} is optional when its complement \code{\link{spec}} is provided.
#'
#' @param N The population size \code{\link{N}}.
#'
#'
#' @return A list containing a valid quadruple of probabilities
#' (\code{\link{prev}}, \code{\link{sens}},
#' \code{\link{spec}}, and \code{\link{fart}})
#' and one frequency (population size \code{\link{N}}).
#'
#'
#' @examples
#' # ways to succeed:
#' init_num(1, 1, 1, 0, 100)  # => succeeds
#' init_num(1, 1, 0, 1, 100)  # => succeeds
#'
#' # watch out for:
#' init_num(1, 1, 0, 1)           # => succeeds (with N computed)
#' init_num(1, 1, NA, 1, 100)     # => succeeds (with spec computed)
#' init_num(1, 1, 0, NA, 100)     # => succeeds (with fart computed)
#' init_num(1, 1, NA, 1)          # => succeeds (with spec and N computed)
#' init_num(1, 1, 0, NA)          # => succeeds (with fart and N computed)
#' init_num(1, 1, .51, .50, 100)  # => succeeds (as spec and fart are within tolarated range)
#'
#' # ways to fail:
#' init_num(prev = NA)                                  # => NAs + warning (NA)
#' init_num(prev = 88)                                  # => NAs + warning (beyond range)
#' init_num(prev =  1, sens = NA)                       # => NAs + warning (NA)
#' init_num(prev =  1, sens = 1, spec = NA, fart = NA)  # => NAs + warning (NAs)
#' init_num(1, 1, .52, .50, 100)   # => NAs + warning (complements beyond range)
#'
#'
#' @family functions initializing scenario information
#'
#'
#' @seealso
#' \code{\link{num}} contains basic numeric parameters;
#' \code{\link{pal}} contains current color settings;
#' \code{\link{txt}} contains current text settings;
#' \code{\link{freq}} contains current frequency information;
#' \code{\link{comp_freq}} computes frequencies from probabilities;
#' \code{\link{prob}} contains current probability information;
#' \code{\link{comp_prob}} computes current probability information;
#' \code{\link{is_valid_prob_set}} verifies sets of probability inputs;
#' \code{\link{is_extreme_prob_set}} verifies sets of extreme probabilities;
#' \code{\link{comp_min_N}} computes a suitable minimum population size \code{\link{N}}.
#'
#' @importFrom stats runif
#' @importFrom stats setNames
#'
#' @export

init_num <- function(prev = num.def$prev, sens = num.def$sens, # no mirt (yet)
                     spec = num.def$spec, fart = num.def$fart,
                     N = num.def$N) {

  ## (0) Initialize num:    # Description:                                       # Type of input:
  num <- list("prev"  = NA, # prevalence in target population = p(condition TRUE)       [basic p]
              "sens"  = NA, # sensitivity = p(decision POS | condition TRUE)      [conditional p]
              "spec"  = NA, # specificity = p(decision NEG | condition FALSE)     [conditional p]
              "fart"  = NA, # false alarm rate = 1 - spec          [optional, complement of spec]
              "N"     = NA  # population size (N of individuals in population)    [optional freq]
  )


  ## (1) Only if basic quadruple of probabilities is valid:
  if (is_valid_prob_set(prev = prev, sens = sens, mirt = NA, spec = spec, fart = fart, tol = .01)) { # provided probabilities are valid:

    ## (2) Compute missing fart or spec (4th argument) value (if applicable):
    cur.spec.fart <- comp_comp_pair(spec, fart)
    spec <- cur.spec.fart[1] # 1st argument
    fart <- cur.spec.fart[2] # 2nd argument

    ## (3) Issue a warning if probabilities describe an extreme case:
    is_extreme_prob_set(prev = prev, sens = sens, spec = spec)  # prints a warning if TRUE

    ## (4) Compute a missing value for N (5th argument) value (if applicable):
    if (is.na(N)) {

      N <- comp_min_N(prev = prev, sens = sens, spec = spec, min.freq = 1)

      warning(paste0("Unknown population size N. A suitable minimum value of N = ", N, " was computed."))

      }

    ## (5) Initialize num with current arguments:
    num$prev <- prev
    num$sens <- sens
    num$spec <- spec
    num$fart <- fart
    num$N    <- N

  } ## if(is_valid_prob_set...

  ## (6) Return the entire list num:
  return(num)

}

## Check:
{
  # # ways to succeed:
  # init_num(1, 1, 1, 0, 100)  # => succeeds
  # init_num(1, 1, 0, 1, 100)  # => succeeds
  #
  # # watch out for:
  # init_num(1, 1, 0, 1)           # => succeeds (with N computed)
  # init_num(1, 1, NA, 1, 100)     # => succeeds (with spec computed)
  # init_num(1, 1, 0, NA, 100)     # => succeeds (with fart computed)
  # init_num(1, 1, NA, 1)          # => succeeds (with spec and N computed)
  # init_num(1, 1, 0, NA)          # => succeeds (with fart and N computed)
  # init_num(1, 1, .51, .50, 100)  # => succeeds (as spec and fart are within tolarated range)
  #
  # # ways to fail:
  # init_num(prev = NA)                                  # => NAs + warning (NA)
  # init_num(prev = 88)                                  # => NAs + warning (beyond range)
  # init_num(prev =  1, sens = NA)                       # => NAs + warning (NA)
  # init_num(prev =  1, sens = 1, spec = NA, fart = NA)  # => NAs + warning (NAs)
  # init_num(1, 1, .52, .50, 100)                        # => NAs + warning (complements beyond tolerated range)
}


## (4) Apply to initialize num: ------------------

#' List current values of basic numeric variables.
#'
#' \code{num} is a list of named numeric variables containing
#' 4 basic probabilities (\code{\link{prev}}, \code{\link{sens}},
#' \code{\link{spec}}, and \code{\link{fart}})
#' and 1 frequency parameter (the population size \code{\link{N}}).
#'
#'
#' @family lists containing current scenario information
#'
#'
#' @examples
#' num <- init_num()  # => initialize num to default parameters
#' num                # => show defaults
#' length(num)        # => 5
#'
#' @seealso
#' \code{\link{init_num}} initializes basic numeric parameters;
#' \code{\link{txt}} contains current text information;
#' \code{\link{init_txt}} initializes text information;
#' \code{\link{pal}} contains current color information;
#' \code{\link{init_pal}} initializes color information;
#' \code{\link{freq}} contains current frequency information;
#' \code{\link{comp_freq}} computes current frequency information;
#' \code{\link{prob}} contains current probability information;
#' \code{\link{comp_prob}} computes current probability information.
#'
#' @export

num <- init_num()  # => initialize num to default parameters
# num              # => show current values
# length(num)      # => 5

## (*) Done: ----------

## - Clean up + add randomness to num_def  [2018 08 21].

## (+) ToDo: ----------

## 1. Re-organize "scenarios.xls" according to data structure of num.
##    and read in pre-defined datasets ("scenarios.csv") from "/data".
##
## 2. Use either spec as basic probability (and compute fart from it)
##            OR fart as basic probability (and compute spec from it).
##    num should always contain both (with is_complement = TRUE).
##
## - [init_num]: Verify that input parameters are in the correct range [0; 1].
## - [init_num]: If both spec and fart values are provided,
##       make sure that they are complements of each other.

## eof. ------------------------------------------
