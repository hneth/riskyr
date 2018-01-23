## comp_prob_freq.R | riskyR
## 2018 01 22
## -----------------------------------------------
## Compute probabilities (prob) from frequencies (freq):

## Note: Always use num (essential) and freq (derived)
##       rather than env (NON-essential)!

## -----------------------------------------------
## ToDo:

## 1. Define 4 basic frequencies

# 1 - hi = true positive (TP)
# 2 - mi = false negative (FN)
# 3 - fa = false positive (FP)
# 4 - cr = true negative (TN)

##  2. Compute basic parameters (prev, sens, spec, fart)
##     from existing frequencies!

# ALL probabilities can be computed from 4 frequencies
# of the confusion table!

# See https://en.wikipedia.org/wiki/Confusion_matrix
# for a collection of metrics.


## (...)


## -----------------------------------------------
## (X) Compute predictive values (PVs:
##     PPV and NPV, FDR and FOR)
##     from various frequencies (alternative versions):

## -----------------------------------------------
## 1. Positive predictive value (PPV) from frequencies:

comp_PPV_freq <- function(n.hi = freq$hi, n.fa = freq$fa) {

  PPV <- NA # initialize

  ## PPV = hits / positive decisions
  ##     = hits / (hits + false alarms)

  if ((n.hi + n.fa) == 0) {
    stop( "Stop: Division by zero: n.hi + n.fa = 0 [comp_PPV_freq()]." )
  }

  PPV <- n.hi / (n.hi + n.fa)

  return(PPV)
}

## -----------------------------------------------
## 2. Negative predictive value (NPV) from frequencies:

comp_NPV_freq <- function(n.cr = freq$cr, n.mi = freq$mi) {

  NPV <- NA # initialize

  ## NPV = correct rejections / negative decisions
  ##     = correct rejections / (correct rejections + misses)

  if ((n.cr + n.mi) == 0) {
    stop( "Stop: Division by zero: n.cr + n.mi = 0 [comp_NPV_freq()]." )
  }

  NPV <- n.cr / (n.cr + n.mi)

  return(NPV)
}

## -----------------------------------------------
## ToDo: Add alternative ways to compute probabilities
## from frequencies (based on different elements of freq)!
## (...)

## -----------------------------------------------
## (C): Comparing the alternative PV calculations:

{
  # ## A: Using default settings:
  # num$N
  # freq <- comp_freq(round = TRUE)
  # freq
  #
  # comp_PPV()
  # comp_PPV_freq() # Note: Deviation due to rounding of freq to nearest integers!
  #
  # comp_NPV()
  # comp_NPV_freq() # Note: Deviation due to rounding of freq to nearest integers!
  #
  # ## B: But when using exact frequencies for freq:
  # freq <- comp_freq(round = FALSE) # do NOT round to nearest integers:
  # all.equal(comp_PPV_freq(), comp_PPV()) # => TRUE
  # all.equal(comp_NPV_freq(), comp_NPV()) # => TRUE

}

## -----------------------------------------------
## (D) Compute the set of ALL current probabilities:
##     from frequencies (freq):

## (...)

## -----------------------------------------------
## (+) ToDo:

## - Add alternative ways to compute probabilities
##   from frequencies (based on various elements of freq)!
##
## - Compute basic parameters (probabilities and frequencies)
##   from MIX of existing probabilities and frequencies!
##
## - Compute alternative prob from freq with
##   a. N of dec.pos (rather than N of fa) and
##   b. N of dec.neg (rather than N of mi) provided.

## -----------------------------------------------
## eof.
