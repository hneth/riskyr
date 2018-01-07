## comp_prob.R | riskyR
## 2018 01 07
## -----------------------------------------------
## Compute current probabilities (prob) based on env
## (using only the necessary parameters of env):

## -----------------------------------------------
## (1) Basic functions for predictive values (based on probabilities):

## 1. Positive predictive value (PPV) from probabilities:
comp_PPV <- function(prev = env$prev, sens = env$sens, spec = env$spec) {

  PPV <- NA # initialize

  ## Bayesian formula contains 3 terms:
  num <- (prev * sens)
  den1 <- num
  den2 <- (1 - prev) * (1 - spec)

  if (den1 + den2 == 0) {
    warning( "Warning: Division by zero [comp_PPV()]." )
  }

  PPV <- num / (den1 + den2) # combine

  return(PPV)
}

## 2. False discovery/detection rate (FDR = complement of PPV):
comp_FDR <- function(prev = env$prev, sens = env$sens, spec = env$spec) {

  PPV <- NA # initialize
  FDR <- NA

  ## Bayesian formula contains 3 terms:
  num <- (prev * sens)
  den1 <- num
  den2 <- (1 - prev) * (1 - spec)

  if (den1 + den2 == 0) {
    warning( "Warning: Division by zero [comp_FDR()]." )
  }

  PPV <- num / (den1 + den2) # combine
  FDR <- (1 - PPV) # FDR is the complement of PPV

  return(FDR)
}

## 3. Negative predictive value (NPV) from probabilities:
comp_NPV <- function(prev = env$prev, sens = env$sens, spec = env$spec) {

  NPV <- NA # initialize

  ## Bayesian formula contains 3 terms:
  num <- (1 - prev) * spec
  den1 <- num
  den2 <- (prev) * (1 - sens)

  if (den1 + den2 == 0) {
    warning( "Warning: Division by zero [comp_NPV()]." )
  }

  NPV <- num / (den1 + den2) # combine

  return(NPV)
}

## 4. False omission rate (FOR = complement of NPV):
comp_FOR <- function(prev = env$prev, sens = env$sens, spec = env$spec) {

  NPV <- NA # initialize
  FOR <- NA

  ## Bayesian formula contains 3 terms:
  num <- (1 - prev) * spec
  den1 <- num
  den2 <- (prev) * (1 - sens)

  if (den1 + den2 == 0) {
    warning( "Warning: Division by zero [comp_FOR()]." )
  }

  NPV <- num / (den1 + den2) # combine
  FOR <- (1 - NPV)

  return(FOR)
}

## -----------------------------------------------
## (2) Alternative versions (based on frequencies):

## 1. Positive predictive value (PPV) from frequencies:
comp_PPV_freq <- function(n.hi = freq$hi, n.fa = freq$fa) {

  PPV <- NA # initialize

  ## PPV = hits / positive decisions
  ##     = hits / (hits + false alarms)
  num <- n.hi
  den <- n.hi + n.fa

  if (den == 0) {
    warning( "Warning: Division by zero [comp_PPV_freq()]." )
  }

  PPV <- num / den # combine

  return(PPV)
}

## 2. Negative predictive value (NPV) from frequencies:
comp_NPV_freq <- function(n.cr = freq$cr, n.mi = freq$mi) {

  NPV <- NA # initialize

  ## NPV = correct rejections / negative decisions
  ##     = correct rejections / (correct rejections + misses)
  num <- n.cr
  den <- n.cr + n.mi

  if (den == 0) {
    warning( "Warning: Division by zero [comp_NPV_freq()]." )
  }

  NPV <- num / den # combine

  return(NPV)
}

## -----------------------------------------------
## (3) Compare alternative calculations:

{
  # ## A: Using default settings:
  # env$N
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
## (4) Compute PPV and NPV as a function of prev, sens, and spec:
##     using Bayes' formula:

comp_prob <- function(prev = env$prev, sens = env$sens, spec = env$spec) {

  ## (1) Initialize prob as a list:
  prob <- list(

    ## predictive values (PVs):
    "ppv" = NA, # positive predictive value
    "npv" = NA  # negative predictive value
  )

  ## (2) Compute all values of prob based on arguments:
  prob$ppv <- comp_PPV(prev, sens, spec)
  prob$npv <- comp_NPV(prev, sens, spec)

  ## (3) Checks:
  if ( is.na(prob$ppv) | (prob$ppv < 0) | (prob$ppv > 1) |
       is.na(prob$npv) | (prob$npv < 0) | (prob$npv > 1) ) {
    warning( "Warning: Something peculiar about PVs [comp_prob()]." )
  }

  return(prob)

}

## Apply:
cur.prob <- comp_prob()

## -----------------------------------------------
## (+) ToDo:

## - compute alternative prob from freq with
##   a. N of dec.pos (rather than N of fa) and
##   b. N of dec.neg (rather than N of mi) provided.

## -----------------------------------------------
## eof.
