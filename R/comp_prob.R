## comp_prob.R | riskyR
## 2018 01 10
## -----------------------------------------------
## Compute current probabilities (prob) based on num
## (using only the necessary parameters of num):

## Note: Always use num (essential) rather than env (NON-essential)!

## -----------------------------------------------
## (A) Predictive values (based on probabilities):

## 1. Positive predictive value (PPV) from probabilities:
comp_PPV <- function(prev = num$prev, sens = num$sens, spec = num$spec) {

  PPV <- NA # initialize

  ## PPV = hits / positive decision = hits / (hits + false alarms):
  hi <- (prev * sens)
  fa <- (1 - prev) * (1 - spec)

  PPV <- hi / (hi + fa)

  return(PPV)
}

## 2. False discovery/detection rate (FDR = complement of PPV):
comp_FDR <- function(prev = num$prev, sens = num$sens, spec = num$spec) {

  PPV <- NA # initialize
  FDR <- NA

  PPV <- comp_PPV(prev, sens, spec)
  FDR <- (1 - PPV) # FDR is the complement of PPV

  return(FDR)
}

## 3. Negative predictive value (NPV) from probabilities:
comp_NPV <- function(prev = num$prev, sens = num$sens, spec = num$spec) {

  NPV <- NA # initialize

  ## NPV = cr / negative decision = cr / (cr + mi):
  cr <- (1 - prev) * spec
  mi <- prev * (1 - sens)

  NPV <- cr / (cr + mi)

  return(NPV)
}

## 4. False omission rate (FOR = complement of NPV):
comp_FOR <- function(prev = num$prev, sens = num$sens, spec = num$spec) {

  NPV <- NA # initialize
  FOR <- NA

  NPV <- comp_NPV(prev, sens, spec)
  FOR <- (1 - NPV) # FOR is the complement of NPV

  return(FOR)
}

## -----------------------------------------------
## (B) Predictive values (alternative versions
##     based on frequencies):

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
## Compare alternative PV calculations:

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
## Compute ALL current probabilities:
## So far: Compute current values of PPV and NPV
##         as functions of prev, sens, and spec (using Bayes):

comp_prob <- function(prev = num$prev, sens = num$sens, spec = num$spec) {

  ## (1) Initialize prob as a list:
  prob <- list(
    ## predictive values (PVs):
    "ppv" = NA, # positive predictive value
    "npv" = NA  # negative predictive value
  )

  ## (2) Compute all values of prob based on arguments:
  prob$ppv <- comp_PPV(prev, sens, spec) # Note: using probabilistic version (Bayes)
  prob$npv <- comp_NPV(prev, sens, spec)

  ## (3) Check:
  if ( is.na(prob$ppv) | (prob$ppv < 0) | (prob$ppv > 1) |
       is.na(prob$npv) | (prob$npv < 0) | (prob$npv > 1) ) {
    warning( "Warning: Something peculiar about PVs [comp_prob()]." )
  }

  ## (4) Return the entire list:
  return(prob)

}


## Apply:
# prob <- comp_prob()
# prob
# prob$ppv


## -----------------------------------------------
## Compute either PPV or NPV for an entire matrix of values
## (when sens and spec are given as vectors):

comp_PV_matrix <- function(prev, sens, spec, metric = "PPV") {

  # Initialize matrix as df:
  n.rows <- length(sens)
  n.cols <- length(spec)
  matrix <- as.data.frame(matrix(NA,
                                 nrow = n.rows,
                                 ncol = n.cols))
  names(matrix) <- sens

  ## Loop through rows and columns of matrix:
  for (row in 1:n.rows) {
    for (col in 1:n.cols) {

      cell.val <- NA # initialize current cell value

      if (metric == "PPV") {cell.val <- get.PPV(prev, sens[row], spec[col])} # compute PPV
      if (metric == "NPV") {cell.val <- get.NPV(prev, sens[row], spec[col])} # compute NPV

      matrix[row, col] <- cell.val # store result in matrix

    }
  }

  return(matrix)

}


## -----------------------------------------------
## (+) ToDo:

## - allow using fart instead of spec to comp_prob()
## - compute alternative prob from freq with
##   a. N of dec.pos (rather than N of fa) and
##   b. N of dec.neg (rather than N of mi) provided.

## -----------------------------------------------
## eof.
