## comp_freq.R | riskyR
## 2018 01 11
## -----------------------------------------------
## Compute all current frequencies (freq) based on num
## (using only the 4 necessary parameters of num):

## Note: Always use num (essential) rather than env (NON-essential)!

## -----------------------------------------------
## Compute all current frequencies:

comp_freq <- function(N = num$N, prev = num$prev, sens = num$sens, spec = num$spec, round = TRUE) {

  ## (1) Initialize freq as a list:
  freq <- list(

    ## Population size:
    "N" = NA, # Number of cases overall

    ## Number of true cases by condition:
    "cond.true"  = NA, # N of cond TRUE
    "cond.false" = NA, # N of cond FALSE

    ## Number of decisions:
    "dec.pos" = NA, # N of dec POS [was: dec.pos]
    "dec.neg" = NA, # N of dec NEG [was: dec.neg]

    ## SDT combinations:
    "hi" = NA, # true positive
    "mi" = NA, # false negative
    "cr" = NA, # true negative
    "fa" = NA  # false positive

  )

  ## (2) Compute all values of freq based on arguments:
  freq$N <- N # copy N from argument/num (input)

  ## (A) Number of true cases by condition:
  ##     (= 1st level of natural frequency tree):
  if (round) {
    freq$cond.true <- round((N * prev), 0)  # 1a. cond.true  = N x prev [rounded to nearest integer]
  } else {
    freq$cond.true <- (N * prev)            # 1b. cond.true  = N x prev [not rounded]
  }
  freq$cond.false <- (N - freq$cond.true)   # 2. cond.false = complement of cond.true (to N)

  ## (B) Number of SDT combinations:
  ##     (= 2nd level/leaves of natural frequency tree):
  if (round) {
    freq$hi <- round((sens * freq$cond.true), 0)  # a1. N of hi [rounded to nearest integer]
  } else {
    freq$hi <- (sens * freq$cond.true)            # a2. N of hi [not rounded]
  }
  freq$mi <- (freq$cond.true - freq$hi)           # b.  N of mi = complement of hi (to cond.true)

  if (round) {
    freq$cr <- round((spec * freq$cond.false), 0) # c1. N of cr [rounded to nearest integer]
  } else {
    freq$cr <- (spec * freq$cond.false)           # c2. N of cr [not rounded]
  }
  freq$fa <- (freq$cond.false - freq$cr)          # d.  N of fa - complement of cr (to cond.false)

  ## (C) Number of decisions:
  freq$dec.pos <- freq$hi + freq$fa # 1. positive decisions (true & false positives)
  freq$dec.neg <- freq$mi + freq$cr # 2. negative decisions (false & true negatives)

  ## (3) Check:
  if ((freq$cond.true != freq$hi + freq$mi) |
      (freq$cond.false != freq$fa + freq$cr) |
      (freq$N != freq$cond.true + freq$cond.false) |
      (freq$N != freq$hi + freq$mi + freq$fa + freq$cr)) {
    warning( "Warning: Something fishy in freq [comp_freq()]." )
  }

  return(freq)

}

## Apply:
freq <- comp_freq()
# freq

## -----------------------------------------------
## (+) ToDo:

## - Allow providing fart as an alternative to spec
##   (spec = 1 - fart)

## -----------------------------------------------
## eof.
