## comp_freq.R | riskyR
## 2018 01 07
## -----------------------------------------------
## Compute current frequencies (freq) based on env
## (using only 4 necessary parameters of env):

comp_freq <- function(N = env$N, prev = env$prev, sens = env$sens, spec = env$spec) {

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
  freq$N <- N # copy N from argument/env (input)

  ## (A) Number of true cases by condition:
  ##     (= 1st level of natural frequency tree):
  freq$cond.true <- round((N * prev), 0)  # 1. cond.true  = N x prev [rounded to nearest integer]
  freq$cond.false <- (N - freq$cond.true) # 2. cond.false = complement of cond.true (to N)

  ## (B) Number of SDT combinations:
  ##     (= 2nd level/leaves of natural frequency tree):
  freq$hi <- round((sens * freq$cond.true), 0)  # a. N of hi [rounded to nearest integer]
  freq$mi <- (freq$cond.true - freq$hi)         # b. N of mi = complement of hi (to cond.true)
  freq$cr <- round((spec * freq$cond.false), 0) # d. N of cr [rounded to nearest integer]
  freq$fa <- (freq$cond.false - freq$cr)        # c. N of fa - complement of cr (to cond.false)

  ## (C) Number of decisions:
  freq$dec.pos <- freq$hi + freq$fa # 1. positive decisions (true & false positives)
  freq$dec.neg <- freq$mi + freq$cr # 2. negative decisions (false & true negatives)

  ## (3) Checks:
  if ((freq$cond.true != freq$hi + freq$mi) |
      (freq$cond.false != freq$fa + freq$cr) |
      (env$N != freq$cond.true + freq$cond.false) |
      (env$N != freq$hi + freq$mi + freq$fa + freq$cr)) {
    warning( "Warning: Something fishy in frequencies [comp_freq()]." )
  }

  return(freq)

}

cur.freq <- comp_freq()

## -----------------------------------------------
## (+) ToDo:

## - allow providing fart as an alternative to spec
##   (spec = 1 - fart)

## -----------------------------------------------
## eof.
