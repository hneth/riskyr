## comp_util.R | riskyR
## 2018 01 08
## -----------------------------------------------
## Generic utility functions:

## -----------------------------------------------
## (1) Toggle display formats between
##     probabilities and percentages:

## (a) Probability as percentage (rounded to 2 decimals):
as.perc <- function(prob, n.digits = 2) {

  if (any(prob < 0) | any(prob > 1)) {
    warning( "Warning: prob is not in range 0 to 1." )
  }

  perc <- NA # initialize

  perc <- round(prob * 100, n.digits) # compute

  return(perc)
  }


## (b) Percentage as probability (rounded to 4 decimals):
as.prob <- function(perc, n.digits = 4) {

  if (any(perc < 0) | any(perc > 100)) {
    warning( "Warning: perc is not in range 0 to 100." )
  }

  prob <- NA # initialize

  prob <- round(perc/100, n.digits) # compute

  return(prob)
}

## Checks:
{
  # as.perc(1/3)          # => perc (rounded to 2 decimals)
  # as.prob(as.perc(2/3)) # => prob (rounded to 4 decimals)
  # s.prob <- seq(0, 1, by = 1/30)
  # s.perc <- seq(0, 100, by = 10/3)
  # as.perc(s.prob)
  # as.prob(s.perc)
  # s.prob == as.prob(as.perc(s.prob)) # some FALSE due to rounding errors!
  # round(s.prob, 4) == as.prob(as.perc(s.prob)) # all TRUE (as both rounded to 4 decimals)
}

## -----------------------------------------------
## (+) ToDo:

## - ...

## -----------------------------------------------
## eof.
