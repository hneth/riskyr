## cum_risk.R | riskyr
## 2023 12 26
## Compute cumulative risks


# Parameters: ----

# r ... risk
# i ... periods/rounds/times
#
# ev ... events
# pc ... percent/population


# Recursive application of repeated risk r: ------

# 1. apply_risk() as recursive partition of population pc: ----

apply_risk <- function(pc = 100, ev = 0, r, i){

  # cat(i, ": ", pc, "\n")


  if (i == 0){ # stop:

    # print(paste0(ev, ": ", pc))

  } else { # recurse:

    j <- (i - 1)  # simplify

    pc <- c(apply_risk(pc * r, ev + 1, r, j), apply_risk(pc * (1 - r), ev, r, j))

  }

  # Output:
  return(pc)

}

# # Check:
# apply_risk(pc = 100, ev = 0, r = .25, i = 4)


# 2. Separately for p and ev: ------

# A: comp_p: Probabilities ----

comp_p <- function(p = 100, r, i){

  if (i == 0){ # stop:

    # print(paste0(p))

  } else { # recurse:

    p <- c(comp_p(p * r, r, (i - 1)), comp_p(p * (1 - r), r, (i - 1)))

  }

  return(p)

}

# # Check:
# comp_p(p = 100, r = .25, i = 2)
# comp_p(p = 100, r = .25, i = 4)


# B: comp_ev: Number of events ----

comp_ev <- function(ev = 0, r, i){

  if (i == 0){ # stop:

    # print(paste0(ev))

  } else { # recurse:

    ev <- c(comp_ev((ev + 1), r, (i - 1)), comp_ev(ev, r, (i - 1)))

  }

  return(ev)

}

# # Check:
# comp_ev(ev = 0, r = .25, i = 2)
# comp_ev(ev = 0, r = .25, i = 4)


# C: comp_ev_p: Combination of comp_ev() and comp_p(): ----

comp_ev_p <- function(p = 100, ev = 0, r, i){

  out <- NA  # initialize

  events <- comp_ev(ev, r, i) # A
  cum_ps <- comp_p(p,   r, i) # B

  # Combine output:
  out <- cum_ps
  names(out) <- paste0(events, "x")

  return(out)

}

# # Check:
# comp_ev_p(p = 100, ev = 0, r = .25, i = 1)
# comp_ev_p(p = 100, ev = 0, r = .25, i = 2)
# comp_ev_p(p = 100, ev = 0, r = .25, i = 3)
# comp_ev_p(p = 100, ev = 0, r = .25, i = 4)





## (*) Done: ----------

## - etc.

## (+) ToDo: ----------

## - etc.

## eof. ------------------------------------------
