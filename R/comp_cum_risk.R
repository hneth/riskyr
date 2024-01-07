## comp_cum_risk.R | riskyr
## 2024 01 07
## Compute cumulative risks

# Task analysis: ------


# A. Two different problem types: ----

# 1. Fixed/stable population size N:
#    Risk factor affects some property of a stable population
#    (e.g., diseases of individuals, rainy days, successful projects, etc.).
#
#    DV: Proportions of affected individuals.
#    Absence and presence of risk factor for individuals lead to different proportions in population:
#    a. individual perspective: affected vs. unaffected proportions individuals
#    b. collective perspective: # of risky events per individual (i.e., classes/segments of individuals)

# Note on range of r:
#
# In 1. most risk factors assume a positive range 0 <= r <= 1 (i.e., only increasing risk):
# (e.g., earthquakes, diseases, death).
# Occurrences can accumulate, but not be cancelled, reduced, or withdrawn later.
#
# Allowing for recovery or risk reduction (r < 0) requires
# further assumptions and a different process,
# as this would change (reduce) the affected proportion of the population.
# The EV counts remain stable, but the proportion(s) affected decrease
# (but unclear, whether all affected segments decrease by the same rate).


# 2. Changing population size N:
#    Risk factor affects and changes the (magnitude/size of the) population
#    (e.g., sequential percentage changes, cumulative interest, reducing portfolio value, etc.)
#
# DV: Population magnitude/size.

# Note on range of r: Allowing for increases and decreases: -1 <= r <= +1.



# B. Problem variants and generalizations: ----

# Basic problems involve a constant value of r and r > 0.
#
# This implies 2 generalizations:
# 1. Variable values of r (as a vector of values).
# 2. Risk increases (0 < r <= 1) and risk reductions (-1 <= r < 0).




# ad 1. Fixed/stable population size N: ------
#       Risk factor affects some property of a stable population
#       (e.g., diseases of individuals, rainy days, successful projects, etc.)

# Parameters: ----

# IV:
# r ... risk
# t ... time periods/rounds
#
# DV:
# ev ... number of events
# pc ... probability of risk factor affecting population / percent of population



# A. Recursive application of repeated risk r: ------

# 1. apply_risk() as a recursive partition of a stable population pc: ----

apply_risk <- function(r, t, pc = 100, ev = 0){

  # cat(t, ": ", pc, "\n")

  if (t == 0){ # stop:

    # print(paste0(ev, ": ", pc))

  } else { # recurse:

    t_prev <- (t - 1)  # simplify

    pc <- c(apply_risk(r = r, t = t_prev, pc * r, ev + 1), apply_risk(r = r, t = t_prev, pc * (1 - r), ev))

  }

  # Output:
  return(pc)

}

# # Check:
# apply_risk(pc = 100, ev = 0, r = .25, t = 2)


# 2. Apply risk separately for p and ev: ------

# - a: comp_ps: Probabilities ----

comp_ps <- function(r, t, p = 100){

  if (t == 0){ # stop:

    # print(paste0(p))

  } else { # recurse:

    p <- c(comp_ps(r = r, t = (t - 1), p = (p * r)), comp_ps(r = r, t = (t - 1), p = (p * (1 - r))))

  }

  return(p)

}

# Check:
# comp_ps(r = .25, t = 2, p = 100, )
# comp_ps(r = .25, t = 4, p = 100, )


# - b: comp_ev: Number of events ----

comp_ev <- function(r, t, ev = 0){

  if (t == 0){ # stop:

    # print(paste0(ev))

  } else { # recurse:

    ev <- c(comp_ev(r = r, t = (t - 1), ev = (ev + 1)), comp_ev(r = r, t = (t - 1), ev = ev))

  }

  return(ev)

}

# # Check:
# comp_ev(r = .25, t = 2, ev = 0)
# comp_ev(r = .25, t = 3, ev = 0)


# - c: comp_ev_p: Combine comp_ev() and comp_ps() ----

comp_ev_p <- function(r, t, p = 100, ev = 0){

  out <- NA  # initialize

  # Compute (using recursive functions):
  events <- comp_ev(r = r, t = t, ev = ev)  # a. events
  cum_ps <- comp_ps(r = r, t = t, p  = p )  # b. probabilities

  # Combine outputs:
  out <- cum_ps
  names(out) <- paste0(events, "x")

  return(out)

}

# # Check:
# comp_ev_p(r = .25, t = 1, p = 100, ev = 0)
# comp_ev_p(r = .25, t = 2, p = 100, ev = 0)
# comp_ev_p(r = .25, t = 3, p = 100, ev = 0)
# comp_ev_p(r = .25, t = 4, p = 100, ev = 0)

# # Note:
# cumsum(comp_ev_p(r = .25, t = 4, p = 100, ev = 0))




# B. comp_cum_ps(): Iterative generation of cumulative risks: ------

comp_cum_ps <- function(r = 1/2,  # risk per time period
                        t = NA,   # number of time periods/rounds
                        N = 100   # population size
){

  # Generalize version of constant r value to a vector of r values
  # (with potentially different risk values, and length(r) = t):

  if ((length(r) == 1) && (is.numeric(t))){

    # Turn r into a vector of length t:
    r <- rep(r, times = t)

    # User feedback:
    # message(paste0("Made r a vector of length t = ", t, ":"))
    # print(r)

  }

  t <- length(r)


  # Iterative generation:
  for (i in 0:t){ # each time period i:

    if (i == 0){

      # prepare data structures:
      ev <- vector(mode = "list", length = t)
      ps <- vector(mode = "list", length = t)

    } else if (i == 1){

      # initialize:
      ev[[i]] <- c(1, 0)
      ps[[i]] <- c(N * r[i], N * (1 - r[i]))

      names(ps[[i]]) <- paste0(ev[[i]], "x")

    } else {

      for (e in 1:length(ev[[i - 1]])){ # each element e in the previous ev vector:

        ev[[i]][c((2 * e - 1), 2 * e)] <- ev[[i - 1]][e] + c(1, 0)
        ps[[i]][c((2 * e - 1), 2 * e)] <- ps[[i - 1]][e] * c(r[i], (1 - r[i]))

        names(ps[[i]]) <- paste0(ev[[i]], "x")

      } # for e.

    } # if i.

  } # for i.

  # Output:
  return(ps)

} # comp_cum_ps().

# # Check:
# comp_cum_ps()
# comp_cum_ps(r = .1, t = 2, N = 100)
# comp_cum_ps(r = .1, t = 5, N = 100)

# Generalization to variable values of r (as a vector):
# comp_cum_ps(r = c(.1, .2, .3), t = NA, N = 100)

# ToDo: Does this work for risk values r < 0 (reducing risks)?
# comp_cum_ps(r = c(.1, -.2, .3), t = NA, N = 100)  # Answer: No, see negative segments!

# # Note:
# lapply(X = comp_cum_ps(r = .1, t = 5, N = 100), FUN = cumsum)



# C. plot_cbar(): Compute and plot cumulative risks (as bar chart): ------

# See file "plot_cum_risk.R".


# ad 2. Changing population size N ------
#       Risk factor affects and changes the (size of the) population
#       (e.g., sequential percentage changes, cumulative interest, reducing value, etc.)


# Parameters: ----

# IV:
# r ... risk (as a constant value)
# t ... time periods/rounds
# OR:
# r ... as a vector of risk factor values (e.g. sequential changes)


# Generalizations of r:
#
# 1. Variable r values: To allow for vector r with a range of different values,
# Constant values of r and given value of t are interpreted as:
# rep(r, t).
#
# 2. Risk increases and reductions: -1 <= r <= +1.


# DV:
# N ... magnitude/size of population
# sg ... 2 segments (from original vs. recent change) or 2^t segments?


apply_risk_to_population <- function(r, t = NA, N = 100){

  # ToDo: Ensure that r values are in -1 <= r <= +1.

  if ((length(r) == 1) && (is.numeric(t))){

    # Turn r into a vector of length t:
    r <- rep(r, times = t)

    # User feedback:
    message(paste0("Made r a vector of length t = ", t, ":"))
    print(r)
  }




  # +++ here now +++




  # Output:
  r

}

# # Check:
# apply_risk_to_population(.50)




## (*) Done: ----------

# - 1. Generalization to variable values of r (as a vector).


## (+) ToDo: ----------

# - 2. Generalization: Make comp_cum_risk() work for
#   NEGATIVE risk values r < 0 (i.e., risk reductions).

# - Use more appropriate data structure for cumulative probabilities (ps)?
# - How to grow a (binary) tree structure in R?


## eof. ------------------------------------------
