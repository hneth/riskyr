## comp_cum_risk.R | riskyr
## 2024 01 23
## Compute cumulative risks

# Task analysis: ----------


# A. Two different problem types: ------

# 1. Fixed/stable population size N: ------

#    Risk factor affects some property of a stable population
#    (e.g., diseases of individuals, rainy days, successful projects, etc.).
#
#    DV: Proportions of affected individuals.
#    Absence and presence of risk factor for individuals lead to different proportions in population:
#    a. individual perspective: affected vs. unaffected proportions individuals
#    b. collective perspective: # of risky events per individual (i.e., classes/segments of individuals)

# Mathematically, risk categories can be computed by binomial or multi-nominal distributions.
# However, a generative process model assumes a binary tree with 2 edges/links (risk present vs. absent) for each node.


# - Note on range of r: ----
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

# - Note on binary tree data structure: ----
#
# Data can be generated and stored as a binary tree.
# Each state corresponds to an binary expansion:
#   1      0
# 11 10  01 00
# etc.
# Thus, each state contains the entire history of each leaf
# and representation allows for all kinds of questions:
# - number of events: e.g., more than 2 events?
# - parity: e.g., more 1 than 0?
# - patterns: e.g., alternating 0 and 1?
# Constraint: Dependencies run strictly from stem to leaf,
# any reverse dependencies (from leaf to stem) cannot be captured.



# 2. Changing/variable population/reference set size N: --------
#
#    Risk factor affects and changes the (magnitude/size of the reference set) population
#    (e.g., sequential percentage changes, cumulative interest, reducing portfolio value, etc.)
#
# DV: Population magnitude/size of reference set.

# Note on range of r: Allowing for increases and decreases: -1 <= r <= +1.



# B. Problem variants and generalizations: ------

# Basic problems involve a constant value of r and r > 0.
#
# This implies 2 generalizations:
# 1. Variable values of r (as a vector of values).
# 2. Risk increases (0 < r <= 1) and risk reductions (-1 <= r < 0).




# ad 1. Fixed/stable population size N: ----------
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

} # apply_risk().

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

} # comp_ps().

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

} # comp_ev().

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

} # comp_ev_p().

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
                        N = 100,  # population size
                        bin_names = TRUE  # name states by binary sequence?
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

      # Prepare data structures:
      ev <- vector(mode = "list", length = t)  # number of events/occurrences
      ps <- vector(mode = "list", length = t)  # cumulative probability of segment
      bi <- vector(mode = "list", length = t)  # binary representation of state

    } else if (i == 1){

      # initialize:
      ev[[i]] <- c(1,         0)
      ps[[i]] <- c(N * r[i],  N * (1 - r[i]))
      bi[[i]] <- c("1",       "0")

      if (bin_names){
        names(ps[[i]]) <- bi[[i]]
      } else {
        names(ps[[i]]) <- paste0(ev[[i]], "x")
      }

    } else {

      for (e in 1:length(ev[[i - 1]])){ # each element e in the previous ev vector:

        ev[[i]][c((2 * e - 1), 2 * e)] <- ev[[i - 1]][e] + c(1, 0)
        ps[[i]][c((2 * e - 1), 2 * e)] <- ps[[i - 1]][e] * c(r[i], (1 - r[i]))
        bi[[i]][c((2 * e - 1), 2 * e)] <- paste0(bi[[i - 1]][e],  c("1", "0"))

        if (bin_names){
          names(ps[[i]]) <- bi[[i]]
        } else {
          names(ps[[i]]) <- paste0(ev[[i]], "x")
        }

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

# # Vector names:
# comp_cum_ps(r = .1, t = 5, bin_names = TRUE)   # names show binary state (full event history)
# comp_cum_ps(r = .1, t = 5, bin_names = FALSE)  # names show number of event occurrences (e.g., 2x)

# Generalization to variable values of r (as a vector):
# comp_cum_ps(r = c(.1, .2, .3), t = NA, N = 100)


# # Operations on list data structure:

# # 1. Cumulative sums:
# lapply(X = comp_cum_ps(r = .1, t = 5, N = 100), FUN = cumsum)

# # 2. Converting binary state representation to number of event occurrences and decimal state nr:
# ls <- comp_cum_ps(r = .1, t = 5, bin_names = TRUE)  # use binary names
# ls
# # Get names of a list element (vector):
# l_names <- names(ls[[5]])  # extract names of list element
# sapply(X = l_names, FUN = tally, USE.NAMES = FALSE)  # Number of event occurrences
# sapply(X = l_names, FUN = base_dec, base = 2)        # state as decimal number (starting with 0)


# ToDo: ----

# 2. Generalization: Does this work for risk values r < 0 (reducing risks)?
# comp_cum_ps(r = c(.1, -.2, .3), t = NA, N = 100)  # Answer: No, see negative segments!





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

  # Prepare: ----

  # ToDo: Ensure that r values are in -1 <= r <= +1.

  if ((length(r) == 1) && (is.numeric(t))){

    # Turn r into a vector of length t:
    r <- rep(r, times = t)

    # User feedback:
    # message(paste0("Created r as a vector of length t = ", t, ":"))
    # print(r)

  }

  # Turn probabilities (or negative probabilities) into percentages:
  if (is_prob(abs(r))){

    r_neg <- (r < 0)  # index of negative values

    r_pc <- as_pc(abs(r))

    r_pc[r_neg] <- r_pc[r_neg] * -1  # re-apply negative values

    # User feedback:
    # message(paste0("Turned probability r into percentages r_pc = "))
    # print(r_pc)

  }

  # Data structure:
  nr <- length(r)
  N_out <- rep(NA, nr)


  # Main: ----

  for (i in 1:nr){

    if (i == 1){

      N_out[i] <- N * pc_2_fac(r_pc[i])

    } else {

      N_out[i] <- N_out[(i - 1)] * pc_2_fac(r_pc[i])

    } # if.

  } # for loop.


  # +++ here now +++

  # Verify by comparing to aggregate change:
  N_final <- N * pc_2_fac(aggr_pcs(r_pc))
  eps <- .000001  # epsilon

  if (abs(N_final - N_out[nr]) > eps){

    warning(paste0("Final value of N_out = ", N_out[nr], " differs from aggregate change N_final = ", N_final))

  }


  # Output: ----

  names(N_out) <- paste0(r_pc, "%")

  return(N_out)

} # apply_risk_to_population().


# # Check:
# apply_risk_to_population(.10)
# apply_risk_to_population(.10, t = 5)
# apply_risk_to_population(c(.10, .20, .30), t = 3)
#
# # Contrast:
# apply_risk_to_population(c( .20,  .30, -.50))
# apply_risk_to_population(c(-.20, -.30,  .50))
#
# apply_risk_to_population(c( .10,  .20,  .30, -.60))
# apply_risk_to_population(c(-.10, -.20, -.30,  .60))



# Binomial distribution function: ------

# Example from stats::Binomial:
# Compute P(45 < X < 55) for X Binomial(100, 0.50):
# sum(dbinom(x = 46:54, size = 100, prob = .50))  # 0.6317984

# # Adaptation:
# r <- .20
# t <- 4
#
# # Compare:
# comp_cum_ps(r = r, t = t, N = 1)     # computes p for ALL paths and states
# dbinom(x = 0:t, size = t, prob = r)  # computes summary prob for # of ev occurrences
#
# # Connection: ---
#
# ls <- comp_cum_ps(r = r, t = t, N = 1, bin_names = TRUE)  # use binary names
# # Get names of a list element (vector):
# v <- ls[[t]]
# l_names <- names(v)  # extract names of list element
# l_names
# n_ev <- sapply(X = l_names, FUN = tally, USE.NAMES = FALSE)  # Number of event occurrences
# n_ev
#
# # Aggregate:
# tapply(v, n_ev, sum)  # yields array
#
# # Verify equality:
# all.equal(as.vector(tapply(v, n_ev, sum)), dbinom(x = 0:t, size = t, prob = r))
#
# # As df: ----
#
# df <- data.frame(n_ev = n_ev,
#                  prob = v)
# # Aggregate df:
# tapply(df$prob, df$n_ev, sum)  # yields array
#
# # Summarize using magrittr and dplyr: ----
#
# library(tidyverse)
# tb_sum <- df |>
#   group_by(n_ev) |>
#   summarize(sum_p = sum(prob))
# tb_sum
#
# # Verify equality:
# all.equal(tb_sum$sum_p, dbinom(x = 0:t, size = t, prob = r))



## (*) Done: ----------

# - 1. Generalization to variable values of r (as a vector).

# - Binary state names: comp_cum_ps() uses a binary state names (bin_names = TRUE).
#   As those state names preserve a path's full risky event history,
#   they can be used to compute the number of event occurrences (using the tally() function)
#   of decimal state number (using the bin_dec() function).


## (+) ToDo: ----------

# - Using the binomial formula for computing cumulative risk of
#   exact event occurrences (X = c) and ranges (X >= c).

# - 2. Generalization: Make comp_cum_risk() work for
#   NEGATIVE risk values r < 0 (i.e., risk reductions).

# - Use more appropriate data structure for cumulative probabilities (ps)?
# - How to grow a (binary) tree structure in R?


## eof. ------------------------------------------
