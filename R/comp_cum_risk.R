## comp_cum_risk.R | riskyr
## 2024 01 04
## Compute cumulative risks

# Analysis: Two different problem types ------

# 1. Fixed/stable population size N:
#    Risk factor affects some property of a stable population
#    (e.g., diseases of individuals, rainy days, successful projects, etc.)

# 2. Changing population size N:
#    Risk factor affects and changes the (size of the) population
#    (e.g., sequential percentage changes, cumulative interest, reducing value, etc.)



# ad 1. Fixed population size: Parameters: ----

# IV:
# r ... risk
# t ... time periods/rounds
#
# DV:
# ev ... events
# pc ... percent/population



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
                        t = 1,    # time periods/rounds
                        N = 100   # population size
){

  # iterative generation:
  for (i in 0:t){ # each time period i:

    if (i == 0){

      # prepare data structures:
      ev <- vector(mode = "list", length = t)
      ps <- vector(mode = "list", length = t)

    } else if (i == 1){

      # initialize:
      ev[[i]] <- c(1, 0)
      ps[[i]] <- c(N * r, N * (1 - r))

      names(ps[[i]]) <- paste0(ev[[i]], "x")

    } else {

      for (e in 1:length(ev[[i - 1]])){ # each element e in the previous ev vector:

        ev[[i]][c((2 * e - 1), 2 * e)] <- ev[[i - 1]][e] + c(1, 0)
        ps[[i]][c((2 * e - 1), 2 * e)] <- ps[[i - 1]][e] * c(r, (1 - r))

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

# # Note:
# lapply(X = comp_cum_ps(r = .1, t = 5, N = 100), FUN = cumsum)



# C. plot_cbar(): Compute and plot cumulative risks (as bar chart): ------

# See file "plot_cum_risk.R".

# ?: +++ here now +++



## (*) Done: ----------

# - etc.


## (+) ToDo: ----------


# - Use more appropriate data structure for cumulative probabilities (ps)?
# - How to grow a (binary) tree structure in R?


## eof. ------------------------------------------
