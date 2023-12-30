## cum_risk.R | riskyr
## 2023 12 30
## Compute cumulative risks

# Parameters: ----

# r ... risk
# i ... periods/rounds/times
#
# ev ... events
# pc ... percent/population


# A. Recursive application of repeated risk r: ------

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

# - a: comp_p: Probabilities ----

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


# - b: comp_ev: Number of events ----

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


# - c: comp_ev_p: Combination of comp_ev() and comp_p(): ----

comp_ev_p <- function(p = 100, ev = 0, r, i){

  out <- NA  # initialize

  # Compute (using recursive functions):
  events <- comp_ev(ev, r, i) # A
  cum_ps <- comp_p(p,   r, i) # B

  # Combine outputs:
  out <- cum_ps
  names(out) <- paste0(events, "x")

  return(out)

}

# # Check:
# comp_ev_p(p = 100, ev = 0, r = .25, i = 1)
# comp_ev_p(p = 100, ev = 0, r = .25, i = 2)
# comp_ev_p(p = 100, ev = 0, r = .25, i = 3)
# comp_ev_p(p = 100, ev = 0, r = .25, i = 4)



# B. comp_cum_ps(): Iterative generation of cumulative risks ------

comp_cum_ps <- function(r = 1/2,  # risk per time period
                        t = 1,    # time periods/rounds
                        N = 100   # population size
){

  # iterative generation:
  for (i in 0:t){ # each period i:

    if (i == 0){

      # prepare:
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
# comp_cum_ps(.1, 5, 100)



# C. plot_cum_bar(): Compute and plot cum-risks (as barchart) ------


#' Plot cumulative risk dynamics
#'
#' plot_cum_bar plots the results of cumulative risk dynamics
#' as a barchart (with percentages of risk event counts
#' for each period t on a horizontal bar).
#'
#' @param r risk (per time period)
#' @param t number of time periods
#' @param N population size.
#' Default: \code{N = 100} expresses risks as percentages,
#' \code{N = 1} as probabilities, else frequencies.
#'
#' @param sort logical: sort outputs (by number of event occurrences)?
#' @param x_max maximum x-value (for zooming).
#' Default should equal population size \code{N}.
#'
#' @importFrom grDevices colorRampPalette

plot_cum_bar <- function(r = 1/2, t = 1, N = 100,
                         sort = FALSE, x_max = 100){

  # Compute cumulative probability data: ----
  data <- comp_cum_ps(r = r, t = t, N = N)

  # Prepare plot: ----

  # Plot dimensions:
  t_max <- t
  x_max <- x_max
  y_max <- t_max + 1

  # Constants:
  bar_width <- .50
  cex_lbl <- 1 - (5 * t_max/100)

  if (N == 100){
    x_lbl <- "Percentage"
  } else if (N == 1){
    x_lbl <- "Probability"
  } else {
    x_lbl <- "Frequency"
  }

  # Initialize plotting area:
  plot(0:1, 0:1, type = "n",
       xlab = x_lbl, ylab = "Time period",
       xlim = c(0, x_max), ylim = c(0.5, y_max + 0.2),
       axes = FALSE)

  grid()

  # Axes:
  # X-axis at bottom, horizontal labels:
  axis(side = 1, labels = TRUE,
       las = 1, lwd = 1, cex.axis = 1)

  # # Y-axis on left, horizontal labels:
  # axis(side = 2, labels = TRUE,
  #      las = 1, lwd = 1, cex.axis = 1)

  # Colors:
  n_cols <- 1 + t_max
  col_lo <- "grey98"
  col_hi <- "firebrick" # "steelblue" # "deepskyblue" # "deeppink" # "olivedrab" # "grey20" # "red3"

  # pal <- unikn::usecol(pal = c(col_lo, col_hi), n = n_cols, alpha = .80)
  pal <- grDevices::colorRampPalette(colors = c(col_lo, col_hi))(n_cols)
  # unikn::seecol(pal)


  # Plot 1st bar/box (for time period 0): ----

  lbl_0 <- paste0("0 (r = ", round(r, 2), ")")  # events (risk)
  cur_col <- pal[1]

  # Draw bar/box 0:
  plot_cbox(x = N/2, y = y_max, lx = N, ly = bar_width,
            lbl = lbl_0, cex = cex_lbl,
            col_fill = cur_col, col_brd = "grey20")

  # Add label (on left):
  text(x = 0, y = y_max, labels = "0:", pos = 2, xpd = TRUE)


  # Plot bars/boxes (for each time period): ----
  for (t in 1:length(data)){

    # print(t)

    v <- data[[t]]  # get current vector
    # print(v)

    if (sort){

      # Sort by vector values by value names:
      v <- v[order(names(v), decreasing = TRUE)]

    }

    x_prv <- 0  #  initialize x-store
    y_val <- y_max - t

    # For each p/ev-value in v:
    for (i in 1:length(v)){

      p_cur <- v[i]  # current p value (as named probability)
      # print(p_cur)

      x_width <- p_cur

      x_val <- x_prv + x_width/2

      x_name <- names(p_cur)  # current name
      x_ev <- as.numeric(substr(x_name, 1, nchar(x_name) - 1))  # current value of ev

      cur_col <- pal[x_ev + 1]

      # Draw box:
      plot_cbox(x = x_val, y = y_val, lx = x_width, ly = bar_width,
                lbl = x_ev, cex = cex_lbl,
                col_fill = cur_col, col_brd = "grey20")

      x_prv <- x_prv + p_cur  # increment x_store

    } # for each p/ev-value i.

    # Add label (on left):
    text(x = 0, y = y_val, labels = paste0(t, ":"), pos = 2, xpd = TRUE)

  } # for each time period t.


  # Titles: ----

  plot_title <- paste0("Cumulative risk dynamics (r = ", r, "; t = ", t, "; N = ", N, ")")
  plot_title <- paste0("Cumulative risk dynamics")

  title(main = plot_title, adj = 0)


  # Output: Return data ----

  return(invisible(data))

} # plot_cum_bar().


# # # Check:
# plot_cum_bar()
#
# plot_cum_bar(r = .25, t = 3, N = 100)
# plot_cum_bar(r = .25, t = 3, N = 100, sort = TRUE)  # sorting
#
# plot_cum_bar(r = .50, t = 3, N = 100)
# plot_cum_bar(r = .50, t = 3, N = 100, sort = TRUE)  # sorting
#
# plot_cum_bar(r = .75, t = 4, N = 100)
# plot_cum_bar(r = .75, t = 4, N = 100, sort = TRUE)  # sorting
#
# plot_cum_bar(r = .05, t = 5, x_max = 25)  # zooming in
# plot_cum_bar(r = .05, t = 5, sort = TRUE, x_max = 1)  # sorting & zooming in


# ?: +++ here now +++



## (*) Done: ----------

# - Added option to sort cum-risk vectors by names:
#
# # Sort vector by element names:
# v <- 1:4
# names(v) <- c("B", "C", "D", "A")
# v[order(names(v))]


## (+) ToDo: ----------

# - Add option for plotting as vertical bars (with 2 directions/levels).
# - Use more appropriate data structure for cumulative probabilities (ps)?
# - How to grow a (binary) tree structure in R?


## eof. ------------------------------------------
