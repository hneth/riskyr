## plot_cum_risk.R | riskyr
## 2024 01 26
## Plot cumulative risks

# Task analysis: ------


# A. Two different problem types: ----

# 1. Fixed/stable population size N:
#    Risk factor affects some property of a stable population
#    (e.g., diseases of individuals, rainy days, successful projects, etc.)

# 2. Changing population size N:
#    Risk factor affects and changes the (size of the) population
#    (e.g., sequential percentage changes, cumulative interest, reducing value, etc.)


# B. Problem variants and generalizations: ----

# Basic problems involve a constant value of r and r > 0.
#
# This implies 2 generalizations:
# 1. Variable values of r (as a vector of values).
# 2. Risk increases (0 < r <= 1) and risk reductions (-1 <= r < 0).



# B. Problem variants and generalizations: ----

# Basic problems involve a constant value of r and r > 0.
#
# This implies 2 generalizations:
# 1. Variable values of r (as a vector of values).
# 2. Risk increases (0 < r <= 1) and risk reductions (-1 <= r < 0).

# ad 1. Compute cumulative risks: ------

# See file "comp_cum_risk.R".




# plot_cbar(): Plot cumulative risks (as bar chart): ------

#' Plot cumulative risk dynamics (as bar chart)
#'
#' \code{plot_cbar} plots the results of cumulative risk dynamics
#' as a bar chart (with percentages of risk event counts
#' for each period t on a horizontal bar).
#'
#' @param r risk (probability of occurrence per time period).
#' A non-scalar vector allows for different risk values
#' at different times (and \code{t = length(r)}).
#'
#' @param t time periods/rounds.
#' Default: \code{t = NA}, setting \code{t <- length(r)}.
#' A scalar \code{r} and numeric \code{t} sets \code{r <- rep(r, t)}.
#'
#' @param N population size.
#' Default: \code{N = 100} expresses risks as percentages,
#' \code{N = 1} as probabilities, else frequencies.
#'
#' @param horizontal logical: Draw horizontal vs. vertical bars?
#'
#' @param sort logical: Sort outputs by number of event occurrences?
#' Default: \code{sort = FALSE}.
#'
#' @param N_max maximum N value plotted (for zooming in for small \code{r} values).
#' Default value should be set to population size \code{N}.
#'
#' @param bar_width width of (horizontal/vertical) bar per time period.
#' Default: \code{bar_width = .50}.
#'
#' @param show_trans numeric: Show transition polygons (between bars)?
#' Values of 0/1/2/3 focus on no/new/remaining/both risk segments, respectively.
#' Default: \code{show_trans = 1} (i.e., focus on increasing risk segments).
#'
#' @param show_ev logical: Show number of risky event occurrence (as bar label)?
#' Default: \code{show_ev = TRUE}.
#'
#' @param show_n logical: Show population frequency of risky event occurrences (as bar label)?
#' Default: \code{show_n = FALSE}.
#'
#' @param show_bin logical: Show risky event history as binary state representation (as bar label)?
#' Default: \code{show_bin = FALSE}.
#'
#' @param colors A vector of color values
#' (for risk event frequency being \code{"hi"}, \code{"lo"}, \code{"no"}, and \code{"bd"} for borders, respectively).
#' Default: \code{colors = c("firebrick", "grey96", "green4", "grey40")}.
#'
#' @return data of p-values, named by number of event occurrences
#' (invisibly, as list of named vectors, for each time period t).
#'
#' @importFrom grDevices colorRampPalette
#'

plot_cbar <- function(r = .50, t = NA, N = 100,
                      horizontal = TRUE, sort = FALSE,
                      N_max = 100, bar_width = .50,
                      show_trans = 1, show_ev = TRUE, show_n = FALSE, show_bin = FALSE,
                      colors = c("firebrick", "grey96", "green4", "grey40")  # for (hi, lo, no, bd), respectively
){


  # Handle inputs: ----

  if (any(r < 0) || any(r > 1)){
    message("All risk values of r currently need to be in (0, 1)")
  }
  # ToDo: Make comp_cum_risk() and plot_cbar() work for
  #       NEGATIVE risk values r < 0 (reducing risks).

  if (all(!is.na(r)) && is.na(t)){
    t <- length(r)
  }

  if (bar_width < 0 || bar_width > 1){
    message("bar_width should be in (0, 1)")
  }

  if (show_n && (show_ev | show_bin)){
    message("Set show_n = TRUE: Setting shown_ev = FALSE and show_bin = FALSE")
    show_ev  <- FALSE
    show_bin <- FALSE
  }

  if (show_bin && (show_ev | show_n)){
    message("Set show_bin = TRUE: Setting shown_ev = FALSE and show_n = FALSE")
    show_ev <- FALSE
    show_n  <- FALSE
  }

  if (show_trans %in% 0:3 == FALSE){
    message("show_trans should be in 0:3: Setting shown_trans = 1 (i.e., focus on risk increase)")
    show_trans <- 1  # use default
  }

  if (sort && (show_trans != 0)){
    message("Transitions correspond to unsorted arrangement of bar segments (sort = FALSE)")
  }


  # Compute cumulative probability data: ----

  bin_names <- TRUE # FALSE  # as labels and colors currently use ev (in names)

  data <- comp_cum_ps(r = r, t = t, N = N, bin_names = bin_names)  # list of p values (with ev names)

  data_cs <- lapply(X = data, FUN = cumsum)  # cumsum() of p values


  # Prepare plot: ----

  opar <- par(no.readonly = TRUE)  # all par settings that can be changed.
  on.exit(par(opar))  # par(opar)  # restore original settings


  # Plot dimensions:

  t_max <- t + 1


  # Plot constants:
  cex_lbl   <- 1 - (5 * t/100)  # bar labels

  # Main axis label:
  if (N == 100){
    x_lbl <- "Percentage"
  } else if (N == 1){
    x_lbl <- "Probability"
  } else {
    x_lbl <- "Frequency"
  }

  # Initialize plotting area:

  if (horizontal){ # horizontal bars:

    par(mar = c(3, 2, 4, 1) + 0.1)  # margins; default: par("mar") = 5.1 4.1 4.1 2.1.

    plot(0:1, 0:1, type = "n",
         xlab = NA, ylab = NA,
         xlim = c(0, N_max), ylim = c(0.5, t_max + 0.35),
         axes = FALSE)

    mtext(x_lbl, adj = .50, side = 1, line = 2)          # x-axis label
    mtext("Time period", adj = .50, side = 2, line = 1)  # y-axis label


  } else { # vertical bars:

    par(mar = c(3, 4, 4, 1) + 0.1)  # margins; default: par("mar") = 5.1 4.1 4.1 2.1.

    t_max <- t + .50  # adjust

    plot(0:1, 0:1, type = "n",
         xlab = NA, ylab = NA,
         xlim = c(-0.5, t_max), ylim = c(0, N_max),
         axes = FALSE)

    mtext("Time period", adj = .50, side = 1, line = 1)  # x-axis label
    mtext(x_lbl, adj = .50, side = 2, line = 3)          # y-axis label

  }

  grid()


  # Axes:

  if (horizontal){ # horizontal bars:

    # X-axis at bottom, horizontal labels:
    axis(side = 1, labels = TRUE,
         las = 1, lwd = 1, cex.axis = 1)

  } else { # vertical bars:

    # Y-axis on left, horizontal labels:
    axis(side = 2, labels = TRUE,
         las = 1, lwd = 1, cex.axis = 1)

  }

  # Colors:

  # colors <- c("steelblue", "grey96", "gold", "grey40")    # r(rain) vs. sun
  # colors <- c("firebrick", "grey96", "grey90", "grey40")  # r(danger) vs. neutral
  # colors <- c("grey40", "grey90", "white", "black")       # grayscale
  # colors <- c("firebrick", "grey96", "green4", "grey40")  # r(danger) vs. safe (default)

  names(colors) <- c("hi", "lo", "no", "bd")

  col_lo <- colors["lo"]  # "grey96", "white"
  col_hi <- colors["hi"]  # "firebrick", "steelblue", "deepskyblue", "deeppink", "olivedrab", "grey20", "red3"
  col_bd <- colors["bd"]  # "grey40", "white", "grey20"
  col_no <- colors["no"]  # "gold", "green4", "forestgreen", "deepskyblue"




  # Color palette:
  n_cols  <- 1 + t

  # pal <- unikn::usecol(pal = c(col_lo, col_hi), n = n_cols, alpha = .80)
  pal <- grDevices::colorRampPalette(colors = c(col_lo, col_hi))(n_cols)

  # Replace 1st color (ev = 0):
  pal[1] <- grDevices::colorRampPalette(colors = c("white", col_no))(10)[3]  # 2-10 := intensity of col_no
  # unikn::seecol(pal)


  # Plot 1st bar/box (for time period 0): ----

  if (show_ev){
    lbl_0 <- paste0("0")  # events
  } else if (show_n){
    lbl_0 <- paste0(round(N, 0))  # N
  } else { # default;
    lbl_0 <- NA  # nothing
  }

  cur_col <- pal[1]  # current color


  if (horizontal){ # horizontal bars:

    # Draw bar/box 0:
    plot_cbox(x = N/2, y = t_max, lx = N, ly = bar_width,
              lbl = lbl_0, cex = cex_lbl,
              col_fill = cur_col, col_brd = col_bd)

    # Add label (on left):
    text(x = 0, y = t_max, labels = "0:", pos = 2, xpd = TRUE)

  } else { # vertical bars:

    # Draw bar/box 0:
    plot_cbox(x = 0, y = N/2, lx = bar_width, ly = N,
              lbl = lbl_0, cex = cex_lbl,
              col_fill = cur_col, col_brd = col_bd)

    # Add label (on top):
    text(x = 0, y = N_max + 5/N_max, labels = "0:", pos = 3, xpd = TRUE)

  }


  # Plot bars/boxes (for each time period): ----

  for (t in 1:length(data)){

    # print(paste0("t = ", t, ":"))

    p_ev <- data[[t]]  # get current vector (of p values)
    # print(p_ev)

    p_ev_cs <- data_cs[[t]]  # get current vector (of cumulative p-values)
    # print(p_ev)

    poly_cs <- c(0, p_ev_cs, N)  # all polygon x-values (+ 2 extremes, for transitions)
    # print(poly_cs)

    if (sort){

      # Sort vector values by value names:
      new_order <- order(names(p_ev), decreasing = TRUE)
      p_ev <- p_ev[new_order]

    }

    x_prv <- 0  #  initialize x-store (previous x-value)
    cur_t <- t_max - t

    # For each p/ev-value in p_ev:
    for (i in 1:length(p_ev)){

      p_cur <- p_ev[i]  # current p value (as named probability)
      # print(p_cur)

      x_width <- p_cur

      x_val <- x_prv + x_width/2  # current x-value
      x_name <- names(p_cur)      # current x-name

      # x_ev <- as.numeric(substr(x_name, 1, nchar(x_name) - 1))  # current ev-value (from x_name)
      x_ev <- tally(x_name)  # using utility function on binary state representation

      if (show_ev){ # show ev value:
        lbl_i <- paste0(x_ev)
      } else if (show_bin){ # show binary state name:
        lbl_i <- paste0(x_name)
      } else if (show_n){ # show N value:
        lbl_i <- paste0(round(p_cur, 2))
      } else { # else: No label
        lbl_i <- NA
      }

      cur_col <- pal[x_ev + 1]  # current color


      # Compute transition values (for polygon segments):

      if ((show_trans == 1 | show_trans == 3) && (i %% 2 == 1)){ # odd segments (1, 3, 5...):

        x_top <- c(poly_cs[i], poly_cs[i])
        x_bot <- c(poly_cs[i + 1], poly_cs[i])

        y_top <- c(t_max - (t - 1), t_max - (t - 1)) - bar_width/2
        y_bot <- c(t_max - t, t_max - t) + bar_width/2

        xx <- c(x_top, x_bot)
        yy <- c(y_top, y_bot)

        pf_col <- cur_col  # polygon fill color

      }

      if ((show_trans == 2 | show_trans == 3) && (i %% 2 == 0)){ # even segments (2, 4, 6...):

        x_top <- c(poly_cs[i - 1], poly_cs[i + 1])
        x_bot <- c(poly_cs[i + 1], poly_cs[i])

        y_top <- c(t_max - (t - 1), t_max - (t - 1)) - bar_width/2
        y_bot <- c(t_max - t, t_max - t) + bar_width/2

        xx <- c(x_top, x_bot)
        yy <- c(y_top, y_bot)

        pf_col <- cur_col  # polygon fill color

      }


      # Draw i-th element:

      if (horizontal){ # horizontal bars:

        # Draw transition:
        if ((show_trans == 1 | show_trans == 3) && (i %% 2 == 1)){ # odd segments (1, 3, 5...):
          polygon(x = xx, y = yy, col = pf_col, border = col_hi)
        }

        if ((show_trans == 2 | show_trans == 3) && (i %% 2 == 0)){ # even segments (2, 4, 6...):
          polygon(x = xx, y = yy, col = pf_col, border = col_bd)
        }

        # Draw box:
        plot_cbox(x = x_val, y = cur_t, lx = x_width, ly = bar_width,
                  lbl = lbl_i, cex = cex_lbl,
                  col_fill = cur_col, col_brd = col_bd)


      } else { # vertical bars:

        # Draw transition:
        if ((show_trans == 1 | show_trans == 3) && (i %% 2 == 1)){ # odd segments (1, 3, 5...):
          polygon(x = t_max - yy, y = xx, col = pf_col, border = col_hi)
        }

        if ((show_trans == 2 | show_trans == 3) && (i %% 2 == 0)){ # even segments (2, 4, 6...):
          polygon(x = t_max - yy, y = xx, col = pf_col, border = col_bd)
        }

        # Draw box:
        plot_cbox(x = t_max - cur_t, y = x_val, lx = bar_width, ly = x_width,
                  lbl = lbl_i, cex = cex_lbl,
                  col_fill = cur_col, col_brd = col_bd)

      }

      x_prv <- x_prv + p_cur  # increment x_store (previous x-value)

    } # for each p/ev-value i.


    # Add text label:
    if (horizontal){ # horizontal bars:

      # label on left:
      text(x = 0, y = cur_t, labels = paste0(t, ":"), pos = 2, xpd = TRUE)

    } else { # vertical bars:

      # label on top:
      text(x = t_max - cur_t, y = N_max + 5/N_max, labels = paste0(t, ":"), pos = 3, xpd = TRUE)

    }

  } # for each time period t.


  # Title: ----

  # plot_title <- paste0("Cumulative risk dynamics (r = ", r, "; t = ", t, "; N = ", N, ")")
  # plot_title <- paste0("Cumulative risks (r = ", round(r, 2), ")")

  if (range(r)[1] != range(r)[2]){ # range of different r values:
    plot_title <- paste0("Cumulative risks (r = ", paste(round(r, 2), collapse = ", "), "; t = ", t, ")")
  } else { # constant r values:
    plot_title <- paste0("Cumulative risks (r = ", round(r, 2), "; t = ", t, ")")
  }

  title(main = plot_title, adj = 0)


  # Output: ----

  return(invisible(data))  # return data

} # plot_cbar().


# # Check:
# plot_cbar()  # default plot
#
# # Basic options:
# plot_cbar(r = .25, t = 4, N = 100)    # basic settings: horizontal bars
# plot_cbar(r = .25, t = 4, N = 100, horizontal = FALSE)  # vertical bars
#
# plot_cbar(r = .25, t = 4, N = 100, show_n = TRUE)  # view Ns
# plot_cbar(r = .25, t = 4, N = 100, sort = TRUE)    # sorting by ev
# plot_cbar(r = .25, t = 4, N = 100, N_max = 50)     # zooming
#
# plot_cbar(r = .25, t = 4, N = 100, show_trans = 0)  # only bars
# plot_cbar(r = .25, t = 4, N = 100, show_trans = 1)  # bars and new risk transitions
# plot_cbar(r = .25, t = 4, N = 100, show_trans = 2)  # bars and remaining risk trans
#
# plot_cbar(r = .25, t = 4, N = 100, bar_width = 1)   # only bars
# plot_cbar(r = .25, t = 4, N = 100, bar_width = 0)   # no bars, only transitions
#
# # Bar labels:
# plot_cbar(t = 4, show_ev = TRUE, show_n = FALSE, show_bin = FALSE)   # event occurrences
# plot_cbar(t = 4, show_ev = FALSE, show_n = TRUE, show_bin = FALSE)   # Ns
# plot_cbar(t = 4, show_ev = FALSE, show_n = FALSE, show_bin = TRUE)   # binary state
# plot_cbar(t = 4, show_ev = FALSE, show_n = FALSE, show_bin = FALSE)  # blank/nothing
# plot_cbar(t = 5, show_bin = TRUE, horizontal = FALSE)  # vertical bars for longer binary states

# Dynamics for multiple events:
# plot_cbar(r = .25, t = 8, N = 100, bar_width = 1, sort = TRUE, show_ev = FALSE)
# plot_cbar(r = .25, t = 8, N = 100, bar_width = 0, sort = TRUE, show_ev = FALSE)
# plot_cbar(r = .25, t = 8, N = 100, bar_width = 0, sort = TRUE, N_max = 10)  # zooming
#
# # Note: t = 8 implies 2^8 = 256 segments.

# # Generalization to variable values of r (as a vector):
# plot_cbar(r = seq(.50, .10, by = -.10), t = NA, N = 100)
# plot_cbar(r = seq(.10, .50, by = +.10), t = NA, show_bin = TRUE, horizontal = FALSE)
# plot_cbar(r = seq(.50, 1.0, by = +.25), t = NA, show_bin = TRUE)  # final value of 1 => slices of 0 width


# # Rainy days vs. always sun (with appropriate colors):
# plot_cbar(r = .20, t = 7, colors = c("steelblue", "grey96", "gold", "grey40"))
# plot_cbar(r = .20, t = 7, bar_width = 0,
#           colors = c("steelblue", "grey96", "gold", "grey40"))
# plot_cbar(r = .20, t = 7, bar_width = 1, hor = TRUE,
#           colors = c("steelblue", "grey96", "gold", "grey40"))
# plot_cbar(r = .20, t = 7, bar_width = 1, sort = TRUE, hor = TRUE,
#           colors = c("steelblue", "grey96", "gold", "grey40"))


# # Large values of r:
# plot_cbar(r = .90, t = 3, show_bin = TRUE)

# ?: +++ here now +++


# ToDo: Generalize to allow for risk reductions (-1 <= r < 0):

# Note: Allowing for recovery or risk reduction (r < 0) requires
# further assumptions and a different process,
# as this would change (reduce) the affected proportion of the population.
# The EV counts remain stable, but the proportion(s) affected decrease
# (but unclear, whether all affected segments decrease by the same rate).


# Further tests:

# plot_cbar(r = .50, t = 3, N = 100)
# plot_cbar(r = .50, t = 3, N = 100, sort = TRUE)  # sorting
# plot_cbar(r = .50, t = 3, N = 100, horizontal = FALSE)  # sorting
#
# plot_cbar(r = .75, t = 4, N = 100)
# plot_cbar(r = .75, t = 4, N = 100, sort = TRUE)  # sorting
# plot_cbar(r = .75, t = 4, N = 100, horizontal = FALSE)  # vertical bars
#
# plot_cbar(r = .05, t = 5, N_max = 25)  # zooming in
# plot_cbar(r = .05, t = 5, sort = TRUE, N_max = 5)  # sorting & zooming in

# # Problems from McCloy, Byrne, & Johnson-Laird (2010): p. 508
# # 1. Pregnancy (10%/90%), conjunctive: 1/2/3 year, disjunctive 3 year:
# plot_cbar(r = .10, t = 3, N = 1000, N_max = 1000)
# # 2. Kapi fruit (20%/80%), conjunctive: 1/2/3 year, disjunctive 3 year:
# plot_cbar(r = .20, t = 3, N = 1000, N_max = 1000)
# # 3. Eye infection (30%/70%), conjunctive: 1/2/3 year, disjunctive 3 year:
# plot_cbar(r = .30, t = 3, N = 1000, N_max = 1000)


# # Note some visual insights: ------
# # (i.e., insights based on visualizations):
#
# # 1. Small cumulative risks (r < .10) behave almost additively/linearly:
# plot_cbar(r = .01, t = 5, sort = F, N_max =  5)
# plot_cbar(r = .05, t = 5, sort = F, N_max = 25)
# plot_cbar(r = .10, t = 5, sort = F, N_max = 50)
#
# # 2. Large cumulative risks (r > .40) rapidly affect the entire population:
# plot_cbar(r = .40, t = 5, sort = F, N_max = 100)
# plot_cbar(r = .50, t = 5, sort = F, N_max = 100)
# plot_cbar(r = .60, t = 5, sort = F, N_max = 100)
# plot_cbar(r = .70, t = 5, sort = F, N_max = 100)
#
# # 3. => Intermediate range (.10 <= r <= .40) is crucial/problematic: sub-additive
# plot_cbar(r = .20, t = 5, sort = F, N_max = 100)
# plot_cbar(r = .30, t = 5, sort = F, N_max = 100)

# # 4. Permutation/order effects for varying risk magnitudes: Compare
# plot_cbar(r = c(.2, .3, .4))  # (seemingly linear increase of affected proportion) with
# plot_cbar(r = c(.4, .3, .2))  # (seemingly rapid decrease), but same affected proportions overall:
# plot_cbar(r = c(.4, .2, .3), sort = TRUE)  # => The 8 final segments always
# plot_cbar(r = c(.3, .2, .4), sort = TRUE)  #    contain the same permutations!



## (*) Done: ----------

# - Added option to sort cum-risk vectors by names:
#
# # Sort vector by element names:
# v <- 1:4
# names(v) <- c("B", "C", "D", "A")
# v[order(names(v))]
#
# - Added option for plotting as vertical bars (with 2 directions/levels).
#
# - Included show_n option to show frequency value in bar
#   (in addition to ev-occurrences)
#
# - Added transition links between time periods (as polygons)
#
# - Made bar_width and show_trans arguments of plot_cbar().
#
# - 1. Generalization to variable values of r (as a vector).

# - Used binary state names in comp_cum_ps() and
#   utility fun tally() and base_dec() to convert those into
#   number of event occurrences (e.g., for color hues).


## (+) ToDo: ----------

# - 2. Generalization: Make comp_cum_risk() and plot_cbar() work for
#   NEGATIVE risk values r < 0 (i.e., risk reductions).

# - rename x and y variables: x in terms of p, y in terms of t, etc.


## eof. ------------------------------------------
