## cum_risk.R | riskyr
## 2024 01 01
## Compute and plot cumulative risks

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


# - c: comp_ev_p: Combine comp_ev() and comp_p() ----

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



# B. comp_cum_ps(): Iterative generation of cumulative risks: ------

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



# C. plot_cum_bar(): Compute and plot cum-risks (as barchart): ------


#' Plot cumulative risk dynamics (as bar chart)
#'
#' plot_cum_bar plots the results of cumulative risk dynamics
#' as a bar chart (with percentages of risk event counts
#' for each period t on a horizontal bar).
#'
#' @param r risk (probability of occurrence per time period).
#'
#' @param t time periods/rounds.
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
#' Values of 0/1/2 focus on no/new/remaining risk segments, respectively.
#' Default: \code{show_trans = 1} (i.e., focus on increasing risk segments).
#'
#' @param show_ev logical: Show number of risky event occurrence (as bar label)?
#' Default: \code{show_ev = TRUE}.
#'
#' @param show_n logical: Show population frequency of risky event occurrences (as bar label)?
#' Default: \code{show_n = FALSE}.
#'
#' @return data of p-values, named by number of event occurrences
#' (invisibly, as list of named vectors, for each time period t).
#'
#' @importFrom grDevices colorRampPalette
#'

plot_cum_bar <- function(r = .50, t = 1, N = 100,
                         horizontal = TRUE, sort = FALSE,
                         N_max = 100, bar_width = .50,
                         show_trans = 1, show_ev = TRUE, show_n = FALSE){


  # Handle inputs: ----

  if (r < 0 || r > 1){
    message("risk r should be in (0, 1)")
  }

  if (bar_width < 0 || bar_width > 1){
    message("bar_width should be in (0, 1)")
  }

  if (show_n && show_ev){
    message("Set show_n = TRUE: Setting shown_ev = FALSE")
    show_ev <- FALSE
  }

  if (show_trans %in% 0:2 == FALSE){
    message("show_trans should be in 0:2: Setting shown_trans = 1 (i.e., focus on risk increase)")
    show_trans <- 1  # use default
  }

  if (sort && (show_trans != 0)){
    message("Transitions correspond to unsorted arrangement of bar segments (sort = FALSE)")
  }


  # Compute cumulative probability data: ----

  data <- comp_cum_ps(r = r, t = t, N = N)  # list of p values (with ev names)

  data_cs <- lapply(data, cumsum)  # cumsum() of p values


  # Prepare plot: ----

  opar <- par(no.readonly = TRUE)  # all par settings that can be changed.
  on.exit(par(opar))  # par(opar)  # restore original settings


  # Plot dimensions:

  t_max <- t
  y_max <- t_max + 1


  # Constants:

  # bar_width  <- .50  # (from 0 to 1)

  # show_trans <- TRUE # FALSE

  # # bar labels:
  # show_ev <- TRUE
  # show_n  <- FALSE


  cex_lbl <- 1 - (5 * t_max/100)

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
         xlim = c(0, N_max), ylim = c(0.5, y_max + 0.5),
         axes = FALSE)

    mtext(x_lbl, adj = .50, side = 1, line = 2)          # x-axis label
    mtext("Time period", adj = .50, side = 2, line = 1)  # y-axis label


  } else { # vertical bars:

    par(mar = c(3, 4, 4, 1) + 0.1)  # margins; default: par("mar") = 5.1 4.1 4.1 2.1.

    y_max <- t_max + .50  # adjust

    plot(0:1, 0:1, type = "n",
         xlab = NA, ylab = NA,
         xlim = c(-0.5, y_max), ylim = c(0, N_max),
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
  col_lo  <- "grey98"
  col_hi  <- "firebrick" # "steelblue", "deepskyblue", "deeppink", "olivedrab", "grey20", "red3"
  brd_col <- "grey40"    # "white" # "grey20"
  col_no  <- "forestgreen" # "deepskyblue"

  # Color palette:
  n_cols  <- 1 + t_max

  # pal <- unikn::usecol(pal = c(col_lo, col_hi), n = n_cols, alpha = .80)
  pal <- grDevices::colorRampPalette(colors = c(col_lo, col_hi))(n_cols)

  # Replace 1st color (ev = 0):
  pal[1] <- grDevices::colorRampPalette(colors = c("white", col_no))(8)[2]
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
    plot_cbox(x = N/2, y = y_max, lx = N, ly = bar_width,
              lbl = lbl_0, cex = cex_lbl,
              col_fill = cur_col, col_brd = brd_col)

    # Add label (on left):
    text(x = 0, y = y_max, labels = "0:", pos = 2, xpd = TRUE)

  } else { # vertical bars:

    # Draw bar/box 0:
    plot_cbox(x = y_max - y_max, y = N/2, lx = bar_width, ly = N,
              lbl = lbl_0, cex = cex_lbl,
              col_fill = cur_col, col_brd = brd_col)

    # Add label (on top):
    text(x = y_max - y_max, y = N_max + 5/N_max, labels = "0:", pos = 3, xpd = TRUE)

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
    y_val <- y_max - t

    # For each p/ev-value in p_ev:
    for (i in 1:length(p_ev)){

      p_cur <- p_ev[i]  # current p value (as named probability)
      # print(p_cur)

      x_width <- p_cur

      x_val <- x_prv + x_width/2  # current x-value
      x_name <- names(p_cur)      # current x-name
      x_ev <- as.numeric(substr(x_name, 1, nchar(x_name) - 1))  # current ev-value

      if (show_ev){
        lbl_i <- paste0(x_ev)
      } else if (show_n){
        lbl_i <- paste0(round(p_cur, 2))
      } else { # default;
        lbl_i <- NA
      }

      cur_col <- pal[x_ev + 1]  # current color


      # Compute transition values (for polygon segments):

      if ((show_trans == 1) && (i %% 2 == 1)){ # odd segments (1, 3, 5...):

        x_top <- c(poly_cs[i], poly_cs[i])
        x_bot <- c(poly_cs[i + 1], poly_cs[i])

        y_top <- c(y_max - (t - 1), y_max - (t - 1)) - bar_width/2
        y_bot <- c(y_max - t, y_max - t) + bar_width/2

        xx <- c(x_top, x_bot)
        yy <- c(y_top, y_bot)

        pf_col <- cur_col  # polygon fill color

      }

      if ((show_trans == 2) && (i %% 2 == 0)){ # even segments (2, 4, 6...):

        x_top <- c(poly_cs[i - 1], poly_cs[i + 1])
        x_bot <- c(poly_cs[i + 1], poly_cs[i])

        y_top <- c(y_max - (t - 1), y_max - (t - 1)) - bar_width/2
        y_bot <- c(y_max - t, y_max - t) + bar_width/2

        xx <- c(x_top, x_bot)
        yy <- c(y_top, y_bot)

        pf_col <- cur_col  # polygon fill color

      }


      # Draw i-th element:

      if (horizontal){ # horizontal bars:

        # Draw transition:
        if ((show_trans == 1) && (i %% 2 == 1)){ # odd segments (1, 3, 5...):
          polygon(x = xx, y = yy, col = pf_col, border = col_hi)
        }

        if ((show_trans == 2) && (i %% 2 == 0)){ # even segments (2, 4, 6...):
          polygon(x = xx, y = yy, col = pf_col, border = brd_col)
        }

        # Draw box:
        plot_cbox(x = x_val, y = y_val, lx = x_width, ly = bar_width,
                  lbl = lbl_i, cex = cex_lbl,
                  col_fill = cur_col, col_brd = brd_col)


      } else { # vertical bars:

        # Draw transition:
        if ((show_trans == 1) && (i %% 2 == 1)){ # odd segments (1, 3, 5...):
          polygon(x = y_max - yy, y = xx, col = pf_col, border = col_hi)
        }

        if ((show_trans == 2) && (i %% 2 == 0)){ # even segments (2, 4, 6...):
          polygon(x = y_max - yy, y = xx, col = pf_col, border = brd_col)
        }

        # Draw box:
        plot_cbox(x = y_max - y_val, y = x_val, lx = bar_width, ly = x_width,
                  lbl = lbl_i, cex = cex_lbl,
                  col_fill = cur_col, col_brd = brd_col)

      }

      x_prv <- x_prv + p_cur  # increment x_store (previous x-value)

    } # for each p/ev-value i.


    # Add text label:
    if (horizontal){ # horizontal bars:

      # label on left:
      text(x = 0, y = y_val, labels = paste0(t, ":"), pos = 2, xpd = TRUE)

    } else { # vertical bars:

      # label on top:
      text(x = y_max - y_val, y = N_max + 5/N_max, labels = paste0(t, ":"), pos = 3, xpd = TRUE)

    }

  } # for each time period t.


  # Title: ----

  # plot_title <- paste0("Cumulative risk dynamics (r = ", r, "; t = ", t, "; N = ", N, ")")
  plot_title <- paste0("Cumulative risk dynamics (r = ", round(r, 2), ")")

  title(main = plot_title, adj = 0)


  # Output: ----

  return(invisible(data))  # return data

} # plot_cum_bar().


# # # Check:
# plot_cum_bar()
#
# plot_cum_bar(r = .25, t = 4, N = 100)
# plot_cum_bar(r = .25, t = 4, N = 100, show_n = TRUE)
# plot_cum_bar(r = .25, t = 4, N = 100, sort = TRUE)  # sorting
# plot_cum_bar(r = .25, t = 4, N = 100, horizontal = FALSE)  # vertical bars
# plot_cum_bar(r = .25, t = 5, N = 100, show_trans = FALSE)  # only bars
# plot_cum_bar(r = .25, t = 5, N = 100, bar_width = 1)  # only bars
# plot_cum_bar(r = .25, t = 5, N = 100, bar_width = 0)  # no bars

# plot_cum_bar(r = .50, t = 3, N = 100)
# plot_cum_bar(r = .50, t = 3, N = 100, sort = TRUE)  # sorting
# plot_cum_bar(r = .50, t = 3, N = 100, horizontal = FALSE)  # sorting
#
# plot_cum_bar(r = .75, t = 4, N = 100)
# plot_cum_bar(r = .75, t = 4, N = 100, sort = TRUE)  # sorting
# plot_cum_bar(r = .75, t = 4, N = 100, horizontal = FALSE)  # vertical bars
#
# plot_cum_bar(r = .05, t = 5, N_max = 25)  # zooming in
# plot_cum_bar(r = .05, t = 5, sort = TRUE, N_max = 1)  # sorting & zooming in

# # Problems from McCloy, Byrne, & Johnson-Laird (2010): p. 508
# # 1. Pregnancy (10%/90%), conjunctive: 1/2/3 year, disjunctive 3 year:
# plot_cum_bar(r = .10, t = 3, N = 1000, N_max = 1000)
# # 2. Kapi fruit (20%/80%), conjunctive: 1/2/3 year, disjunctive 3 year:
# plot_cum_bar(r = .20, t = 3, N = 1000, N_max = 1000)
# # 3. Eye infection (30%/70%), conjunctive: 1/2/3 year, disjunctive 3 year:
# plot_cum_bar(r = .30, t = 3, N = 1000, N_max = 1000)


# # Note some insights (from visualizations):
#
# # 1. Small cumulative risks (r < .10) behave almost additively/linearly:
# plot_cum_bar(r = .01, t = 5, sort = F, N_max =  5)
# plot_cum_bar(r = .05, t = 5, sort = F, N_max = 25)
# plot_cum_bar(r = .10, t = 5, sort = F, N_max = 50)
#
# # 2. Large cumulative risks (r > .40) rapidly affect the entire population:
# plot_cum_bar(r = .40, t = 5, sort = F, N_max = 100)
# plot_cum_bar(r = .50, t = 5, sort = F, N_max = 100)
# plot_cum_bar(r = .60, t = 5, sort = F, N_max = 100)
# plot_cum_bar(r = .70, t = 5, sort = F, N_max = 100)
#
# # 3. => Intermediate range (.10 <= r <= .40) is crucial/problematic: sub-additive
# plot_cum_bar(r = .20, t = 5, sort = F, N_max = 100)
# plot_cum_bar(r = .30, t = 5, sort = F, N_max = 100)


# ?: +++ here now +++


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

## (+) ToDo: ----------

# - make bar_width and show_trans arguments of plot_cum_bar().
# - rename x and y variables: x in terms of p, y in terms of t, etc.

# - Use more appropriate data structure for cumulative probabilities (ps)?
# - How to grow a (binary) tree structure in R?


## eof. ------------------------------------------
