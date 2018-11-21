## plot_mosaic.R | riskyr
## 2018 11 21
## -----------------------------------------------
## Plot mosaicplot that expresses freq as area
## (size and proportion)
## from 3 essential probabilities (prev, sens, spec)
## or current population data.frame popu.

## Dependencies: ----------

# library("vcd")   # moved to "Imports:" in in DESCRIPTION!
# library("grid")

## -----------------------------------------------
## plot_mosaic: Plot mosaic plot (with "vcd" and "grid")
## using only necessary arguments with good defaults:

## plot_mosaic: Documentation ----------

#' Plot a mosaic plot of population frequencies.
#'
#' \code{plot_mosaic} draws a mosaic plot that
#' represents the proportions of frequencies in the current
#' population \code{\link{popu}} as relatives sizes of
#' rectangular areas.
#'
#' If a sufficient and valid set of 3 essential probabilities
#' (\code{\link{prev}}, and
#' \code{\link{sens}} or its complement \code{\link{mirt}}, and
#' \code{\link{spec}} or its complement \code{\link{fart}})
#' is provided, new frequency information \code{\link{freq}}
#' and a new population table \code{\link{popu}}
#' are computed from scratch.  Otherwise, the existing
#' population \code{\link{popu}} is shown.
#'
#' Rectangles corresponding to the areas of the mosaic plot
#' can be visualized byopting for vertical rectangles (by selecting
#' the option \code{box = "vr"}) in \code{\link{plot_tree}}
#' and \code{\link{plot_fnet}}.
#'
#' Note that exact accuracy information is computed based on probabilities
#' (by \code{\link{comp_accu_prob}}), not approximations based on
#' rounded frequencies (by \code{\link{comp_accu_freq}}).
#'
#' \code{plot_mosaic} requires and uses the R packages "vcd" and
#' "grid" (\code{library("vcd", "grid")}).
#'
#'
#' @param prev The condition's prevalence \code{\link{prev}}
#' (i.e., the probability of condition being \code{TRUE}).
#'
#' @param sens The decision's sensitivity \code{\link{sens}}
#' (i.e., the conditional probability of a positive decision
#' provided that the condition is \code{TRUE}).
#' \code{sens} is optional when its complement \code{mirt} is provided.
#'
#' @param mirt The decision's miss rate \code{\link{mirt}}
#' (i.e., the conditional probability of a negative decision
#' provided that the condition is \code{TRUE}).
#' \code{mirt} is optional when its complement \code{sens} is provided.
#'
#' @param spec The decision's specificity value \code{\link{spec}}
#' (i.e., the conditional probability
#' of a negative decision provided that the condition is \code{FALSE}).
#' \code{spec} is optional when its complement \code{fart} is provided.
#'
#' @param fart The decision's false alarm rate \code{\link{fart}}
#' (i.e., the conditional probability
#' of a positive decision provided that the condition is \code{FALSE}).
#' \code{fart} is optional when its complement \code{spec} is provided.
#'
#' @param N The number of individuals in the population.
#' (This value is not represented in the plot,
#' but used when new frequency information \code{\link{freq}}
#' and a new population table \code{\link{popu}}
#' are computed from scratch from current probabilities.)
#'
#' @param by A character code specifying the perspective (or 1st category by which the population is split into subsets) with 2 options:
#'   \enumerate{
#'   \item \code{"cd"} ... by condition (vertical split first);
#'   \item \code{"dc"} ... by decision (horizontal split first).
#'   }
#'
#' @param vsplit Deprecated option for toggling between
#' vertical and horizontal split. Please use \code{by} instead.
#'
#' @param show_accu Option for showing current and exact
#' accuracy metrics \code{\link{accu}} in the plot
#' (computed by \code{\link{comp_accu_prob}}).
#' Default: \code{show_accu = TRUE}.
#'
#' @param w_acc Weigthing parameter \code{w} used to compute
#' weighted accuracy \code{w_acc} in \code{\link{comp_accu_prob}}.
#' Default: \code{w_acc = .50}.
#'
#' @param title_lbl Text label for current plot title.
#' Default: \code{title_lbl = txt$scen_lbl}.
#'
#' @param col_sdt Colors for cases of 4 essential frequencies.
#' Default: \code{col_sdt = c(pal["hi"], pal["mi"], pal["fa"], pal["cr"])}.
#'
#' @examples
#' # Basics:
#' plot_mosaic()                # => default options
#' plot_mosaic(title_lbl = "")  # => no title
#' plot_mosaic(by = "dc")       # => by decision (horizontal split 1st)
#' plot_mosaic(title_lbl = "My favorite scenario", col_sdt = "goldenrod")
#'
#' # Accuracy:
#' plot_mosaic(show_accu = TRUE)               # => default w = .5 (balanced accuracy "bacc")
#' plot_mosaic(show_accu = TRUE, w_acc = 1/3)  # => (weighted accuracy "wacc")
#' plot_mosaic(show_accu = FALSE)              # => no accuracy info.
#'
#'
#' @family visualization functions
#'
#'
#' @seealso
#' \code{\link{comp_popu}} computes the current population;
#' \code{\link{popu}} contains the current population;
#' \code{\link{comp_freq}} computes current frequency information;
#' \code{\link{freq}} contains current frequency information;
#' \code{\link{num}} for basic numeric parameters;
#' \code{\link{txt}} for current text settings;
#' \code{\link{pal}} for current color settings
#'
#' @export

## plot_mosaic: Definition ----------

plot_mosaic <- function(prev = num$prev,             # probabilities
                        sens = num$sens, mirt = NA,
                        spec = num$spec, fart = NA,
                        N = num$N,                   # not needed in Mosaic plot (but used in comp_freq below)
                        ## Options:
                        by = "cd",  # "cd"...condition 1st vs. "dc"...decision 1st
                        vsplit,     # deprecated predecessor of "by": toggle vertical vs. horizontal split
                        show_accu = TRUE, # compute and show accuracy metrics
                        w_acc = .50,      # weight w for wacc (from 0 to 1)
                        ## Text and color options: ##
                        title_lbl = txt$scen_lbl,
                        col_sdt = c(pal["hi"], pal["mi"], pal["fa"], pal["cr"])
) {

  ## (0) Handle deprecated arguments: ----------
  if (!missing(vsplit)) {
    warning("Boolean argument 'vsplit' is deprecated; please use 'by' instead.",
            call. = FALSE)
    if ( vsplit) { by <- "cd" }
    if (!vsplit) { by <- "dc" }
  }

  # ## (0) Get probabilities from global numeric parameters (num):
  # prev <- num$prev
  # sens <- num$sens
  # spec <- num$spec

  ## (1) Compute or use current popu: ----------

  ## (A) If a valid set of probabilities was provided:
  if (is_valid_prob_set(prev = prev, sens = sens, mirt = mirt, spec = spec, fart = fart, tol = .01)) {

    ## (a) Compute the complete quintet of probabilities:
    prob_quintet <- comp_complete_prob_set(prev, sens, mirt, spec, fart)
    sens <- prob_quintet[2] # gets sens (if not provided)
    mirt <- prob_quintet[3] # gets mirt (if not provided)
    spec <- prob_quintet[4] # gets spec (if not provided)
    fart <- prob_quintet[5] # gets fart (if not provided)

    ## (b) Compute cur_freq and popu based on current parameters (N and probabilities):
    cur_freq <- comp_freq(prev = prev, sens = sens, spec = spec, N = N, round = TRUE)  # compute cur_freq (with round = TRUE)

    ## Assign (only needed) elements of cur_freq:
    n.hi  <- cur_freq$hi
    n.mi  <- cur_freq$mi
    n.fa  <- cur_freq$fa
    n.cr  <- cur_freq$cr

    ## (c) Compute cur_popu from computed frequencies:
    cur_popu <- comp_popu(hi = n.hi, mi = n.mi, fa = n.fa, cr = n.cr)  # compute cur_popu (from 4 essential frequencies)

    # warning("Generated new population (cur_popu) to draw mosaic plot...")


  } else {  # (B) NO valid set of probabilities was provided:

    ## Use the current popu:
    cur_popu <- popu

    # warning("Using existing population (popu) to draw mosaic plot...")

  } # if (is_valid_prob_set...)

  ## (2) Text labels: ----------

  if (nchar(title_lbl) > 0) { title_lbl <- paste0(title_lbl, ":\n") }  # put on top (in separate line)
  cur_title_lbl <- paste0(title_lbl, "Mosaic plot") # , "(N = ", N, ")")

  ## 1. freq label:
  freq_lbl <- make_freq_lbl(hi = n.hi, mi = n.mi, fa = n.fa, cr = n.cr)   # use current freq values
  # mtext(freq_lbl, side = 1, line = 0, adj = 0, col = m_col, cex = m_cex)  # print freq label

  ## 2. Condition / p(cond) label:
  cur_cond_lbl <- make_cond_lbl(prev, sens, spec)  # use utility function to format label
  # cur_dec_lbl <- make_dec_lbl(ppod, PPV, NPV)  # use utility function to format label

  ## 3. Accuracy label:
  cur_accu_lbl <- ""
  if (show_accu) {

    if (!is.na(prev) && !is.na(sens) && !is.na(spec)) {  # prob are known:

      # (1) Compute exact accuracy from prob:
      cur_accu <- comp_accu_prob(prev = prev, sens = sens, spec = spec, w = w_acc)

    } else {  # use freq:

      # (2) Compute accuracy info from (rounded) freq:
      cur_accu <- comp_accu_freq(hi = n.hi, mi = n.mi, fa = n.fa, cr = n.cr, w = w_acc)

    }

    cur_accu_lbl <- make_accu_lbl(acc = cur_accu$acc, w = w_acc, wacc = cur_accu$wacc, mcc = cur_accu$mcc)  # use utility function

  }

  ## Combine 3 labels:
  cur_par_lbl <- paste0(freq_lbl, "\n", cur_cond_lbl, "\n", cur_accu_lbl, "\n")

  ## (3) Define plotting area: --------

  ## Record graphical parameters (par):
  opar <- par(no.readonly = TRUE)  # all par settings that can be changed.
  on.exit(par(opar))

  ## Define margin areas:
  n_lines_mar <- 3
  n_lines_oma <- 0
  par(mar = c(n_lines_mar, 1, 2, 1) + 0.1)  # margins; default: par("mar") = 5.1 4.1 4.1 2.1.
  par(oma = c(n_lines_oma, 0, 0, 0) + 0.1)  # outer margins; default: par("oma") = 0 0 0 0.


  ## (4) Mosaic plot: ----------

  if (by == "cd") {

    ## (a) by condition (vertical split 1st):
    vcd::mosaic(Decision ~ Truth, data = cur_popu,
                shade = TRUE, colorize = TRUE,
                split_vertical = TRUE,
                gp = grid::gpar(fill = matrix(data = col_sdt, nrow = 2, ncol = 2, byrow = TRUE)),
                main_gp = grid::gpar(fontsize = 12, fontface = 1, adj = 0),
                sub_gp = grid::gpar(fontsize = 10, fontface = 1, adj = 0),
                main = paste0(cur_title_lbl), #, "\n", cur_par_lbl),
                sub = paste0(cur_par_lbl)  # print label
    )
  }

  else if (by == "dc") {

    ## (b) by decision (horizontal split 1st):
    vcd::mosaic(Truth ~ Decision, data = cur_popu,
                shade = TRUE, colorize = TRUE,
                split_vertical = FALSE,
                gp = grid::gpar(fill = matrix(data = col_sdt, nrow = 2, ncol = 2, byrow = FALSE)),
                main_gp = grid::gpar(fontsize = 12, fontface = 1),
                sub_gp = grid::gpar(fontsize = 10, fontface = 1),
                main = paste0(cur_title_lbl), #, "\n", cur_par_lbl),
                sub = paste0(cur_par_lbl)
    )
  } # if (by == ...)

  ## Title and margin text:
  # title(cur_title_lbl, adj = 0.5, line = -0.5, font.main = 1) # (left, lowered, normal font)

  ## (5) Finish: ---------

  invisible()  # restores par(opar)

} # plot_mosaic(...) end.

## Check: ----------
  plot_mosaic()
  # plot_mosaic(title_lbl = "")
  # plot_mosaic(by = "dc")
  # plot_mosaic(title_lbl = "Just testing", col_sdt = "goldenrod")

## (+) ToDo: ---------

## - add labels for (prev, sens, spec), (ppod, PPV, NPV), bacc.
## - add Decisions panel.
## - consider adding 3rd perspective: (c) by correspondence (accu)
## - make mosaic plot dependent on basic parameters
##   (i.e., compute comp_popu() from probabilities and N,
##    rather than providing it as input)?
## - add a simpler version that only shows cond.true vs. cond.false
## - adjust parameters (zero size and gap width)
## - add labels (frequencies) to plot?

## eof. ---------
