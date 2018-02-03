## plot_mosaic.R | riskyr
## 2018 02 03
## -----------------------------------------------
## Plot mosaicplot that expresses freq as area
## (size and proportion)
## from 3 essential probabilities (prev, sens, spec)
## or current population data.frame popu.

## -----------------------------------------------
## Dependencies:

# library("vcd")   # moved to "Imports:" in in DESCRIPTION!
# library("grid")

## -----------------------------------------------
## plot_mosaic: Plot mosaic plot (with "vcd" and "grid")
## using only necessary arguments with good defaults:
## -----------------------------------------------


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
#'
#' @param N The number of individuals in the population.
#' (This value is not represented in the plot,
#' but used when new frequency information \code{\link{freq}}
#' and a new population table \code{\link{popu}}
#' are computed from scratch from current probabilities.)
#'
#' @param vsplit Option for toggling between
#' vertical and horizontal split.
#' Default: \code{vsplit = TRUE}.
#'
#' @param show.accu Option for showing current
#' accuracy metrics \code{\link{accu}} in the plot.
#' Default: \code{show.accu = TRUE}.
#'
#' @param w.acc Weigthing parameter \code{w} used to compute
#' weighted accuracy \code{w.acc} in \code{\link{comp_accu}}.
#' Default: \code{w.acc = .50}.
#'
#'
#' @examples
#' plot_mosaic()                # => default options
#' plot_mosaic(title.lbl = "")  # => no title
#' plot_mosaic(vsplit = FALSE)  # => horizontal split
#' plot_mosaic(title.lbl = "My favorite scenario", col.sdt = "goldenrod")
#'
#' # Accuracy:
#' plot_mosaic(show.accu = TRUE)               # => default w = .5 (balanced accuracy "bacc")
#' plot_mosaic(show.accu = TRUE, w.acc = 1/3)  # => (weighted accuracy "wacc")
#' plot_mosaic(show.accu = FALSE)              # => no accuracy info.
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

plot_mosaic <- function(prev = num$prev,             # probabilities
                        sens = num$sens, mirt = NA,
                        spec = num$spec, fart = NA,
                        N = num$N,                   # not needed in Mosaic plot (but used in comp_freq below)
                        ## Options: ##
                        vsplit = TRUE,    # toggle vertical vs. horizontal split
                        show.accu = TRUE, # compute and show accuracy metrics
                        w.acc = .50,      # weight w for wacc (from 0 to 1)
                        ## Text and color options: ##
                        title.lbl = txt$scen.lbl,
                        col.sdt = c(pal["hi"], pal["mi"], pal["fa"], pal["cr"])
) {

  # ## (0) Get probabilities from global numeric parameters (num):
  # prev <- num$prev
  # sens <- num$sens
  # spec <- num$spec


  ## (1) Compute or use current popu:

  ## (A) If a valid set of probabilities was provided:
  if (is_valid_prob_set(prev = prev, sens = sens, mirt = mirt, spec = spec, fart = fart, tol = .01)) {

    ## (a) Compute the complete quintet of probabilities:
    prob_quintet <- comp_complete_prob_set(prev, sens, mirt, spec, fart)
    sens <- prob_quintet[2] # gets sens (if not provided)
    mirt <- prob_quintet[3] # gets mirt (if not provided)
    spec <- prob_quintet[4] # gets spec (if not provided)
    fart <- prob_quintet[5] # gets fart (if not provided)

    ## (b) Compute cur.freq and popu based on current parameters (N and probabilities):
    cur.freq <- comp_freq(prev = prev, sens = sens, spec = spec, N = N, round = TRUE)  # compute cur.freq (with round = TRUE)

    ## Assign (only needed) elements of cur.freq:
    n.hi  <- cur.freq$hi
    n.mi  <- cur.freq$mi
    n.fa  <- cur.freq$fa
    n.cr  <- cur.freq$cr

    ## (c) Compute cur.popu from computed frequencies:
    cur.popu <- comp_popu(hi = n.hi, mi = n.mi, fa = n.fa, cr = n.cr)  # compute cur.popu (from 4 essential frequencies)

    # warning("Generated new population (cur.popu) to draw mosaic plot...")


  } else {  # (B) NO valid set of probabilities was provided:

    ## Use the current popu:
    cur.popu <- popu

    # warning("Using existing population (popu) to draw mosaic plot...")

  } # if (is_valid_prob_set...)

  ## (2) Text labels:
  if (nchar(title.lbl) > 0) { title.lbl <- paste0(title.lbl, ":\n") }  # put on top (in separate line)
  cur.title.lbl <- paste0(title.lbl, "Mosaic plot") # , "(N = ", N, ")")
  cur.par.lbl <-  paste0("Conditions: ", "prev = ", as_pc(prev), "%, ", "sens = ", as_pc(sens), "%, ", "spec = ", as_pc(spec), "%")

  ## (3) Accuracy:

  if (show.accu) {
    cur.accu <- comp_accu(hi = n.hi, mi = n.mi, fa = n.fa, cr = n.cr, w = w.acc)

    if (w.acc == .50) {  # wacc is bacc:
      wacc.lbl <- paste0("bacc = ", as_pc(cur.accu$wacc, n.digits = 1), "%, ")
    } else {  # show wacc with w:
      wacc.lbl <- paste0("wacc = ", as_pc(cur.accu$wacc, n.digits = 1), "% ",
                         "(w = ", round(w.acc, 2), "), ")
    }

    cur.accu.lbl <- paste0("",
                           "Acc = ", as_pc(cur.accu$acc, n.digits = 1), "%, ",
                           wacc.lbl,
                           "mcc = ", round(cur.accu$mcc, 2), "")

    # mtext(cur.accu.lbl, side = 1, line = 2, adj = 1, col = grey(.33, .99), cex = .85)
    cur.par.lbl <- paste0(cur.par.lbl, "\n", cur.accu.lbl, "\n") # add accuracy lbl
  }


  ## (4) Define plot area:
  # plot(0, type = 'n')


  ## (5) Mosaic plot:

  if (vsplit) {

    ## (a) vertical split:
    vcd::mosaic(Decision ~ Truth, data = cur.popu,
                shade = TRUE, colorize = TRUE,
                split_vertical = TRUE,
                gp = grid::gpar(fill = matrix(data = col.sdt, nrow = 2, ncol = 2, byrow = TRUE)),
                main_gp = grid::gpar(fontsize = 12, fontface = 1, adj = 0),
                sub_gp = grid::gpar(fontsize = 10, fontface = 1, adj = 1),
                main = paste0(cur.title.lbl), #, "\n", cur.par.lbl),
                sub = paste0(cur.par.lbl)
    )
  }

  else {

    ## (b) horizontal split:
    vcd::mosaic(Truth ~ Decision, data = cur.popu,
                shade = TRUE, colorize = TRUE,
                split_vertical = FALSE,
                gp = grid::gpar(fill = matrix(data = col.sdt, nrow = 2, ncol = 2, byrow = FALSE)),
                main_gp = grid::gpar(fontsize = 12, fontface = 1),
                sub_gp = grid::gpar(fontsize = 10, fontface = 1),
                main = paste0(cur.title.lbl), #, "\n", cur.par.lbl),
                sub = paste0(cur.par.lbl)
    )

  } # if (vsplit)...

  ## Title and margin text:
  # title(cur.title.lbl, adj = 0.5, line = -0.5, font.main = 1) # (left, lowered, normal font)

}

## Check:
# plot_mosaic()
# plot_mosaic(title.lbl = "")
# plot_mosaic(vsplit = FALSE)
# plot_mosaic(title.lbl = "Just testing", col.sdt = "goldenrod")

## -----------------------------------------------
## (+) ToDo:

## - make mosaic plot dependent on basic parameters
##   (i.e., compute comp_popu() from probabilities and N,
##    rather than providing it as input)?
## - add a simpler version that only shows cond.true vs. cond.false
## - adjust parameters (zero size and gap width)
## - add labels (frequencies) to plot?

## -----------------------------------------------
## eof.
