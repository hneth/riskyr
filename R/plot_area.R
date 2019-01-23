## plot_area.R | riskyr
## 2019 01 23
## Plot area diagram (replacing plot_mosaic.R).
## -----------------------------------------------

## (1) plot_area: Documentation ----------

#' Plot an area diagram of probabilities or frequencies.
#'
#' \code{plot_area} assigns the total probability
#' or population frequency to an area (square or rectangle)
#' and shows the probability or frequency of
#' 4 classification cases (\code{\link{hi}}, \code{\link{mi}},
#' \code{\link{fa}}, \code{\link{cr}})
#' as relative proportions of this area.
#'
#' \code{plot_area} computes probabilities \code{\link{prob}}
#' and frequencies \code{\link{freq}}
#' from a sufficient and valid set of 3 essential probabilities
#' (\code{\link{prev}}, and
#' \code{\link{sens}} or its complement \code{\link{mirt}}, and
#' \code{\link{spec}} or its complement \code{\link{fart}})
#' or existing frequency information \code{\link{freq}}
#' and a population size of \code{\link{N}} individuals.
#'
#' \code{plot_area} generalizes and replaces \code{\link{plot_mosaic}}.
#' by removing the dependency on the R packages \code{vcd} and \code{grid}
#' and providing many additional options.
#'
#' @param prev  The condition's prevalence \code{\link{prev}}
#' (i.e., the probability of condition being \code{TRUE}).
#'
#' @param sens  The decision's sensitivity \code{\link{sens}}
#' (i.e., the conditional probability of a positive decision
#' provided that the condition is \code{TRUE}).
#' \code{sens} is optional when its complement \code{mirt} is provided.
#'
#' @param mirt  The decision's miss rate \code{\link{mirt}}
#' (i.e., the conditional probability of a negative decision
#' provided that the condition is \code{TRUE}).
#' \code{mirt} is optional when its complement \code{sens} is provided.
#'
#' @param spec  The decision's specificity value \code{\link{spec}}
#' (i.e., the conditional probability
#' of a negative decision provided that the condition is \code{FALSE}).
#' \code{spec} is optional when its complement \code{fart} is provided.
#'
#' @param fart  The decision's false alarm rate \code{\link{fart}}
#' (i.e., the conditional probability
#' of a positive decision provided that the condition is \code{FALSE}).
#' \code{fart} is optional when its complement \code{spec} is provided.
#'
#' @param N  The number of individuals in the population.
#' A suitable value of \code{\link{N}} is computed, if not provided.
#' Note: \code{\link{N}} is not represented in the plot,
#' but used for computing frequency information \code{\link{freq}}
#' from current probabilities \code{\link{prob}}.
#'
#' @param by  A character code specifying 2 perspectives that split the population into subsets,
#' with 6 options:
#'   \enumerate{
#'   \item \code{"cddc"}: by condition (cd) and by decision (dc) (default);
#'   \item \code{"cdac"}: by condition (cd) and by accuracy (ac);
#'   \item \code{"dccd"}: by decision (dc) and by condition (cd);
#'   \item \code{"dcac"}: by decision (dc) and by accuracy (ac);
#'   \item \code{"accd"}: by accuracy (ac) and by condition (cd);
#'   \item \code{"acdc"}: by accuracy (ac) and by decision (dc).
#'   }
#'
#' @param p_split  Primary perspective for population split,
#' with 2 options:
#'   \enumerate{
#'   \item \code{"v"}: vertical (default);
#'   \item \code{"h"}: horizontal.
#'   }
#'
#' @param area  A character code specifying the shape of the main area,
#' with 2 options:
#'   \enumerate{
#'   \item \code{"sq"}: main area is scaled to square (default);
#'   \item \code{"no"}: no scaling (rectangular area fills plot size).
#'   }
#'
#' @param scale  Scale probabilities and corresponding area dimensions either by
#' exact probability or by (rounded or non-rounded) frequency, with 2 options:
#'   \enumerate{
#'   \item \code{"p"}: scale main area dimensions by exact probability (default);
#'   \item \code{"f"}: re-compute probabilities from (rounded or non-rounded) frequencies
#'   and scale main area dimensions by their frequency.
#'   }
#'  Note: \code{scale} setting matters for the display of probability values and for
#'  area plots with small population sizes \code{\link{N}} when \code{round = TRUE}.
#'
#' @param round  A Boolean option specifying whether computed frequencies
#' are rounded to integers. Default: \code{round = TRUE}.
#'
#' @param sum_w  Border width of 2 perspective summaries
#' (on top and left borders) of main area as a proportion of area size
#' (i.e., in range \code{0 <= sum_w <= 1}).
#' Default: \code{sum_w = .10}.
#' Setting \code{sum_w = 0}, \code{NA}, or \code{NULL} removes summaries;
#' setting \code{sum_w = 1} scales summaries to same size as main areas.
#'
#' @param gaps Size of gaps (as binary numeric vector) specifying
#' the width of vertical and horizontal gaps as proportions of area size.
#' Defaults: \code{gaps = c(.02, .00)} for \code{p_split = "v"} and
#' \code{gaps = c(.00, .02)} for \code{p_split = "h"}.
#'
#' @param f_lbl  Type of label for showing frequency values in 4 main areas,
#' with 6 options:
#'   \enumerate{
#'   \item \code{"def"}: abbreviated names and frequency values;
#'   \item \code{"abb"}: abbreviated frequency names only (as specified in code);
#'   \item \code{"nam"}: names only (as specified in \code{lbl_txt = txt});
#'   \item \code{"num"}: numeric frequency values only (default);
#'   \item \code{"namnum"}: names (as specified in \code{lbl_txt = txt}) and numeric values;
#'   \item \code{"no"}: no frequency labels (same for \code{f_lbl = NA} or \code{NULL}).
#'   }
#'
#' @param f_lbl_sep  Label separator for main frequencies
#' (used for \code{f_lbl = "def" OR "namnum"}).
#' Use \code{f_lbl_sep = ":\n"} to add a line break between name and numeric value.
#' Default: \code{f_lbl_sep = NA} (set to \code{" = "} or \code{":\n"} based on \code{f_lbl}).
#'
#' @param f_lbl_sum  Type of label for showing frequency values in summary cells,
#' with same 6 options as \code{f_lbl} (above).
#' Default: \code{f_lbl_sum = "num"}: numeric values only.
#'
#' @param f_lbl_hd  Type of label for showing frequency values in header,
#' with same 6 options as \code{f_lbl} (above).
#' Default: \code{f_lbl_hd = "nam"}: names only (as specified in \code{lbl_txt = txt}).
#'
#' @param f_lwd  Line width of areas.
#' Default: \code{f_lwd = 0}.
#'
#' @param p_lbl  Type of label for showing 3 key probability links and values,
#' with 7 options:
#'   \enumerate{
#'   \item \code{"def"}: show links and abbreviated names and probability values;
#'   \item \code{"abb"}: show links and abbreviated probability names;
#'   \item \code{"nam"}: show links and probability names (as specified in code);
#'   \item \code{"num"}: show links and numeric probability values;
#'   \item \code{"namnum"}: show links with names and numeric probability values;
#'   \item \code{"no"}: show links with no labels;
#'   \item \code{NA}: show no labels or links (same for \code{p_lbl = NULL}, default).
#'   }
#'
#' @param arr_c  Arrow code for symbols at ends of probability links
#' (as a numeric value \code{-3 <= arr_c <= +6}),
#' with the following options:
#'   \itemize{
#'   \item \code{-1} to \code{-3}: points at one/other/both end/s;
#'   \item \code{0}: no symbols;
#'   \item \code{+1} to \code{+3}: V-arrow at one/other/both end/s;
#'   \item \code{+4} to \code{+6}: T-arrow at one/other/both end/s.
#' }
#' Default: \code{arr_c = -3} (points at both ends).
#'
#' @param col_p  Colors of probability links (as vector of 3 colors).
#' Default: \code{col_p = c(grey(.15, .99), "yellow", "yellow")}.
#' (Also consider: "black", "cornsilk", "whitesmoke").
#'
#' @param brd_dis  Distance of probability links from area border
#' (as proportion of area width).
#' Default: \code{brd_dis = .06}.
#' Note: Adjust to avoid overlapping labels.
#' Negative values show links outside of main area.
#'
#' @param lbl_txt  Default label set for text elements.
#' Default: \code{lbl_txt = \link{txt}}.
#'
#' @param title_lbl  Text label for current plot title.
#' Default: \code{title_lbl = txt$scen_lbl}.
#'
#' @param cex_lbl  Scaling factor for text labels (frequencies and headers).
#' Default: \code{cex_lbl = .90}.
#'
#' @param cex_p_lbl  Scaling factor for text labels (probabilities).
#' Default: \code{cex_p_lbl = cex_lbl - .05}.
#'
#' @param col_pal  Color palette.
#' Default: \code{col_pal = \link{pal}}.
#'
#' @param mar_notes  Boolean option for showing margin notes.
#' Default: \code{mar_notes = TRUE}.
#'
#' @param ...  Other (graphical) parameters.
#'
#' @return Nothing (NULL).
#'
#' @examples
#' ## Basics:
#' plot_area()  # default area plot,
#' # same as:
#' # plot_area(by = "cddc", p_split = "v", area = "sq", scale = "p")
#'
#' # Local freq and prob values:
#' plot_area(prev = .5, sens = 4/5, spec = 3/5, N = 10)
#'
#' # Customizing text and color:
#' plot_area(prev = .2, sens = 4/5, spec = 3/5, N = 10,
#'           by = "cddc", p_split = "v", scale = "p",
#'           title_lbl = "Custom text and color:",
#'           lbl_txt = txt_org, f_lbl = "namnum",
#'           f_lwd = 2, col_pal = pal_rgb)
#' plot_area(prev = .4, sens = 6/7, spec = 4/7, N = 5,
#'           by = "cdac", p_split = "h", scale = "f",
#'           title_lbl = "Custom text and color:",
#'           lbl_txt = txt_org, f_lbl = "namnum", f_lbl_sep = ":\n",
#'           f_lwd = 1, col_pal = pal_kn)
#'
#' ## Versions:
#' ## by x p_split (= [3 x 2 x 2] = 12 versions):
#' plot_area(by = "cddc", p_split = "v")  # v01 (see v07)
#' plot_area(by = "cdac", p_split = "v")  # v02 (see v11)
#' plot_area(by = "cddc", p_split = "h")  # v03 (see v05)
#' plot_area(by = "cdac", p_split = "h")  # v04 (see v09)
#'
#' # plot_area(by = "dccd", p_split = "v")  # v05 (is v03 rotated)
#' plot_area(by = "dcac", p_split = "v")  # v06 (see v12)
#' # plot_area(by = "dccd", p_split = "h")  # v07 (is v01 rotated)
#' plot_area(by = "dcac", p_split = "h")  # v08 (see v10)
#'
#' plot_area(by = "accd", p_split = "v")  # v09 (is v04 rotated)
#' # plot_area(by = "acdc", p_split = "v")  # v10 (is v08 rotated)
#' # plot_area(by = "accd", p_split = "h")  # v11 (is v02 rotated)
#' # plot_area(by = "acdc", p_split = "h")  # v12 (is v06 rotated)
#'
#' ## Options:
#' # area:
#' plot_area(area = "sq")  # main area as square (by scaling x-values)
#' plot_area(area = "no")  # rectangular main area (using full plotting region)
#'
#' # scale (matters for small N):
#' plot_area(N = 5, prev = .5, sens = .8, spec = .6,
#'           by = "cddc", p_split = "v", scale = "p", p_lbl = "def")  # scaled by prob (default)
#' plot_area(N = 5, prev = .5, sens = .8, spec = .6,
#'           by = "cddc", p_split = "v", scale = "f", p_lbl = "def")  # scaled by freq (for small N)
#' plot_area(N = 4, prev = .4, sens = .8, spec = .6,
#'           by = "cdac", p_split = "h", scale = "p", p_lbl = "def")  # scaled by prob (default)
#' plot_area(N = 4, prev = .4, sens = .8, spec = .6,
#'           by = "cdac", p_split = "h", scale = "f", p_lbl = "def")  # scaled by freq (for small N)
#'
#' # gaps (sensible range: 0--.10):
#' plot_area(gaps = NA)           # use default gaps (based on p_split)
#' plot_area(gaps = c(0, 0))      # no gaps
#' plot_area(gaps = c(.05, .01))  # v_gap > h_gap
#'
#' # freq labels:
#' plot_area(f_lbl = "def", f_lbl_sep = " = ")  # default
#' plot_area(f_lbl = NA)      # NA/NULL: no freq labels (in main area & top/left boxes)
#' plot_area(f_lbl = "abb")   # abbreviated name (i.e., variable name)
#' plot_area(f_lbl = "nam")   # only freq name
#' plot_area(f_lbl = "num")   # only freq number
#' plot_area(f_lbl = "namnum", f_lbl_sep = ":\n", cex_lbl = .75)  # explicit & smaller
#'
#' # prob labels:
#' plot_area(p_lbl = NA)      # default: no prob labels, no links
#' plot_area(p_lbl = "no")    # show links, but no labels
#' plot_area(p_lbl = "namnum", cex_lbl = .70)  # explicit & smaller labels
#'
#' # prob arrows:
#' plot_area(arr_c = +3, f_lbl = NA)  # V-shape arrows
#' plot_area(arr_c = +6, f_lbl = NA)  # T-shape arrows
#' plot_area(arr_c = +6, f_lbl = NA,
#'           brd_dis = -.02, col_p = c("black")) # adjust arrow type/position
#'
#' # f_lwd:
#' plot_area(f_lwd =  3)  # thicker lines
#' plot_area(f_lwd = .5)  # thinner lines
#' plot_area(f_lwd =  0)  # no lines (if f_lwd = 0/NULL/NA: lty = 0)
#'
#' # sum_w:
#' plot_area(sum_w = .10)  # default (showing top and left freq panels & labels)
#' plot_area(sum_w =  0)   # remove top and left freq panels
#' plot_area(sum_w =  1,         # top and left freq panels scaled to size of main areas
#'           col_pal = pal_org)  # custom colors
#'
#' ## Plain and suggested plot versions:
#' plot_area(sum_w = 0, f_lbl = "abb", p_lbl = NA)  # no compound indicators (on top/left)
#' plot_area(gap = c(0, 0), sum_w = 0, f_lbl = "num", p_lbl = "num",  # no gaps, numeric labels
#'           f_lwd = .5, col_pal = pal_bwp, title_lbl = "Black-and-white")  # b+w print version
#' plot_area(f_lbl = "nam", p_lbl = NA, col_pal = pal_mod) # plot with freq labels
#' plot_area(f_lbl = "num", p_lbl = NA, col_pal = pal_rgb) # no borders around boxes
#'
#' @importFrom graphics par
#' @importFrom graphics plot
#' @importFrom graphics box
#' @importFrom graphics axis
#' @importFrom graphics grid
#' @importFrom graphics abline
#' @importFrom graphics rect
#' @importFrom graphics arrows
#' @importFrom graphics points
#' @importFrom graphics text
#' @importFrom graphics title
#' @importFrom graphics mtext
#' @importFrom graphics legend
#' @importFrom graphics lines
#' @importFrom grDevices dev.size
#'
#' @family visualization functions
#'
#' @seealso
#' \code{\link{plot_mosaic}} for older (obsolete) version;
#' \code{\link{plot_tab}} for plotting table (without scaling area dimensions);
#' \code{\link{pal}} contains current color settings;
#' \code{\link{txt}} contains current text settings.
#'
#' @export


## (2) plot_area: Definition ----------

plot_area <- function(prev = num$prev,    # probabilities
                      sens = num$sens, mirt = NA,
                      spec = num$spec, fart = NA,
                      N = num$N,          # population size N

                      ## Plot options:
                      by = "cddc",        # 2 perspectives (top + left): by = "cd" "dc" "ac"  (default: "cddc")
                      p_split = "v",      # primary/perspective split: "v": vertical vs. "h": horizontal
                      area = "sq",        # sq" (default: correcting x-values for aspect ratio of current plot) vs. "no" (NA, NULL, "fix", "hr" )
                      scale = "p",        # "p": exact probabilities (default) vs. "f": Re-compute prob from (rounded or non-rounded) freq.
                      round = TRUE,       # round freq to integers? (default: round = TRUE), when not rounded: n_digits = 2 (currently fixed).

                      ## Freq boxes:
                      sum_w = .10,        # border width: (default: sum_w = .10), setting sum_w = NULL/NA/<=0  hides top and left panels.
                      gaps = c(NA, NA),   # c(v_gap, h_gap). Note: c(NA, NA) is changed to defaults: c(.02, 0) if p_split = "v"; c(0, .02) if p_split = "h".

                      f_lbl = "num",      # freq label: "def" vs. "abb"/"nam"/"num"/"namnum". (Set to "no"/NA/NULL to hide freq labels).
                      f_lbl_sep = NA,     # freq label separator (default: " = ", use ":\n" to add an extra line break)
                      f_lbl_sum = "num",  # freq label of summary cells (bottom row and right column)
                      f_lbl_hd  = "nam",  # freq labels of headers at top (for columns) and left (for rows)
                      f_lwd = 0,          # lwd of freq boxes: 0 (set to tiny_lwd, lty = 0) vs. 1 (numeric), or NULL/NA (set to 0).
                      # f_lty = 0,        # lty of freq boxes: 1 ("solid") vs. 0 ("blank"), etc. (currently not used)

                      ## Prob links:
                      p_lbl = NA,         # prob label: "def" (default) vs. "abb"/"nam"/"num"/"namnum". (Set to "no"/NA/NULL to hide prob lines).
                      # p_lwd,            # lwd of prob links: set to default = 1 (currently not used)
                      # p_lty,            # lty of prob links: set to default = 1 (currently not used)
                      arr_c = -3,         # arrow code (-3 to +6): 0: no arrow, 1--3: V-shape, 4--6: T-shape, -1 to -3: point at ends.
                      col_p = c(grey(.15, .99), "yellow", "yellow"),  # colors for prob-links: use 1-3 bright colors (visible on SDT rectangles). WAS: "black", "cornsilk", "whitesmoke"
                      brd_dis = .06,      # distance of prob links from border. (Adjust to avoid overlapping labels).

                      ## Text and color:
                      lbl_txt = txt,      # labels and text elements
                      title_lbl = txt$scen_lbl,  # main plot title
                      cex_lbl = .90,      # size of freq & text labels
                      cex_p_lbl = NA,     # size of prob labels (set to cex_lbl - .05 by default)
                      col_pal = pal,      # color palette

                      ## Generic options:
                      mar_notes = TRUE,   # show margin notes?
                      ...                 # other (graphical) parameters (passed to plot_line and plot_ftype_label)
) {

  ## (0) Compute new freq and prob objects (based on probability inputs): ----------

  ## (A) If a valid set of probabilities was provided:
  if (is_valid_prob_set(prev = prev, sens = sens, mirt = mirt, spec = spec, fart = fart, tol = .01)) {

    ## (a) Compute the complete quintet of probabilities:
    prob_quintet <- comp_complete_prob_set(prev = prev, sens = sens, mirt = mirt, spec = spec, fart = fart)
    sens <- prob_quintet[2]  # gets sens (if not provided)
    mirt <- prob_quintet[3]  # gets mirt (if not provided)
    spec <- prob_quintet[4]  # gets spec (if not provided)
    fart <- prob_quintet[5]  # gets fart (if not provided)

    ## (b) Compute LOCAL freq and prob based on current parameters (N and probabilities):
    freq <- comp_freq(prev = prev, sens = sens, spec = spec, N = N, round = round)  # compute freq (default: round = TRUE)
    prob <- comp_prob_prob(prev = prev, sens = sens, spec = spec)

    # message("Computed local freq and prob to plot prism.")

    ## (c) Compute cur.popu from computed frequencies (not needed):
    # cur.popu <- comp_popu(hi = freq$hi, mi = freq$mi, fa = freq$fa, cr = freq$cr)  # compute cur.popu (from 4 essential frequencies)
    # message("Generated new population (cur.popu) to plot.")

  } else {  # (B) NO valid set of probabilities was provided:

    message("No valid set of probabilities provided. Using global freq & prob to plot prism.")

  } # if (is_valid_prob_set etc.)

  ## (1) Prepare parameters: ----------

  ## (A) Generic:

  opar <- par(no.readonly = TRUE)  # copy of current settings
  on.exit(par(opar))  # par(opar)  # restore original settings

  ## (B) Interpret current user input parameters:

  ## 1. Perspective: ----

  # by:
  if ( !is.null(by) && !is.na(by) ) { by <- tolower(by) }  # by in lowercase
  if ( is.null(by) || is.na(by) )  { by <- "cddc" }        # use default
  if (by == "any" || by == "all" || by == "default" || by == "def" || by == "no" ) { by <- "cddc" }

  # use by input:
  by_top <- substr(by, 1, 2)  # top perspective (row 2): by = "cd" "dc" "ac"
  by_bot <- substr(by, 3, 4)  # bottom perspective (row 4): by = "cd" "dc" "ac"

  # catch & correct invalid entries:
  if (by_top == by_bot) {
    message("Please specify 2 different perspectives.")
  }
  # Invalid perspectives:
  if ((by_top %in% c("cd", "dc", "ac")) == FALSE) {
    message("Invalid 1st perspective! Valid by = {'cddc', 'cdac', 'dccd', 'dcac', 'accd', 'acdc'}.\nUsing by = 'cd..'.")
    by_top <- "cd"  # default
  }
  if ((by_bot %in% c("cd", "dc", "ac")) == FALSE) {
    message("Invalid 2nd perspective! Valid by = {'cddc', 'cdac', 'dccd', 'dcac', 'accd', 'acdc'}.\nUsing by = '..dc'.")
    by_bot <- "dc"  # default
  }
  # Valid 1st but invalid 2nd perspective:
  if ((by_top == "cd") && (by_bot != ("dc") & by_bot != ("ac"))) {
    message("If 1st perspective by = 'cd', 2nd perspective must be 'dc' or 'ac'.\nUsing by = 'cddc'.")
    by_bot <- "dc"  # default
  }
  if ((by_top == "dc") && (by_bot != ("cd") & by_bot != ("ac"))) {
    message("If 1st perspective by = 'dc', 2nd perspective must be 'cd' or 'ac'.\nUsing by = 'dccd'.")
    by_bot <- "cd"  # default
  }
  if ((by_top == "ac") && (by_bot != ("cd") & by_bot != ("dc"))) {
    message("If 1st perspective by = 'ac', 2nd perspective must be 'cd' or 'dc'.\nUsing by = 'accd'.")
    by_bot <- "cd"  # default
  }

  ## 2. Freq boxes: ----

  # area (correcting for aspect ratio by scale_x):
  if ( is.null(area) || is.na(area) ) { area <- "no" }  # sensible zero/NULL/NA
  if ( !is.null(area) && !is.na(area) ) { area <- tolower(area) }  # area in lowercase
  if ( area == "square" || area == "mosaic" || area == "fix" ) { area <- "sq" }
  # print(paste0("area = ", area))

  # use area input:
  if (area == "sq") {

    plot_xy <- par("pin")                  # use par("pin") OR dev.size("in") to determine aspect ratio
    plot_ratio <- plot_xy[1]/plot_xy[2]    # current aspect ratio
    cc_scale_x <- .055                     # correction constant for scale_x
    scale_x  <- 1/plot_ratio + cc_scale_x  # multiplicative correction factor (for x-widths)

  } else { # default:

    scale_x <- 1  # (i.e., no scaling)

  }

  # gaps:
  if ( any(is.null(gaps)) ) { gaps <- c(0, 0) }   # sensible zero
  if ( any(is.na(gaps)) )   { gaps <- c(NA, NA) } # set to default

  if ( any(!is.na(gaps)) && (length(gaps) != 2 || any(!is.numeric(gaps))) ) {
    message("gaps should have 2 numeric arguments. Using default gaps = c(NA, NA).")
    gaps <- c(NA, NA)  # use defaults
  }

  # gap defaults:
  v_gap_def <- .02
  h_gap_def <- .02

  # use current gap input:
  v_gap <- gaps[1]  # default: v_gap = NA
  h_gap <- gaps[2]  # default: h_gap = NA

  if (is.na(v_gap) || v_gap < 0 ) { # use default:

    if (p_split == "v") {
      v_gap <- v_gap_def
    } else if (p_split == "h") {
      v_gap <- 0
    } else { # default:
      v_gap <- v_gap_def
    }
  }

  if (is.na(h_gap) || h_gap < 0 ) { # use default:

    if (p_split == "v") {
      h_gap <- 0
    } else if (p_split == "h") {
      h_gap <- h_gap_def
    } else { # default:
      h_gap <- h_gap_def
    }
  }

  if ( h_gap > .10 ) {
    message("Horizontal gap (i.e., gaps[2]) should be in the range from 0 to .10.")
  }

  # scale:
  if ( is.null(scale) || is.na(scale)  ) { scale <- "p" }  # default
  if ( !is.null(scale) && !is.na(scale) ) { scale <- tolower(scale) }  # scale in lowercase
  if ((scale == "freq") || (scale == "f")) { scale <- "f" }
  if ((scale == "prob") || (scale == "p")) { scale <- "p" }

  # use scale input:
  if (scale == "f") {

    ## (A) Use scale for area dimensions:
    ## Recompute specific probabilities from current (4 essential) freq
    ## which may be rounded or not rounded:

    prob_from_freq <- comp_prob_freq(hi = freq$hi, mi = freq$mi, fa = freq$fa, cr = freq$cr)

    # (a) by condition:
    prev <- prob_from_freq$prev
    sens <- prob_from_freq$sens
    spec <- prob_from_freq$spec
    # mirt <- (1 - sens)
    # fart <- (1 - spec)

    # (b) by decision:
    ppod <- prob_from_freq$ppod
    PPV  <- prob_from_freq$PPV
    NPV  <- prob_from_freq$NPV

    # (c) by accuracy:
    acc <- prob_from_freq$acc
    p_acc_hi <- freq$hi/freq$dec_cor  # p(hi | acc)
    p_err_fa <- freq$fa/freq$dec_err  # p(fa | err)

    ## Note: (A) only re-computes probabilities used for scaling,
    ##       but does not change prob values (used for labels).

    ## (B) Use scale for area dimensions AND prob values:
    ## A more radical type of scale (i.e., re-defining prob based on current freq)
    ## also changes the prob values displayed in links and margins:
    prob <- prob_from_freq  # use re-computed values for all prob values!

  } else {

    # default: (scale == "p"):
    # Use EXACT probabilities from current prob:

    # (a) by condition:
    prev <- prob$prev
    sens <- prob$sens
    spec <- prob$spec
    # mirt <- (1 - sens)
    # fart <- (1 - spec)

    # (b) by decision:
    ppod <- prob$ppod
    PPV  <- prob$PPV
    NPV  <- prob$NPV

    # (c) by accuracy:
    acc <- prob$acc
    p_acc_hi <- prob$p_acc_hi  # comp_prob_pname("acc_hi")  # p(hi | acc)
    p_err_fa <- prob$p_err_fa  # comp_prob_pname("err_fa")  # p(fa | err)
    # sum(comp_prob_pname(c("acc_hi", "acc_cr"))) == 1
    # sum(comp_prob_pname(c("err_mi", "err_fa"))) == 1

  } # if (scale == etc.)

  # f_lwd & lty:
  tiny_lwd <- .001   # initialize tiny, invisible width

  if ( is.null(f_lwd) || is.na(f_lwd) || f_lwd <= 0 ) {

    f_lwd <- tiny_lwd  # to avoid error (for lwd = 0)
    lty <- 0           # "blank" (no lines) [only when f_lty and p_lty are NOT used]

  }

  # f_lbl_sep: Set smart default:
  if ( !is.null(f_lbl) && !is.na(f_lbl) ) {
    if (is.na(f_lbl_sep)) {
      if (f_lbl == "def" || f_lbl == "namnum" || f_lbl == "namval" || f_lbl == "abbnum" || f_lbl == "abbval") {
        f_lbl_sep <- ":\n"  # add an extra line break
      } else {
        f_lbl_sep <- " = "  # use default
      }
    }
  }

  ## 3. Prob links: ----

  # No probability labels: Detect special strings:
  if (!is.null(p_lbl)) {
    if (is.na(p_lbl) ||
        p_lbl == "" || tolower(p_lbl) == "null" | tolower(p_lbl) == "na") {
      p_lbl <- NA  # set to NA or NULL
    }
  }

  # p_lwd <- 1  # lwd of p-links
  # p_lty <- 1  # lty of p-links

  # arr_c:
  if ( is.null(arr_c) || is.na(arr_c) ) { arr_c <- 0 }  # sensible zero


  ## 4. Text labels: ----

  # Plot title:
  if (is.null(title_lbl)) { title_lbl <- "" }              # adjust NULL to "" (i.e., no title)
  if (is.na(title_lbl)) { title_lbl <- lbl_txt$scen_lbl }  # use scen_lbl as default plot title

  if ( is.null(cex_lbl) ) { cex_lbl <- .001 }  # sensible zero
  if ( is.na(cex_lbl) ) { cex_lbl <- .90 }  # default size of cex
  if ( cex_lbl == 0 )  { cex_lbl <- .001 }  # other sensible zero

  if ( is.null(cex_p_lbl) ) { cex_p_lbl <- .001 }  # sensible zero
  if ( is.na(cex_p_lbl) ) { cex_p_lbl <- (cex_lbl - .05) }  # default size of cex
  if ( cex_p_lbl == 0 )  { cex_p_lbl <- .001 } # other sensible zero


  ## 5. Colors / color palettes: ----

  # (a) Set plot background color:
  par(bg = col_pal[["bg"]])  # col_pal[["bg"]] / "white" / NA (for transparent background)

  # (b) Detect and handle special cases of color equality (e.g., pal_bwp):
  if ( (par("bg") %in% col_pal[1:11]) && # if bg is equal to ANY fbox color AND
       (f_lwd <= tiny_lwd) ) {           # f_lwd is tiny_lwd (default):
    f_lwd <- 1
    if (lty == 0) {lty <- 1}  # prevent lty = 0
    # message(paste0("f_lwd = ", f_lwd, "; col_pal[['brd']] = ",  col_pal[["brd"]], "; lty = ", lty)) # debugging
  }

  # (c) Probability link colors:
  if (!is.na(p_lbl)) {  # only when labels OR links are being shown:

    # if (!all(col_p %in% colors())) { message("col_p must contain (1 to 3) valid color names.") }
    if (length(col_p) == 1) { col_p <- rep(col_p, 3) }                   # 1 color: use for all 3 p-links
    if (length(col_p) == 2) { col_p <- c(col_p[1], col_p[2], col_p[2]) } # 2 colors: use 2nd for 2 and 3

    p_col_1 <- col_p[1]  # prob color 1 (must be visible on hi)
    p_col_2 <- col_p[2]  # prob color 2 (must be visible on hi)
    p_col_3 <- col_p[3]  # prob color 3 (must be visible on cr AND fa)

    # (+) Detect and handle special case (when color of hi is "white", so that defaults of "yellow" would not be visible):
    if (all_equal(c("white", col_pal[["hi"]]))) {
      if (p_col_1 == "yellow" || p_col_1 == "white") { p_col_1 <- grey(.01, .99) }  # change if set to "yellow"
      if (p_col_2 == "yellow" || p_col_2 == "white") { p_col_2 <- grey(.11, .99) }  # change if still default color
      if (p_col_3 == "yellow" || p_col_3 == "white") { p_col_3 <- grey(.22, .99) }  # change if still default color
    }

  }  # if if (!is.na(p_lbl)) end.

  ## 6. Plot borders: ----

  # sum_w: Must be a value in range 0 to 1:
  if ( is.null(sum_w) || is.na(sum_w) ) { sum_w <- 0 }  # set to 0 (min)
  sum_w_max <- 1  # maximal value
  sum_w_min <- 0  # minimal value
  if ( sum_w > sum_w_max ) { sum_w <- sum_w_max }  # set to sum_w_max
  if ( sum_w < sum_w_min ) { sum_w <- sum_w_min }  # set to sum_w_min


  ## 7. Additional parameters (currently fixed): ----

  lty <- 1  # default

  # Correction values (as constants):
  buffer  <- +.09  # blank buffer space (on top and left) of plotting area
  v_shift <- +.03  # shifting vertical/rotated text labels (left)
  h_shift <- -.01  # shifting horizontal text labels (up)

  # Frequency bars (on top and left):
  bar_lx <- sum_w  # bar widths (constant based on sum_w (from 0 to 1)
  tbar_y <- (1 + buffer) + (.52 * sum_w)    # y-value of top  bar (constant based on sum_w)
  lbar_x <- ((0 - buffer) - (.52 * sum_w))  # x-value of left bar (constant based on sum_w)

  # # f_lbl_sum: freq labels for sums (inside bars on top and left:
  # if ( is.null(f_lbl) || is.na(f_lbl) ) {
  #   f_lbl_sum <- f_lbl   # hide labels
  # } else {
  #   f_lbl_sum <- "num" # currently fixed (due to limited size), OR: set to f_lbl
  # }
  #
  # # f_lbl_hd : freq labels for headers (labels on top/left area borders):
  # if ( is.null(f_lbl) || is.na(f_lbl) ) {
  #   f_lbl_hd  <- "nam"  # use full names
  # } else if (f_lbl == "namnum" || f_lbl == "nam") {
  #   f_lbl_hd  <- "nam"  # use full names
  # } else {
  #   f_lbl_hd  <- "abb"  # use abbreviated names
  # }


  ## (2) Define plot and margin areas: ----------

  ## Define margin areas:

  if (nchar(title_lbl) > 0) { n_lines_top <- 2 } else { n_lines_top <- 0 }
  if (mar_notes) { n_lines_bot <- 3 } else { n_lines_bot <- 0 }

  par(mar = c(n_lines_bot, 1, n_lines_top, 1) + 0.1)  # margins; default: par("mar") = 5.1 4.1 4.1 2.1.
  par(oma = c(0, 0, 0, 0) + 0.1)                      # outer margins; default: par("oma") = 0 0 0 0.

  ## Axis label locations:
  par(mgp = c(3, 1, 0)) # default: c(3, 1, 0)

  ## Orientation of the tick mark labels (and corresponding mtext captions below):
  par(las = 0)  # Options: parallel to the axis (0 = default), horizontal (1), perpendicular to axis (2), vertical (3).

  ## (3) Plot setup: ----------

  ## Plot dimensions:

  # sum_w <- .250  # border width (default)
  # sum_w <- 0     # to HIDE borders

  y_min <- 0

  if (!is.null(v_gap) && !is.na(v_gap) ) {
    x_max <- (1 + v_gap + buffer)
  } else {
    x_max <- (1 + buffer)
  }

  if ( (sum_w > 0) ) {  # 2 x buffer (for text labels):
    x_min <- (0 - buffer - sum_w - buffer)
    y_max <- (1 + h_gap) + (buffer + sum_w + buffer)
  } else {
    x_min <- (0 - buffer) # 1 x buffer (for text labels)
    y_max <- (1 + h_gap) + (buffer)
  }

  ## Draw empty plot:
  plot(0, 0, type = "n",      # type = "n" hides the points
       xlab = "x", ylab = "y",
       xlim = c(x_min, x_max), ylim = c(y_min, y_max),
       axes = FALSE)

  ## Axes:
  # axis(side = 1, las = 1) # x-axis, horizontal labels
  # axis(side = 2, las = 2) # y-axis, horizontal labels

  ## Grid:
  # grid(nx = NULL, ny = NA,  # x-axes only (at tick marks)
  #      col = grey(.75, .99), lty = 2, lwd = par("lwd"), equilogs = TRUE)

  ## (4) Draw plot points: ----------

  ## Grid of points:
  # grid_x <- rep(seq(0, 1, by = .25), times = length(seq(0, 1, by = .25))) # x/horizontal
  # grid_y <- rep(seq(0, 1, by = .25), each =  length(seq(0, 1, by = .25))) # y/vertical
  # points(grid_x, grid_y, pch = 3, col = grey(.66, .50), cex = 3/4)        # grid points

  # points(grid_x * scale_x, grid_y, pch = 3, col = grey(.66, .50), cex = 3/4)  # grid points (scaled)
  # points(0, 0, pch = 1, col = grey(.33, .50), cex = 1)  # mark origin

  ## (5) Main: Custom area/mosaic plot: -----------

  ##   (a) Plot 4 SDT cases/cells in center (coordinates + boxes): ----------

  ## 4 SDT cases in center (by perspective):

  if (by_top == "cd") {

    # (a) by condition (on top):

    if (p_split == "v") {

      # key probabilities:
      v_split   <- prev
      h_split_l <- sens
      h_split_r <- spec

      # widths (lx):
      hi_lx <- v_split
      mi_lx <- v_split
      fa_lx <- (1 - v_split)
      cr_lx <- (1 - v_split)

      # heights (ly):
      hi_ly <- h_split_l
      mi_ly <- (1 - h_split_l)
      fa_ly <- (1 - h_split_r)
      cr_ly <- h_split_r

      # x-coordinates:
      hi_x <- hi_lx/2
      mi_x <- mi_lx/2
      fa_x <- hi_lx + fa_lx/2 + v_gap
      cr_x <- mi_lx + cr_lx/2 + v_gap

      # y-coordinates (left):
      mi_y <- mi_ly/2
      hi_y <- mi_ly + hi_ly/2 + h_gap

      if (by_bot == "dc") {

        #    cddc:      by cd:
        #               +: -:
        # by dc:  pos:  hi fa
        #         neg:  mi cr

        # y-coordinates (right):
        cr_y <- cr_ly/2
        fa_y <- cr_ly + fa_ly/2 + h_gap

      } else if (by_bot == "ac") {

        #    cdac:      by cd:
        #               +: -:
        # by ac:  cor:  hi cr
        #         err:  mi fa

        # y-coordinates (right):
        fa_y <- fa_ly/2
        cr_y <- fa_ly + cr_ly/2 + h_gap

      } # if (by_bot == etc.)


    } else if (p_split == "h") {

      if (by_bot == "dc") {

        # by decision:

        #    cddc:      by cd:
        #               +: -:
        # by dc:  pos:  hi fa
        #         neg:  mi cr

        # key probabilities:
        h_split <- ppod
        v_split_t <- PPV
        v_split_b <- NPV

        # heights (ly):
        hi_ly <- h_split
        mi_ly <- (1 - h_split)
        fa_ly <- h_split
        cr_ly <- (1 - h_split)

        # widths (lx):
        hi_lx <- v_split_t
        mi_lx <- (1 - v_split_b)
        fa_lx <- (1 - v_split_t)
        cr_lx <- v_split_b

        # x-coordinates:
        hi_x <- hi_lx/2
        mi_x <- mi_lx/2
        fa_x <- hi_lx + fa_lx/2 + v_gap
        cr_x <- mi_lx + cr_lx/2 + v_gap

        # y-coordinates (left):
        mi_y <- mi_ly/2
        hi_y <- mi_ly + hi_ly/2 + h_gap

        # y-coordinates (right):
        cr_y <- cr_ly/2
        fa_y <- cr_ly + fa_ly/2 + h_gap

      } else if (by_bot == "ac") {

        #    cdac:      by cd:
        #               +: -:
        # by ac:  cor:  hi cr
        #         err:  mi fa

        # key probabilities:
        h_split <- acc
        v_split_t <- p_acc_hi
        v_split_b <- p_err_fa

        # heights (ly):
        hi_ly <- h_split
        mi_ly <- (1 - h_split)
        fa_ly <- (1 - h_split)
        cr_ly <- h_split

        # widths (lx):
        hi_lx <- v_split_t
        mi_lx <- (1 - v_split_b)
        fa_lx <- v_split_b
        cr_lx <- (1 - v_split_t)

        # x-coordinates:
        hi_x <- hi_lx/2
        mi_x <- mi_lx/2
        fa_x <- mi_lx + fa_lx/2 + v_gap
        cr_x <- hi_lx + cr_lx/2 + v_gap

        # y-coordinates (left):
        mi_y <- mi_ly/2
        hi_y <- mi_ly + hi_ly/2 + h_gap

        # y-coordinates (right):
        fa_y <- fa_ly/2
        cr_y <- fa_ly + cr_ly/2 + h_gap

      } # if (by_bot == etc.)

    } # if (p_split == etc.)


  } else if (by_top == "dc") {

    # (b) by decision (on top):

    if (p_split == "v") {

      # key probabilities:
      v_split   <- ppod
      h_split_l <- PPV
      h_split_r <- NPV

      # widths (lx):
      hi_lx <- v_split
      mi_lx <- (1 - v_split)
      fa_lx <- v_split
      cr_lx <- (1 - v_split)

      # heights (ly):
      hi_ly <- h_split_l
      mi_ly <- (1 - h_split_r)
      fa_ly <- (1 - h_split_l)
      cr_ly <- h_split_r

      # x-coordinates:
      hi_x <- hi_lx/2
      mi_x <- hi_lx + mi_lx/2 + v_gap
      fa_x <- fa_lx/2
      cr_x <- fa_lx + cr_lx/2 + v_gap

      # y-coordinates (left):
      fa_y <- fa_ly/2
      hi_y <- fa_ly + hi_ly/2 + h_gap

      if (by_bot == "cd") {

        #    dccd:       by dc:
        #                +: -:
        # by cd: true:   hi mi
        #        false:  fa cr

        # y-coordinates (right):
        cr_y <- cr_ly/2
        mi_y <- cr_ly + mi_ly/2 + h_gap

      } else if (by_bot == "ac") {

        #    dcac:      by dc:
        #               +: -:
        # by ac:  cor:  hi cr
        #         err:  fa mi

        # y-coordinates (right):
        mi_y <- mi_ly/2
        cr_y <- mi_ly + cr_ly/2 + h_gap

      } # if (by_bot == etc.)

    } else if (p_split == "h") {

      if (by_bot == "cd") {

        # by decision:

        #    dccd:      by dc:
        #               +: -:
        # by cd:  pos:  hi mi
        #         neg:  fa cr

        # key probabilities:
        h_split <- prev
        v_split_t <- sens
        v_split_b <- spec

        # heights (ly):
        hi_ly <- h_split
        mi_ly <- h_split
        fa_ly <- (1 - h_split)
        cr_ly <- (1 - h_split)

        # widths (lx):
        hi_lx <- v_split_t
        mi_lx <- (1 - v_split_t)
        fa_lx <- (1 - v_split_b)
        cr_lx <- v_split_b

        # x-coordinates:
        hi_x <- hi_lx/2
        fa_x <- fa_lx/2
        mi_x <- hi_lx + mi_lx/2 + v_gap
        cr_x <- fa_lx + cr_lx/2 + v_gap

        # y-coordinates (left):
        fa_y <- fa_ly/2
        hi_y <- fa_ly + hi_ly/2 + h_gap

        # y-coordinates (right):
        cr_y <- cr_ly/2
        mi_y <- cr_ly + mi_ly/2 + h_gap


      } else if (by_bot == "ac") {

        #    dcac:      by dc:
        #               +: -:
        # by ac:  cor:  hi cr
        #         err:  fa mi

        # key probabilities:
        h_split <- acc
        v_split_t <- p_acc_hi
        v_split_b <- p_err_fa

        # heights (ly):
        hi_ly <- h_split
        mi_ly <- (1 - h_split)
        fa_ly <- (1 - h_split)
        cr_ly <- h_split

        # widths (lx):
        hi_lx <- v_split_t
        mi_lx <- (1 - v_split_b)
        fa_lx <- v_split_b
        cr_lx <- (1 - v_split_t)

        # x-coordinates:
        hi_x <- hi_lx/2
        fa_x <- fa_lx/2
        cr_x <- hi_lx + cr_lx/2 + v_gap
        mi_x <- fa_lx + mi_lx/2 + v_gap

        # y-coordinates (left):
        fa_y <- fa_ly/2
        hi_y <- fa_ly + hi_ly/2 + h_gap

        # y-coordinates (right):
        mi_y <- mi_ly/2
        cr_y <- mi_ly + cr_ly/2 + h_gap

      } # if (by_bot == etc.)

    } # if (p_split == etc.)


  } else if (by_top == "ac") {

    # (c) by accuracy (on top):

    if (p_split == "v") {

      #    accd:      by ac:
      #               +: -:
      # by cd: true:  hi mi
      #        false: cr fa

      # key probabilities:
      v_split   <- acc
      h_split_l <- p_acc_hi
      h_split_r <- p_err_fa

      # widths (lx):
      hi_lx <- v_split
      mi_lx <- (1 - v_split)
      fa_lx <- (1 - v_split)
      cr_lx <- v_split

      # heights (ly):
      hi_ly <- h_split_l
      mi_ly <- (1 - h_split_r)
      fa_ly <- h_split_r
      cr_ly <- (1 - h_split_l)

      # x-coordinates:
      hi_x <- hi_lx/2
      cr_x <- cr_lx/2
      mi_x <- hi_lx + mi_lx/2 + v_gap
      fa_x <- cr_lx + fa_lx/2 + v_gap

      # y-coordinates (left):
      cr_y <- cr_ly/2
      hi_y <- cr_ly + hi_ly/2 + h_gap

      if (by_bot == "dc") {

        #    acdc:      by ac:
        #               +: -:
        # by dc:  pos:  hi fa
        #         neg:  cr mi

        # y-coordinates (right):
        mi_y <- mi_ly/2
        fa_y <- mi_ly + fa_ly/2 + h_gap

      } else if (by_bot == "cd") {

        #    accd:      by ac:
        #               +: -:
        # by cd: true:  hi mi
        #        false: cr fa

        # y-coordinates (right):
        fa_y <- fa_ly/2
        mi_y <- fa_ly + mi_ly/2 + h_gap

      } # if (by_bot == etc.)

    } else if (p_split == "h") {

      if (by_bot == "dc") {

        # by decision:

        #    acdc:      by ac:
        #               +: -:
        # by dc:  pos:  hi fa
        #         neg:  cr mi

        # key probabilities:
        h_split <- ppod
        v_split_t <- PPV
        v_split_b <- NPV

        # heights (ly):
        hi_ly <- h_split
        mi_ly <- (1 - h_split)
        fa_ly <- h_split
        cr_ly <- (1 - h_split)

        # widths (lx):
        hi_lx <- v_split_t
        mi_lx <- (1 - v_split_b)
        fa_lx <- (1 - v_split_t)
        cr_lx <- v_split_b

        # x-coordinates:
        hi_x <- hi_lx/2
        cr_x <- cr_lx/2
        fa_x <- hi_lx + fa_lx/2 + v_gap
        mi_x <- cr_lx + mi_lx/2 + v_gap

        # y-coordinates (left):
        cr_y <- cr_ly/2
        hi_y <- cr_ly + hi_ly/2 + h_gap

        # y-coordinates (right):
        mi_y <- mi_ly/2
        fa_y <- mi_ly + fa_ly/2 + h_gap

      } else if (by_bot == "cd") {

        #    accd:      by ac:
        #               +: -:
        # by cd: true:  hi mi
        #        false: cr fa

        # key probabilities:
        h_split <- prev
        v_split_t <- sens
        v_split_b <- spec

        # heights (ly):
        hi_ly <- h_split
        mi_ly <- h_split
        fa_ly <- (1 - h_split)
        cr_ly <- (1 - h_split)

        # widths (lx):
        hi_lx <- v_split_t
        mi_lx <- (1 - v_split_t)
        fa_lx <- (1 - v_split_b)
        cr_lx <- v_split_b

        # x-coordinates:
        hi_x <- hi_lx/2
        mi_x <- hi_lx + mi_lx/2 + v_gap
        cr_x <- cr_lx/2
        fa_x <- cr_lx + fa_lx/2 + v_gap

        # y-coordinates (left):
        cr_y <- cr_ly/2
        hi_y <- cr_ly + hi_ly/2 + h_gap

        # y-coordinates (right):
        fa_y <- fa_ly/2
        mi_y <- fa_ly + mi_ly/2 + h_gap

      } # if (by_bot == etc.)

    } # if (p_split == etc.)

  } else {

    message("Defining 4 SDT cases/cells: Unknown by_top argument/perspective!")

  } # if (by_top == etc.)


  ## Define 4 boxes (with optional scale_x):

  box_hi <- make_box("hi", (hi_x * scale_x), hi_y, (hi_lx * scale_x), hi_ly)  # hi
  box_mi <- make_box("mi", (mi_x * scale_x), mi_y, (mi_lx * scale_x), mi_ly)  # mi
  box_fa <- make_box("fa", (fa_x * scale_x), fa_y, (fa_lx * scale_x), fa_ly)  # fa
  box_cr <- make_box("cr", (cr_x * scale_x), cr_y, (cr_lx * scale_x), cr_ly)  # cr

  ## Plot 4 boxes:

  ## OLD: Plot 4 SDT cases/cells separately:
  # plot(box_hi, lbl_type = f_lbl, cex = cex_lbl, lwd = f_lwd)
  # plot(box_mi, lbl_type = f_lbl, cex = cex_lbl, lwd = f_lwd, density = NULL)  # illustrate diagonal lines (via density)
  # plot(box_fa, lbl_type = f_lbl, cex = cex_lbl, lwd = NULL, density = 25)  # illustrate diagonal lines (via density)
  # plot(box_cr, lbl_type = f_lbl, cex = cex_lbl, lwd = f_lwd)

  ## NEW: Plot 4 boxes by decreasing freq/prob (to prevent occlusion of box labels):
  sdt_boxes <- list(box_hi, box_mi, box_fa, box_cr)  # list of 4 main boxes (lists)
  plot_fbox_list(sdt_boxes,  # plot list of 4 sdt_boxes:
                 scale_lx = 1,
                 cur_freq = freq, lbl_txt = lbl_txt, col_pal = col_pal,  # PASS current freq/txt/pal arguments!
                 lbl_type = f_lbl, lbl_sep = f_lbl_sep, cex = cex_lbl, lwd = f_lwd, lty = lty)  # no ...!

  # plot_fbox_list(sdt_boxes, scale_lx = scale_x,
  #               lbl_type = f_lbl, cex = cex_lbl, lwd = f_lwd)  # plot list of boxes (scaled)
  # plot_fbox_list(sdt_boxes, lbl_type = f_lbl, cex = cex_lbl, lwd = NULL, density = 10)  # plot list of boxes (bw version)



  ##   (+) Check: Mark 2 key points/checkpoints (per plot): ------
  mark_key_points <- FALSE  # default

  if (mark_key_points) {

    ## Adjust parameters of point appearance:
    pch_p1 <- 21  # set to: 3 OR 12 OR 21
    pch_p2 <- 22  # set to: 4 OR 13 OR 22
    pt_cex <- 1
    pt_lwd <- 1
    col_brd <- col_pal["brd"]
    col_fill_p1 <- make_transparent("gold", .33)
    col_fill_p2 <- make_transparent("red3", .33)

    # (a) by condition:

    if (by == "cddc" && p_split == "v") {  # v01:
      points(prev * scale_x, (1 - sens + h_gap), pch = pch_p1,
             cex = pt_cex, lwd = pt_lwd, col = col_brd, bg = col_fill_p1)   # key point p1: hi
      points(((prev + v_gap) * scale_x), spec, pch = pch_p2,
             cex = pt_cex, lwd = pt_lwd, col = col_brd, bg = col_fill_p2)   # key point p2: cr
    }

    if (by == "cdac" && p_split == "v") {  # v02:
      points(prev * scale_x, (1 - sens + h_gap), pch = pch_p1,
             cex = pt_cex, lwd = pt_lwd, col = col_brd, bg = col_fill_p1)   # key point p1: hi
      points(((prev + v_gap) * scale_x), (1 - spec + h_gap), pch = pch_p2,
             cex = pt_cex, lwd = pt_lwd, col = col_brd, bg = col_fill_p2)   # key point p2: cr
    }

    if (by == "cddc" && p_split == "h") {  # v03:
      points(PPV * scale_x, (1 - ppod + h_gap), pch = 0)         # key point p1: hi
      points(((1 - NPV + v_gap) * scale_x), (1 - ppod), pch = 5) # key point p2: cr
    }

    if (by == "cdac" && p_split == "h") {  # v04:
      points(p_acc_hi * scale_x, (1 - acc + h_gap), pch = 0)         # key point p1: hi
      points(((1 - p_err_fa + v_gap) * scale_x), (1 - acc), pch = 5) # key point p2: fa!
    }

    # (b) by decision:

    if (by == "dccd" && p_split == "v") {  # v05:
      points(ppod * scale_x, (1 - PPV + h_gap), pch = 0) # key point p1: hi
      points(((ppod + v_gap) * scale_x), NPV, pch = 5)   # key point p2: cr
    }

    if (by == "dcac" && p_split == "v") {  # v06:
      points(ppod * scale_x, (1 - PPV + h_gap), pch = 0)              # key point p1: hi
      points(((ppod + v_gap) * scale_x), (1 - NPV + h_gap), pch = 5)  # key point p2: cr
    }

    if (by == "dccd" && p_split == "h") {  # v07:
      points(sens * scale_x, (1 - prev + h_gap), pch = 0)         # key point p1: hi
      points(((1 - spec + v_gap) * scale_x), (1 - prev), pch = 5) # key point p2: cr
    }

    if (by == "dcac" && p_split == "h") {  # v08:
      points(p_acc_hi * scale_x, (1 - acc + h_gap), pch = 0)  # key point p1: hi
      points((p_err_fa * scale_x), (1 - acc), pch = 5)        # key point p2: fa!
    }

    # (c) by accuracy:

    if (by == "accd" && p_split == "v") {  # v09:
      points(acc * scale_x, (1 - p_acc_hi + h_gap), pch = 0) # key point p1: hi
      points(((acc + v_gap) * scale_x), p_err_fa, pch = 5)   # key point p2: fa!
    }

    if (by == "acdc" && p_split == "v") {  # v10:
      points(acc * scale_x, (1 - p_acc_hi + h_gap), pch = 0)             # key point p1: hi
      points(((acc + v_gap) * scale_x), (1 - p_err_fa + h_gap), pch = 5) # key point p2: fa!
    }

    if (by == "accd" && p_split == "h") {  # v11:
      points(sens * scale_x, (1 - prev + h_gap), pch = 0) # key point p1: hi
      points((spec * scale_x), (1 - prev), pch = 5)       # key point p2: cr
    }

    if (by == "acdc" && p_split == "h") {  # v12:
      points(PPV * scale_x, (1 - ppod + h_gap), pch = 0) # key point p1: hi
      points((NPV * scale_x), (1 - ppod), pch = 5)       # key point p2: cr
    }

  } # if (mark_key_points) etc.


  ##   (b) Plot 2 perspectives (compound frequencies and labels) on 2 sides: ----------

  # (A) top (horizontal):

  if (by_top == "cd") {

    # (a) by condition (at top):
    # cond_y <- 1 + (.65 * sum_w)  # constant
    cond_true_x  <- prev/2
    cond_false_x <- prev + (1 - prev)/2 + v_gap

    if (sum_w > 0) {

      # Define 2 horizontal boxes (with optional scale_x):
      box_cond_true  <- make_box("cond_true",  (cond_true_x * scale_x),  tbar_y, (prev * scale_x), bar_lx)        # cond_true
      box_cond_false <- make_box("cond_false", (cond_false_x * scale_x), tbar_y, ((1 - prev) * scale_x), bar_lx)  # cond_false

      fbox_top <- list(box_cond_true, box_cond_false)  # as list of boxes
      plot_fbox_list(fbox_top,  # plot list of boxes (on top):
                     cur_freq = freq, lbl_txt = lbl_txt, col_pal = col_pal,  # PASS current freq/txt/pal arguments!
                     lbl_type = f_lbl_sum, lbl_sep = f_lbl_sep,
                     cex = cex_lbl, lwd = f_lwd, lty = lty)  # no ...!

    } # if (sum_w > 0) etc.

  } else if (by_top == "dc") {

    # (b) by decision (at top):
    # dec_y <- 1 + (.65 * sum_w)  # constant
    dec_pos_x <- ppod/2
    dec_neg_x <- ppod + (1 - ppod)/2 + v_gap

    if (sum_w > 0) {

      # Define 2 horizontal boxes (with optional scale_x):
      box_dec_pos <- make_box("dec_pos", (dec_pos_x * scale_x), tbar_y, (ppod * scale_x), bar_lx)        # dec_pos
      box_dec_neg <- make_box("dec_neg", (dec_neg_x * scale_x), tbar_y, ((1 - ppod) * scale_x), bar_lx)  # dec_neg

      fbox_top <- list(box_dec_pos, box_dec_neg)  # as list of boxes
      plot_fbox_list(fbox_top,  # plot list of boxes (on top):
                     cur_freq = freq, lbl_txt = lbl_txt, col_pal = col_pal,  # PASS current freq/txt/pal arguments!
                     lbl_type = f_lbl_sum, lbl_sep = f_lbl_sep,
                     cex = cex_lbl, lwd = f_lwd, lty = lty)  # no ...!

    } # if (sum_w > 0) etc.

  } else if (by_top == "ac") {

    # (c) by accuracy (at top):
    dec_cor_x <- acc/2
    dec_err_x <- acc + (1 - acc)/2 + v_gap

    if (sum_w > 0) {

      # Define 2 horizontal boxes (with optional scale_x):
      box_dec_cor <- make_box("dec_cor", (dec_cor_x * scale_x), tbar_y, (acc * scale_x), bar_lx)        # dec_cor
      box_dec_err <- make_box("dec_err", (dec_err_x * scale_x), tbar_y, ((1 - acc) * scale_x), bar_lx)  # dec_err

      fbox_top <- list(box_dec_cor, box_dec_err)  # as list of boxes
      plot_fbox_list(fbox_top,  # plot list of boxes (on top):
                     cur_freq = freq, lbl_txt = lbl_txt, col_pal = col_pal,  # PASS current freq/txt/pal arguments!
                     lbl_type = f_lbl_sum, lbl_sep = f_lbl_sep,
                     cex = cex_lbl, lwd = f_lwd, lty = lty)  # no ...!

    } # if (sum_w > 0) etc.

  } else {

    message("Plotting top fboxes: Unknown by_top argument/perspective!")

  } # if (by_top == etc.)


  ## (B) bottom/left (vertical):

  if (by_bot == "cd") {

    # (a) by condition (on left):
    # cond_x <- 0 - (.60 * sum_w) # constant
    cond_false_y  <- (1 - prev)/2
    cond_true_y <- (1 - prev) + prev/2 + h_gap

    if (sum_w > 0) {

      # Define 2 vertical boxes (withOUT scale_x):
      box_cond_true  <- make_box("cond_true",  (lbar_x * scale_x), cond_true_y,  (bar_lx * scale_x), prev)        # cond_true
      box_cond_false <- make_box("cond_false", (lbar_x * scale_x), cond_false_y, (bar_lx * scale_x), (1 - prev))  # cond_false

      fbox_left <- list(box_cond_true, box_cond_false)  # as list of boxes
      plot_fbox_list(fbox_left,
                     cur_freq = freq, lbl_txt = lbl_txt, col_pal = col_pal,  # PASS current freq/txt/pal arguments!
                     lbl_type = f_lbl_sum, lbl_sep = f_lbl_sep,
                     cex = cex_lbl, lwd = f_lwd, lty = lty)  # no ...!

    } # if (sum_w > 0) etc.

  } else if (by_bot == "dc") {

    # (b) by decision (on left):
    # dec_x <- 0 - (.60 * sum_w) # constant
    dec_neg_y <- (1 - ppod)/2
    dec_pos_y <- (1 - ppod) + ppod/2  + h_gap

    if (sum_w > 0) {

      # Define 2 vertical boxes (withOUT scale_x):
      box_dec_pos <- make_box("dec_pos", (lbar_x * scale_x), dec_pos_y, (bar_lx * scale_x), ppod)        # dec_pos
      box_dec_neg <- make_box("dec_neg", (lbar_x * scale_x), dec_neg_y, (bar_lx * scale_x), (1 - ppod))  # dec_neg

      fbox_left <- list(box_dec_pos, box_dec_neg)  # as list of boxes
      plot_fbox_list(fbox_left,  # plot list of boxes (on left):
                     cur_freq = freq, lbl_txt = lbl_txt, col_pal = col_pal,  # PASS current freq/txt/pal arguments!
                     lbl_type = f_lbl_sum, lbl_sep = f_lbl_sep,
                     cex = cex_lbl, lwd = f_lwd, lty = lty)  # no ...!

    } # if (sum_w > 0) etc.


  } else if (by_bot == "ac") {

    # (c) by accuracy (on left):
    # acc_x <- 0 - (.60 * sum_w) # constant
    dec_err_y <- (1 - acc)/2                # OR (rounded freq)
    dec_cor_y <- (1 - acc) + acc/2 + h_gap  # OR (rounded freq): (freq$dec_cor/freq$N * 1/2)

    if (sum_w > 0) {

      # Define 2 vertical boxes (withOUT scale_x):
      box_dec_cor <- make_box("dec_cor", (lbar_x * scale_x), dec_cor_y, (bar_lx * scale_x), acc)        # dec_cor
      box_dec_err <- make_box("dec_err", (lbar_x * scale_x), dec_err_y, (bar_lx * scale_x), (1 - acc))  # dec_err

      fbox_left <- list(box_dec_cor, box_dec_err)  # as list of boxes
      plot_fbox_list(fbox_left,  # plot list of boxes (on left):
                     cur_freq = freq, lbl_txt = lbl_txt, col_pal = col_pal,  # PASS current freq/txt/pal arguments!
                     lbl_type = f_lbl_sum, lbl_sep = f_lbl_sep,
                     cex = cex_lbl, lwd = f_lwd, lty = lty)  # no ...!

    } # if (sum_w > 0) etc.

  } else {

    message("Plotting left fboxes: Unknown by_bot argument/perspective!")

  } # if (by_bot == etc.)


  ##   (c) Plot 3 key probabilities (as lines/arrows): ----------

  if ( !is.null(p_lbl) && !is.na(p_lbl) ) {

    if (by_top == "cd") {

      ## (a) by condition:

      if (p_split == "v") { # v01:

        # constants:
        prev_y <- (1 - brd_dis + h_gap)              # horizontal
        sens_x <- ((0 + brd_dis) * scale_x)          # vertical
        spec_x <- ((1 - brd_dis + v_gap) * scale_x)  # vertical

        # prev (horizontal, left, top): hi
        plot_line(0, prev_y, (prev * scale_x), prev_y,
                  arr_code = arr_c, lbl = label_prob("prev", cur_prob = prob, lbl_type = p_lbl),
                  col_fill = p_col_1, col_txt = p_col_1,
                  lbl_pos = 3, lbl_off = .33, cex = cex_p_lbl, ...)  # Allow ...!

        # sens (vertical, left, top): hi
        plot_line(sens_x, (1 - sens + h_gap), sens_x, (1 + h_gap),
                  arr_code = arr_c, lbl = label_prob("sens", cur_prob = prob, lbl_type = p_lbl),
                  col_fill = p_col_2, col_txt = p_col_2,
                  srt = 90, lbl_pos = 3, lbl_off = 0, cex = cex_p_lbl, ...)  # Allow ...!

        if (by_bot == "dc") { # v01:

          # spec (vertical, right, bottom): cr
          plot_line(spec_x, 0, spec_x, spec,
                    arr_code = arr_c, lbl = label_prob("spec", cur_prob = prob, lbl_type = p_lbl),
                    col_fill = p_col_3, col_txt = p_col_3,
                    srt = 90, lbl_pos = 3, lbl_off = 0, cex = cex_p_lbl, ...)  # Allow ...!

        } else if (by_bot == "ac") { # v02:

          # spec (vertical, right, top): cr
          plot_line(spec_x, (1 - spec + h_gap), spec_x, (1 + h_gap),
                    arr_code = arr_c, lbl = label_prob("spec", cur_prob = prob, lbl_type = p_lbl),
                    col_fill = p_col_3, col_txt = p_col_3,
                    srt = 90, lbl_pos = 3, lbl_off = 0, cex = cex_p_lbl, ...)  # Allow ...!

        } # if (by_bot == etc.)

      } else if (p_split == "h") {

        if (by_bot == "dc") { # v03:

          # constants:
          ppod_x <- ((0 + brd_dis) * scale_x)  # vertical (inside)
          PPV_y <- (1 - brd_dis + h_gap)       # horizontal
          NPV_y <- (0 + brd_dis)               # horizontal

          # ppod (vertical, left, top): hi
          plot_line(ppod_x, (1 - ppod + h_gap), ppod_x, (1 + h_gap),  # ppod
                    arr_code = arr_c, lbl = label_prob("ppod", cur_prob = prob, lbl_type = p_lbl),
                    col_fill = p_col_1, col_txt = p_col_1,
                    srt = 90, lbl_pos = 3, lbl_off = 0, cex = cex_p_lbl, ...)  # Allow ...!

          # ppod_x <- ((0 - sum_w * .90) * scale_x)        # vertical (out left)
          # ppod_x <- ((1 + v_gap + brd_dis) * scale_x)    # vertical (out right)
          # plot_line(ppod_x, (1 - ppod + h_gap), ppod_x, (1 + h_gap),  # ppod
          #           arr_code = arr_c, lbl = label_prob("ppod", cur_prob = prob, lbl_type = p_lbl),
          #           col_fill = "black", col_txt = "black",  # WAS: p_col_1
          #           srt = 90, lbl_pos = 3, lbl_off = 0, cex = cex_p_lbl)

          # PPV (horizontal, left, top): hi
          plot_line(0, PPV_y, (PPV * scale_x), PPV_y,
                    arr_code = arr_c, lbl = label_prob("PPV", cur_prob = prob, lbl_type = p_lbl),
                    col_fill = p_col_2, col_txt = p_col_2,
                    lbl_pos = 3, lbl_off = .33, cex = cex_p_lbl, ...)  # Allow ...!

          # NPV (horizontal, right, bottom): cr
          plot_line(((1 - NPV + v_gap) * scale_x), NPV_y, ((1 + v_gap) * scale_x), NPV_y,
                    arr_code = arr_c, lbl = label_prob("NPV", cur_prob = prob, lbl_type = p_lbl),
                    col_fill = p_col_3, col_txt = p_col_3,
                    lbl_pos = 3, lbl_off = .33, cex = cex_p_lbl, ...)  # Allow ...!

        } else if (by_bot == "ac") { # v04:

          # constants:
          acc_x <- ((0 + brd_dis) * scale_x)  # vertical (inside)
          acc_hi_y <- (1 - brd_dis + h_gap)   # horizontal
          err_fa_y <- (0 + brd_dis)           # horizontal

          # acc (vertical, left, top): hi
          plot_line(acc_x, (1 - acc + h_gap), acc_x, (1 + h_gap),
                    arr_code = arr_c, lbl = label_prob("acc", cur_prob = prob, lbl_type = p_lbl),
                    col_fill = p_col_1, col_txt = p_col_1,
                    srt = 90, lbl_pos = 3, lbl_off = 0, cex = cex_p_lbl, ...)  # Allow ...!

          # p_acc_hi (horizontal, left, top): hi
          plot_line(0, acc_hi_y, (p_acc_hi * scale_x), acc_hi_y,
                    arr_code = arr_c, lbl = label_prob("acc_hi", cur_prob = prob, lbl_type = p_lbl),
                    col_fill = p_col_2, col_txt = p_col_2,
                    lbl_pos = 3, lbl_off = .33, cex = cex_p_lbl, ...)  # Allow ...!

          # p_err_fa (horizontal, right, bottom): fa!
          plot_line(((1 - p_err_fa + v_gap) * scale_x), err_fa_y, ((1 + v_gap) * scale_x), err_fa_y,
                    arr_code = arr_c, lbl = label_prob("err_fa", cur_prob = prob, lbl_type = p_lbl),
                    col_fill = p_col_3, col_txt = p_col_3,
                    lbl_pos = 3, lbl_off = .33, cex = cex_p_lbl, ...)  # Allow ...!

        } # if (by_bot == etc.)

      } # if (p_split == etc.)

    } else if (by_top == "dc") {

      ## (b) by decision:

      if (p_split == "v") { # v05:

        # constants:
        ppod_y <- (1 - brd_dis + h_gap)             # horizontal
        PPV_x <- ((0 + brd_dis) * scale_x)          # vertical
        NPV_x <- ((1 - brd_dis + v_gap) * scale_x)  # vertical

        # ppod (horizontal, left, top): hi
        plot_line(0, ppod_y, (ppod * scale_x), ppod_y,
                  arr_code = arr_c, lbl = label_prob("ppod", cur_prob = prob, lbl_type = p_lbl),
                  col_fill = p_col_1, col_txt = p_col_1,
                  lbl_pos = 3, lbl_off = .33, cex = cex_p_lbl, ...)  # Allow ...!

        # PPV (vertical, left, top): hi
        plot_line(PPV_x, (1 - PPV + h_gap), PPV_x, (1 + h_gap),
                  arr_code = arr_c, lbl = label_prob("PPV", cur_prob = prob, lbl_type = p_lbl),
                  col_fill = p_col_2, col_txt = p_col_2,
                  srt = 90, lbl_pos = 3, lbl_off = 0, cex = cex_p_lbl, ...)  # Allow ...!

        if (by_bot == "cd") { # v05:

          # NPV (vertical, right, bottom): cr
          plot_line(NPV_x, 0, NPV_x, NPV,
                    arr_code = arr_c, lbl = label_prob("NPV", cur_prob = prob, lbl_type = p_lbl),
                    col_fill = p_col_3, col_txt = p_col_3,
                    srt = 90, lbl_pos = 3, lbl_off = 0, cex = cex_p_lbl, ...)  # Allow ...!

        } else if (by_bot == "ac") { # v06:

          # NPV (vertical, right, top): cr
          plot_line(NPV_x, (1 - NPV + h_gap), NPV_x, (1 + h_gap),
                    arr_code = arr_c, lbl = label_prob("NPV", cur_prob = prob, lbl_type = p_lbl),
                    col_fill = p_col_3, col_txt = p_col_3,
                    srt = 90, lbl_pos = 3, lbl_off = 0, cex = cex_p_lbl, ...)  # Allow ...!

        } # if (by_bot == etc.)

      } else if (p_split == "h") {

        if (by_bot == "cd") { # v07:

          # constants:
          prev_x <- ((0 + brd_dis) * scale_x)  # vertical (inside)
          sens_y <- (1 - brd_dis + h_gap)       # horizontal
          spec_y <- (0 + brd_dis)               # horizontal

          # prev (vertical, left, top): hi
          plot_line(prev_x, (1 - prev + h_gap), prev_x, (1 + h_gap),
                    arr_code = arr_c, lbl = label_prob("prev", cur_prob = prob, lbl_type = p_lbl),
                    col_fill = p_col_1, col_txt = p_col_1,
                    srt = 90, lbl_pos = 3, lbl_off = 0, cex = cex_p_lbl, ...)  # Allow ...!

          # sens (horizontal, left, top): hi
          plot_line(0, sens_y, (sens * scale_x), sens_y,
                    arr_code = arr_c, lbl = label_prob("sens", cur_prob = prob, lbl_type = p_lbl),
                    col_fill = p_col_2, col_txt = p_col_2,
                    lbl_pos = 3, lbl_off = .33, cex = cex_p_lbl, ...)  # Allow ...!

          # spec (horizontal, right, bottom): cr
          plot_line(((1 - spec + v_gap) * scale_x), spec_y, ((1 + v_gap) * scale_x), spec_y,
                    arr_code = arr_c, lbl = label_prob("spec", cur_prob = prob, lbl_type = p_lbl),
                    col_fill = p_col_3, col_txt = p_col_3,
                    lbl_pos = 3, lbl_off = .33, cex = cex_p_lbl, ...)  # Allow ...!

        } else if (by_bot == "ac") { # v08:

          # constants:
          acc_x <- ((0 + brd_dis) * scale_x)  # vertical (inside)
          acc_hi_y <- (1 - brd_dis + h_gap)   # horizontal
          err_fa_y <- (0 + brd_dis)           # horizontal

          # acc (vertical, left, top): hi
          plot_line(acc_x, (1 - acc + h_gap), acc_x, (1 + h_gap),
                    arr_code = arr_c, lbl = label_prob("acc", cur_prob = prob, lbl_type = p_lbl),
                    col_fill = p_col_1, col_txt = p_col_1,
                    srt = 90, lbl_pos = 3, lbl_off = 0, cex = cex_p_lbl, ...)  # Allow ...!

          # p_acc_hi (horizontal, left, top): hi
          plot_line(0, acc_hi_y, (p_acc_hi * scale_x), acc_hi_y,
                    arr_code = arr_c, lbl = label_prob("acc_hi", cur_prob = prob, lbl_type = p_lbl),
                    col_fill = p_col_2, col_txt = p_col_2,
                    lbl_pos = 3, lbl_off = .33, cex = cex_p_lbl, ...)  # Allow ...!

          # p_err_fa (horizontal, right, bottom): fa!
          plot_line(0, err_fa_y, (p_err_fa * scale_x), err_fa_y,
                    arr_code = arr_c, lbl = label_prob("err_fa", cur_prob = prob, lbl_type = p_lbl),
                    col_fill = p_col_3, col_txt = p_col_3,
                    lbl_pos = 3, lbl_off = .33, cex = cex_p_lbl, ...)  # Allow ...!

        } # if (by_bot == etc.)

      } # if (p_split == etc.)


    } else if (by_top == "ac") {

      ## (c) by accuracy:

      if (p_split == "v") { # v09:

        # constants:
        acc_y <- (1 - brd_dis + h_gap)               # horizontal
        acc_hi_x <- ((0 + brd_dis) * scale_x)          # vertical
        err_fa_x <- ((1 - brd_dis + v_gap) * scale_x)  # vertical

        # acc (horizontal, left, top): hi
        plot_line(0, acc_y, (acc * scale_x), acc_y,
                  arr_code = arr_c, lbl = label_prob("acc", cur_prob = prob, lbl_type = p_lbl),
                  col_fill = p_col_1, col_txt = p_col_1,
                  lbl_pos = 3, lbl_off = .33, cex = cex_p_lbl, ...)  # Allow ...!

        # p_acc_hi (vertical, left, top): hi
        plot_line(acc_hi_x, (1 - p_acc_hi + h_gap), acc_hi_x, (1 + h_gap),
                  arr_code = arr_c, lbl = label_prob("acc_hi", cur_prob = prob, lbl_type = p_lbl),
                  col_fill = p_col_2, col_txt = p_col_2,
                  srt = 90, lbl_pos = 3, lbl_off = 0, cex = cex_p_lbl, ...)  # Allow ...!

        if (by_bot == "cd") { # v09:

          # p_err_fa (vertical, right, bottom): fa!
          plot_line(err_fa_x, 0, err_fa_x, p_err_fa,
                    arr_code = arr_c, lbl = label_prob("err_fa", cur_prob = prob, lbl_type = p_lbl),
                    col_fill = p_col_3, col_txt = p_col_3,
                    srt = 90, lbl_pos = 3, lbl_off = 0, cex = cex_p_lbl, ...)  # Allow ...!

        } else if (by_bot == "dc") { # v10:

          # p_err_fa (vertical, right, top): fa!
          plot_line(err_fa_x, (1 - p_err_fa + h_gap), err_fa_x, (1 + h_gap),
                    arr_code = arr_c, lbl = label_prob("err_fa", cur_prob = prob, lbl_type = p_lbl),
                    col_fill = p_col_3, col_txt = p_col_3,
                    srt = 90, lbl_pos = 3, lbl_off = 0, cex = cex_p_lbl, ...)  # Allow ...!

        } # if (by_bot == etc.)

      } else if (p_split == "h") {

        if (by_bot == "cd") { # v11:

          # constants:
          prev_x <- ((0 + brd_dis) * scale_x)  # vertical (inside)
          sens_y <- (1 - brd_dis + h_gap)       # horizontal
          spec_y <- (0 + brd_dis)               # horizontal

          # prev (vertical, left, top): hi
          plot_line(prev_x, (1 - prev + h_gap), prev_x, (1 + h_gap),
                    arr_code = arr_c, lbl = label_prob("prev", cur_prob = prob, lbl_type = p_lbl),
                    col_fill = p_col_1, col_txt = p_col_1,
                    srt = 90, lbl_pos = 3, lbl_off = 0, cex = cex_p_lbl, ...)  # Allow ...!

          # sens (horizontal, left, top): hi
          plot_line(0, sens_y, (sens * scale_x), sens_y,
                    arr_code = arr_c, lbl = label_prob("sens", cur_prob = prob, lbl_type = p_lbl),
                    col_fill = p_col_2, col_txt = p_col_2,
                    lbl_pos = 3, lbl_off = .33, cex = cex_p_lbl, ...)  # Allow ...!

          # spec (horizontal, left, bottom): cr
          plot_line(0, spec_y, (spec * scale_x), spec_y,
                    arr_code = arr_c, lbl = label_prob("spec", cur_prob = prob, lbl_type = p_lbl),
                    col_fill = p_col_3, col_txt = p_col_3,
                    lbl_pos = 3, lbl_off = .33, cex = cex_p_lbl, ...)  # Allow ...!

        } else if (by_bot == "dc") { # v12:

          # constants:
          ppod_x <- ((0 + brd_dis) * scale_x)  # vertical (inside)
          PPV_y <- (1 - brd_dis + h_gap)   # horizontal
          NPV_y <- (0 + brd_dis)           # horizontal

          # ppod (vertical, left, top): hi
          plot_line(ppod_x, (1 - ppod + h_gap), ppod_x, (1 + h_gap),
                    arr_code = arr_c, lbl = label_prob("ppod", cur_prob = prob, lbl_type = p_lbl),
                    col_fill = p_col_1, col_txt = p_col_1,
                    srt = 90, lbl_pos = 3, lbl_off = 0, cex = cex_p_lbl, ...)  # Allow ...!

          # PPV (horizontal, left, top): hi
          plot_line(0, PPV_y, (PPV * scale_x), PPV_y,
                    arr_code = arr_c, lbl = label_prob("PPV", cur_prob = prob, lbl_type = p_lbl),
                    col_fill = p_col_2, col_txt = p_col_2,
                    lbl_pos = 3, lbl_off = .33, cex = cex_p_lbl, ...)  # Allow ...!

          # NPV (horizontal, left, bottom): cr
          plot_line(0, NPV_y, (NPV * scale_x), NPV_y,
                    arr_code = arr_c, lbl = label_prob("NPV", cur_prob = prob, lbl_type = p_lbl),
                    col_fill = p_col_3, col_txt = p_col_3,
                    lbl_pos = 3, lbl_off = .33, cex = cex_p_lbl, ...)  # Allow ...!

        } # if (by_bot == etc.)

      } # if (p_split == etc.)

    } else {

      message("Plotting p-lines: Unknown by argument/perspective!")

    } # if (by_top == etc.)

  } # if ( !is.null(p_lbl) && !is.na(p_lbl) ) etc.


  ##   (d) Plot text labels (for 2 perspectives) on 2 sides: ----------

  ## (A) top labels (horizontal):

  if (by_top == "cd") {

    ## (a) by condition:

    #if ( !is.null(sum_w) && !is.na(sum_w) ) {

    # ftype label: Condition (center, horizontal)
    plot_ftype_label("cond_true", (1/2 * scale_x), y_max,
                     lbl_txt = lbl_txt, suffix = ":", pos = NULL, col = col_pal["txt"], cex = cex_lbl, ...)  # Allow ...!

    #} # if (sum_w etc.)

    # 2 sub-group labels:
    plot_freq_label("cond_true", (cond_true_x * scale_x), (1 + h_gap + h_shift),
                    lbl_type = f_lbl_hd, lbl_sep = f_lbl_sep,
                    cur_freq = freq, lbl_txt = lbl_txt,
                    suffix = ":", pos = 3, col = col_pal["txt"], cex = cex_lbl)
    plot_freq_label("cond_false", (cond_false_x * scale_x), (1 + h_gap + h_shift),
                    lbl_type = f_lbl_hd, lbl_sep = f_lbl_sep,
                    cur_freq = freq, lbl_txt = lbl_txt,
                    suffix = ":", pos = 3, col = col_pal["txt"], cex = cex_lbl)

  } else if (by_top == "dc") {

    ## (b) by decision:

    #if ( !is.null(sum_w) && !is.na(sum_w) ) {

    # ftype label: Decision (center, horizontal)
    plot_ftype_label("dec_pos", (1/2 * scale_x), y_max,
                     lbl_txt = lbl_txt, suffix = ":", pos = NULL, col = col_pal["txt"], cex = cex_lbl, ...)  # Allow ...!

    #} # if (sum_w etc.)

    # 2 sub-group labels:
    plot_freq_label("dec_pos", (dec_pos_x * scale_x), (1 + h_gap + h_shift),
                    lbl_type = f_lbl_hd, lbl_sep = f_lbl_sep,
                    cur_freq = freq, lbl_txt = lbl_txt,
                    suffix = ":", pos = 3, col = col_pal["txt"], cex = cex_lbl)
    plot_freq_label("dec_neg", (dec_neg_x * scale_x), (1 + h_gap + h_shift),
                    lbl_type = f_lbl_hd, lbl_sep = f_lbl_sep,
                    cur_freq = freq, lbl_txt = lbl_txt,
                    suffix = ":", pos = 3, col = col_pal["txt"], cex = cex_lbl)


  } else if (by_top == "ac") {

    ## (c) by accuracy:

    # if ( !is.null(sum_w) && !is.na(sum_w) ) {

    # ftype label: Accuracy (center, horizontal)
    plot_ftype_label("dec_cor", (1/2 * scale_x), y_max,
                     lbl_txt = lbl_txt, suffix = ":", pos = NULL, col = col_pal["txt"], cex = cex_lbl, ...)  # Allow ...!

    #} # if (sum_w etc.)

    # 2 sub-group labels:
    plot_freq_label("dec_cor", (dec_cor_x * scale_x), (1 + h_gap + h_shift),
                    lbl_type = f_lbl_hd, lbl_sep = f_lbl_sep,
                    cur_freq = freq, lbl_txt = lbl_txt,
                    suffix = ":", pos = 3, col = col_pal["txt"], cex = cex_lbl)
    plot_freq_label("dec_err", (dec_err_x * scale_x), (1 + h_gap + h_shift),
                    lbl_type = f_lbl_hd, lbl_sep = f_lbl_sep,
                    cur_freq = freq, lbl_txt = lbl_txt,
                    suffix = ":", pos = 3, col = col_pal["txt"], cex = cex_lbl)


  } else {

    message("Plotting top labels: Unknown by_top argument/perspective!")

  } # if (by_top ==  etc.)


  ## (B) bottom/left labels (vertical):

  if (by_bot == "cd") {

    # (a) by condition:

    # if (sum_w > 0) {

    ## ftype label: # Condition (left, vertical up)
    plot_ftype_label("cond_true", (x_min * scale_x), .5,
                     lbl_txt = lbl_txt, suffix = ":",
                     srt = 90, pos = 3, col = col_pal["txt"], cex = cex_lbl, ...)  # Allow ...!

    #} # if (sum_w > 0) etc.

    # 2 sub-group labels:
    plot_freq_label("cond_true",  ((0 - v_shift) * scale_x), cond_true_y,
                    lbl_type = f_lbl_hd, lbl_sep = f_lbl_sep,
                    cur_freq = freq, lbl_txt = lbl_txt,
                    suffix = ":", srt = 90, pos = 3, col = col_pal["txt"], cex = cex_lbl)
    plot_freq_label("cond_false", ((0 - v_shift) * scale_x), cond_false_y,
                    lbl_type = f_lbl_hd, lbl_sep = f_lbl_sep,
                    cur_freq = freq, lbl_txt = lbl_txt,
                    suffix = ":", srt = 90, pos = 3, col = col_pal["txt"], cex = cex_lbl)


  } else if (by_bot == "dc") {

    # (b) by decision:

    # if (sum_w > 0) {

    ## ftype label: Decision (left, vertical up)
    plot_ftype_label("dec_pos", (x_min * scale_x), .5, lbl_txt = lbl_txt, suffix = ":",
                     srt = 90, pos = 3, col = col_pal["txt"], cex = cex_lbl, ...)  # Allow ...!

    #} # if (sum_w > 0) etc.

    # 2 sub-group labels:
    plot_freq_label("dec_pos", ((0 - v_shift) * scale_x), dec_pos_y,
                    lbl_type = f_lbl_hd, lbl_sep = f_lbl_sep,
                    cur_freq = freq, lbl_txt = lbl_txt,
                    suffix = ":", srt = 90, pos = 3, col = col_pal["txt"], cex = cex_lbl)
    plot_freq_label("dec_neg", ((0 - v_shift) * scale_x), dec_neg_y,
                    lbl_type = f_lbl_hd, lbl_sep = f_lbl_sep,
                    cur_freq = freq, lbl_txt = lbl_txt,
                    suffix = ":", srt = 90, pos = 3, col = col_pal["txt"], cex = cex_lbl)


  } else if (by_bot == "ac") {

    # (c) by accuracy:

    # if (sum_w > 0) {

    ## ftype label: Accuracy (left, vertical up)
    plot_ftype_label("dec_cor", (x_min * scale_x), .5,
                     lbl_txt = lbl_txt, suffix = ":",
                     srt = 90, pos = 3, col = col_pal["txt"], cex = cex_lbl, ...)  # Allow ...!

    #} # if (sum_w > 0) etc.

    # 2 sub-group labels:
    plot_freq_label("dec_cor", ((0 - v_shift) * scale_x), dec_cor_y,
                    lbl_type = f_lbl_hd, lbl_sep = f_lbl_sep,
                    cur_freq = freq, lbl_txt = lbl_txt,
                    suffix = ":", srt = 90, pos = 3, col = col_pal["txt"], cex = cex_lbl)
    plot_freq_label("dec_err", ((0 - v_shift) * scale_x), dec_err_y,
                    lbl_type = f_lbl_hd, lbl_sep = f_lbl_sep,
                    cur_freq = freq, lbl_txt = lbl_txt,
                    suffix = ":", srt = 90, pos = 3, col = col_pal["txt"], cex = cex_lbl)


  } else {

    message("Plotting left labels: Unknown by_bot argument/perspective!")

  } # if (by_bot == etc.)


  ##   (f) Plot other stuff: ----------

  # box_else <- make_box("else_box", 9, -2, b_w, b_h)  # define some arbitrary box
  # plot(box_else, col = "firebrick1", cex = 1/2, font = 2)     # plot box


  ## (6) Title: ------

  # Define parts:
  if (nchar(title_lbl) > 0) { title_lbl <- paste0(title_lbl, ":\n") }  # put on top (in separate line)

  if (title_lbl == "") {  # if title has been set to "":
    type_lbl <- ""        # assume that no subtitle is desired either
  } else {
    type_lbl <- paste0(lbl["plot_area_lbl"], " (by ", as.character(by), ")")  # plot name: Area/Mosaic/Eikosogram/etc.
  }

  # Compose label:
  cur_title_lbl <- paste0(title_lbl, type_lbl)

  # Plot title:
  title(cur_title_lbl, adj = 0, line = 0, font.main = 1, cex.main = 1.2)  # (left, not raised, normal font)


  ## (7) Margins: ------

  if (mar_notes) {

    # Note:
    note_lbl <- ""  # initialize

    if (scale == "f") {
      note_lbl <- label_note(area = area, scale = scale)
    }

    plot_mar(show_freq = TRUE, show_cond = TRUE, show_dec = TRUE,
             show_accu = TRUE, accu_from_freq = FALSE,
             note = note_lbl,
             cur_freq = freq, cur_prob = prob, lbl_txt = lbl_txt)

  } # if (mar_notes) etc.


  ## Finish: ---------

  # on.exit(par(opar))  # par(opar)  # restore original settings
  invisible()# restores par(opar)

} # plot_area end.


## (3) Check: ------

# ## Basics:
# plot_area()  # default area plot, same as:
# plot_area(by = "cddc", p_split = "v", area = "sq", scale = "p")
#
# # Computing local freq and prob values:
# plot_area(prev = .5, sens = 4/5, spec = 3/5, N = 4)
#
# # Customizing text and color:
# plot_area(prev = .2, sens = 4/5, spec = 3/5, N = 10,
#           by = "cddc", p_split = "v", scale = "p",
#           title_lbl = "Custom text and color:",
#           lbl_txt = txt_org, f_lbl = "namnum", f_lbl_sep = ":\n",
#           f_lwd = 2, col_pal = pal_rgb)  # custom color
# plot_area(prev = .4, sens = 6/7, spec = 4/7, N = 5,
#           by = "cdac", p_split = "h", scale = "p",
#           title_lbl = "Custom text and color:",
#           lbl_txt = txt_org, f_lbl = "namnum", f_lbl_sep = ":\n",  # custom text
#           f_lwd = 1, col_pal = pal_kn)  # custom color
#
# ## Versions:
# # by x p_split (= [3 x 2 x 2] = 12 versions):
# plot_area(by = "cddc", p_split = "v")  # v01 (see v07)
# plot_area(by = "cdac", p_split = "v")  # v02 (see v11)
# plot_area(by = "cddc", p_split = "h")  # v03 (see v05)
# plot_area(by = "cdac", p_split = "h")  # v04 (see v09)
#
# plot_area(by = "dccd", p_split = "v")  # v05 (is v03 rotated)
# plot_area(by = "dcac", p_split = "v")  # v06 (see v12)
# plot_area(by = "dccd", p_split = "h")  # v07 (is v01 rotated)
# plot_area(by = "dcac", p_split = "h")  # v08 (see v10)
#
# plot_area(by = "accd", p_split = "v")  # v09 (is v04 rotated)
# plot_area(by = "acdc", p_split = "v")  # v10 (is v08 rotated)
# plot_area(by = "accd", p_split = "h")  # v11 (is v02 rotated)
# plot_area(by = "acdc", p_split = "h")  # v12 (is v06 rotated)
#
# ## Options:
#
# # area:
# plot_area(area = "sq")  # main area as square (by scaling x-values)
# plot_area(area = "no")  # rectangular main area (using full plotting region)
#
# # scale (matters for small N):
# plot_area(by = "cddc", p_split = "v", scale = "p")  # scaled by prob (default)
# plot_area(by = "cddc", p_split = "v", scale = "f")  # scaled by freq (for small N)
# plot_area(by = "cdac", p_split = "h", scale = "p")  # scaled by prob (default)
# plot_area(by = "cdac", p_split = "h", scale = "f")  # scaled by freq (for small N)
#
# # gaps (sensible range: 0--.10):
# plot_area(gaps = NA)           # use default gaps (based on p_split)
# plot_area(gaps = NULL)         # no gaps (in main area and top/left bars)
# plot_area(gaps = c(0, 0))      # no gaps
# plot_area(gaps = c(.05, .01))  # v_gap > h_gap
# plot_area(gaps = c(.01, .05))  # v_gap < h_gap
#
# # freq labels:
# plot_area(f_lbl = "default", f_lbl_sep = " = ")  # default
# plot_area(f_lbl = NA)      # NA/NULL: no freq labels (in main area & top/left boxes)
# plot_area(f_lbl = "abb")   # abbreviated name (i.e., variable name)
# plot_area(f_lbl = "nam")   # only freq name
# plot_area(f_lbl = "num")   # only freq number
# plot_area(f_lbl = "namnum", f_lbl_sep = ":\n", cex_lbl = .75)  # explicit & smaller
#
# # prob labels:
# plot_area(p_lbl = "default")  # default
# plot_area(p_lbl = NA)      # no prob labels (and no links)
# plot_area(p_lbl = "no")    # show links, but no labels
# plot_area(p_lbl = "abb")   # abbreviated name (i.e., variable name)
# plot_area(p_lbl = "nam")   # only prob name
# plot_area(p_lbl = "num")   # only prob number
# plot_area(p_lbl = "namnum", cex_lbl = .70)  # explicit & small (Size of p-labels: cex_lbl - .05).
#
# # prob arrows:
# plot_area(arr_c = -3, f_lbl = NA)  # 2 points (default)
# plot_area(arr_c = +3, f_lbl = NA)  # V-shape arrows
# plot_area(arr_c = +6, f_lbl = NA)  # T-shape arrows
# plot_area(arr_c = +6, f_lbl = NA,
#           brd_dis = -.02, col_p = c("black")) # adjust arrow type/position
#
# # f_lwd:
# plot_area(f_lwd =  NA) # default: no lines (if f_lwd = 0/NULL/NA: lty = 0)
# plot_area(f_lwd =  3)  # thicker lines
# plot_area(f_lwd = .5)  # thinner lines
# plot_area(f_lwd =  0)  # no lines (if f_lwd = 0/NULL/NA: lty = 0)
#
# # sum_w:
# plot_area(sum_w = .25)  # default (showing top and left freq panels & labels)
# plot_area(sum_w = .33)  # 1/3 borders
# plot_area(sum_w = .50)  # max. sum_w
# plot_area(sum_w = 999)  # corrected to max. sum_w = 1
# plot_area(sum_w = 0)    # if sum_w = 0 / NULL / NA => remove top and left freq panels
# plot_area(by = "cdac", p_split = "h", sum_w = 0)
#
# ## Plain plot versions:
# plot_area(sum_w = 0, f_lbl = "abb", p_lbl = NA)  # no compound indicators (on top/left)
# plot_area(gap = c(0, 0), sum_w = 0, f_lbl = "num", p_lbl = "num") # no gaps, numeric labels
# plot_area(f_lbl = "abb", p_lbl = NA) # plot with abbreviated labels
# plot_area(f_lbl = "num", p_lbl = NA) # no borders around boxes
#
# ## Suggested combinations:
# plot_area(f_lbl = "nam", p_lbl = "nam")
# plot_area(f_lbl = "num", p_lbl = "num")
# plot_area(f_lbl = "namnum", p_lbl = "nam", cex_lbl = .80)


## (+) ToDo: ------

## - Fix top/left captions when sum_w = 0 (and consider setting
##   3 summary shapes to 3 squares to boundary case of sum_w = 1).

## - Shift entire plot to center (right) when area == "sq".

## - Shorten and simplify code (by removing redundancies).


## (*) Done: ----------

## (1) Make plot area (nearly) square (by removing compound freq on right)
##     and correct 1 side (x) for current aspect ratio
##     (to always show 4 SDT cases as square).

## (2) Allow for independent 2 gaps (in primary vs. secondary perspective).

## (3) Distinguish 2 basic geometric variants per layout version:
##   Compute current dimensions (locations of 4 boxes: hi mi fa cr) based on
##   (a) scale = "p": probabilities ( == non-rounded frequencies ): always show ALL areas > 0.
##   (b) scale = "f": current frequencies (rounded or non-rounded) frequencies.
##
## Strategy:
## - Always determine plot coordinates and dimensions from probabilities!
## - Use either (a) exact prob OR (b) re-compute prob from current freq.

## (4) Base code/plot on appropriate version and transponation of 2 x 2 table:
##     Same plotting code can cover all 12 versions of (4 x 3)!

## (5) Distinguish 12 layout versions:
##     3 primary x 2 secondary perspectives (each by "cd" x "dc" x "ac")
##     each in 2 primary splits: vertical vs. horizontal

# (a) 6 vertical splits:
# - "cddc" "cdac"
# - "dccd" "dcac"
# - "accd" "acdc"
# (b) 6 horizontal splits:
# - same 6 plots flipped

# ad (a): 6 vertical splits:

# 1. cddc:      by cd:   v_split = prev
#               +: -:    h_split
# by dc:  pos:  hi fa    _l =   _r =
#         neg:  mi cr    sens   spec

# 2. cdac:      by cd:
#               +: -:
# by ac:  cor:  hi cr
#         err:  mi fa

# 3. dccd:      by dc:   v-split = ppod
#               +:  -:
# by cd: true:  hi  mi   h-splits =
#        false: fa  cr   PPV & NPV

# 4. dcac:      by dc:
#               +:  -:
# by ac:  cor:  hi  cr
#         err:  fa  mi

# 5. accd:      by ac:    v-split = acc
#               +:  -:
# by cd: true:  hi  mi
#        false: cr  fa

# 6. acdc:      by ac:
#               +:  -:
# by dc:  pos:  hi  fa
#         neg:  cr  mi

## - Complete links (& consider dedicated parameters (p_lwd, p_lty) etc.)

## - Complete check points for all 12 plots.

## - Set sensible defaults.

## - Make a function of 3 essential probabilities (generating local freq and prob),
##   and pass local txt and pal arguments (to allow changes when calling function).

## - Add documentation and integrate in riskyr project [2018 10 22].

## - Use scale == "f" to re-compute prob values from (rounded or non-rounded) freq.


## eof. ----------
