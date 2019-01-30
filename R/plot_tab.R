## plot_tab.R | riskyr
## 2019 01 30
## Plot contingency/frequency table
## (based on plot_area.R).
## -----------------------------------------------

## (1) plot_tab: Documentation ----------

#' Plot a 2 x 2 contingency table of population frequencies.
#'
#' \code{plot_tab} plots a 2 x 2 contingency table
#' (aka. confusion table) of
#' 4 classification cases (\code{\link{hi}}, \code{\link{mi}},
#' \code{\link{fa}}, \code{\link{cr}})
#' and corresponding row and column sums.
#'
#' \code{plot_tab} computes its frequencies \code{\link{freq}}
#' from a sufficient and valid set of 3 essential probabilities
#' (\code{\link{prev}}, and
#' \code{\link{sens}} or its complement \code{\link{mirt}}, and
#' \code{\link{spec}} or its complement \code{\link{fart}})
#' or existing frequency information \code{\link{freq}}
#' and a population size of \code{\link{N}} individuals.
#'
#' \code{plot_tab} is derived from \code{\link{plot_area}},
#' but does not scale the dimensions of table cells.
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
#' Note: In contrast to \code{\link{plot_area}}, this setting only
#' determines which 3 probability links are shown
#' (e.g., when \code{p_link = "def"}).
#'
#' @param area  A character code specifying the shape of the main area,
#' with 4 options:
#'   \enumerate{
#'   \item \code{"sq"}: main area is scaled to square;
#'   \item \code{"no"}: no scaling (rectangular area fills plot size; default).
#'   }
#'
#' @param scale  Scale probabilities (but not table cell dimensions) either by
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
#' @param brd_w  Border width for showing 2 perspective summaries
#' on top and left borders of main area (as a proportion of area size)
#' in a range \code{0 <= brd_w <= 1}.
#' Default: \code{brd_w = .10}.
#'
#' @param gaps Size of gaps (as binary numeric vector) specifying
#' the widths of vertical and horizontal gaps between 2 x 2 table
#' and sums (in bottom row and right column).
#' Default: \code{gaps = c(.05, .06)}.
#'
#' @param f_lbl  Type of label for showing frequency values in 4 main areas,
#' with 6 options:
#'   \enumerate{
#'   \item \code{"def"}: abbreviated names and frequency values (default);
#'   \item \code{"abb"}: abbreviated frequency names only (as specified in code);
#'   \item \code{"nam"}: names only (as specified in \code{lbl_txt = txt});
#'   \item \code{"num"}: numeric frequency values only;
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
#' Default: \code{f_lbl_sum = "def"}: abbreviated names and numeric values.
#'
#' @param f_lbl_hd  Type of label for showing frequency values in header,
#' with same 6 options as \code{f_lbl} (above).
#' Default: \code{f_lbl_hd = "nam"}: names only (as specified in \code{lbl_txt = txt}).
#'
#' @param f_lwd  Line width of areas.
#' Default: \code{f_lwd = 1}.
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
#'   \item \code{NA}: no link (same for \code{p_lbl = NULL}, default).
#'   }
#'
#' @param arr_c Arrow code for symbols at ends of probability links
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
#' @param col_p Colors of probability links (as vector of 3 colors).
#' Default: \code{col_p = c(grey(.15, .99), "yellow", "yellow")}.
#'
#' @param brd_dis Distance of probability links from cell center
#' (as a constant).
#' Default: \code{brd_dis = .30}.
#' Note: Adjust to avoid overlapping labels.
#'
#' @param lbl_txt Default label set for text elements.
#' Default: \code{lbl_txt = \link{txt}}.
#'
#' @param title_lbl Text label for current plot title.
#' Default: \code{title_lbl = txt$scen_lbl}.
#'
#' @param cex_lbl Scaling factor for text labels (frequencies and headers).
#' Default: \code{cex_lbl = .90}.
#'
#' @param cex_p_lbl Scaling factor for text labels (probabilities).
#' Default: \code{cex_p_lbl = cex_lbl - .05}.
#'
#' @param col_pal Color palette.
#' Default: \code{col_pal = \link{pal}}.
#'
#' @param mar_notes Boolean option for showing margin notes.
#' Default: \code{mar_notes = TRUE}.
#'
#' @param ...  Other (graphical) parameters.
#'
#' @return Nothing (NULL).
#'
#' @examples
#' ## Basics:
#' # (1) Plotting global freq and prob values:
#' plot_tab()
#' plot_tab(area = "sq", f_lwd = 3, col_pal = pal_rgb)
#' plot_tab(f_lbl = "namnum", f_lbl_sep = " = ", brd_w = .10, f_lwd = .5)
#'
#' # (2) Computing local freq and prob values:
#' plot_tab(prev = .5, sens = 4/5, spec = 3/5, N = 10, f_lwd = 1)
#'
#' ## Plot versions:
#' # by x p_split [yields (3 x 2) x 2] = 12 versions]:
#' plot_tab(by = "cddc", p_split = "v", p_lbl = "def")  # v01 (see v07)
#' plot_tab(by = "cdac", p_split = "v", p_lbl = "def")  # v02 (see v11)
#' plot_tab(by = "cddc", p_split = "h", p_lbl = "def")  # v03 (see v05)
#' plot_tab(by = "cdac", p_split = "h", p_lbl = "def")  # v04 (see v09)
#'
#' plot_tab(by = "dccd", p_split = "v", p_lbl = "def")  # v05 (is v03 rotated)
#' plot_tab(by = "dcac", p_split = "v", p_lbl = "def")  # v06 (see v12)
#' plot_tab(by = "dccd", p_split = "h", p_lbl = "def")  # v07 (is v01 rotated)
#' plot_tab(by = "dcac", p_split = "h", p_lbl = "def")  # v08 (see v10)
#'
#' plot_tab(by = "accd", p_split = "v", p_lbl = "def")  # v09 (is v04 rotated)
#' plot_tab(by = "acdc", p_split = "v", p_lbl = "def")  # v10 (is v08 rotated)
#' plot_tab(by = "accd", p_split = "h", p_lbl = "def")  # v11 (is v02 rotated)
#' plot_tab(by = "acdc", p_split = "h", p_lbl = "def")  # v12 (is v06 rotated)
#'
#' ## Explore labels and links:
#' plot_tab(f_lbl = "abb", p_lbl = NA)  # abbreviated labels, no probability links
#' plot_tab(f_lbl = "num", f_lbl_sum = "abb", p_lbl = "num", f_lbl_hd = "abb")
#' plot_tab(f_lbl = "def", f_lbl_sum = "def", p_lbl = "def", f_lbl_hd = "nam")
#' plot_tab(f_lbl = "namnum", f_lbl_sep = " = ",
#'          f_lbl_sum = "namnum", f_lbl_hd = "num", p_lbl = "namnum")
#'
#' ## Misc. options:
#' plot_tab(area = "sq")        # area: square
#' plot_tab(title_lbl = "")     # no titles
#' plot_tab(mar_notes = FALSE)  # no margin notes
#'
#' plot_tab(by = "cddc", gaps = c(.08, .00), area = "sq")    # gaps
#' plot_tab(by = "cddc", gaps = c(.02, .08), p_split = "h")  # gaps
#'
#' # Showing prob as lines:
#' plot_tab(prev = 1/4, sens = 6/7, spec = 3/5, N = 100,
#'          by = "cddc", p_split = "v", col_pal = pal_rgb,
#'          p_lbl = "def", brd_dis = .25, arr_c = -3)
#' plot_tab(prev = 1/3, sens = 6/7, spec = 3/4, N = 100, scale = "f",
#'          by = "cddc", p_split = "h",
#'          lwd = 2, p_lbl = "def", brd_dis = .20, arr_c = +3,
#'          f_lwd = .5, col_pal = pal_bwp)  # bw-print version
#'
#' # Custom text labels and colors:
#' plot_tab(prev = .5, sens = 4/5, spec = 3/5, N = 10,
#'          by = "cddc", p_split = "v", area = "no",
#'          lbl_txt = txt_TF,  # custom text
#'          f_lbl = "namnum", f_lbl_sep = ":\n", f_lbl_sum = "num", f_lbl_hd  = "nam",
#'          col_pal = pal_vir, f_lwd = 3)  # custom colors
#' plot_tab(prev = .5, sens = 3/5, spec = 4/5, N = 10,
#'          by = "cddc", p_split = "h", area = "sq",
#'          lbl_txt = txt_org,  # custom text
#'          f_lbl = "namnum", f_lbl_sep = ":\n", f_lbl_sum = "num", f_lbl_hd  = "nam",
#'          col_pal = pal_kn, f_lwd = 1)  # custom colors
#'
#' ## Note some differences to plot_area (i.e., area/mosaic plot):
#' #
#' # In plot_tab:
#' #
#' # (1) p_split does not matter (except for selecting different prob links):
#' plot_tab(by = "cddc", p_split = "v")  # v01 (see v07)
#' plot_tab(by = "cddc", p_split = "h")  # v03 (see v05)
#' #
#' # (2) scale does not matter for dimensions (which are constant),
#' #     BUT matters for values shown in prob links and on margins:
#' plot_tab(N = 5, prev = .3, sens = .9, spec = .5,
#'          by = "cddc", scale = "p", p_lbl = "def", round = TRUE)  # (a) exact prob values
#' plot_tab(N = 5, prev = .3, sens = .9, spec = .5,
#'          by = "cddc", scale = "f", p_lbl = "def", round = TRUE)  # (b) prob from rounded freq!
#' plot_tab(N = 5, prev = .3, sens = .9, spec = .5,
#'          by = "cddc", scale = "f", p_lbl = "def", round = FALSE) # (c) same values as (a)
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
#' \code{\link{plot_area}} for plotting mosaic plot (scaling area dimensions);
#' \code{\link{pal}} contains current color settings;
#' \code{\link{txt}} contains current text settings.
#'
#' @export

## (2) plot_tab: Definition ----------

plot_tab <- function(prev = num$prev,    # probabilities
                     sens = num$sens, mirt = NA,
                     spec = num$spec, fart = NA,
                     N = num$N,          # population size N

                     # Plot options:
                     by = "cddc",        # 2 perspectives (top + left): by = "cd" "dc" "ac"  (default: "cddc")
                     p_split = "v",      # primary/perspective split: "v": vertical vs. "h": horizontal
                     area = "no",        # sq" (default: correcting x-values for aspect ratio of current plot) vs. "no" (NA, NULL, "fix", "hr")
                     scale = "p",        # in plot_area: "p": scale boxes by exact probabilities (default) vs. "f": scale boxes by (rounded or non-rounded) freq.

                     # Freq boxes:
                     round = TRUE,       # round freq to integers? (default: round = TRUE), when not rounded: n_digits = 2 (currently fixed).
                     f_lbl = "num",      # freq label of 4 SDT & N cells: "default" vs. "abb", "nam", "num", "namnum". (Set to NA/NULL to hide freq labels).
                     f_lbl_sep = NA,     # freq label separator (default: " = ", use ":\n" to add an extra line break)
                     f_lbl_sum = f_lbl,  # freq label of summary cells (bottom row and right column)
                     f_lbl_hd  = "nam",  # freq labels of headers at top (for columns) and left (for rows)
                     f_lwd = 0,          # lwd of freq boxes: 0 (set to tiny_lwd, lty = 0) vs. 1 (numeric), or NULL/NA (set to 0).
                     # f_lty = 0,        # lty of freq boxes: 1 ("solid") vs. 0 ("blank"), etc. (currently not used)

                     gaps = c(NA, NA),   # c(v_gap, h_gap). Note: c(NA, NA) is changed to defaults: c(.02, 0) if p_split = "v"; c(0, .02) if p_split = "h".
                     brd_w = .10,        # border width: (default: brd_w = .25), setting brd_w = NULL/NA/<=0  hides top and left panels.

                     # Prob links:
                     p_lbl = NA,         # prob label: "default" vs. "no" vs. "abb"/"nam"/"num"/"namnum". (Set to NA/NULL to hide prob lines).
                     # p_lwd,            # lwd of prob links: set to default = 1 (currently not used)
                     # p_lty,            # lty of prob links: set to default = 1 (currently not used)
                     arr_c = -3,         # arrow code (-3 to +6): 0: no arrow, 1--3: V-shape, 4--6: T-shape, -1 to -3: point at ends.
                     col_p = c(grey(.15, .99), "yellow", "yellow"),  # colors for prob-links: use 1-3 bright colors (visible on SDT rectangles).
                     brd_dis = .30,      # distance of prob links from cell center. (Adjust to avoid overlapping labels).

                     # Text and color:
                     lbl_txt = txt,      # labels and text elements
                     title_lbl = txt$scen_lbl,  # main plot title
                     cex_lbl = .90,      # size of freq & text labels
                     cex_p_lbl = NA,     # size of prob labels (set to cex_lbl - .05 by default)
                     col_pal = pal,      # color palette

                     # Generic options:
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

  } # if (is_valid_prob_set)


  ## (1) Prepare parameters: ----------

  opar <- par(no.readonly = TRUE)  # copy of current settings
  on.exit(par(opar))  # par(opar)  # restore original settings

  ## (2) Key options and parameters: ----------

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
  if ( area == "square" || area == "mosaic" ) { area <- "sq" }
  # print(paste0("area = ", area))

  # use area input:
  if (area == "sq") {

    plot_xy <- dev.size("in")  # use par("pin") OR dev.size("in") to determine aspect ratio
    plot_ratio <- plot_xy[1]/plot_xy[2]  # current aspect ratio
    scale_x <- 1/plot_ratio              # multiplicative correction factor (for x-widths)

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
  v_gap_def <- .05
  h_gap_def <- .06

  # use current gap input:
  v_gap <- gaps[1]  # default: v_gap = NA
  h_gap <- gaps[2]  # default: h_gap = NA

  if (is.na(v_gap) || v_gap < 0 ) { # use default:
    v_gap <- v_gap_def
  }

  if (is.na(h_gap) || h_gap < 0 ) { # use default:
    h_gap <- h_gap_def
  }

  if ( h_gap > .20 ) {
    message("Horizontal gap (i.e., gaps[2]) should be in the range from 0 to .20.")
  }
  if ( v_gap > 1 ) {
    message("Vertical gap (i.e., gaps[1]) should be in the range from 0 to 1.")
  }

  # scale:
  if ( is.null(scale) || is.na(scale)  ) { scale <- "p" }  # default
  if (tolower(scale) == "freq" || tolower(scale) == "f") { scale <- "f" }
  if (tolower(scale) == "prob" || tolower(scale) == "p") { scale <- "p" }

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
    if (length(col_p) == 1) { col_p <- rep(col_p, 3) }  # 1 color: use for all 3 p-links
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

  # brd_w:
  if ( is.null(brd_w) || is.na(brd_w) ) { brd_w <- 0 }  # set to 0 (min)
  if ( brd_w > .5 ) { brd_w <- 0.5 }                    # set to 0.5 (max)


  ## 7. Additional parameters (currently fixed): ----

  lty <- 1  # default

  ## Cell width and height:
  sdt_lx <- 1
  sdt_ly <- 1

  # 3 columns (center points):
  c1_x <- 0 + sdt_lx/2
  c2_x <- sdt_lx + sdt_lx/2
  c3_x <- sdt_lx + v_gap + sdt_lx + sdt_lx/2  # v_gap between c2 and c3

  # 3 rows (center points):
  r3_y <- 0 + sdt_ly/2  # h_gap between r1 and r2
  r2_y <- sdt_ly + h_gap + sdt_ly/2
  r1_y <- sdt_ly + h_gap + sdt_ly + sdt_ly/2

  # Correction values (as constants):
  v_top   <- .03  # blank space on top of plotting area
  v_shift <- .04  # shifting vertical/rotated text labels (left)
  h_shift <- .01  # shifting horizontal text labels (up)
  # brd_dis <- .35  # shifting probability labels (down and/or to left/right side from cell center)

  # Frequency bars (on top and left):
  bar_lx <- sdt_lx  # brd_w/3            # bar widths (constant based on brd_w)
  tbar_y <- r3_y    # 1 + (.65 * brd_w)  # y-value of top  bar (constant based on brd_w)
  lbar_x <- c3_x    # 0 - (.60 * brd_w)  # x-value of left bar (constant based on brd_w)

  # # f_lbl_sum: freq labels of summary cells (bottom row and right column):
  # if ( is.null(f_lbl) || is.na(f_lbl) ) {
  #   f_lbl_sum <- f_lbl   # hide labels
  # } else {
  #   f_lbl_sum <- "num" # currently fixed (due to limited size), OR: set to f_lbl
  # }
  #
  # # f_lbl_hd : freq labels of headers (on top for columns and left for rows):
  # if ( is.null(f_lbl) || is.na(f_lbl) ) {
  #   f_lbl_hd  <- "nam"  # use full names
  # } else if (f_lbl == "namnum" || f_lbl == "nam") {
  #   f_lbl_hd  <- "nam"  # use full names
  # } else {
  #   f_lbl_hd  <- "abb"  # use abbreviated names
  # }


  ## (3) Define plot and margin areas: ----------

  ## Define margin areas:

  if (nchar(title_lbl) > 0) { n_lines_top <- 2 } else { n_lines_top <- 0 }
  if (mar_notes) { n_lines_bot <- 3 } else { n_lines_bot <- 0 }

  par(mar = c(n_lines_bot, 1, n_lines_top, 1) + 0.1)  # margins; default: par("mar") = 5.1 4.1 4.1 2.1.
  par(oma = c(0, 0, 0, 0) + 0.1)                      # outer margins; default: par("oma") = 0 0 0 0.

  ## Axis label locations:
  par(mgp = c(3, 1, 0)) # default: c(3, 1, 0)

  ## Orientation of the tick mark labels (and corresponding mtext captions below):
  par(las = 0)  # Options: parallel to the axis (0 = default), horizontal (1), perpendicular to axis (2), vertical (3).


  ## (4) Plot setup: ----------

  ## Plot dimensions:
  n_rows <- 3
  n_cols <- 3
  brd_w  <- brd_w * max(n_rows, n_cols)

  y_min <- 0

  if (!is.null(v_gap) && !is.na(v_gap) ) {
    x_max <- (n_rows + v_gap)
  } else {
    x_max <- n_rows
  }

  if (!is.null(brd_w) && !is.na(brd_w) ) {
    x_min <- (0 - brd_w)
    y_max <- (n_cols + h_gap) + (brd_w + v_top)
  } else {
    x_min <- 0
    y_max <- (n_cols + h_gap) + v_top
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
  #      col = grey(.75, .99), lty = 2,
  #      lwd = par("lwd"), equilogs = TRUE)


  ## (+) Mark plot areas: ------------

  ## (A) Mark plot area:
  # col.plot <- "firebrick3"
  # box("plot", col = col.plot)
  # text(x_max/2, y_max/2, "Plot area", col = col.plot, cex = 1, font = 2)  ## plot text

  ## (B) Mark margin area:
  # mar.col <- "forestgreen"
  # box("figure", col = mar.col)
  # mtext("Margin area", side = 3, line = 2, cex = 1, font = 2, col = mar.col)
  # mtext("side 1, line 3, adj 0", side = 1, line = 3, adj = 0.0, cex = cex_lbl, col = mar.col)
  # mtext("side 1, line 3, adj 1", side = 1, line = 3, adj = 1.0, cex = cex_lbl, col = mar.col)
  # mtext("side 3, line 0, adj 0", side = 3, line = 0, adj = 0.0, cex = cex_lbl, col = mar.col)
  # mtext("side 3, line 0, adj 1", side = 3, line = 0, adj = 1.0, cex = cex_lbl, col = mar.col)

  ## (C) Mark outer margin area (oma):
  # oma.col <- "steelblue4"
  # box("outer", col = oma.col)
  # mtext("Outer margin area", side = 1, line = 1, cex = 1, font = 2, col = oma.col, outer = TRUE)
  # mtext("side 1, line 0, adj 0", side = 1, line = 0, adj = 0.0, cex = cex_lbl, col = oma.col, outer = TRUE)
  # mtext("side 1, line 0, adj 1", side = 1, line = 0, adj = 1.0, cex = cex_lbl, col = oma.col, outer = TRUE)

  ## (3) Draw plot points: ----------

  ## Grid of points:
  # step_size <- 1
  # grid_x <- rep(seq(0, n_cols, by = step_size), each = (n_rows + 1))   # x/horizontal
  # grid_y <- rep(seq(0, n_rows, by = step_size), times = (n_cols + 1))  # y/vertical
  # points(grid_x, grid_y, pch = 3, col = grey(.66, .50), cex = 3/4)   # grid points

  # points(grid_x * scale_x, grid_y, pch = 3, col = grey(.66, .50), cex = 3/4)  # grid points (scaled)
  # points(0, 0, pch = 1, col = grey(.33, .50), cex = 1)                        # mark origin


  ## (5) Main: Custom table plot: -----------

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
      hi_lx <- sdt_lx  # v_split
      mi_lx <- sdt_lx  # v_split
      fa_lx <- sdt_lx  # (1 - v_split)
      cr_lx <- sdt_lx  # (1 - v_split)

      # heights (ly):
      hi_ly <- sdt_ly  # h_split_l
      mi_ly <- sdt_ly  # (1 - h_split_l)
      fa_ly <- sdt_ly  # (1 - h_split_r)
      cr_ly <- sdt_ly  # h_split_r

      # x-coordinates:
      hi_x <- c1_x  # hi_lx/2
      mi_x <- c1_x  # mi_lx/2
      fa_x <- c2_x  # hi_lx + fa_lx/2 + v_gap
      cr_x <- c2_x  # mi_lx + cr_lx/2 + v_gap

      # y-coordinates (left):
      mi_y <- r2_y  # mi_ly/2
      hi_y <- r1_y  # mi_ly + hi_ly/2 + h_gap

      if (by_bot == "dc") {

        #    cddc:      by cd:
        #               +: -:
        # by dc:  pos:  hi fa
        #         neg:  mi cr

        # y-coordinates (right):
        cr_y <- r2_y  # cr_ly/2
        fa_y <- r1_y  # cr_ly + fa_ly/2 + h_gap

      } else if (by_bot == "ac") {

        #    cdac:      by cd:
        #               +: -:
        # by ac:  cor:  hi cr
        #         err:  mi fa

        # y-coordinates (right):
        fa_y <- r2_y  # fa_ly/2
        cr_y <- r1_y  # fa_ly + cr_ly/2 + h_gap

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
        hi_ly <- sdt_ly  # h_split
        mi_ly <- sdt_ly  # (1 - h_split)
        fa_ly <- sdt_ly  # h_split
        cr_ly <- sdt_ly  # (1 - h_split)

        # widths (lx):
        hi_lx <- sdt_lx  # v_split_t
        mi_lx <- sdt_lx  # (1 - v_split_b)
        fa_lx <- sdt_lx  # (1 - v_split_t)
        cr_lx <- sdt_lx  # v_split_b

        # x-coordinates:
        hi_x <- hi_lx/2
        mi_x <- mi_lx/2
        fa_x <- hi_lx + fa_lx/2  # + v_gap
        cr_x <- mi_lx + cr_lx/2  # + v_gap

        # y-coordinates (left):
        mi_y <- r2_y  # mi_ly/2
        hi_y <- r1_y  # mi_ly + hi_ly/2 + h_gap

        # y-coordinates (right):
        cr_y <- r2_y  # cr_ly/2
        fa_y <- r1_y  # cr_ly + fa_ly/2 + h_gap

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
        hi_ly <- sdt_ly  # h_split
        mi_ly <- sdt_ly  # (1 - h_split)
        fa_ly <- sdt_ly  # (1 - h_split)
        cr_ly <- sdt_ly  # h_split

        # widths (lx):
        hi_lx <- sdt_lx  # v_split_t
        mi_lx <- sdt_lx  # (1 - v_split_b)
        fa_lx <- sdt_lx  # v_split_b
        cr_lx <- sdt_lx  # (1 - v_split_t)

        # x-coordinates:
        hi_x <- hi_lx/2
        mi_x <- mi_lx/2
        fa_x <- mi_lx + fa_lx/2  # + v_gap
        cr_x <- hi_lx + cr_lx/2  # + v_gap

        # y-coordinates (left):
        mi_y <- r2_y  # mi_ly/2
        hi_y <- r1_y  # mi_ly + hi_ly/2 + h_gap

        # y-coordinates (right):
        fa_y <- r2_y  # fa_ly/2
        cr_y <- r1_y  # fa_ly + cr_ly/2 + h_gap

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
      hi_lx <- sdt_lx  # v_split
      mi_lx <- sdt_lx  # (1 - v_split)
      fa_lx <- sdt_lx  # v_split
      cr_lx <- sdt_lx  # (1 - v_split)

      # heights (ly):
      hi_ly <- sdt_ly  # h_split_l
      mi_ly <- sdt_ly  # (1 - h_split_r)
      fa_ly <- sdt_ly  # (1 - h_split_l)
      cr_ly <- sdt_ly  # h_split_r

      # x-coordinates:
      hi_x <- hi_lx/2
      mi_x <- hi_lx + mi_lx/2  # + v_gap
      fa_x <- fa_lx/2
      cr_x <- fa_lx + cr_lx/2  # + v_gap

      # y-coordinates (left):
      fa_y <- r2_y  # fa_ly/2
      hi_y <- r1_y  # fa_ly + hi_ly/2 + h_gap

      if (by_bot == "cd") {

        #    dccd:       by dc:
        #                +: -:
        # by cd: true:   hi mi
        #        false:  fa cr

        # y-coordinates (right):
        cr_y <- r2_y  # cr_ly/2
        mi_y <- r1_y  # cr_ly + mi_ly/2 + h_gap

      } else if (by_bot == "ac") {

        #    dcac:      by dc:
        #               +: -:
        # by ac:  cor:  hi cr
        #         err:  fa mi

        # y-coordinates (right):
        mi_y <- r2_y  # mi_ly/2
        cr_y <- r1_y  # mi_ly + cr_ly/2 + h_gap

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
        hi_ly <- sdt_ly  # h_split
        mi_ly <- sdt_ly  # h_split
        fa_ly <- sdt_ly  # (1 - h_split)
        cr_ly <- sdt_ly  # (1 - h_split)

        # widths (lx):
        hi_lx <- sdt_lx  # v_split_t
        mi_lx <- sdt_lx  # (1 - v_split_t)
        fa_lx <- sdt_lx  # (1 - v_split_b)
        cr_lx <- sdt_lx  # v_split_b

        # x-coordinates:
        hi_x <- hi_lx/2
        fa_x <- fa_lx/2
        mi_x <- hi_lx + mi_lx/2  # + v_gap
        cr_x <- fa_lx + cr_lx/2  # + v_gap

        # y-coordinates (left):
        fa_y <- r2_y  # fa_ly/2
        hi_y <- r1_y  # fa_ly + hi_ly/2 + h_gap

        # y-coordinates (right):
        cr_y <- r2_y  # cr_ly/2
        mi_y <- r1_y  # cr_ly + mi_ly/2 + h_gap


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
        hi_ly <- sdt_ly  # h_split
        mi_ly <- sdt_ly  # (1 - h_split)
        fa_ly <- sdt_ly  # (1 - h_split)
        cr_ly <- sdt_ly  # h_split

        # widths (lx):
        hi_lx <- sdt_lx  # v_split_t
        mi_lx <- sdt_lx  # (1 - v_split_b)
        fa_lx <- sdt_lx  # v_split_b
        cr_lx <- sdt_lx  # (1 - v_split_t)

        # x-coordinates:
        hi_x <- hi_lx/2
        fa_x <- fa_lx/2
        cr_x <- hi_lx + cr_lx/2  # + v_gap
        mi_x <- fa_lx + mi_lx/2  # + v_gap

        # y-coordinates (left):
        fa_y <- r2_y  # fa_ly/2
        hi_y <- r1_y  # fa_ly + hi_ly/2 + h_gap

        # y-coordinates (right):
        mi_y <- r2_y  # mi_ly/2
        cr_y <- r1_y  # mi_ly + cr_ly/2 + h_gap

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
      hi_lx <- sdt_lx  # v_split
      mi_lx <- sdt_lx  # (1 - v_split)
      fa_lx <- sdt_lx  # (1 - v_split)
      cr_lx <- sdt_lx  # v_split

      # heights (ly):
      hi_ly <- sdt_ly  # h_split_l
      mi_ly <- sdt_ly  # (1 - h_split_r)
      fa_ly <- sdt_ly  # h_split_r
      cr_ly <- sdt_ly  # (1 - h_split_l)

      # x-coordinates:
      hi_x <- hi_lx/2
      cr_x <- cr_lx/2
      mi_x <- hi_lx + mi_lx/2  # + v_gap
      fa_x <- cr_lx + fa_lx/2  # + v_gap

      # y-coordinates (left):
      cr_y <- r2_y  # cr_ly/2
      hi_y <- r1_y  # cr_ly + hi_ly/2 + h_gap

      if (by_bot == "dc") {

        #    acdc:      by ac:
        #               +: -:
        # by dc:  pos:  hi fa
        #         neg:  cr mi

        # y-coordinates (right):
        mi_y <- r2_y  # mi_ly/2
        fa_y <- r1_y  # mi_ly + fa_ly/2 + h_gap

      } else if (by_bot == "cd") {

        #    accd:      by ac:
        #               +: -:
        # by cd: true:  hi mi
        #        false: cr fa

        # y-coordinates (right):
        fa_y <- r2_y  # fa_ly/2
        mi_y <- r1_y  # fa_ly + mi_ly/2 + h_gap

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
        hi_ly <- sdt_ly  # h_split
        mi_ly <- sdt_ly  # (1 - h_split)
        fa_ly <- sdt_ly  # h_split
        cr_ly <- sdt_ly  # (1 - h_split)

        # widths (lx):
        hi_lx <- sdt_lx  # v_split_t
        mi_lx <- sdt_lx  # (1 - v_split_b)
        fa_lx <- sdt_lx  # (1 - v_split_t)
        cr_lx <- sdt_lx  # v_split_b

        # x-coordinates:
        hi_x <- hi_lx/2
        cr_x <- cr_lx/2
        fa_x <- hi_lx + fa_lx/2  # + v_gap
        mi_x <- cr_lx + mi_lx/2  # + v_gap

        # y-coordinates (left):
        cr_y <- r2_y  # cr_ly/2
        hi_y <- r1_y  # cr_ly + hi_ly/2 + h_gap

        # y-coordinates (right):
        mi_y <- r2_y  # mi_ly/2
        fa_y <- r1_y  # mi_ly + fa_ly/2 + h_gap

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
        hi_ly <- sdt_ly  # h_split
        mi_ly <- sdt_ly  # h_split
        fa_ly <- sdt_ly  # (1 - h_split)
        cr_ly <- sdt_ly  # (1 - h_split)

        # widths (lx):
        hi_lx <- sdt_lx  # v_split_t
        mi_lx <- sdt_lx  # (1 - v_split_t)
        fa_lx <- sdt_lx  # (1 - v_split_b)
        cr_lx <- sdt_lx  # v_split_b

        # x-coordinates:
        hi_x <- hi_lx/2
        mi_x <- hi_lx + mi_lx/2  # + v_gap
        cr_x <- cr_lx/2
        fa_x <- cr_lx + fa_lx/2  # + v_gap

        # y-coordinates (left):
        cr_y <- r2_y  # cr_ly/2
        hi_y <- r1_y  # cr_ly + hi_ly/2 + h_gap

        # y-coordinates (right):
        fa_y <- r2_y  # fa_ly/2
        mi_y <- r1_y  # fa_ly + mi_ly/2 + h_gap

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

  ## NEW: Plot list of 4 boxes (by decreasing freq/prob to prevent occlusion of box labels):
  sdt_boxes <- list(box_hi, box_mi, box_fa, box_cr)  # list of boxes (lists)
  # plot_fbox_list(sdt_boxes, scale_lx = 1, lbl_type = f_lbl, cex = cex_lbl, lwd = f_lwd)  # plot list of fboxes

  # plot_fbox_list(sdt_boxes, scale_lx = scale_x,
  #               lbl_type = f_lbl, cex = cex_lbl, lwd = f_lwd)  # plot list of boxes (scaled)
  # plot_fbox_list(sdt_boxes, lbl_type = f_lbl, cex = cex_lbl, lwd = NULL, density = 10)  # plot list of boxes (bw version)

  plot_fbox_list(sdt_boxes,  # plot list of 4 sdt_boxes:
                 cur_freq = freq, lbl_txt = lbl_txt, col_pal = col_pal,  # PASS current freq/txt/pal arguments!
                 lbl_type = f_lbl, lbl_sep = f_lbl_sep, cex = cex_lbl, lwd = f_lwd, lty = lty)  # No ...!

  ##   (+) Check: Mark 2 key points/checkpoints (per plot): NOT RELEVANT for plot_tab! ------
  mark_key_points <- FALSE  # default
  mark_key_points <- TRUE   # debugging/test

  if (mark_key_points) {

    # (a) by condition:

    if (by == "cddc" && p_split == "v") {  # v01:
      points(prev * scale_x, (1 - sens + h_gap), pch = 0) # key point p1: hi
      points(((prev + v_gap) * scale_x), spec, pch = 5)   # key point p2: cr
    }

    if (by == "cdac" && p_split == "v") {  # v02:
      points(prev * scale_x, (1 - sens + h_gap), pch = 0)              # key point p1: hi
      points(((prev + v_gap) * scale_x), (1 - spec + h_gap), pch = 5)  # key point p2: cr
    }

    if (by == "cddc" && p_split == "h") {  # v03:
      points(PPV * scale_x, (1 - ppod + h_gap), pch = 0)         # key point p1: hi
      points(((1 - NPV + v_gap) * scale_x), (1 - ppod), pch = 5) # key point p2: cr
    }

    if (by == "cdac" && p_split == "h") {  # v04:
      points(p_acc_hi * scale_x, (1 - acc + h_gap), pch = 0)     # key point p1: hi
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

  } # if (mark_key_points)


  ##   (b) Plot 2 perspectives (compound frequencies and labels) on 2 sides: ----------

  # (A) Sums in bottom row (horizontal):

  if (by_top == "cd") {

    # (a) by condition (at top):
    # cond_y <- 1 + (.65 * brd_w)  # constant
    cond_true_x  <- c1_x  # prev/2
    cond_false_x <- c2_x  # prev + (1 - prev)/2 + v_gap

    if (brd_w > 0) {

      # Define 2 horizontal boxes (with optional scale_x):
      box_cond_true  <- make_box("cond_true",  (cond_true_x * scale_x),  tbar_y, (sdt_lx * scale_x), bar_lx)  # cond_true
      box_cond_false <- make_box("cond_false", (cond_false_x * scale_x), tbar_y, (sdt_lx * scale_x), bar_lx)  # cond_false

      fbox_r3 <- list(box_cond_true, box_cond_false)  # as list of boxes
      plot_fbox_list(fbox_r3, # plot list of boxes (in row 3):
                     cur_freq = freq, lbl_txt = lbl_txt, col_pal = col_pal,  # PASS current freq/txt/pal arguments!
                     lbl_type = f_lbl_sum, lbl_sep = f_lbl_sep,
                     cex = cex_lbl, lwd = f_lwd, lty = lty)  # No ...!

    } # if (brd_w > 0) etc.

  } else if (by_top == "dc") {

    # (b) by decision (at top):
    # dec_y <- 1 + (.65 * brd_w)  # constant
    dec_pos_x <- c1_x  # ppod/2
    dec_neg_x <- c2_x  # ppod + (1 - ppod)/2 + v_gap

    if (brd_w > 0) {

      # Define 2 horizontal boxes (with optional scale_x):
      box_dec_pos <- make_box("dec_pos", (dec_pos_x * scale_x), tbar_y, (sdt_lx * scale_x), bar_lx)  # dec_pos
      box_dec_neg <- make_box("dec_neg", (dec_neg_x * scale_x), tbar_y, (sdt_lx * scale_x), bar_lx)  # dec_neg

      fbox_r3 <- list(box_dec_pos, box_dec_neg)  # as list of boxes
      plot_fbox_list(fbox_r3, # plot list of boxes (in row 3):
                     cur_freq = freq, lbl_txt = lbl_txt, col_pal = col_pal,  # PASS current freq/txt/pal arguments!
                     lbl_type = f_lbl_sum, lbl_sep = f_lbl_sep,
                     cex = cex_lbl, lwd = f_lwd, lty = lty)  # No ...!

    } # if (brd_w > 0) etc.

  } else if (by_top == "ac") {

    # (c) by accuracy (at top):
    dec_cor_x <- c1_x  # acc/2
    dec_err_x <- c2_x  # acc + (1 - acc)/2 + v_gap

    if (brd_w > 0) {

      # Define 2 horizontal boxes (with optional scale_x):
      box_dec_cor <- make_box("dec_cor", (dec_cor_x * scale_x), tbar_y, (sdt_lx * scale_x), bar_lx)  # dec_cor
      box_dec_err <- make_box("dec_err", (dec_err_x * scale_x), tbar_y, (sdt_lx * scale_x), bar_lx)  # dec_err

      fbox_r3 <- list(box_dec_cor, box_dec_err)  # as list of boxes
      plot_fbox_list(fbox_r3, # plot list of boxes (in row 3):
                     cur_freq = freq, lbl_txt = lbl_txt, col_pal = col_pal,  # PASS current freq/txt/pal arguments!
                     lbl_type = f_lbl_sum, lbl_sep = f_lbl_sep,
                     cex = cex_lbl, lwd = f_lwd, lty = lty)  # No ...!

    } # if (brd_w > 0) etc.

  } else {

    message("Plotting bottom fboxes (row 3): Unknown by_top argument/perspective!")

  } # if (by_top == etc.)


  ## (B) Sums in right column (col 3, vertical):

  if (by_bot == "cd") {

    # (a) by condition (col 3, on right):
    # cond_x <- 0 - (.60 * brd_w) # constant
    cond_false_y  <- r2_y  # (1 - prev)/2
    cond_true_y   <- r1_y  # (1 - prev) + prev/2 + h_gap

    if (brd_w > 0) {

      # Define 2 vertical boxes (withOUT scale_x):
      box_cond_true  <- make_box("cond_true",  (lbar_x * scale_x), cond_true_y,  (bar_lx * scale_x), sdt_ly)  # cond_true
      box_cond_false <- make_box("cond_false", (lbar_x * scale_x), cond_false_y, (bar_lx * scale_x), sdt_ly)  # cond_false

      fbox_c3 <- list(box_cond_true, box_cond_false)  # as list of boxes
      plot_fbox_list(fbox_c3, # plot list of boxes (in col 3):
                     cur_freq = freq, lbl_txt = lbl_txt, col_pal = col_pal,  # PASS current freq/txt/pal arguments!
                     lbl_type = f_lbl_sum, lbl_sep = f_lbl_sep,
                     cex = cex_lbl, lwd = f_lwd, lty = lty)  # No ...!


    } # if (brd_w > 0) etc.

  } else if (by_bot == "dc") {

    # (b) by decision (on left):
    # dec_x <- 0 - (.60 * brd_w) # constant
    dec_neg_y <- r2_y  # (1 - ppod)/2
    dec_pos_y <- r1_y  # (1 - ppod) + ppod/2  + h_gap

    if (brd_w > 0) {

      # Define 2 vertical boxes (withOUT scale_x):
      box_dec_pos <- make_box("dec_pos", (lbar_x * scale_x), dec_pos_y, (bar_lx * scale_x), sdt_ly)  # dec_pos
      box_dec_neg <- make_box("dec_neg", (lbar_x * scale_x), dec_neg_y, (bar_lx * scale_x), sdt_ly)  # dec_neg

      fbox_c3 <- list(box_dec_pos, box_dec_neg)  # as list of boxes
      plot_fbox_list(fbox_c3, # plot list of boxes (in col 3):
                     cur_freq = freq, lbl_txt = lbl_txt, col_pal = col_pal,  # PASS current freq/txt/pal arguments!
                     lbl_type = f_lbl_sum, lbl_sep = f_lbl_sep,
                     cex = cex_lbl, lwd = f_lwd, lty = lty)  # No ...!

    } # if (brd_w > 0) etc.

  } else if (by_bot == "ac") {

    # (c) by accuracy (on left):
    # acc_x <- 0 - (.60 * brd_w) # constant
    dec_err_y <- r2_y  # (1 - acc)/2                # OR (rounded freq):
    dec_cor_y <- r1_y  # (1 - acc) + acc/2 + h_gap  # OR (rounded freq): (freq$dec_cor/freq$N * 1/2)

    if (brd_w > 0) {

      # Define 2 vertical boxes (withOUT scale_x):
      box_dec_cor <- make_box("dec_cor", (lbar_x * scale_x), dec_cor_y, (bar_lx * scale_x), sdt_ly)  # dec_cor
      box_dec_err <- make_box("dec_err", (lbar_x * scale_x), dec_err_y, (bar_lx * scale_x), sdt_ly)  # dec_err

      fbox_c3 <- list(box_dec_cor, box_dec_err)  # as list of boxes
      plot_fbox_list(fbox_c3, # plot list of boxes (in col 3):
                     cur_freq = freq, lbl_txt = lbl_txt, col_pal = col_pal,  # PASS current freq/txt/pal arguments!
                     lbl_type = f_lbl_sum, lbl_sep = f_lbl_sep,
                     cex = cex_lbl, lwd = f_lwd, lty = lty)  # No ...!

    } # if (brd_w > 0) etc.

  } else {

    message("Plotting right fboxes (col 3): Unknown by_bot argument/perspective!")

  } # if (by_bot == etc.)

  ##   (c) Plot N (population frequency) as box: --------

  box_N <- make_box("N", (lbar_x * scale_x), r3_y, (bar_lx * scale_x), sdt_ly)  # N
  fbox_N <- list(box_N)  # as list of boxes
  plot_fbox_list(fbox_N, # plot only 1 box (in row 3, col 3):
                 cur_freq = freq, lbl_txt = lbl_txt, col_pal = col_pal,  # PASS current freq/txt/pal arguments!
                 lbl_type = f_lbl, lbl_sep = f_lbl_sep,
                 cex = cex_lbl, lwd = f_lwd, lty = lty)  # No ...!


  ##   (d) Plot key probabilities (as lines/arrows): ----------

  if ( !is.null(p_lbl) && !is.na(p_lbl) ) {

    if (by_top == "cd") {

      ## (a) by condition:

      if (p_split == "v") { # v01:

        # constants:
        prev_y <- (1 - brd_dis + h_gap)              # horizontal
        sens_x <- ((0 + brd_dis) * scale_x)          # vertical
        spec_x <- ((1 - brd_dis + v_gap) * scale_x)  # vertical

        # prev [horizontal r3: from cond_true (r3c1) to N (r3c3)]:
        plot_line(# 0, prev_y, (prev * scale_x), prev_y,
          (c1_x * scale_x), (r3_y - brd_dis),  (c3_x * scale_x), (r3_y - brd_dis),
          arr_code = arr_c,
          lbl = label_prob("prev", cur_prob = prob, lbl_type = p_lbl),
          col_fill = p_col_1, col_txt = p_col_1,
          lbl_pos = 3, lbl_off = .33, cex = cex_p_lbl, ...)  # Allow ...!

        # sens [vertical c1: from hi to cond_true)]:
        plot_line(# sens_x, (1 - sens + h_gap), sens_x, (1 + h_gap),
          ((c1_x - brd_dis) * scale_x), (r1_y),  ((c1_x - brd_dis) * scale_x), (r3_y),
          arr_code = arr_c,
          lbl = label_prob("sens", cur_prob = prob, lbl_type = p_lbl),
          col_fill = p_col_2, col_txt = p_col_2,
          srt = 90, lbl_pos = 3, lbl_off = 0, cex = cex_p_lbl, ...)  # Allow ...!

        if (by_bot == "dc") { # v01:

          # spec [vertical c2: from cr to cond_false)]:
          plot_line(# spec_x, 0, spec_x, spec,
            ((c2_x - brd_dis) * scale_x), (r2_y),  ((c2_x - brd_dis) * scale_x), (r3_y),
            arr_code = arr_c,
            lbl = label_prob("spec", cur_prob = prob, lbl_type = p_lbl),
            col_fill = p_col_3, col_txt = p_col_3,
            srt = 90, lbl_pos = 3, lbl_off = 0, cex = cex_p_lbl, ...)  # Allow ...!

        } else if (by_bot == "ac") { # v02:

          # spec [vertical c2: from cr to cond_false)]:
          plot_line(#spec_x, (1 - spec + h_gap), spec_x, (1 + h_gap),
            ((c2_x - brd_dis) * scale_x), (r1_y),  ((c2_x - brd_dis) * scale_x), (r3_y),
            arr_code = arr_c,
            lbl = label_prob("spec", cur_prob = prob, lbl_type = p_lbl),
            col_fill = p_col_3, col_txt = p_col_3,
            srt = 90, lbl_pos = 3, lbl_off = 0, cex = cex_p_lbl, ...)  # Allow ...!

        } # if (by_bot == etc.)

      } else if (p_split == "h") {

        if (by_bot == "dc") { # v03:

          # constants:
          ppod_x <- ((0 + brd_dis) * scale_x)  # vertical (inside)
          PPV_y <- (1 - brd_dis + h_gap)       # horizontal
          NPV_y <- (0 + brd_dis)               # horizontal

          # ppod [vertical, right (c3)]:
          plot_line(#ppod_x, (1 - ppod + h_gap), ppod_x, (1 + h_gap),  # ppod
            ((c3_x + brd_dis) * scale_x), (r1_y),  ((c3_x + brd_dis) * scale_x), (r3_y),
            arr_code = arr_c,
            lbl = label_prob("ppod", cur_prob = prob, lbl_type = p_lbl),
            col_fill = p_col_1, col_txt = p_col_1,
            srt = 90, lbl_pos = 3, lbl_off = 0, cex = cex_p_lbl, ...)  # Allow ...!

          # ppod_x <- ((0 - brd_w * .90) * scale_x)        # vertical (out left)
          # ppod_x <- ((1 + v_gap + brd_dis) * scale_x)    # vertical (out right)
          # plot_line(ppod_x, (1 - ppod + h_gap), ppod_x, (1 + h_gap),  # ppod
          #           arr_code = arr_c, lbl = label_prob("ppod", lbl_type = p_lbl),
          #           col_fill = "black", col_txt = "black",  # WAS: p_col_1
          #           srt = 90, lbl_pos = 3, lbl_off = 0, cex = cex_p_lbl)

          # PPV (horizontal, top r1): hi-dec_pos
          plot_line(#0, PPV_y, (PPV * scale_x), PPV_y,
            ((c1_x) * scale_x), (r1_y - brd_dis), ((c3_x) * scale_x), (r1_y - brd_dis),
            arr_code = arr_c,
            lbl = label_prob("PPV", cur_prob = prob, lbl_type = p_lbl),
            col_fill = p_col_2, col_txt = p_col_2,
            lbl_pos = 3, lbl_off = .33, cex = cex_p_lbl, ...)  # Allow ...!

          # NPV (horizontal, middle r2): cr-dec_neg
          plot_line(#((1 - NPV + v_gap) * scale_x), NPV_y, ((1 + v_gap) * scale_x), NPV_y,
            ((c2_x) * scale_x), (r2_y - brd_dis), ((c3_x) * scale_x), (r2_y - brd_dis),
            arr_code = arr_c,
            lbl = label_prob("NPV", cur_prob = prob, lbl_type = p_lbl),
            col_fill = p_col_3, col_txt = p_col_3,
            lbl_pos = 3, lbl_off = .33, cex = cex_p_lbl, ...)  # Allow ...!

        } else if (by_bot == "ac") { # v04:

          # constants:
          acc_x <- ((0 + brd_dis) * scale_x)  # vertical (inside)
          acc_hi_y <- (1 - brd_dis + h_gap)   # horizontal
          err_fa_y <- (0 + brd_dis)           # horizontal

          # acc (vertical, right c3): dec_cor-N
          plot_line(# acc_x, (1 - acc + h_gap), acc_x, (1 + h_gap),
            ((c3_x + brd_dis) * scale_x), (r1_y),  ((c3_x + brd_dis) * scale_x), (r3_y),
            arr_code = arr_c,
            lbl = label_prob("acc", cur_prob = prob, lbl_type = p_lbl),
            col_fill = p_col_1, col_txt = p_col_1,
            srt = 90, lbl_pos = 3, lbl_off = 0, cex = cex_p_lbl, ...)  # Allow ...!

          # p_acc_hi (horizontal, top r1): hi-dec_cor
          plot_line(#0, acc_hi_y, (p_acc_hi * scale_x), acc_hi_y,
            ((c1_x) * scale_x), (r1_y - brd_dis), ((c3_x) * scale_x), (r1_y - brd_dis),
            arr_code = arr_c,
            lbl = label_prob("acc_hi", cur_prob = prob, lbl_type = p_lbl),
            col_fill = p_col_2, col_txt = p_col_2,
            lbl_pos = 3, lbl_off = .33, cex = cex_p_lbl, ...)  # Allow ...!

          # p_err_fa (horizontal, middle r2): fa!
          plot_line(#((1 - p_err_fa + v_gap) * scale_x), err_fa_y, ((1 + v_gap) * scale_x), err_fa_y,
            ((c2_x) * scale_x), (r2_y - brd_dis), ((c3_x) * scale_x), (r2_y - brd_dis),
            arr_code = arr_c,
            lbl = label_prob("err_fa", cur_prob = prob, lbl_type = p_lbl),
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

        # ppod (horizontal, r3):
        plot_line(#0, ppod_y, (ppod * scale_x), ppod_y,
          (c1_x * scale_x), (r3_y - brd_dis),  (c3_x * scale_x), (r3_y - brd_dis),
          arr_code = arr_c,
          lbl = label_prob("ppod", cur_prob = prob, lbl_type = p_lbl),
          col_fill = p_col_1, col_txt = p_col_1,
          lbl_pos = 3, lbl_off = .33, cex = cex_p_lbl, ...)  # Allow ...!

        # PPV (vertical, c1):
        plot_line(#PPV_x, (1 - PPV + h_gap), PPV_x, (1 + h_gap),
          ((c1_x - brd_dis) * scale_x), (r1_y),  ((c1_x - brd_dis) * scale_x), (r3_y),
          arr_code = arr_c,
          lbl = label_prob("PPV", cur_prob = prob, lbl_type = p_lbl),
          col_fill = p_col_2, col_txt = p_col_2,
          srt = 90, lbl_pos = 3, lbl_off = 0, cex = cex_p_lbl, ...)  # Allow ...!

        if (by_bot == "cd") { # v05:

          # NPV (vertical, c2 a):
          plot_line(#NPV_x, 0, NPV_x, NPV,
            ((c2_x - brd_dis) * scale_x), (r2_y),  ((c2_x - brd_dis) * scale_x), (r3_y),
            arr_code = arr_c,
            lbl = label_prob("NPV", cur_prob = prob, lbl_type = p_lbl),
            col_fill = p_col_3, col_txt = p_col_3,
            srt = 90, lbl_pos = 3, lbl_off = 0, cex = cex_p_lbl, ...)  # Allow ...!

        } else if (by_bot == "ac") { # v06:

          # NPV (vertical, c2 b):
          plot_line(#NPV_x, (1 - NPV + h_gap), NPV_x, (1 + h_gap),
            ((c2_x - brd_dis) * scale_x), (r1_y),  ((c2_x - brd_dis) * scale_x), (r3_y),
            arr_code = arr_c,
            lbl = label_prob("NPV", cur_prob = prob, lbl_type = p_lbl),
            col_fill = p_col_3, col_txt = p_col_3,
            srt = 90, lbl_pos = 3, lbl_off = 0, cex = cex_p_lbl, ...)  # Allow ...!

        } # if (by_bot == etc.)


      } else if (p_split == "h") {

        if (by_bot == "cd") { # v07:

          # constants:
          prev_x <- ((0 + brd_dis) * scale_x)  # vertical (inside)
          sens_y <- (1 - brd_dis + h_gap)      # horizontal
          spec_y <- (0 + brd_dis)              # horizontal

          # prev (vertical, right c3):
          plot_line(#prev_x, (1 - prev + h_gap), prev_x, (1 + h_gap),
            ((c3_x + brd_dis) * scale_x), (r1_y),  ((c3_x + brd_dis) * scale_x), (r3_y),
            arr_code = arr_c,
            lbl = label_prob("prev", cur_prob = prob, lbl_type = p_lbl),
            col_fill = p_col_1, col_txt = p_col_1,
            srt = 90, lbl_pos = 3, lbl_off = 0, cex = cex_p_lbl, ...)  # Allow ...!

          # sens (horizontal, top r1): hi
          plot_line(#0, sens_y, (sens * scale_x), sens_y,
            ((c1_x) * scale_x), (r1_y - brd_dis), ((c3_x) * scale_x), (r1_y - brd_dis),
            arr_code = arr_c,
            lbl = label_prob("sens", cur_prob = prob, lbl_type = p_lbl),
            col_fill = p_col_2, col_txt = p_col_2,
            lbl_pos = 3, lbl_off = .33, cex = cex_p_lbl, ...)  # Allow ...!

          # spec (horizontal, middle r2): cr
          plot_line(#((1 - spec + v_gap) * scale_x), spec_y, ((1 + v_gap) * scale_x), spec_y,
            ((c2_x) * scale_x), (r2_y - brd_dis), ((c3_x) * scale_x), (r2_y - brd_dis),
            arr_code = arr_c,
            lbl = label_prob("spec", cur_prob = prob, lbl_type = p_lbl),
            col_fill = p_col_3, col_txt = p_col_3,
            lbl_pos = 3, lbl_off = .33, cex = cex_p_lbl, ...)  # Allow ...!

        } else if (by_bot == "ac") { # v08:

          # constants:
          acc_x <- ((0 + brd_dis) * scale_x)  # vertical (inside)
          acc_hi_y <- (1 - brd_dis + h_gap)   # horizontal
          err_fa_y <- (0 + brd_dis)           # horizontal

          # acc (vertical, right c3):
          plot_line(#acc_x, (1 - acc + h_gap), acc_x, (1 + h_gap),
            ((c3_x + brd_dis) * scale_x), (r1_y),  ((c3_x + brd_dis) * scale_x), (r3_y),
            arr_code = arr_c,
            lbl = label_prob("acc", cur_prob = prob, lbl_type = p_lbl),
            col_fill = p_col_1, col_txt = p_col_1,
            srt = 90, lbl_pos = 3, lbl_off = 0, cex = cex_p_lbl, ...)  # Allow ...!

          # p_acc_hi (horizontal, top r1):
          plot_line(#0, acc_hi_y, (p_acc_hi * scale_x), acc_hi_y,
            ((c1_x) * scale_x), (r1_y - brd_dis), ((c3_x) * scale_x), (r1_y - brd_dis),
            arr_code = arr_c,
            lbl = label_prob("acc_hi", cur_prob = prob, lbl_type = p_lbl),
            col_fill = p_col_2, col_txt = p_col_2,
            lbl_pos = 3, lbl_off = .33, cex = cex_p_lbl, ...)  # Allow ...!

          # p_err_fa (horizontal, middle r2): fa!
          plot_line(#0, err_fa_y, (p_err_fa * scale_x), err_fa_y,
            ((c1_x) * scale_x), (r2_y - brd_dis), ((c3_x) * scale_x), (r2_y - brd_dis),
            arr_code = arr_c,
            lbl = label_prob("err_fa", cur_prob = prob, lbl_type = p_lbl),
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

        # acc (horizontal, bottom r3):
        plot_line(#0, acc_y, (acc * scale_x), acc_y,
          ((c1_x) * scale_x), (r3_y - brd_dis), ((c3_x) * scale_x), (r3_y - brd_dis),
          arr_code = arr_c,
          lbl = label_prob("acc", cur_prob = prob, lbl_type = p_lbl),
          col_fill = p_col_1, col_txt = p_col_1,
          lbl_pos = 3, lbl_off = .33, cex = cex_p_lbl, ...)  # Allow ...!

        # p_acc_hi (vertical, c1):
        plot_line(#acc_hi_x, (1 - p_acc_hi + h_gap), acc_hi_x, (1 + h_gap),
          ((c1_x - brd_dis) * scale_x), (r1_y),  ((c1_x - brd_dis) * scale_x), (r3_y),
          arr_code = arr_c,
          lbl = label_prob("acc_hi", cur_prob = prob, lbl_type = p_lbl),
          col_fill = p_col_2, col_txt = p_col_2,
          srt = 90, lbl_pos = 3, lbl_off = 0, cex = cex_p_lbl, ...)  # Allow ...!

        if (by_bot == "cd") { # v09:

          # p_err_fa (vertical, c2 a): fa!
          plot_line(#err_fa_x, 0, err_fa_x, p_err_fa,
            ((c2_x - brd_dis) * scale_x), (r2_y),  ((c2_x - brd_dis) * scale_x), (r3_y),
            arr_code = arr_c,
            lbl = label_prob("err_fa", cur_prob = prob, lbl_type = p_lbl),
            col_fill = p_col_3, col_txt = p_col_3,
            srt = 90, lbl_pos = 3, lbl_off = 0, cex = cex_p_lbl, ...)  # Allow ...!

        } else if (by_bot == "dc") { # v10:

          # p_err_fa (vertical, c2 b): fa!
          plot_line(# err_fa_x, (1 - p_err_fa + h_gap), err_fa_x, (1 + h_gap),
            ((c2_x - brd_dis) * scale_x), (r1_y),  ((c2_x - brd_dis) * scale_x), (r3_y),
            arr_code = arr_c,
            lbl = label_prob("err_fa", cur_prob = prob, lbl_type = p_lbl),
            col_fill = p_col_3, col_txt = p_col_3,
            srt = 90, lbl_pos = 3, lbl_off = 0, cex = cex_p_lbl, ...)  # Allow ...!

        } # if (by_bot == etc.)

      } else if (p_split == "h") {

        if (by_bot == "cd") { # v11:

          # constants:
          prev_x <- ((0 + brd_dis) * scale_x)  # vertical (inside)
          sens_y <- (1 - brd_dis + h_gap)       # horizontal
          spec_y <- (0 + brd_dis)               # horizontal

          # prev (vertical, right c3): hi
          plot_line(#prev_x, (1 - prev + h_gap), prev_x, (1 + h_gap),
            ((c3_x + brd_dis) * scale_x), (r1_y),  ((c3_x + brd_dis) * scale_x), (r3_y),
            arr_code = arr_c,
            lbl = label_prob("prev", cur_prob = prob, lbl_type = p_lbl),
            col_fill = p_col_1, col_txt = p_col_1,
            srt = 90, lbl_pos = 3, lbl_off = 0, cex = cex_p_lbl, ...)  # Allow ...!

          # sens (horizontal, top r1): hi
          plot_line(#0, sens_y, (sens * scale_x), sens_y,
            ((c1_x) * scale_x), (r1_y - brd_dis), ((c3_x) * scale_x), (r1_y - brd_dis),
            arr_code = arr_c,
            lbl = label_prob("sens", cur_prob = prob, lbl_type = p_lbl),
            col_fill = p_col_2, col_txt = p_col_2,
            lbl_pos = 3, lbl_off = .33, cex = cex_p_lbl, ...)  # Allow ...!

          # spec (horizontal, middle r2): cr
          plot_line(#0, spec_y, (spec * scale_x), spec_y,
            ((c1_x) * scale_x), (r2_y - brd_dis), ((c3_x) * scale_x), (r2_y - brd_dis),
            arr_code = arr_c,
            lbl = label_prob("spec", cur_prob = prob, lbl_type = p_lbl),
            col_fill = p_col_3, col_txt = p_col_3,
            lbl_pos = 3, lbl_off = .33, cex = cex_p_lbl, ...)  # Allow ...!

        } else if (by_bot == "dc") { # v12:

          # constants:
          ppod_x <- ((0 + brd_dis) * scale_x)  # vertical (inside)
          PPV_y <- (1 - brd_dis + h_gap)   # horizontal
          NPV_y <- (0 + brd_dis)           # horizontal

          # ppod (vertical, right c3):
          plot_line(#ppod_x, (1 - ppod + h_gap), ppod_x, (1 + h_gap),
            ((c3_x + brd_dis) * scale_x), (r1_y),  ((c3_x + brd_dis) * scale_x), (r3_y),
            arr_code = arr_c,
            lbl = label_prob("ppod", cur_prob = prob, lbl_type = p_lbl),
            col_fill = p_col_1, col_txt = p_col_1,
            srt = 90, lbl_pos = 3, lbl_off = 0, cex = cex_p_lbl, ...)  # Allow ...!

          # PPV (horizontal, top r1): hi
          plot_line(#0, PPV_y, (PPV * scale_x), PPV_y,
            ((c1_x) * scale_x), (r1_y - brd_dis), ((c3_x) * scale_x), (r1_y - brd_dis),
            arr_code = arr_c,
            lbl = label_prob("PPV", cur_prob = prob, lbl_type = p_lbl),
            col_fill = p_col_2, col_txt = p_col_2,
            lbl_pos = 3, lbl_off = .33, cex = cex_p_lbl, ...)  # Allow ...!

          # NPV (horizontal, middle r2): cr
          plot_line(#0, NPV_y, (NPV * scale_x), NPV_y,
            ((c1_x) * scale_x), (r2_y - brd_dis), ((c3_x) * scale_x), (r2_y - brd_dis),
            arr_code = arr_c,
            lbl = label_prob("NPV", cur_prob = prob, lbl_type = p_lbl),
            col_fill = p_col_3, col_txt = p_col_3,
            lbl_pos = 3, lbl_off = .33, cex = cex_p_lbl, ...)  # Allow ...!

        } # if (by_bot == etc.)

      } # if (p_split == etc.)

    } else {

      message("Plotting p-lines: Unknown by argument/perspective!")

    } # if (by_top == etc.)

  } # if ( !is.null(p_lbl) && !is.na(p_lbl) )

  ##   (e) Plot text labels (for 2 perspectives) on 2 sides: ----------

  ## (A) top labels (horizontal):

  if (by_top == "cd") {

    ## (a) by condition:

    if (brd_w > 0) {

      # ftype label: Condition (center, horizontal)
      plot_ftype_label("cond_true", (sdt_lx * scale_x), (n_rows + brd_w),
                       lbl_txt = lbl_txt, suffix = ":", pos = NULL,
                       col = col_pal["txt"], cex = cex_lbl, ...)  # Allow ...!

    } # if (brd_w > 0)

    # 2 sub-group labels:
    plot_freq_label("cond_true", (cond_true_x * scale_x), (n_rows + h_gap + h_shift),
                    lbl_type = f_lbl_hd, lbl_sep = f_lbl_sep,
                    cur_freq = freq, lbl_txt = lbl_txt,
                    suffix = ":", pos = 3, col = col_pal["txt"], cex = cex_lbl)
    plot_freq_label("cond_false", (cond_false_x * scale_x), (n_rows + h_gap + h_shift),
                    lbl_type = f_lbl_hd, lbl_sep = f_lbl_sep,
                    cur_freq = freq, lbl_txt = lbl_txt,
                    suffix = ":", pos = 3, col = col_pal["txt"], cex = cex_lbl)

  } else if (by_top == "dc") {

    ## (b) by decision:

    if (brd_w > 0) {

      # ftype label: Decision
      plot_ftype_label("dec_pos", (sdt_lx * scale_x), (n_rows + brd_w),
                       lbl_txt = lbl_txt, suffix = ":", pos = NULL,
                       col = col_pal["txt"], cex = cex_lbl, ...)  # Allow ...!

    } # if (brd_w > 0)

    # 2 sub-group labels:
    plot_freq_label("dec_pos", (dec_pos_x * scale_x), (n_rows + h_gap + h_shift),
                    lbl_type = f_lbl_hd, lbl_sep = f_lbl_sep,
                    cur_freq = freq, lbl_txt = lbl_txt,
                    suffix = ":", pos = 3, col = col_pal["txt"], cex = cex_lbl)
    plot_freq_label("dec_neg", (dec_neg_x * scale_x), (n_rows + h_gap + h_shift),
                    lbl_type = f_lbl_hd, lbl_sep = f_lbl_sep,
                    cur_freq = freq, lbl_txt = lbl_txt,
                    suffix = ":", pos = 3, col = col_pal["txt"], cex = cex_lbl)


  } else if (by_top == "ac") {

    ## (c) by accuracy:

    if (brd_w > 0) {

      # ftype label: Accuracy
      plot_ftype_label("dec_cor", (sdt_lx * scale_x), (n_rows + brd_w),
                       lbl_txt = lbl_txt, suffix = ":", pos = NULL,
                       col = col_pal["txt"], cex = cex_lbl, ...)  # Allow ...!

    } # if (brd_w > 0) etc.

    # 2 sub-group labels:
    plot_freq_label("dec_cor", (dec_cor_x * scale_x), (n_rows + h_gap + h_shift),
                    lbl_type = f_lbl_hd, lbl_sep = f_lbl_sep,
                    cur_freq = freq, lbl_txt = lbl_txt,
                    suffix = ":", pos = 3, col = col_pal["txt"], cex = cex_lbl)
    plot_freq_label("dec_err", (dec_err_x * scale_x), (n_rows + h_gap + h_shift),
                    lbl_type = f_lbl_hd, lbl_sep = f_lbl_sep,
                    cur_freq = freq, lbl_txt = lbl_txt,
                    suffix = ":", pos = 3, col = col_pal["txt"], cex = cex_lbl)


  } else {

    message("Plotting top labels: Unknown by_top argument/perspective!")

  } # if (by_top == etc.)


  ## (B) bottom/left labels (vertical):

  if (by_bot == "cd") {

    # (a) by condition:

    if (brd_w > 0) {

      ## ftype label: Condition
      plot_ftype_label("cond_true", ((0 - brd_w) * scale_x), (r1_y + r2_y)/2, lbl_txt = lbl_txt, suffix = ":",
                       srt = 90, pos = 3, col = col_pal["txt"], cex = cex_lbl, ...)  # Allow ...!

    } # if (brd_w > 0) etc.

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

    if (brd_w > 0) {

      ## ftype label: Decision
      plot_ftype_label("dec_pos", ((0 - brd_w) * scale_x), (r1_y + r2_y)/2, lbl_txt = lbl_txt, suffix = ":",
                       srt = 90, pos = 3, col = col_pal["txt"], cex = cex_lbl, ...)  # Allow ...!

    } # if (brd_w > 0) etc.

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

    if (brd_w > 0) {

      ## ftype label: Accuracy
      plot_ftype_label("dec_cor", ((0 - brd_w) * scale_x), (r1_y + r2_y)/2, lbl_txt = lbl_txt, suffix = ":",
                       srt = 90, pos = 3, col = col_pal["txt"], cex = cex_lbl, ...)  # Allow ...!

    } # if (brd_w > 0) etc.

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

  ##   Sum labels:
  sum_lbl <- paste0(lbl["sum_lbl"], ":")

  text((c3_x * scale_x), (n_rows + h_gap + h_shift),  # top: right column (c3):
       labels = sum_lbl,
       xpd = TRUE,    # NA...plotting clipped to device region; T...figure region; F...plot region
       pos = 3, col = col_pal["txt"], cex = cex_lbl)

  text((0 * scale_x), (r3_y),  # left: bottom row (r3):
       labels = sum_lbl,
       xpd = TRUE,    # NA...plotting clipped to device region; T...figure region; F...plot region
       pos = 2, col = col_pal["txt"], cex = cex_lbl)


  # box_else <- make_box("else_box", 9, -2, b_w, b_h)  # define some arbitrary box
  # plot(box_else, col = "firebrick1", cex = 1/2, font = 2)     # plot box

  ## (5) Title: ------

  # Define parts:
  if (nchar(title_lbl) > 0) { title_lbl <- paste0(title_lbl, ":\n") }  # put on top (in separate line)

  if (title_lbl == "") {  # if title has been set to "":
    type_lbl <- ""        # assume that no subtitle is desired either
  } else {
    type_lbl <- paste0(lbl["plot_tab_lbl"], " (by ", as.character(by), ")")  # plot name: Table/Contingency table/etc.
  }

  # Compose label:
  cur_title_lbl <- paste0(title_lbl, type_lbl)

  # Plot title:
  title(cur_title_lbl, adj = 0, line = 0, font.main = 1, cex.main = 1.2)  # (left, not raised, normal font)


  ## (6) Margins: ------

  if (mar_notes) {

    # Note:
    note_lbl <- ""  # initialize

    if (scale == "f") {
      if (round == TRUE) {
        note_lbl <- "Probabilities are based on rounded frequencies."
      } else {
        note_lbl <- "Probabilities are based on exact frequencies."
      }
    }

    plot_mar(show_freq = TRUE, show_cond = TRUE, show_dec = TRUE,
             show_accu = TRUE, accu_from_freq = FALSE,
             note = note_lbl,
             cur_freq = freq, cur_prob = prob, lbl_txt = lbl_txt)

  } # if (mar_notes) etc.


  ## Finish: ---------

  # on.exit(par(opar))  # par(opar)  # restore original settings
  invisible() # restores par(opar)

} # plot_tab end.


## (3) Check: ------

# ## Basics:
# plot_tab()
# plot_tab(area = "sq", f_lwd = 3)
# plot_tab(f_lbl = "namnum", f_lbl_sep = ":\n", brd_w = .10, f_lwd = .5)
#
# ## Computing local freq and prob values:
# plot_tab(prev = .5, sens = 4/5, spec = 3/5, N = 10, f_lwd = 1)
#
# ## Plot versions:
# # by x p_split [yields (3 x 2) x 2] = 12 versions]:
# plot_tab(by = "cddc", p_split = "v", p_lbl = "def")  # v01 (see v07)
# plot_tab(by = "cdac", p_split = "v", p_lbl = "def")  # v02 (see v11)
# plot_tab(by = "cddc", p_split = "h", p_lbl = "def")  # v03 (see v05)
# plot_tab(by = "cdac", p_split = "h", p_lbl = "def")  # v04 (see v09)
#
# plot_tab(by = "dccd", p_split = "v", p_lbl = "def")  # v05 (is v03 rotated)
# plot_tab(by = "dcac", p_split = "v", p_lbl = "def")  # v06 (see v12)
# plot_tab(by = "dccd", p_split = "h", p_lbl = "def")  # v07 (is v01 rotated)
# plot_tab(by = "dcac", p_split = "h", p_lbl = "def")  # v08 (see v10)
#
# plot_tab(by = "accd", p_split = "v", p_lbl = "def")  # v09 (is v04 rotated)
# plot_tab(by = "acdc", p_split = "v", p_lbl = "def")  # v10 (is v08 rotated)
# plot_tab(by = "accd", p_split = "h", p_lbl = "def")  # v11 (is v02 rotated)
# plot_tab(by = "acdc", p_split = "h", p_lbl = "def")  # v12 (is v06 rotated)
#
#
# ## Explore labels and links:
# plot_tab(f_lbl = "abb", p_lbl = NA)  # abbreviated labels, no probability links
# plot_tab(f_lbl = "num", f_lbl_sum = "abb", p_lbl = "num", f_lbl_hd = "abb")
# plot_tab(f_lbl = "abb", f_lbl_sum = "abb", p_lbl = "nam", f_lbl_hd = "nam")
# plot_tab(f_lbl = "namnum", f_lbl_sep = ":\n", f_lbl_sum = "namnum", f_lbl_hd = "nam", p_lbl = "namnum")
#
# ## Misc. options:
# plot_tab(area = "sq")        # area: square
# plot_tab(title_lbl = "")     # no titles
# plot_tab(mar_notes = FALSE)  # no margin notes
#
# plot_tab(by = "cddc", gaps = c(.08, .00), area = "sq")    # gaps
# plot_tab(by = "cddc", gaps = c(.02, .08), p_split = "h")  # gaps
#
# # Showing prob as lines:
# plot_tab(prev = 1/4, sens = 6/7, spec = 3/5, N = 100,
#          by = "cddc", p_split = "v", col_pal = pal_rgb,
#          p_lbl = "def", brd_dis = .25, arr_c = -3)
# plot_tab(prev = 1/3, sens = 6/7, spec = 3/4, N = 100, scale = "f",
#          by = "cddc", p_split = "h", col_pal = pal_rgb,
#          p_lbl = "namnum", brd_dis = .15, arr_c = +3)
#
# ## Custom text labels and colors:
# plot_tab(prev = .5, sens = 4/5, spec = 3/5, N = 10,
#          by = "cddc", p_split = "v", area = "sq",
#          lbl_txt = txt_org,  # custom text
#          f_lbl = "namnum", f_lbl_sep = ":\n", f_lbl_sum = "num", f_lbl_hd  = "nam",
#          col_pal = pal_org, f_lwd = 1)  # custom colors
#
# plot_tab(prev = .5, sens = 3/5, spec = 4/5, N = 10,
#          by = "cddc", p_split = "h", area = "sq",
#          lbl_txt = txt_org,  # custom text
#          f_lbl = "namnum", f_lbl_sep = ":\n", f_lbl_sum = "num", f_lbl_hd  = "nam",
#          col_pal = pal_kn, f_lwd = 1)  # custom colors
#
# ## Note some differences to plot_area (area/mosaic plot):
# #
# # In plot_tab:
# #
# # (1) p_split does not matter (except for selecting different prob links):
# plot_tab(by = "cddc", p_split = "v")  # v01 (see v07)
# plot_tab(by = "cddc", p_split = "h")  # v03 (see v05)
# #
# # (2) scale does not matter for dimensions (which are constant),
# #     but for values shown in prob links and on margins:
# plot_tab(by = "cddc", scale = "p", p_lbl = "def", round = TRUE)  # (a) prob show exact probabilities
# plot_tab(by = "cddc", scale = "f", p_lbl = "def", round = TRUE)  # (b) re-compute prob from rounded freq!
# plot_tab(by = "cddc", scale = "f", p_lbl = "def", round = FALSE) # (c) same as (a)


## (+) ToDo: ------

## - Allow showing more than 1 set of probabilities at once.

## - Shorten and simplify code:
#    a. specify perspectives => position of 4 table cells
#    b. specify probabilities (lines) to be shown

## (*) Done: ------

## - Make a function of 3 essential probabilities (generating local freq and prob),
##   and pass local txt and pal arguments (to allow changes when calling function).
## - Add probability link options for perspectives beyond "cd.."
## - Use scale == "f" to re-compute prob values from (rounded or non-rounded) freq.
## - Set sensible defaults.
## - Add documentation and integrate in riskyr package  [2018 10 24].

## eof. ----------
