## plot_fn.R | riskyr
## 2021 01 03
## Plot frequency net from Binder et al. (2020):
## See doi: 10.3389/fpsyg.2020.00750
## -----------------------------------------------

# This function complements plot_prism.R and the older functions
# - plot_tree.R: plot single tree
# - plot_fnet.R: plot double tree
# (and removes dependency on 'diagram' pkg).

## plot_fn: Documentation ----------

#' Plot frequency net diagram of frequencies and probabilities.
#'
#' \code{plot_fn} plots a frequency net of
#' from a sufficient and valid set of 3 essential probabilities
#' (\code{\link{prev}}, and
#' \code{\link{sens}} or its complement \code{\link{mirt}}, and
#' \code{\link{spec}} or its complement \code{\link{fart}})
#' or existing frequency information \code{\link{freq}}
#' and a population size of \code{\link{N}} individuals.
#'
#' \code{plot_fn} generalizes and replaces \code{\link{plot_fnet}}
#' by removing the dependency on the R package \code{diagram}
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
#' Note that a population size \code{\link{N}} is not needed
#' for computing current probability information \code{\link{prob}},
#' but is needed for computing frequency information
#' \code{\link{freq}} from current probabilities \code{\link{prob}}.
#'
#' @param by  A character code specifying 1 or 2 perspective(s)
#' that split(s) the population into 2 subsets.
#' Specifying 1 perspective plots a frequency tree (single tree)
#' with 3 options:
#'   \enumerate{
#'   \item \code{"cd"}: by condition only;
#'   \item \code{"dc"}: by decision only;
#'   \item \code{"ac"}: by accuracy only.
#'   }
#' Specifying 2 perspectives plots a frequency prism (double tree)
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
#' @param area  A character code specifying the shapes of the frequency boxes,
#' with 2 options:
#'   \enumerate{
#'   \item \code{"no"}: rectangular frequency boxes, not scaled (default);
#'   \item \code{"sq"}: frequency boxes are squares (scaled relative to N).
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
#' @param round  Boolean option specifying whether computed frequencies
#' are rounded to integers. Default: \code{round = TRUE}.
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
#' @param f_lwd  Line width of areas.
#' Default: \code{f_lwd = 0}.
#'
#' @param p_lwd  Line width of probability links.
#' Default: \code{p_lwd = 1}, but consider increasing when setting \code{p_scale = TRUE}.
#'
#' @param p_scale  Boolean option for scaling current widths of probability links
#' (as set by \code{p_lwd}) by the current probability values.
#' Default: \code{p_scale = FALSE}.
#'
#' @param p_lbl  Type of label for showing 3 key probability links and values,
#' with many options:
#'   \enumerate{
#'   \item \code{"abb"}: show links and abbreviated probability names;
#'   \item \code{"def"}: show links and abbreviated probability names and values;
#'   \item \code{"min"}: show links and minimum (prominent) probability names;
#'   \item \code{"mix"}: show links and prominent probability names and all values (default);
#'   \item \code{"nam"}: show links and probability names (as specified in code);
#'   \item \code{"num"}: show links and numeric probability values;
#'   \item \code{"namnum"}: show links with names and numeric probability values;
#'   \item \code{"no"}: show links with no labels (same for \code{p_lbl = NA} or \code{NULL}).
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
#' Default: \code{arr_c = NA}, but adjusted by \code{area}.
#'
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
#' Default: \code{mar_notes = FALSE}.
#'
#' @param ...  Other (graphical) parameters.
#'
#' @return Nothing (NULL).
#'
#' @examples
#' ## Basics:
#' # (1) Using global prob and freq values:
#' plot_fn()  # default frequency net,
#' # same as:
#' # plot_fn(by = "cddc", area = "no", scale = "p",
#' #            f_lbl = "num", f_lwd = 0, cex_lbl = .90,
#' #            p_lbl = "mix", arr_c = -2, cex_p_lbl = NA)
#'
#' # (2) Providing values:
#' plot_fn(N = 10000, prev = .02, sens = .8, spec = .9) # Binder et al. (2020, Fig. 3)
#'
#' # Variants:
#' plot_fn(N = 10000, prev = .02, sens = .8, spec = .9, by = "cdac")
#' plot_fn(N = 10000, prev = .02, sens = .8, spec = .9, by = "dccd")
#' plot_fn(N = 10000, prev = .02, sens = .8, spec = .9, by = "dcac")
#' plot_fn(N = 10000, prev = .02, sens = .8, spec = .9, by = "accd")
#' plot_fn(N = 10000, prev = .02, sens = .8, spec = .9, by = "acdc")
#'
#' # Area:
#' plot_fn(N = 10, prev = 1/4, sens = 3/5, spec = 2/5, area = "sq", mar_notes = TRUE)
#'
#' ## Custom color and text settings:
#' plot_fn(col_pal = pal_bw, f_lwd = .5, p_lwd = .5, lty = 2, # custom fbox color, prob links,
#'            font = 3, cex_p_lbl = .75)                         # and text labels
#'
#' my_txt <- init_txt(cond_lbl = "The Truth", cond_true_lbl = "so true", cond_false_lbl = "so false",
#'                    hi_lbl = "TP", mi_lbl = "FN", fa_lbl = "FP", cr_lbl = "TN")
#' my_col <- init_pal(N_col = rgb(0, 169, 224, max = 255),  # seeblau
#'                    hi_col = "gold", mi_col = "firebrick1", fa_col = "firebrick2", cr_col = "orange")
#' plot_fn(f_lbl = "nam", lbl_txt = my_txt,
#'            col_pal = my_col, f_lwd = .5)
#'
#' ## Local values and custom color/txt settings:
#' plot_fn(N = 7, prev = 1/2, sens = 3/5, spec = 4/5, round = FALSE,
#'            by = "cdac", lbl_txt = txt_org, f_lbl = "namnum", f_lbl_sep = ":\n",
#'            f_lwd = 1, col_pal = pal_rgb)  # custom colors
#'
#' plot_fn(N = 5, prev = 1/2, sens = .8, spec = .5, scale = "p",  # note scale!
#'            by = "cddc", area = "hr", col_pal = pal_bw, f_lwd = 1) # custom colors
#'
#' plot_fn(N = 3, prev = .50, sens = .50, spec = .50, scale = "p",                 # note scale!
#'            area = "sq", lbl_txt = txt_org, f_lbl = "namnum", f_lbl_sep = ":\n", # custom text
#'            col_pal = pal_kn, f_lwd = .5)                                        # custom colors
#'
#' ## Plot versions:
#' # (A) tree/single tree (nchar(by) == 2):
#' #     3 versions:
#' plot_fn(by = "cd", f_lbl = "def", col_pal = pal_mod) # by condition (freq boxes: hi mi fa cr)
#' plot_fn(by = "dc", f_lbl = "def", col_pal = pal_mod) # by decision  (freq boxes: hi fa mi cr)
#' plot_fn(by = "ac", f_lbl = "def", col_pal = pal_mod) # by accuracy  (freq boxes: hi cr fa mi)
#'
#' # (B) prism/double tree (nchar(by) == 4):
#' #     6 (3 x 2) versions (+ 3 redundant ones):
#' plot_fn(by = "cddc")  # v01 (default)
#' plot_fn(by = "cdac")  # v02
#' plot_fn(by = "cdcd")  # (+) Message
#'
#' plot_fn(by = "dccd")  # v03
#' plot_fn(by = "dcac")  # v04
#' plot_fn(by = "dcdc")  # (+) Message
#'
#' plot_fn(by = "accd")  # v05
#' plot_fn(by = "acdc")  # v06
#' plot_fn(by = "acac")  # (+) Message
#'
#' ## Other options:
#'
#' # area:
#' plot_fn(area = "no")  # rectangular boxes (default): (same if area = NA/NULL)
#' plot_fn(area = "sq")  # squares (areas on each level sum to N)
#'
#' # scale (matters for scaled areas and small N):
#' plot_fn(N = 4, prev = .2, sens = .7, spec = .8,
#'            area = "sq", scale = "p")  # areas scaled by prob
#' plot_fn(N = 4, prev = .2, sens = .7, spec = .8,
#'            area = "sq", scale = "f")  # areas scaled by (rounded or non-rounded) freq
#'
#' ## Frequency boxes:
#'
#' # f_lbl:
#' plot_fn(f_lbl = "abb")     # abbreviated freq names (variable names)
#' plot_fn(f_lbl = "nam")     # only freq names
#' plot_fn(f_lbl = "num")     # only numeric freq values (default)
#' plot_fn(f_lbl = "namnum")  # names and numeric freq values
#' plot_fn(f_lbl = "namnum", cex_lbl = .75)  # smaller freq labels
#' plot_fn(f_lbl = NA)        # no freq labels
#' plot_fn(f_lbl = "def")     # informative default: short name and numeric value (abb = num)
#'
#' # f_lwd:
#' plot_fn(f_lwd =  0)  # no lines (default), set to tiny_lwd = .001, lty = 0 (same if NA/NULL)
#' plot_fn(f_lwd =  1)  # basic lines
#' plot_fn(f_lwd =  3)  # thicker lines
#' plot_fn(f_lwd = .5)  # thinner lines
#'
#' ## Probability links:
#'
#' # Scale link widths (p_lwd & p_scale):
#' plot_fn(p_lwd = 6, p_scale = TRUE)
#' plot_fn(area = "sq", f_lbl = "num", p_lbl = NA, col_pal = pal_bw, p_lwd = 6, p_scale = TRUE)
#' plot_fn(area = "hr", f_lbl = "num", f_lwd = .5, p_lbl = NA, arr_c = 0,
#'            col_pal = pal_mod, p_lwd = 6, p_scale = TRUE)
#'
#' # p_lbl:
#' plot_fn(p_lbl = "mix")     # abbreviated names with numeric values (abb = num)
#' plot_fn(p_lbl = "min")     # minimal names (of key probabilities)
#' plot_fn(p_lbl = NA)        # no prob labels (NA/NULL/"none")
#' plot_fn(p_lbl = "nam")     # only prob names
#' plot_fn(p_lbl = "num")     # only numeric prob values
#' plot_fn(p_lbl = "namnum")  # names and numeric prob values
#' # plot_fn(p_lbl = "namnum", cex_p_lbl = .70)  # smaller prob labels
#' # plot_fn(by = "cddc", p_lbl = "min")  # minimal labels
#' # plot_fn(by = "cdac", p_lbl = "min")
#' # plot_fn(by = "cddc", p_lbl = "mix")  # mix abbreviated names and numeric values
#' # plot_fn(by = "cdac", p_lbl = "mix")
#' # plot_fn(by = "cddc", p_lbl = "abb")  # abbreviated names
#' # plot_fn(by = "cdac", p_lbl = "abb")
#' # plot_fn(p_lbl = "any") # short name and value (abb = num)
#'
#' # arr_c:
#' plot_fn(arr_c =  0)  # acc_c = 0: no arrows
#' plot_fn(arr_c = -3)  # arr_c = -1 to -3: points at both ends
#' plot_fn(arr_c = -2)  # point at far end
#' plot_fn(arr_c = +2)  # crr_c = 1-3: V-shape arrows at far end
#' # plot_fn(arr_c = +3)  # V-shape arrows at both ends
#' # plot_fn(arr_c = +6)  # arr_c = 4-6: T-shape arrows
#'
#' ## Plain plot versions:
#' plot_fn(area = "no", f_lbl = "def", p_lbl = "num", col_pal = pal_mod, f_lwd = 1,
#'            title_lbl = "", mar_notes = FALSE)  # remove titles and margin notes
#' plot_fn(area = "no", f_lbl = "nam", p_lbl = "min", col_pal = pal_rgb)
#' plot_fn(area = "no", f_lbl = "num", p_lbl = "num", col_pal = pal_kn)
#'
#' plot_fn(area = "hr", f_lbl = "nam", f_lwd = .5, p_lwd = .5, col_pal = pal_bwp)
#' plot_fn(area = "hr", f_lbl = "nam", f_lwd = .5, p_lbl = "num")
#'
#' plot_fn(area = "sq", f_lbl = "nam", p_lbl = NA, col_pal = pal_rgb)
#' plot_fn(area = "sq", f_lbl = "def", f_lbl_sep = ":\n", p_lbl = NA, f_lwd = 1, col_pal = pal_kn)
#'
#' ## Suggested combinations:
#' plot_fn(f_lbl = "nam", p_lbl = "mix", col_pal = pal_mod)  # basic plot
#' plot_fn(f_lbl = "namnum", p_lbl = "num", cex_lbl = .80, cex_p_lbl = .75)
#' plot_fn(area = "no", f_lbl = "def", p_lbl = "abb",           # def/abb labels
#'            f_lwd = .8, p_lwd = .8, lty = 3, col_pal = pal_bwp)  # black-&-white
#'
#' plot_fn(area = "sq", f_lbl = "nam", p_lbl = "abb", lbl_txt = txt_TF)
#' plot_fn(area = "sq", f_lbl = "num", p_lbl = "num", f_lwd = 1, col_pal = pal_rgb)
#' plot_fn(area = "sq", f_lbl = "namnum", p_lbl = "mix", f_lwd = .5, col_pal = pal_kn)
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
#' \code{\link{plot_prism}} for plotting prism plot (double tree);
#' \code{\link{plot_area}} for plotting mosaic plot (scaling area dimensions);
#' \code{\link{plot_bar}} for plotting frequencies as vertical bars;
#' \code{\link{plot_tab}} for plotting table (without scaling area dimensions);
#' \code{\link{pal}} contains current color settings;
#' \code{\link{txt}} contains current text settings.
#'
#' @export

## plot_fn: Definition ----------

plot_fn <- function(prev = num$prev,    # probabilities
                    sens = num$sens, mirt = NA,
                    spec = num$spec, fart = NA,
                    N = num$N,          # population size N

                    # Plot options:
                    by = "cddc",        # 2 perspectives (rows 2 and 4): each by = "cd"/"dc"/"ac"  (default: "cddc")
                    area = "no",        # "no" (default = NA, NULL, "fix") vs: "sq"
                    scale = "p",        # "f" vs. "p" (default)
                    round = TRUE,       # round freq to integers? (default: round = TRUE), when not rounded: n_digits = 2 (currently fixed).

                    # Freq boxes:
                    f_lbl = "num",      # freq labels: "def", "nam"/"num"/"namnum", "abb", or NA/NULL/"no" to hide freq labels.
                    f_lbl_sep = NA,     # freq label separator (default: " = ", use ":\n" to add an extra line break)
                    f_lwd = 0,          # lwd of freq boxes: 0 (set to tiny_lwd, lty = 0) vs. 1 (numeric), or NULL/NA (set to 0).
                    # f_lty = 0,        # lty of freq boxes: 1 ("solid") vs. 0 ("blank"), etc. (currently not used)

                    # Prob links:
                    p_lwd = 1,          # lwd of prob links: Default p_lwd = 1 (and used as p_lwd_max when p_scale = TRUE).
                    p_scale = FALSE,    # scale widths of probability links (set by p_lwd) by current p_val?
                    p_lbl = "mix",      # prob labels: "def", "nam"/"num"/"namnum", "abb"/"mix"/"min", or NA/NULL/"no" to hide prob labels.
                    # p_lty,            # lty of prob links: set to default = 1 (currently not used)
                    arr_c = NA,         # arrow code (-3 to +6). Set to defaults of -2 or 0 (by area, below).

                    # Text and color:
                    lbl_txt = txt,      # labels and text elements
                    title_lbl = txt$scen_lbl,  # main plot title
                    cex_lbl = .90,      # size of freq & text labels.
                    cex_p_lbl = NA,     # size of prob labels (set to cex_lbl - .05 by default).
                    col_pal = pal,      # color palette

                    # Generic options:
                    mar_notes = FALSE,  # show margin notes?
                    ...                 # other (graphical) parameters (passed to plot_link and plot_ftype_label)
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

    message("No valid set of probabilities provided: Using global freq & prob to plot prism.")

  } # if (is_valid_prob_set)


  ## (1) Prepare parameters: ----------

  opar <- par(no.readonly = TRUE)  # all par settings that can be changed.
  on.exit(par(opar))  # par(opar)  # restore original settings

  ## (2) Key options and parameters: ----------

  ## 1. by Perspective: ----

  # by:
  by_vec <- read_by(by = by) # helper function returns vector with 3 elements:
  by_top <- by_vec[1]
  by_bot <- by_vec[2]
  by     <- by_vec[3]  # updates original by to (possibly changed) by_now


  ## 2. Freq boxes: ----

  # area:
  if (is.null(area) || is.na(area) || tolower(area) == "none" || tolower(area) == "fix") { area <- "no" }
  if ( !is.null(area) && !is.na(area) ) { area <- tolower(area) }  # area in lowercase
  if ( area == "hr" || area == "vr" || area == "horizontal" || area == "hrect" || area == "hbar" || area == "h" || area == "bar" || area == "bars" ) { area <- "no" }
  if ( area == "square" || area == "squares" ) { area <- "sq" }

  # scale:
  if ( is.null(scale) || is.na(scale)  ) { scale <- "f" }  # default
  if ( !is.null(scale) && !is.na(scale) ) { scale <- tolower(scale) }  # scale in lowercase
  if (scale == "freq" || scale == "f") { scale <- "f" }
  if (scale == "prob" || scale == "p") { scale <- "p" }

  # use scale input:
  if (scale == "f") {

    ## (A) Use scale for area dimensions:
    ## Recompute specific probabilities from current (4 essential) freq
    ## which may be rounded or not rounded:
    prob_from_freq <- comp_prob_freq(hi = freq$hi, mi = freq$mi, fa = freq$fa, cr = freq$cr)

    ## Adjusting width of boxes in comp_lx_fbox (by scale argument) below!

    ## (B) Use scale for area dimensions AND prob values:
    ## A more radical type of scale (i.e., re-defining prob based on current freq)
    ## also changes the prob values displayed in links and margins:
    prob <- prob_from_freq  # use re-computed values for all prob values!

  }

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

  # arr_c:
  # Note that arr_c <- NA by default:

  if ( is.null(arr_c) ) { arr_c <- 0 }  # sensible zero

  # sensible default (based on p_scale):
  if (p_scale && is.na(arr_c)) { arr_c <- 0 }  # default for p_scale (unless arr_c was set)

  # sensible defaults (based on area):
  if ( is.na(arr_c) ) {
    if (area == "no") { arr_c <- -2 }  # point at far end
    if (area == "hr") { arr_c <- -2 }  # point at far end
    if (area == "sq") { arr_c <-  0 }  # no arrows
  }


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
    # lty <- 1
  }


  ## 6. Additional parameters (currently fixed): ----

  lty <- 1
  ftype_x <- -5.5  # x-value of ftype labels
  ftype_pos <- 4   # pos of ftype labels (NULL: centered, 2: right justified, or 4: left justified)


  ## (3) Define plot and margin areas: ----------

  ## (A) Define margin areas:

  if (nchar(title_lbl) > 0) { n_lines_top <- 2 } else { n_lines_top <- 0 }
  if (mar_notes) { n_lines_bot <- 3 } else { n_lines_bot <- 0 }

  par(mar = c(n_lines_bot, 1, n_lines_top, 1) + 0.1)  # margins; default: par("mar") = 5.1 4.1 4.1 2.1.
  par(oma = c(0, 0, 0, 0) + 0.1)                      # outer margins; default: par("oma") = 0 0 0 0.

  ## Axis label locations:
  par(mgp = c(3, 1, 0)) # default: c(3, 1, 0)

  ## Orientation of the tick mark labels (and corresponding mtext captions below):
  par(las = 0)  # Options: parallel to the axis (0 = default), horizontal (1), perpendicular to the axis (2), vertical (3).


  ## (B) Plot setup:

  # Levels to plot with objects:
  y_levels <- 5
  x_levels <- 9

  x_ctr <- 0  # vertical middle / center
  y_ctr <- 0  # horizontal middle / center

  # Dimensions:
  x_min <- -5
  x_max <- +5

  if ( !is.na(by_bot) ) {
    y_min <- -5
  } else { # is.na(by_bot):
    y_min <- -5  # WAS: -1 (in plot_prism)
  } # if ( !is.na(by_bot) ) etc.
  y_max <- +5

  # Plot empty canvas:
  plot(0:1, 0:1, type = "n",
       xlab = "", ylab = "",
       xlim = c(x_min, x_max), ylim = c(y_min, y_max),
       axes = FALSE)

  ## Plot empty canvas:
  # plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n",
  #     xlab = "", ylab = "", xlim = c(0, x_max), ylim = c(0, y_max))

  ## Plot with points:
  # plot(rep(0:x_max, y_max + 1), rep(0:y_max, each = x_max + 1),
  #     xlab = "x-axis", ylab = "y-axis")


  ## (C) Mark plot areas:

  ## (a) Mark plot area:
  # col.plot <- "firebrick3"
  # box("plot", col = col.plot)
  # text(x_max/2, y_max/2, "Plot area", col = col.plot, cex = 1, font = 2)  ## plot text

  ## (b) Mark margin area:
  # mar.col <- "forestgreen"
  # box("figure", col = mar.col)
  # mtext("Margin area", side = 3, line = 2, cex = 1, font = 2, col = mar.col)
  # mtext("side 1, line 3, adj 0", side = 1, line = 3, adj = 0.0, cex = cex_lbl, col = mar.col)
  # mtext("side 1, line 3, adj 1", side = 1, line = 3, adj = 1.0, cex = cex_lbl, col = mar.col)
  # mtext("side 3, line 0, adj 0", side = 3, line = 0, adj = 0.0, cex = cex_lbl, col = mar.col)
  # mtext("side 3, line 0, adj 1", side = 3, line = 0, adj = 1.0, cex = cex_lbl, col = mar.col)

  ## (c) Mark outer margin area (oma):
  # oma.col <- "steelblue4"
  # box("outer", col = oma.col)
  # mtext("Outer margin area", side = 1, line = 1, cex = 1, font = 2, col = oma.col, outer = TRUE)
  # mtext("side 1, line 0, adj 0", side = 1, line = 0, adj = 0.0, cex = cex_lbl, col = oma.col, outer = TRUE)
  # mtext("side 1, line 0, adj 1", side = 1, line = 0, adj = 1.0, cex = cex_lbl, col = oma.col, outer = TRUE)

  ## (d) Draw a grid of plot points:
  # points(0, 0, pch = 1, col = grey(.66, .50), cex = 1)  # mark origin

  ## Plot grid of points:
  # grid_x <- rep(seq(x_min, x_max, by = 1), times = length(seq(y_min, y_max, by = 1))) # x/horizontal
  # grid_y <- rep(seq(y_min, y_max, by = 1), each =  length(seq(x_min, x_max, by = 1))) # y/vertical
  # points(grid_x, grid_y, pch = 3, col = grey(.66, .50), cex = 3/4)                    # plot grid points


  ## (4) Define graphical parameters: --------

  ## (A) Aspect ratio of current plot:
  plot_xy <- par("pin")  # use par("pin") OR dev.size("in")
  plot_ratio <- plot_xy[1]/plot_xy[2]  # current aspect ratio
  scale_x <- 1/plot_ratio              # multiplicative correction factor (for x-widths)


  ## (B) Box parameters:

  ##     Box dimensions: ------

  ## Box areas with fixed size:

  ## (a) rectangular box (area == "no", i.e., default):
  # if ( !is.na(by_bot) ) {

  b_h_scale <- 1.15       # optional scaling factor (for larger box heights)
  b_h <- (1 * b_h_scale)  # basic box height

  # gold_ratio  <- 1.618  # a. golden ratio (= approx. 1.6180339887)
  wide_screen <- 16/9     # b. 1.778
  # compromise  <- 1.70   # c. 1.70
  # wider       <- 1.88   # d. 1.88 (wider than wide_screen)

  # b_w <- comp_lx(b_h, mf = gold_ratio, corf = scale_x)  # a. gold_ratio + corrected for aspect ratio
  b_w <- comp_lx(b_h, mf = wide_screen, corf = scale_x)   # b. wide_screen + corrected
  # b_w <- comp_lx(b_h, mf = compromise, corf = scale_x)  # c. compromise + corrected
  # b_w <- comp_lx(b_h, mf = wider, corf = scale_x)       # d. wider + corrected

  # b_w <- comp_lx(b_h, mf = 2, corf = scale_x)   # x. customized width


  # } # else {

  # b_h <- 1
  # two_to_one <- 2.0

  # b_w <- comp_lx(b_h, mf = two_to_one, corf = scale_x)  # a. two_to_one + corrected for aspect ratio
  # b_w <- comp_lx(b_h, mf = 3.0, corf = scale_x)         # x. customized width (+++ here now +++)

  # } # if ( !is.na(by_bot) ) etc.


  ## (b) Square box:
  if (area == "sq") {

    corr_3 <- 1.00

    ## Scale correction factor for showing 3 (single tree) instead of 5 levels (prism, double tree):
    # if ( !is.na(by_bot) ) { corr_3 <- 1.00 } else { corr_3 <- 1.60 } # if ( !is.na(by_bot) ) etc.

    b_w <- comp_lx(b_h, mf = corr_3, corf = scale_x)  # same as b_h + corrected for aspect ratio
  }

  ## (B) Other graphical parameters:

  ## Dimensions:
  # x_range <- x_max - 2  # range in x direction
  # t_w <- x_range + b_w  # total width = range + width of center box


  ## (5) Main fnet diagram: --------

  ## (A) Define and plot objects: ------

  # Row-by-row strategy:
  # For each row (y):
  # - Determine desired perspective (by "xx") of current row.
  # - For each perspective:
  #   - define all boxes of current row (dimensions => x-pos)
  #   - plot all boxes of current row (from largest to smallest)


  ## Population size N: Center (formerly 1st and 5th rows, top/bot: y = +4/-4): ------

  # box 1 dimensions:

  if (area == "sq") {

    # N area as square:
    N_scale <- 3/2  # optional scaling factor (for larger N squares)
    N_l     <- b_h * N_scale
    N_area  <- N_l^2

    box_1_h <- N_l
    box_1_w <- comp_lx(box_1_h, mf = corr_3, corf = scale_x)  # same, but corrected for aspect ratio

  } else { # default: area == "no" and all others:

    # N area fixed:
    box_1_w <- b_w
    box_1_h <- b_h

  }

  # box 1:
  box_1_x <-  0  # center
  box_1_y <-  0  # center
  box_1 <- make_box("N", box_1_x, box_1_y, box_1_w, box_1_h)

  # plot(box_1, lbl_type = "namnum", cex = cex_lbl, lwd = f_lwd, lbl_sep = ":\nN = ")  # N (top)
  row_1_boxes <- list(box_1)  # list of boxes (lists)

  ## plot label:
  # plot_ftype_label("N", ftype_x, box_1_y, lbl_txt = lbl_txt, suffix = ":", pos = ftype_pos, col = pal["txt"], cex = cex_lbl, ...)  # Allow ...!

  # plot list of fboxes:
  # plot_fbox_list(row_1_boxes, lbl_type = f_lbl, cex = cex_lbl, lwd = f_lwd)  # plot list of boxes


  ## CELLS in 4 corners (formerly 3rd row, y = 0, center): SDT cases/cells as 4 boxes: ------

  # Cell positions (in 4 corners):
  x_left  <- -3
  x_right <- +3
  y_bot   <- -4
  y_top   <- +4

  # dimensions lx:

  if (area == "sq") { # Scale SDT case/cell areas as squares:

    # Compute ly for current scale:
    hi_ly <- comp_ly_fsqr("hi", area_N = N_area, cur_freq = freq, cur_prob = prob, scale = scale)
    mi_ly <- comp_ly_fsqr("mi", area_N = N_area, cur_freq = freq, cur_prob = prob, scale = scale)
    fa_ly <- comp_ly_fsqr("fa", area_N = N_area, cur_freq = freq, cur_prob = prob, scale = scale)
    cr_ly <- comp_ly_fsqr("cr", area_N = N_area, cur_freq = freq, cur_prob = prob, scale = scale)

    # Compute lx corresponding to ly:
    hi_lx <- comp_lx(hi_ly, mf = corr_3, corf = scale_x)  # same, but corrected for aspect ratio
    mi_lx <- comp_lx(mi_ly, mf = corr_3, corf = scale_x)  # same, but corrected for aspect ratio
    fa_lx <- comp_lx(fa_ly, mf = corr_3, corf = scale_x)  # same, but corrected for aspect ratio
    cr_lx <- comp_lx(cr_ly, mf = corr_3, corf = scale_x)  # same, but corrected for aspect ratio

  } else { # default: area == "no" and all others:

    # Set lx: fixed & all equal:
    hi_lx <- b_w
    mi_lx <- b_w
    fa_lx <- b_w
    cr_lx <- b_w

    # Set ly: fixed & all equal:
    hi_ly <- b_h
    mi_ly <- b_h
    fa_ly <- b_h
    cr_ly <- b_h

  } # if (area == etc.)

  ## coordinates:

  # fixed x-coordinates:
  if (by_top == "cd") {  # vertical marginal:

    # by cd(dc):
    hi_x <- x_right
    hi_y <- y_top

    mi_x <- x_left
    mi_y <- y_top

    fa_x <- x_right
    fa_y <- y_bot

    cr_x <- x_left
    cr_y <- y_bot

    # Handle alternative fnet cases: cdac
    if ( !is.na(by_bot) ) {  # Horizontal marginal dimension exists:

      if (by_bot == "ac"){ # Swap X-coord. of fa and cr boxes:
        fa_x <- x_left
        cr_x <- x_right
      }

    }

  } else if (by_top == "dc") {

    # by dc(cd):
    hi_x <- x_right
    hi_y <- y_top

    mi_x <- x_right
    mi_y <- y_bot

    fa_x <- x_left
    fa_y <- y_top

    cr_x <- x_left
    cr_y <- y_bot

    # Handle alternative fnet cases: dcac
    if ( !is.na(by_bot) ) {  # Horizontal marginal dimension exists:

      if (by_bot == "ac"){ # Swap X-coord. of mi and cr boxes:
        mi_x <- x_left
        cr_x <- x_right
      }

    }

  } else if (by_top == "ac") {

    # by ac(cd):
    hi_x <- x_right
    hi_y <- y_top

    mi_x <- x_right
    mi_y <- y_bot

    fa_x <- x_left
    fa_y <- y_bot

    cr_x <- x_left
    cr_y <- y_top

    # Handle alternative fnet cases: acdc
    if ( !is.na(by_bot) ) {  # Horizontal marginal dimension exists:

      if (by_bot == "dc"){ # Swap X-coord. of fa and mi boxes:
        fa_x <- x_right
        mi_x <- x_left
      }

    }

  } else {

    message(paste0("Unknown primary/vertical perspective: by_top = ", by_top))

  } # if (by_top == etc.)


  # } # if (area == etc.)

  # define boxes:
  box_hi <- make_box("hi", hi_x, hi_y, hi_lx, hi_ly)  # hi
  box_mi <- make_box("mi", mi_x, mi_y, mi_lx, mi_ly)  # mi
  box_fa <- make_box("fa", fa_x, fa_y, fa_lx, fa_ly)  # fa
  box_cr <- make_box("cr", cr_x, cr_y, cr_lx, cr_ly)  # cr

  ## plot boxes: 4 SDT cases/cells

  ## OLD: Plot boxes:
  # plot(box_hi, lbl_type = f_lbl, cex = cex_lbl, lwd = f_lwd)
  # plot(box_mi, lbl_type = f_lbl, cex = cex_lbl, lwd = f_lwd)
  # plot(box_fa, lbl_type = f_lbl, cex = cex_lbl, lwd = f_lwd)
  # plot(box_cr, lbl_type = f_lbl, cex = cex_lbl, lwd = f_lwd)

  ## NEW: Plot boxes by decreasing freq/prob (to prevent occlusion of box labels).
  ##      See comp_freq_fbox + plot_fbox_list in plot_util.R.
  row_3_boxes <- list(box_hi, box_mi, box_fa, box_cr)  # list of boxes (lists)
  # plot_fbox_list(row_3_boxes, lbl_type = f_lbl, cex = cex_lbl, lwd = f_lwd)  # plot list of boxes

  ## plot label:
  # plot_ftype_label("hi", ftype_x, box_3_y_top, lbl_txt = lbl_txt, suffix = ":", pos = ftype_pos, col = pal["txt"], cex = cex_lbl, ...)  # Allow ...!


  ##   2nd row (y = +2): by_top perspective ------

  # Note: Express all widths of compound frequencies
  #       as sums of hi_lx mi_lx fa_lx cr_lx!

  # box dimensions (w and h):
  box_2_1_lx <- b_w  # default box width
  box_2_2_lx <- b_w

  box_2_1_ly <- b_h  # default box height
  box_2_2_ly <- b_h

  # default box coordindates (x and y):

  # X horizontal:
  box_2_1_x <- x_right
  box_2_2_x <- x_left
  box_2_1_y <- 0  # center
  box_2_2_y <- 0  # center

  # X vertical:
  box_2_1_x <- 0  # center
  box_2_2_x <- 0  # center
  box_2_1_y <- y_top
  box_2_2_y <- y_bot


  # Define boxes and labels by perspective:
  if (by_top == "cd") {

    ## (a) by condition:

    if (area == "sq") { # Scale area as square:

      # Compute ly for current scale:
      box_2_1_ly <- comp_ly_fsqr("cond_true",  area_N = N_area, cur_freq = freq, cur_prob = prob, scale = scale)  # cond_true
      box_2_2_ly <- comp_ly_fsqr("cond_false", area_N = N_area, cur_freq = freq, cur_prob = prob, scale = scale)  # cond_false

      # Compute lx corresponding to ly:
      box_2_1_lx <- comp_lx(box_2_1_ly, mf = corr_3, corf = scale_x)  # same, but corrected for aspect ratio
      box_2_2_lx <- comp_lx(box_2_2_ly, mf = corr_3, corf = scale_x)  # same, but corrected for aspect ratio

    } # if (area == etc.)

    # define boxes:
    box_2_1 <- make_box("cond_true",  box_2_1_x,  box_2_1_y, box_2_1_lx, box_2_1_ly)
    box_2_2 <- make_box("cond_false", box_2_2_x,  box_2_2_y, box_2_2_lx, box_2_2_ly)

    # plot label (vertical/top):
    plot_ftype_label("cond_true", box_2_1_x, (box_2_1_y + 1), lbl_txt = lbl_txt, suffix = ":", pos = 3, col = pal["txt"], cex = cex_lbl, ...)  # Allow ...!

  } else if (by_top == "dc") {

    ## (b) by decision:

    if (area == "sq") { # Scale area as square:

      # Compute ly for current scale:
      box_2_1_ly <- comp_ly_fsqr("dec_pos", area_N = N_area, cur_freq = freq, cur_prob = prob, scale = scale)  # dec_pos
      box_2_2_ly <- comp_ly_fsqr("dec_neg", area_N = N_area, cur_freq = freq, cur_prob = prob, scale = scale)  # dec_neg

      # Compute lx corresponding to ly:
      box_2_1_lx <- comp_lx(box_2_1_ly, mf = corr_3, corf = scale_x)  # same, but corrected for aspect ratio
      box_2_2_lx <- comp_lx(box_2_2_ly, mf = corr_3, corf = scale_x)  # same, but corrected for aspect ratio

    }  # if (area == etc.)

    # define boxes:
    box_2_1 <- make_box("dec_pos", box_2_1_x,  box_2_1_y, box_2_1_lx, box_2_1_ly)
    box_2_2 <- make_box("dec_neg", box_2_2_x,  box_2_2_y, box_2_2_lx, box_2_2_ly)

    # plot label:
    plot_ftype_label("dec_pos", box_2_1_x, (box_2_1_y + 1), lbl_txt = lbl_txt, suffix = ":", pos = 3, col = pal["txt"], cex = cex_lbl, ...)  # Allow ...!

  } else if (by_top == "ac") {

    ## (c) by accuracy:

    if (area == "sq") { # Scale area as square:

      # Compute ly for current scale:
      box_2_1_ly <- comp_ly_fsqr("dec_cor", area_N = N_area, cur_freq = freq, cur_prob = prob, scale = scale)  # dec_cor
      box_2_2_ly <- comp_ly_fsqr("dec_err", area_N = N_area, cur_freq = freq, cur_prob = prob, scale = scale)  # dec_err

      # Compute lx corresponding to ly:
      box_2_1_lx <- comp_lx(box_2_1_ly, mf = corr_3, corf = scale_x)  # same, but corrected for aspect ratio
      box_2_2_lx <- comp_lx(box_2_2_ly, mf = corr_3, corf = scale_x)  # same, but corrected for aspect ratio

    }  # if (area == etc.)

    # define boxes:
    box_2_1 <- make_box("dec_cor", box_2_1_x,  box_2_1_y, box_2_1_lx, box_2_1_ly)
    box_2_2 <- make_box("dec_err", box_2_2_x,  box_2_2_y, box_2_2_lx, box_2_2_ly)

    # plot label:
    plot_ftype_label("dec_cor", box_2_1_x, (box_2_1_y + 1), lbl_txt = lbl_txt, suffix = ":", pos = 3, col = pal["txt"], cex = cex_lbl, ...)  # Allow ...!

  } else {  # default on top: same as (by_top == "cd")

    ## (+) by condition:

    if (area == "sq") { # Scale area as square:

      # Compute ly for current scale:
      box_2_1_ly <- comp_ly_fsqr("cond_true",  area_N = N_area, cur_freq = freq, cur_prob = prob, scale = scale)  # cond_true
      box_2_2_ly <- comp_ly_fsqr("cond_false", area_N = N_area, cur_freq = freq, cur_prob = prob, scale = scale)  # cond_false

      # Compute lx corresponding to ly:
      box_2_1_lx <- comp_lx(box_2_1_ly, mf = corr_3, corf = scale_x)  # same, but corrected for aspect ratio
      box_2_2_lx <- comp_lx(box_2_2_ly, mf = corr_3, corf = scale_x)  # same, but corrected for aspect ratio

    } # if (area == etc.)

    # define boxes:
    box_2_1 <- make_box("cond_true",  box_2_1_x,  box_2_1_y, box_2_1_lx, box_2_1_ly)
    box_2_2 <- make_box("cond_false", box_2_2_x,  box_2_2_y, box_2_2_lx, box_2_2_ly)

    # plot label:
    plot_ftype_label("cond_true", box_2_1_x, (box_2_1_y + 1), lbl_txt = lbl_txt, suffix = ":", pos = 3, col = pal["txt"], cex = cex_lbl, ...)  # Allow ...!

  }

  ## OLD: plot boxes:
  # plot(box_2_1, lbl_type = f_lbl, cex = cex_lbl, lwd = f_lwd, lbl_sep = ":\n")
  # plot(box_2_2, lbl_type = f_lbl, cex = cex_lbl, lwd = f_lwd, lbl_sep = ":\n")

  # NEW:
  row_2_boxes <- list(box_2_1, box_2_2)  # list of boxes (lists)
  # plot_fbox_list(row_2_boxes, lbl_type = f_lbl, cex = cex_lbl, lwd = f_lwd)  # plot list of boxes


  ##   4th row (y = -2): by perspective ------

  if ( !is.na(by_bot) ) {

    ## Note: Repeat code of 2nd row above (with 4 changes:
    ##       mirrored y-values, "by_bot" for "by_top", "box_4_" for "box_2_", default on top/bot):

    # box dimensions (w and h):
    box_4_1_lx <- b_w  # default box width
    box_4_2_lx <- b_w

    box_4_1_ly <- b_h  # default box height
    box_4_2_ly <- b_h

    # fixed box coordindates (x and y):

    # Y vertical:
    box_4_1_x <- 0  # center
    box_4_2_x <- 0  # center
    box_4_1_y <- y_top
    box_4_2_y <- y_bot

    # Y horizontal:
    box_4_1_x <- x_right
    box_4_2_x <- x_left
    box_4_1_y <- 0  # center
    box_4_2_y <- 0  # center

    # Define boxes and labels by perspective:
    if (by_bot == "cd") {

      ## (a) by condition:

      if (area == "sq") { # Scale area as square:

        # Compute ly for current scale:
        box_4_1_ly <- comp_ly_fsqr("cond_true",  area_N = N_area, cur_freq = freq, cur_prob = prob, scale = scale)  # cond_true
        box_4_2_ly <- comp_ly_fsqr("cond_false", area_N = N_area, cur_freq = freq, cur_prob = prob, scale = scale)  # cond_false

        # Compute lx corresponding to ly:
        box_4_1_lx <- comp_lx(box_4_1_ly, mf = corr_3, corf = scale_x)  # same, but corrected for aspect ratio
        box_4_2_lx <- comp_lx(box_4_2_ly, mf = corr_3, corf = scale_x)  # same, but corrected for aspect ratio

      } # if (area == etc.)

      # define boxes:
      box_4_1 <- make_box("cond_true",  box_4_1_x,  box_4_1_y, box_4_1_lx, box_4_1_ly)
      box_4_2 <- make_box("cond_false", box_4_2_x,  box_4_2_y, box_4_2_lx, box_4_2_ly)

      # plot label:
      plot_ftype_label("cond_true", ftype_x, box_4_1_y, lbl_txt = lbl_txt, suffix = ":", pos = ftype_pos, col = pal["txt"], cex = cex_lbl, ...)  # Allow ...!

    } else if (by_bot == "dc") {

      ## (b) by decision:

      if (area == "sq") { # Scale area as square:

        # Compute ly for current scale:
        box_4_1_ly <- comp_ly_fsqr("dec_pos", area_N = N_area, cur_freq = freq, cur_prob = prob, scale = scale)  # dec_pos
        box_4_2_ly <- comp_ly_fsqr("dec_neg", area_N = N_area, cur_freq = freq, cur_prob = prob, scale = scale)  # dec_neg

        # Compute lx corresponding to ly:
        box_4_1_lx <- comp_lx(box_4_1_ly, mf = corr_3, corf = scale_x)  # same, but corrected for aspect ratio
        box_4_2_lx <- comp_lx(box_4_2_ly, mf = corr_3, corf = scale_x)  # same, but corrected for aspect ratio

      } # if (area == etc.)

      # define boxes:
      box_4_1 <- make_box("dec_pos", box_4_1_x,  box_4_1_y, box_4_1_lx, box_4_1_ly)
      box_4_2 <- make_box("dec_neg", box_4_2_x,  box_4_2_y, box_4_2_lx, box_4_2_ly)

      # plot label (horizontal/left):
      plot_ftype_label("dec_pos", (box_4_2_x - 1), box_4_2_y, lbl_txt = lbl_txt, suffix = ":", pos = 2, col = pal["txt"], cex = cex_lbl, ...)  # Allow ...!

    } else if (by_bot == "ac") {

      ## (c) by accuracy:

      if (area == "sq") { # Scale area as square:

        # Compute ly for current scale:
        box_4_1_ly <- comp_ly_fsqr("dec_cor", area_N = N_area, cur_freq = freq, cur_prob = prob, scale = scale)  # dec_cor
        box_4_2_ly <- comp_ly_fsqr("dec_err", area_N = N_area, cur_freq = freq, cur_prob = prob, scale = scale)  # dec_err

        # Compute lx corresponding to ly:
        box_4_1_lx <- comp_lx(box_4_1_ly, mf = corr_3, corf = scale_x)  # same, but corrected for aspect ratio
        box_4_2_lx <- comp_lx(box_4_2_ly, mf = corr_3, corf = scale_x)  # same, but corrected for aspect ratio

      } # if (area == etc.)

      # define boxes:
      box_4_1 <- make_box("dec_cor", box_4_1_x,  box_4_1_y, box_4_1_lx, box_4_1_ly)
      box_4_2 <- make_box("dec_err", box_4_2_x,  box_4_2_y, box_4_2_lx, box_4_2_ly)

      # plot label:
      plot_ftype_label("dec_cor", ftype_x, box_4_1_y, lbl_txt = lbl_txt, suffix = ":", pos = ftype_pos, col = pal["txt"], cex = cex_lbl, ...)  # Allow ...!

    } else {  # default on bot: same as (by_bot == "dc")

      ## (+) by decision:

      if (area == "sq") { # Scale area as square:

        # Compute ly for current scale:
        box_4_1_ly <- comp_ly_fsqr("dec_pos", area_N = N_area, cur_freq = freq, cur_prob = prob, scale = scale)  # dec_pos
        box_4_2_ly <- comp_ly_fsqr("dec_neg", area_N = N_area, cur_freq = freq, cur_prob = prob, scale = scale)  # dec_neg

        # Compute lx corresponding to ly:
        box_4_1_lx <- comp_lx(box_4_1_ly, mf = corr_3, corf = scale_x)  # same, but corrected for aspect ratio
        box_4_2_lx <- comp_lx(box_4_2_ly, mf = corr_3, corf = scale_x)  # same, but corrected for aspect ratio

      }  # if (area == etc.)

      # define boxes:
      box_4_1 <- make_box("dec_pos", box_4_1_x,  box_4_1_y, box_4_1_lx, box_4_1_ly)
      box_4_2 <- make_box("dec_neg", box_4_2_x,  box_4_2_y, box_4_2_lx, box_4_2_ly)

      # plot label:
      plot_ftype_label("dec_pos", ftype_x, box_4_1_y, lbl_txt = lbl_txt, suffix = ":", pos = ftype_pos, col = pal["txt"], cex = cex_lbl, ...)  # Allow ...!

    }

    ## OLD: plot boxes:
    # plot(box_4_1, lbl_type = f_lbl, cex = cex_lbl, lwd = f_lwd, lbl_sep = ":\n")
    # plot(box_4_2, lbl_type = f_lbl, cex = cex_lbl, lwd = f_lwd, lbl_sep = ":\n")

    # NEW:
    row_4_boxes <- list(box_4_1, box_4_2)  # list of boxes (lists)

  } # if ( !is.na(by_bot) ) etc.

  ## Combine ALL boxes:
  if ( !is.na(by_bot) ) {
    all_boxes <- c(row_1_boxes, row_2_boxes, row_3_boxes, row_4_boxes)
  } else {
    all_boxes <- c(row_1_boxes, row_2_boxes, row_3_boxes)
  } # if ( !is.na(by_bot) ) etc.

  ## Plot ALL boxes at once:
  # plot_fbox_list(all_boxes, lbl_type = f_lbl, lbl_sep = ":\n", cex = cex_lbl, lwd = f_lwd, density = NA)  # plot list of boxes
  plot_fbox_list(all_boxes,  # plot list of boxes
                 cur_freq = freq, lbl_txt = lbl_txt, col_pal = col_pal,  # PASS current freq/txt/pal arguments!
                 lbl_type = f_lbl, lbl_sep = f_lbl_sep,
                 cex = cex_lbl, lwd = f_lwd, lty = lty)  # no ...!


  ## (B) Plot probabilities as links: ------

  ## parameters:

  ## from top:

  ##   row 1 (center) to 2 (marginals): ----

  plot_link(box_1, box_2_1, 3, 1, cur_prob = prob, arr_code = arr_c,
            lbl_type = p_lbl, lbl_pos = NULL, cex = cex_p_lbl,
            col_pal = col_pal, p_lwd = p_lwd, p_scale = p_scale,
            ...)  # Allow ...!
  plot_link(box_1, box_2_2, 1, 3, cur_prob = prob, arr_code = arr_c,
            lbl_type = p_lbl, lbl_pos = NULL, lbl_off = 1, cex = cex_p_lbl,
            lbl_sep = "\n    = ",  # special case: cprev label !!!
            col_pal = col_pal, p_lwd = p_lwd, p_scale = p_scale,
            ...)  # Allow ...!  # link label in 2 lines

  ##   row 2 (vertical marginals) to 3 (cells): ----

  # Links depend on perspectives/box types:

  if (by_top == "cd") {  # row 2: by condition (cond_true vs. cond_false)

    ## (a) by condition:
    plot_link(box_2_1, box_hi, 4, 2, cur_prob = prob, arr_code = arr_c, lbl_type = p_lbl, lbl_pos = 3, cex = cex_p_lbl, col_pal = col_pal, p_lwd = p_lwd, p_scale = p_scale, ...)  # Allow ...!  # cond_true  - hi
    plot_link(box_2_1, box_mi, 2, 4, cur_prob = prob, arr_code = arr_c, lbl_type = p_lbl, lbl_pos = 3, cex = cex_p_lbl, col_pal = col_pal, p_lwd = p_lwd, p_scale = p_scale, ...)  # Allow ...!  # cond_true  - mi

    # Handle fnet cases:
    if ( !is.na(by_bot) ) {  # Horizontal marginal dimension exists:

      if (by_bot == "dc"){ # base case:

        plot_link(box_2_2, box_fa, 4, 2, cur_prob = prob, arr_code = arr_c, lbl_type = p_lbl, lbl_pos = 1, cex = cex_p_lbl, col_pal = col_pal, p_lwd = p_lwd, p_scale = p_scale, ...)  # Allow ...!  # cond_false - fa
        plot_link(box_2_2, box_cr, 2, 4, cur_prob = prob, arr_code = arr_c, lbl_type = p_lbl, lbl_pos = 1, cex = cex_p_lbl, col_pal = col_pal, p_lwd = p_lwd, p_scale = p_scale, ...)  # Allow ...!  # cond_false - cr

      } else if (by_bot == "ac"){ # swapped bottom boxes:

        plot_link(box_2_2, box_fa, 2, 4, cur_prob = prob, arr_code = arr_c, lbl_type = p_lbl, lbl_pos = 1, cex = cex_p_lbl, col_pal = col_pal, p_lwd = p_lwd, p_scale = p_scale, ...)  # Allow ...!  # cond_false - fa
        plot_link(box_2_2, box_cr, 4, 2, cur_prob = prob, arr_code = arr_c, lbl_type = p_lbl, lbl_pos = 1, cex = cex_p_lbl, col_pal = col_pal, p_lwd = p_lwd, p_scale = p_scale, ...)  # Allow ...!  # cond_false - cr

      }

    } else { # default for is.na(by_bot):

      plot_link(box_2_2, box_fa, 4, 2, cur_prob = prob, arr_code = arr_c, lbl_type = p_lbl, lbl_pos = 1, cex = cex_p_lbl, col_pal = col_pal, p_lwd = p_lwd, p_scale = p_scale, ...)  # Allow ...!  # cond_false - fa
      plot_link(box_2_2, box_cr, 2, 4, cur_prob = prob, arr_code = arr_c, lbl_type = p_lbl, lbl_pos = 1, cex = cex_p_lbl, col_pal = col_pal, p_lwd = p_lwd, p_scale = p_scale, ...)  # Allow ...!  # cond_false - cr

    }

  } else if (by_top == "dc") {  # row 2: by decision (dec_pos vs. dec_neg)

    ## (b) by decision:
    plot_link(box_2_1, box_hi, 4, 2, cur_prob = prob, arr_code = arr_c, lbl_type = p_lbl, lbl_pos = 3, cex = cex_p_lbl, col_pal = col_pal, p_lwd = p_lwd, p_scale = p_scale, ...)  # Allow ...!  # dec_pos - hi
    plot_link(box_2_1, box_fa, 2, 4, cur_prob = prob, arr_code = arr_c, lbl_type = p_lbl, lbl_pos = 3, cex = cex_p_lbl, col_pal = col_pal, p_lwd = p_lwd, p_scale = p_scale, ...)  # Allow ...!  # dec_pos - fa !

    # Handle fnet cases:
    if ( !is.na(by_bot) ) {  # Horizontal marginal dimension exists:

      if (by_bot == "cd"){ # base case:

        plot_link(box_2_2, box_mi, 4, 2, cur_prob = prob, arr_code = arr_c, lbl_type = p_lbl, lbl_pos = 1, cex = cex_p_lbl, col_pal = col_pal, p_lwd = p_lwd, p_scale = p_scale, ...)  # Allow ...!  # dec_neg - mi !
        plot_link(box_2_2, box_cr, 2, 4, cur_prob = prob, arr_code = arr_c, lbl_type = p_lbl, lbl_pos = 1, cex = cex_p_lbl, col_pal = col_pal, p_lwd = p_lwd, p_scale = p_scale, ...)  # Allow ...!  # dec_neg - cr

      } else if (by_bot == "ac"){ # swapped bottom boxes:

        plot_link(box_2_2, box_mi, 2, 4, cur_prob = prob, arr_code = arr_c, lbl_type = p_lbl, lbl_pos = 1, cex = cex_p_lbl, col_pal = col_pal, p_lwd = p_lwd, p_scale = p_scale, ...)  # Allow ...!  # dec_neg - mi !
        plot_link(box_2_2, box_cr, 4, 2, cur_prob = prob, arr_code = arr_c, lbl_type = p_lbl, lbl_pos = 1, cex = cex_p_lbl, col_pal = col_pal, p_lwd = p_lwd, p_scale = p_scale, ...)  # Allow ...!  # dec_neg - cr

      }

    } else { # default for is.na(by_bot):

      plot_link(box_2_2, box_mi, 4, 2, cur_prob = prob, arr_code = arr_c, lbl_type = p_lbl, lbl_pos = 1, cex = cex_p_lbl, col_pal = col_pal, p_lwd = p_lwd, p_scale = p_scale, ...)  # Allow ...!  # dec_neg - mi !
      plot_link(box_2_2, box_cr, 2, 4, cur_prob = prob, arr_code = arr_c, lbl_type = p_lbl, lbl_pos = 1, cex = cex_p_lbl, col_pal = col_pal, p_lwd = p_lwd, p_scale = p_scale, ...)  # Allow ...!  # dec_neg - cr

    }

  } else if (by_top == "ac") {  # row 2: by accuracy (dec_cor vs. dec_err)

    ## (c) by accuracy:
    plot_link(box_2_1, box_hi, 4, 2, cur_prob = prob, arr_code = arr_c, lbl_type = p_lbl, lbl_pos = 3, cex = cex_p_lbl, col_pal = col_pal, p_lwd = p_lwd, p_scale = p_scale, ...)  # Allow ...!  # dec_cor - hi: acc_hi
    plot_link(box_2_1, box_cr, 2, 4, cur_prob = prob, arr_code = arr_c, lbl_type = p_lbl, lbl_pos = 3, cex = cex_p_lbl, col_pal = col_pal, p_lwd = p_lwd, p_scale = p_scale, ...)  # Allow ...!  # dec_cor - cr: acc_cr

    # Handle fnet cases:
    if ( !is.na(by_bot) ) {  # Horizontal marginal dimension exists:

      if (by_bot == "cd"){ # base case:

        plot_link(box_2_2, box_mi, 4, 2, cur_prob = prob, arr_code = arr_c, lbl_type = p_lbl, lbl_pos = 1, cex = cex_p_lbl, col_pal = col_pal, p_lwd = p_lwd, p_scale = p_scale, ...)  # Allow ...!  # dec_err - mi: err_mi
        plot_link(box_2_2, box_fa, 2, 4, cur_prob = prob, arr_code = arr_c, lbl_type = p_lbl, lbl_pos = 1, cex = cex_p_lbl, col_pal = col_pal, p_lwd = p_lwd, p_scale = p_scale, ...)  # Allow ...!  # dec_err - fa: err_fa

      } else if (by_bot == "dc"){ # swapped bottom boxes:

        plot_link(box_2_2, box_mi, 2, 4, cur_prob = prob, arr_code = arr_c, lbl_type = p_lbl, lbl_pos = 1, cex = cex_p_lbl, col_pal = col_pal, p_lwd = p_lwd, p_scale = p_scale, ...)  # Allow ...!  # dec_err - mi: err_mi
        plot_link(box_2_2, box_fa, 4, 2, cur_prob = prob, arr_code = arr_c, lbl_type = p_lbl, lbl_pos = 1, cex = cex_p_lbl, col_pal = col_pal, p_lwd = p_lwd, p_scale = p_scale, ...)  # Allow ...!  # dec_err - fa: err_fa

      }

    } else { # default for is.na(by_bot):

      plot_link(box_2_2, box_mi, 4, 2, cur_prob = prob, arr_code = arr_c, lbl_type = p_lbl, lbl_pos = 1, cex = cex_p_lbl, col_pal = col_pal, p_lwd = p_lwd, p_scale = p_scale, ...)  # Allow ...!  # dec_err - mi: err_mi
      plot_link(box_2_2, box_fa, 2, 4, cur_prob = prob, arr_code = arr_c, lbl_type = p_lbl, lbl_pos = 1, cex = cex_p_lbl, col_pal = col_pal, p_lwd = p_lwd, p_scale = p_scale, ...)  # Allow ...!  # dec_err - fa: err_fa

    }

  } else {  # default on top: same as (by_top == "cd")

    ## (+) by condition:
    plot_link(box_2_1, box_hi, 4, 2, cur_prob = prob, arr_code = arr_c, lbl_type = p_lbl, lbl_pos = 2, cex = cex_p_lbl, col_pal = col_pal, p_lwd = p_lwd, p_scale = p_scale, ...)  # Allow ...!  # cond_true  - hi
    plot_link(box_2_1, box_mi, 2, 4, cur_prob = prob, arr_code = arr_c, lbl_type = p_lbl, lbl_pos = 4, cex = cex_p_lbl, col_pal = col_pal, p_lwd = p_lwd, p_scale = p_scale, ...)  # Allow ...!  # cond_true  - mi
    plot_link(box_2_2, box_fa, 4, 2, cur_prob = prob, arr_code = arr_c, lbl_type = p_lbl, lbl_pos = 2, cex = cex_p_lbl, col_pal = col_pal, p_lwd = p_lwd, p_scale = p_scale, ...)  # Allow ...!  # cond_false - fa
    plot_link(box_2_2, box_cr, 2, 4, cur_prob = prob, arr_code = arr_c, lbl_type = p_lbl, lbl_pos = 4, cex = cex_p_lbl, col_pal = col_pal, p_lwd = p_lwd, p_scale = p_scale, ...)  # Allow ...!  # cond_false - cr

  }

  ## from bottom:

  if ( !is.na(by_bot) ) {

    ##   row 4 (horizontal marginals) to 3 (cells): ----

    # Links depend on perspectives/box types:

    if (by_bot == "cd") {  # row 4: by condition (cond_true vs. cond_false)

      ## (a) by condition:
      plot_link(box_4_1, box_hi, 3, 1, cur_prob = prob, arr_code = arr_c, lbl_type = p_lbl, lbl_pos = 4, cex = cex_p_lbl, col_pal = col_pal, p_lwd = p_lwd, p_scale = p_scale, ...)  # Allow ...!  # cond_true  - hi
      plot_link(box_4_1, box_mi, 1, 3, cur_prob = prob, arr_code = arr_c, lbl_type = p_lbl, lbl_pos = 4, cex = cex_p_lbl, col_pal = col_pal, p_lwd = p_lwd, p_scale = p_scale, ...)  # Allow ...!  # cond_true  - mi

      if (by_top == "dc"){

        plot_link(box_4_2, box_fa, 3, 1, cur_prob = prob, arr_code = arr_c, lbl_type = p_lbl, lbl_pos = 2, cex = cex_p_lbl, col_pal = col_pal, p_lwd = p_lwd, p_scale = p_scale, ...)  # Allow ...!  # cond_false - fa
        plot_link(box_4_2, box_cr, 1, 3, cur_prob = prob, arr_code = arr_c, lbl_type = p_lbl, lbl_pos = 2, cex = cex_p_lbl, col_pal = col_pal, p_lwd = p_lwd, p_scale = p_scale, ...)  # Allow ...!  # cond_false - cr

      } else if (by_top == "ac"){

        plot_link(box_4_2, box_fa, 1, 3, cur_prob = prob, arr_code = arr_c, lbl_type = p_lbl, lbl_pos = 2, cex = cex_p_lbl, col_pal = col_pal, p_lwd = p_lwd, p_scale = p_scale, ...)  # Allow ...!  # cond_false - fa
        plot_link(box_4_2, box_cr, 3, 1, cur_prob = prob, arr_code = arr_c, lbl_type = p_lbl, lbl_pos = 2, cex = cex_p_lbl, col_pal = col_pal, p_lwd = p_lwd, p_scale = p_scale, ...)  # Allow ...!  # cond_false - cr

      }

    } else if (by_bot == "dc") {  # row 4: by decision (dec_pos vs. dec_neg)

      ## (b) by decision:
      plot_link(box_4_1, box_hi, 3, 1, cur_prob = prob, arr_code = arr_c, lbl_type = p_lbl, lbl_pos = 4, cex = cex_p_lbl, col_pal = col_pal, p_lwd = p_lwd, p_scale = p_scale, ...)  # Allow ...!  # dec_pos - hi
      plot_link(box_4_1, box_fa, 1, 3, cur_prob = prob, arr_code = arr_c, lbl_type = p_lbl, lbl_pos = 4, cex = cex_p_lbl, col_pal = col_pal, p_lwd = p_lwd, p_scale = p_scale, ...)  # Allow ...!  # dec_pos - fa !

      if (by_top == "cd"){

        plot_link(box_4_2, box_mi, 3, 1, cur_prob = prob, arr_code = arr_c, lbl_type = p_lbl, lbl_pos = 2, cex = cex_p_lbl, col_pal = col_pal, p_lwd = p_lwd, p_scale = p_scale, ...)  # Allow ...!  # dec_neg - mi !
        plot_link(box_4_2, box_cr, 1, 3, cur_prob = prob, arr_code = arr_c, lbl_type = p_lbl, lbl_pos = 2, cex = cex_p_lbl, col_pal = col_pal, p_lwd = p_lwd, p_scale = p_scale, ...)  # Allow ...!  # dec_neg - cr

      } else if (by_top == "ac"){

        plot_link(box_4_2, box_mi, 1, 3, cur_prob = prob, arr_code = arr_c, lbl_type = p_lbl, lbl_pos = 2, cex = cex_p_lbl, col_pal = col_pal, p_lwd = p_lwd, p_scale = p_scale, ...)  # Allow ...!  # dec_neg - mi !
        plot_link(box_4_2, box_cr, 3, 1, cur_prob = prob, arr_code = arr_c, lbl_type = p_lbl, lbl_pos = 2, cex = cex_p_lbl, col_pal = col_pal, p_lwd = p_lwd, p_scale = p_scale, ...)  # Allow ...!  # dec_neg - cr

      }

    } else if (by_bot == "ac") {  # row 4: by accuracy (dec_cor vs. dec_err)

      ## (c) by accuracy:

      plot_link(box_4_1, box_hi, 3, 1, cur_prob = prob, arr_code = arr_c, lbl_type = p_lbl, lbl_pos = 4, cex = cex_p_lbl, col_pal = col_pal, p_lwd = p_lwd, p_scale = p_scale, ...)  # Allow ...!  # dec_cor - hi: acc_hi
      plot_link(box_4_1, box_cr, 1, 3, cur_prob = prob, arr_code = arr_c, lbl_type = p_lbl, lbl_pos = 4, cex = cex_p_lbl, col_pal = col_pal, p_lwd = p_lwd, p_scale = p_scale, ...)  # Allow ...!  # dec_cor - cr: acc_cr

      if (by_top == "cd"){

        plot_link(box_4_2, box_mi, 3, 1, cur_prob = prob, arr_code = arr_c, lbl_type = p_lbl, lbl_pos = 2, cex = cex_p_lbl, col_pal = col_pal, p_lwd = p_lwd, p_scale = p_scale, ...)  # Allow ...!  # dec_err - mi: err_mi
        plot_link(box_4_2, box_fa, 1, 3, cur_prob = prob, arr_code = arr_c, lbl_type = p_lbl, lbl_pos = 2, cex = cex_p_lbl, col_pal = col_pal, p_lwd = p_lwd, p_scale = p_scale, ...)  # Allow ...!  # dec_err - fa: err_fa

      } else if (by_top == "dc"){

        plot_link(box_4_2, box_mi, 1, 3, cur_prob = prob, arr_code = arr_c, lbl_type = p_lbl, lbl_pos = 2, cex = cex_p_lbl, col_pal = col_pal, p_lwd = p_lwd, p_scale = p_scale, ...)  # Allow ...!  # dec_err - mi: err_mi
        plot_link(box_4_2, box_fa, 3, 1, cur_prob = prob, arr_code = arr_c, lbl_type = p_lbl, lbl_pos = 2, cex = cex_p_lbl, col_pal = col_pal, p_lwd = p_lwd, p_scale = p_scale, ...)  # Allow ...!  # dec_err - fa: err_fa

      }

    } else {  # default on bot: same as (by_bot == "dc")

      ## (+) by decision:
      plot_link(box_4_1, box_hi, 3, 1, cur_prob = prob, arr_code = arr_c, lbl_type = p_lbl, lbl_pos = 4, cex = cex_p_lbl, col_pal = col_pal, p_lwd = p_lwd, p_scale = p_scale, ...)  # Allow ...!  # dec_pos - hi
      plot_link(box_4_1, box_fa, 1, 3, cur_prob = prob, arr_code = arr_c, lbl_type = p_lbl, lbl_pos = 4, cex = cex_p_lbl, col_pal = col_pal, p_lwd = p_lwd, p_scale = p_scale, ...)  # Allow ...!  # dec_pos - fa !
      plot_link(box_4_2, box_mi, 3, 1, cur_prob = prob, arr_code = arr_c, lbl_type = p_lbl, lbl_pos = 2, cex = cex_p_lbl, col_pal = col_pal, p_lwd = p_lwd, p_scale = p_scale, ...)  # Allow ...!  # dec_neg - mi !
      plot_link(box_4_2, box_cr, 1, 3, cur_prob = prob, arr_code = arr_c, lbl_type = p_lbl, lbl_pos = 2, cex = cex_p_lbl, col_pal = col_pal, p_lwd = p_lwd, p_scale = p_scale, ...)  # Allow ...!  # dec_neg - cr

    }

    ## Center to horizontal (left/right) (formerly row 5 to 4): ----

    if (by_bot == "cd" || by_bot == "dc" || (by_bot == "ac")) {

      # link to 2 default boxes:
      plot_link(box_1, box_4_1,  4, 2, cur_prob = prob, arr_code = arr_c, lbl_type = p_lbl, lbl_pos = NULL,
                cex = cex_p_lbl, col_pal = col_pal, p_lwd = p_lwd, p_scale = p_scale, ...)  # Allow ...!
      plot_link(box_1, box_4_2,  2, 4, cur_prob = prob, arr_code = arr_c, lbl_type = p_lbl, lbl_pos = NULL, lbl_off = 4/4,
                cex = cex_p_lbl, col_pal = col_pal, p_lwd = p_lwd, p_scale = p_scale, ...)  # Allow ...!

    } else {  # link to 4 boxes (dec_pos / dec_neg) vs. (dec_cor / dec_err):

      # link to 2 default boxes:
      plot_link(box_1, box_4_1,  3, 1, cur_prob = prob, arr_code = arr_c, lbl_type = p_lbl, lbl_pos = 2,
                cex = cex_p_lbl, col_pal = col_pal, p_lwd = p_lwd, p_scale = p_scale, ...)  # Allow ...!
      plot_link(box_1, box_4_2,  3, 1, cur_prob = prob, arr_code = arr_c, lbl_type = p_lbl, lbl_pos = 2, lbl_off = 4/4,
                cex = cex_p_lbl, col_pal = col_pal, p_lwd = p_lwd, p_scale = p_scale, ...)  # Allow ...!

    }

  } # if ( !is.na(by_bot) ) etc.


  ## Plot diagonal links of joint probabilities (from N to joint frequencies): -----

  show_joint_prob <- TRUE

  if (show_joint_prob){

    # compute joint probabilities:
    n_digits <- 3
    p_hi <- as_pc(round(freq$hi/freq$N, n_digits), 1)
    p_mi <- as_pc(round(freq$mi/freq$N, n_digits), 1)
    p_fa <- as_pc(round(freq$fa/freq$N, n_digits), 1)
    p_cr <- as_pc(round(freq$cr/freq$N, n_digits), 1)

    # create labels:
    if (p_lbl == "def"){

      p_hi_lbl <- paste0("p(hi) =", p_hi, "%")
      p_mi_lbl <- paste0("p(mi) =", p_mi, "%")
      p_fa_lbl <- paste0("p(fa) =", p_fa, "%")
      p_cr_lbl <- paste0("p(cr) =", p_cr, "%")

    } else if (p_lbl == "namnum"){

      p_hi_lbl <- paste0("p(", txt$hi_lbl, ")\n = ", p_hi, "%")
      p_mi_lbl <- paste0("p(", txt$mi_lbl, ")\n = ", p_mi, "%")
      p_fa_lbl <- paste0("p(", txt$fa_lbl, ")\n = ", p_fa, "%")
      p_cr_lbl <- paste0("p(", txt$cr_lbl, ")\n = ", p_cr, "%")

    } else { # default: percentages only

      p_hi_lbl <- paste0(p_hi, "%")
      p_mi_lbl <- paste0(p_mi, "%")
      p_fa_lbl <- paste0(p_fa, "%")
      p_cr_lbl <- paste0(p_cr, "%")

    }

    # plot diagonal links:
    if (by_top == "cd"){

      plot_link(box_1, box_hi, 7, 5, lbl = p_hi_lbl, arr_code = arr_c, lbl_type = p_lbl, lbl_pos = NULL,
                cex = cex_p_lbl, col_pal = col_pal, p_lwd = p_lwd, p_scale = p_scale, ...)  # Allow ...!
      plot_link(box_1, box_mi, 6, 8, lbl = p_mi_lbl, arr_code = arr_c, lbl_type = p_lbl, lbl_pos = NULL,
                cex = cex_p_lbl, col_pal = col_pal, p_lwd = p_lwd, p_scale = p_scale, ...)  # Allow ...!

      # Handle fnet cases:
      if ( !is.na(by_bot) ) {  # Horizontal marginal dimension exists:

        if (by_bot == "dc"){

          plot_link(box_1, box_fa, 8, 6, lbl = p_fa_lbl, arr_code = arr_c, lbl_type = p_lbl, lbl_pos = NULL,
                    cex = cex_p_lbl, col_pal = col_pal, p_lwd = p_lwd, p_scale = p_scale, ...)  # Allow ...!
          plot_link(box_1, box_cr, 5, 7, lbl = p_cr_lbl, arr_code = arr_c, lbl_type = p_lbl, lbl_pos = NULL,
                    cex = cex_p_lbl, col_pal = col_pal, p_lwd = p_lwd, p_scale = p_scale, ...)  # Allow ...!

        } else if (by_bot == "ac"){

          plot_link(box_1, box_fa, 5, 7, lbl = p_fa_lbl, arr_code = arr_c, lbl_type = p_lbl, lbl_pos = NULL,
                    cex = cex_p_lbl, col_pal = col_pal, p_lwd = p_lwd, p_scale = p_scale, ...)  # Allow ...!
          plot_link(box_1, box_cr, 8, 6, lbl = p_cr_lbl, arr_code = arr_c, lbl_type = p_lbl, lbl_pos = NULL,
                    cex = cex_p_lbl, col_pal = col_pal, p_lwd = p_lwd, p_scale = p_scale, ...)  # Allow ...!

        } # if (by_bot ==...) end.

      } else { # if is.na(by_bot) ) (i.e., by = "cd" only):

        plot_link(box_1, box_fa, 8, 6, lbl = p_fa_lbl, arr_code = arr_c, lbl_type = p_lbl, lbl_pos = NULL,
                  cex = cex_p_lbl, col_pal = col_pal, p_lwd = p_lwd, p_scale = p_scale, ...)  # Allow ...!
        plot_link(box_1, box_cr, 5, 7, lbl = p_cr_lbl, arr_code = arr_c, lbl_type = p_lbl, lbl_pos = NULL,
                  cex = cex_p_lbl, col_pal = col_pal, p_lwd = p_lwd, p_scale = p_scale, ...)  # Allow ...!

      } # if ( !is.na(by_bot) ) end.

    } # if (by_top == "cd") end.

    # +++ here now +++

  } # if (show_joint_p) end.


  ## (C) Plot other stuff: ------

  # box_else <- make_box("else_box", 9, -2, b_w, b_h)  # define some arbitrary box
  # plot(box_else, col = "firebrick1", cex = 1/2, font = 2)     # plot box

  ## ftype labels:
  # plot_ftype_label("N", ftype_x, 4, lbl_txt = lbl_txt, suffix = ":", pos = ftype_pos, col = pal["txt"], cex = cex_lbl, ...)  # Allow ...!
  # plot_ftype_label("cond_true", ftype_x, 2, lbl_txt = lbl_txt, suffix = ":", pos = ftype_pos, col = pal["txt"], cex = cex_lbl, ...)  # Allow ...!
  # plot_ftype_label("hi", ftype_x, 0, lbl_txt = lbl_txt, suffix = ":", pos = ftype_pos, col = pal["txt"], cex = cex_lbl, ...)  # Allow ...!
  # plot_ftype_label("N", ftype_x, -4, pos = ftype_pos, lbl_txt = lbl_txt, suffix = ":", col = pal["txt"], cex = cex_lbl, ...)  # Allow ...!


  ## (6) Title: ------

  # Define parts:
  # if (is.null(title_lbl)) { title_lbl <- "" }  # adjust NULL to "" (i.e., no title)
  # if (is.na(title_lbl)) { title_lbl <- lbl_txt$scen_lbl }  # use scen_lbl as default plot title
  if (nchar(title_lbl) > 0) { title_lbl <- paste0(title_lbl, ":\n") }  # put on top (in separate line)

  if (title_lbl == "") {  # if title has been set to "":
    type_lbl <- ""        # assume that no subtitle is desired either
  } else {
    if ( !is.na(by_bot) ) {
      type_lbl <- paste0(lbl["plot_fnet_lbl"], " (by ", as.character(by), ")")  # plot name: frequency net.
    } else {
      type_lbl <- paste0(lbl["plot_tree_lbl"], " (by ", as.character(by), ")")  # plot name: frequency tree.
    } # if ( !is.na(by_bot) )
  }

  # Compose label:
  cur_title_lbl <- paste0(title_lbl, type_lbl)

  # Plot title:
  title(cur_title_lbl, adj = 0, line = 0, font.main = 1, cex.main = 1.2)  # (left, NOT raised (by +1), normal font)


  ## (7) Margins: ------

  if (mar_notes) {

    # Note:
    note_lbl <- ""  # initialize
    if ( (area != "no") && (scale == "f") ) { # Note area type and scaling by f:
      note_lbl <- label_note(area = area, scale = scale)
    }

    plot_mar(show_freq = TRUE, show_cond = TRUE, show_dec = TRUE,
             show_accu = TRUE, accu_from_freq = FALSE,
             note = note_lbl,
             cur_freq = freq, cur_prob = prob, lbl_txt = lbl_txt)

  } # if (mar_notes)


  ##   Finish: ---------

  # on.exit(par(opar))  # par(opar)  # restore original settings
  invisible() # restores par(opar)

} # plot_fn end.


## Check: ------

# ## Basics:
#
# ## Global freq and prob objects:
# plot_fn()  # default, same as:
# plot_fn(by = "cddc", area = "no", scale = "f",
#            f_lbl = "default", f_lwd = 0, cex_lbl = .90,
#            p_lbl = "mix", arr_c = -2, cex_p_lbl = NA)
#
# ## Locally computed values:
# plot_fn(N = 10, prev = 1/2, sens = 4/5, spec = 3/5)
# plot_fn(N = 10, prev = 1/3, sens = 3/5, spec = 4/5, area = "hr")
# plot_fn(N = 10, prev = 1/4, sens = 3/5, spec = 2/5, area = "sq", mar_notes = TRUE)
#
# ## Custom text and color settings:
# my_txt <- init_txt(cond_lbl = "The Truth", cond_true_lbl = "so true", cond_false_lbl = "so false",
#                    hi_lbl = "TP", mi_lbl = "FN", fa_lbl = "FP", cr_lbl = "TN")
# my_col <- init_pal(N_col = rgb(0, 169, 224, max = 255),  # seeblau
#                    hi_col = "gold", mi_col = "firebrick1", fa_col = "firebrick2", cr_col = "orange")
# plot_fn(f_lbl = "nam", lbl_txt = my_txt, col_pal = my_col)
#
# ## Local values and custom color and text settings:
# plot_fn(N = 7, prev = 1/2, sens = 3/5, spec = 4/5, round = FALSE,
#            by = "cdac", lbl_txt = txt_org, f_lbl = "namnum", f_lbl_sep = ":\n",
#            col_pal = pal_rgb)  # custom colors
#
# plot_fn(N = 5, prev = 1/2, sens = .8, spec = .5, scale = "p",  # note scale!
#            by = "cddc", area = "hr", col_pal = pal_bw, f_lwd = 1) # custom colors
#
# plot_fn(N = 3, prev = .50, sens = .50, spec = .50, scale = "p",              # note scale!
#            area = "sq", lbl_txt = txt_org, f_lbl = "namnum", f_lbl_sep = ":\n", # custom text
#            col_pal = pal_kn, f_lwd = .5)                                        # custom colors
#
# ## Plot versions:
# # (a) single tree (nchar(by) == 2):
# plot_fn(by = "cd", f_lbl = "def")  # by condition (freq boxes: hi mi fa cr)
# plot_fn(by = "dc", f_lbl = "def")  # by decision  (freq boxes: hi fa mi cr)
# plot_fn(by = "ac", f_lbl = "def")  # by decision  (freq boxes: hi cr mi fa)
#
# # (b) double tree (nchar(by) == 4):
# #     (3 x 2) = 6 versions (+ 3 redundant ones):
# plot_fn(by = "cddc")  # v01 (default)
# plot_fn(by = "cdac")  # v02
# plot_fn(by = "cdcd")  # (+) Message
#
# plot_fn(by = "dccd")  # v03
# plot_fn(by = "dcac")  # v04
# plot_fn(by = "dcdc")  # (+) Message
#
# plot_fn(by = "accd")  # v05
# plot_fn(by = "acdc")  # v06
# plot_fn(by = "acac")  # (+) Message
#
# ## Plot options:
#
# # area:
# plot_fn(area = "no")  # rectangular boxes (default): (same if area = NA/NULL)
# plot_fn(area = "hr")  # horizontal rectangles (widths on each level sum to N)
# plot_fn(area = "sq")  # squares (areas on each level sum to N)
#
# # scale (matters for scaled areas and small N):
# plot_fn(area = "hr", scale = "f")  # widths scaled by (rounded or non-rounded) freq
# plot_fn(area = "hr", scale = "p")  # widths scaled by prob
# plot_fn(area = "sq", scale = "f")  # areas scaled by (rounded or non-rounded) freq
# plot_fn(area = "sq", scale = "p")  # areas scaled by prob
#
# ## Freq (as boxes):
#
# # f_lbl:
# plot_fn(f_lbl = "default") # default: short name and numeric value (abb = num)
# plot_fn(f_lbl = "abb")     # abbreviated freq names (variable names)
# plot_fn(f_lbl = "nam")     # only freq names
# plot_fn(f_lbl = "num")     # only numeric freq values
# plot_fn(f_lbl = "namnum")  # names and numeric freq values
# plot_fn(f_lbl = "namnum", cex_lbl = .75)  # smaller freq labels
# plot_fn(f_lbl = NA)        # no freq labels
# plot_fn(f_lbl = "any")     # short name and value (abb = num)
#
# # f_lwd:
# plot_fn(f_lwd =  0)  # no lines (default), set to tiny_lwd = .001, lty = 0 (same if NA/NULL)
# plot_fn(f_lwd =  1)  # basic lines
# plot_fn(f_lwd =  3)  # thicker lines
# plot_fn(f_lwd = .5)  # thinner lines
#
# ## Prob (as links):
#
# # p_lbl: Label types
# plot_fn(p_lbl = "mix")     # abbreviated names with numeric values (abb = num)
# plot_fn(p_lbl = NA)      # no prob labels (NA/NULL/"none")
# plot_fn(p_lbl = "nam")     # only prob names
# plot_fn(p_lbl = "num")     # only numeric prob values
# plot_fn(p_lbl = "namnum")  # names and numeric prob values
# plot_fn(p_lbl = "namnum", cex_p_lbl = .70)  # smaller prob labels
# plot_fn(by = "cddc", p_lbl = "min")  # minimal labels
# plot_fn(by = "cdac", p_lbl = "min")
# plot_fn(by = "cddc", p_lbl = "mix")  # mix abbreviated names and numeric values
# plot_fn(by = "cdac", p_lbl = "mix")
# plot_fn(by = "cddc", p_lbl = "abb")  # abbreviated names
# plot_fn(by = "cdac", p_lbl = "abb")
# plot_fn(p_lbl = "any") # short name and value (abb = num)
#
# # arr_c:
# plot_fn(arr_c =  0)  # acc_c = 0: no arrows
# plot_fn(arr_c = -3)  # arr_c = -1 to -3: points at both ends
# plot_fn(arr_c = -2)  # point at far end
# plot_fn(arr_c = +2)  # crr_c = 1-3: V-shape arrows at far end
# plot_fn(arr_c = +3)  # V-shape arrows at both ends
# plot_fn(arr_c = +6)  # arr_c = 4-6: T-shape arrows
#
#
# ## Plain plot versions:
#
# plot_fn(area = "no", f_lbl = "nam", p_lbl = NA, col_pal = pal_rgb)
# plot_fn(area = "no", f_lbl = "abb", p_lbl = "abb", col_pal = pal_bw)
# plot_fn(area = "no", f_lbl = "num", p_lbl = "num", col_pal = pal_kn)
#
# plot_fn(area = "hr", f_lbl = "abb", p_lbl = NA, arr_c = 0, col_pal = pal_rgb)
# plot_fn(area = "hr", f_lbl = "num", p_lbl = NA, arr_c = 0)
# plot_fn(area = "hr", f_lbl = "abb", p_lbl = "num", arr_c = 0)
#
# plot_fn(area = "sq", f_lbl = "abb", p_lbl = NA, col_pal = pal_rgb)
# plot_fn(area = "sq", f_lbl = "num", p_lbl = NA, f_lwd = 1, col_pal = pal_bw)
# plot_fn(area = "sq", f_lbl = "def", p_lbl = NA, f_lwd = 1, col_pal = pal_kn)
#
# ## Suggested combinations:
#
# plot_fn(f_lbl = "def", p_lbl = "mix")
# plot_fn(f_lbl = "namnum", p_lbl = "mix", cex_lbl = .80, cex_p_lbl = .75)
#
# plot_fn(area = "hr", f_lbl = "nam", p_lbl = NA, arr_c = 0, lbl_txt = txt_TF)
# plot_fn(area = "hr", f_lbl = "abb", p_lbl = "abb", f_lwd = 1, col_pal = pal_bw)
# plot_fn(area = "hr", f_lbl = "num", p_lbl = "num", col_pal = pal_rgb)
#
# plot_fn(area = "sq", f_lbl = "nam", p_lbl = "abb", lbl_txt = txt_TF)
# plot_fn(area = "sq", f_lbl = "num", p_lbl = "num", f_lwd = 1, col_pal = pal_rgb)
# plot_fn(area = "sq", f_lbl = "def", p_lbl = "mix", f_lwd = 1, col_pal = pal_kn)


read_by <- function(by){
  # Helper function with
  # - input:  by argument and
  # - output: vector of by_top, by_bot, and (possibly different) by_now:

  # Initialize outputs:
  by_top <- NULL
  by_bot <- NULL
  by_now <- NULL

  # Interpret inputs:
  if ( !is.null(by) && !is.na(by) ) { by <- tolower(by) }  # by in lowercase
  if ( is.null(by) || is.na(by) )  { by <- "cddc" }        # use default
  if (by == "any" || by == "all" || by == "default" || by == "def" || by == "no" ) { by <- "cddc" }  # use default

  # Use by input:
  # Case 1: Plot 2 perspectives (prism, double tree):
  if (nchar(by) >= 4) {

    by_top <- substr(by, 1, 2)  # top perspective (row 2): by = "cd" "dc" "ac"
    by_bot <- substr(by, 3, 4)  # bottom perspective (row 4): by = "cd" "dc" "ac"

    # Catch & correct invalid entries:
    if (by_top == by_bot) {
      message("Specified 2 identical perspectives.")
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
    if ((by_top == "cd") && (by_bot != ("dc") & by_bot != ("ac") & by_bot != ("cd"))) {
      message("If 1st perspective by = 'cd', 2nd perspective should be 'dc' or 'ac'.\nUsing by = 'cddc'.")
      by_bot <- "dc"  # default 1
    }
    if ((by_top == "dc") && (by_bot != ("cd") & by_bot != ("ac") & by_bot != ("dc"))) {
      message("If 1st perspective by = 'dc', 2nd perspective should be 'cd' or 'ac'.\nUsing by = 'dccd'.")
      by_bot <- "cd"  # default 2
    }
    if ((by_top == "ac") && (by_bot != ("cd") & by_bot != ("dc") & by_bot != ("ac"))) {
      message("If 1st perspective by = 'ac', 2nd perspective should be 'cd' or 'dc'.\nUsing by = 'accd'.")
      by_bot <- "cd"  # default 3
    }

  }

  # Case 2: Plot 1 perspective (single tree):
  if (nchar(by) <= 2) {

    by_top <- substr(by, 1, 2)  # top perspective (row 2): by = "cd" "dc" "ac"

    if ((by_top %in% c("cd", "dc", "ac")) == FALSE) {
      message("Invalid perspective! Valid by = {'cd', 'dc', 'ac'}.\nUsing by = 'cd'.")
      by_top <- "cd"  # default
    }

    # by_bot <- "dc" # Temporary HACK (to allow testing Case 2 with plot code requiring 2 perspectives)!
    by_bot <- NA  # signal absence of 2nd perspective
  }

  # Determine current version of by (by_now, which may be different from original by):
  if ( !is.na(by_bot) ) {
    by_now <- paste0(by_top, by_bot)
  } else {
    by_now <- by_top
  } # if ( !is.na(by_bot) ) etc.
  # print(by_now)

  # Finish:
  return(c(by_top, by_bot, by_now))

} # read_by() end.

## Check:
# read_by(by = "cd")
# read_by(by = "cddc")
# read_by(by = "xx")
# read_by(by = "cdxx")
# read_by(by = "xxxxxx")

## Done: [2021 01 02] ------

## (0) Design basic cddc case based on plot_prism().

## (1) Removed 2nd population box (box_5) and area = "hr" options.

## (2) Added support for "cdac" and "dcac" cases.


## ToDo: [2021 01 03] ------

## (+) Add diagonal links for joint probabilities (using new options in plot_link() for setting pos1/pos2 to 5-8).

## eof. ----------

