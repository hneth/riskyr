## plot_tree.R | riskyr
## 2018 12 20
## Deprecated function: Using plot_prism instead.
## -----------------------------------------------

## plot_tree: Documentation: ---------

#' Plot a tree diagram of frequencies and probabilities.
#'
#' \code{plot_tree} drew a tree diagram of
#' frequencies (as nodes) and probabilities (as edges).
#'
#' \code{plot_tree} is deprecated -- please use \code{\link{plot_prism}} instead.
#'
#' @param prev The condition's prevalence \code{\link{prev}}.
#'
#' @param sens The decision's sensitivity \code{\link{sens}}.
#'
#' @param mirt The decision's miss rate \code{\link{mirt}}.
#'
#' @param spec The decision's specificity value \code{\link{spec}}.
#'
#' @param fart The decision's false alarm rate \code{\link{fart}}.
#'
#' @param N The number of individuals in the population.
#'
#' @param round A Boolean option specifying whether computed frequencies
#' are rounded to integers. Default: \code{round = TRUE}.
#'
#' @param by A character code specifying the perspective (or category by which the population is split into subsets) with 3 options:
#'   \enumerate{
#'   \item \code{"cd"} ... by condition;
#'   \item \code{"dc"} ... by decision;
#'   \item \code{"ac"} ... by accuracy.
#'   }
#'
#' @param area A character code specifying the area of the boxes (or their relative sizes) with
#' 3 options:
#'   \enumerate{
#'   \item \code{"no"} ... all boxes are shown with the same size;
#'   \item \code{"sq"} ... boxes are squares with area sizes scaled proportional to frequencies (default);
#'   \item \code{"hr"} ... boxes are horizontal rectangles with area sizes scaled proportional to frequencies.
#'   }
#'
#' @param p_lbl A character code specifying the type of probability information (on edges) with
#' 4 options:
#'   \enumerate{
#'   \item \code{"nam"} ... names of probabilities;
#'   \item \code{"num"} ... numeric values of probabilities (rounded to 3 decimals, default);
#'   \item \code{"mix"} ... names of essential probabilities, values of complements;
#'   \item \code{"min"} ... minimal labels: names of essential probabilities.
#'   }
#'
#' @param show_accu Option for showing current
#' accuracy metrics \code{\link{accu}} on the margin of the plot.
#'
#' @param w_acc Weighting parameter \code{w} used to compute
#' weighted accuracy \code{w_acc} in \code{\link{comp_accu_freq}}.
#'
#' Various other options allow the customization of text labels and colors:
#'
#' @param title_lbl Text label for current plot title.
#'
#' @param popu_lbl Text label for current population \code{\link{popu}}.
#'
#' @param cond_true_lbl Text label for current cases of \code{\link{cond_true}}.
#' @param cond_false_lbl Text label for current cases of \code{\link{cond_false}}.
#'
#' @param dec_pos_lbl Text label for current cases of \code{\link{dec_pos}}.
#' @param dec_neg_lbl Text label for current cases of \code{\link{dec_neg}}.
#'
#' @param hi_lbl Text label for hits \code{\link{hi}}.
#' @param mi_lbl Text label for misses \code{\link{mi}}.
#' @param fa_lbl Text label for false alarms \code{\link{fa}}.
#' @param cr_lbl Text label for correct rejections \code{\link{cr}}.
#'
#' @param col_txt Color for text labels (in boxes).
#'
#' @param cex_lbl Scaling factor for text labels (in boxes and on arrows).
#'
#' @param col_boxes Colors of boxes (a single color or a vector with named colors matching the number of current boxes).
#' Default: Current color information contained in \code{\link{pal}}.
#'
#' @param col_border Color of borders.
#' Default: \code{col_border = grey(.33, alpha = .99)}.
#'
#' @param lwd Width of arrows.
#'
#' @param box_lwd Width of boxes.
#'
#' @param col_shadow Color of box shadows.
#' Default: \code{col_shadow = grey(.11, alpha = .99)}.
#'
#' @param cex_shadow Scaling factor of shadows (values > 0 showing shadows).
#' Default: \code{cex_shadow = 0}.
#'
#' @return Nothing (NULL).
#'
#' @examples
#' plot_tree()  # frequency tree with current default options (by = "cd")
#'
#' # alternative perspectives:
#' plot_tree(by = "dc")  # tree by decision
#' plot_tree(by = "ac")  # tree by accuracy
#'
#' # See plot_prism for details and additional options.
#'
#' @family visualization functions
#'
#' @seealso
#' \code{\link{plot_prism}} is the new version of this function.
#'
#' @export

## plot_tree: Definition: ---------

plot_tree <- function(prev = num$prev,             # probabilities
                      sens = num$sens, mirt = NA,
                      spec = num$spec, fart = NA,  # was: num$fart,
                      N = freq$N,    # ONLY freq used (so far)
                      ## Options:
                      round = TRUE,  # Boolean: round freq (if computed), default: round = TRUE.
                      by = "cd",     # 4 perspectives: "cd" by condition, "dc" by decision; "ac" by accuracy.
                      area = "no",   # 4 area types: "no" none, "sq" square (default), "hr" horizontal rectangles, "vr" vertical rectangles
                      p_lbl = "num", # 4 probability (edge) label types: "nam" names, "num" numeric values (default), "mix" essential names + complement values, "min" minimal.
                      ## Compute and show accuracy info:
                      show_accu = TRUE,  # compute and show accuracy metrics
                      w_acc = .50,       # weight w for wacc (from 0 to 1)
                      ## Labels:
                      title_lbl = txt$scen_lbl,
                      popu_lbl = txt$popu_lbl,
                      ## Condition labels:
                      cond_true_lbl = txt$cond_true_lbl,
                      cond_false_lbl = txt$cond_false_lbl,
                      ## Decision labels:
                      dec_pos_lbl = txt$dec_pos_lbl,
                      dec_neg_lbl = txt$dec_neg_lbl,
                      ## SDT combinations:
                      hi_lbl = txt$hi_lbl,
                      mi_lbl = txt$mi_lbl,
                      fa_lbl = txt$fa_lbl,
                      cr_lbl = txt$cr_lbl,
                      ## Box settings:
                      col_txt = grey(.01, alpha = .99),  # black
                      cex_lbl = .85,                     # relative text size (of box and arrow labels)
                      col_boxes = pal, # pal[c(1:9)],    # box colors (9 frequencies/boxes/colors)
                      col_border = grey(.33, alpha = .99),  # mid grey
                      ## Widths of arrows and box borders:
                      lwd = 1.5,      # width of arrows
                      box_lwd = 1.5,  # set to 0.001 to show boxes without borders (but =0 yields ERROR)
                      ## Shadows:
                      col_shadow = grey(.11, alpha = .99),  # dark grey
                      cex_shadow = 0 # ,  # [values > 0 show shadows]
                      # ...  # additional paramters
){

  ## (1) Handle deprecated function: ------

  message("Function 'plot_tree' is deprecated; using 'plot_prism' (with by = 'cd'/'dc'/'ac') instead.")

  ## (2) Pass to current function: ------

  plot_prism(prev = prev, sens = sens, spec = spec, mirt = mirt, fart = fart,
             N = N, round = round, by = by, area = area, p_lbl = p_lbl,
             title_lbl = title_lbl, cex_lbl = cex_lbl, col_pal = pal_mod #,
             # ...  # additional paramters
  )

} # plot_tree(...) end.

## eof. -----------
