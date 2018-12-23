## plot_mosaic.R | riskyr
## 2018 12 20
## Deprecated function: Using plot_area instead.
## -----------------------------------------------

## plot_mosaic: Documentation ----------

#' Plot a mosaic plot of population frequencies.
#'
#' \code{plot_mosaic} drew a mosaic plot that
#' represents the proportions of frequencies in the current
#' population as relatives sizes of rectangular areas.
#'
#' \code{plot_mosaic} is deprecated -- please use \code{\link{plot_area}} instead.
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
#' @param by A character code specifying the perspective (or categories by which the population is split into subsets) with 3 options:
#'   \enumerate{
#'   \item \code{"cddc"} ... by condition x decision;
#'   \item \code{"dccd"} ... by decision x condition;
#'   \item \code{"cdac"} ... by condition x accuracy.
#'   }
#'
#' @param show_accu Option for showing current and exact
#' accuracy metrics \code{\link{accu}} in the plot.
#'
#' @param w_acc Weighting parameter \code{w} used to compute
#' weighted accuracy.
#'
#' @param title_lbl Text label for current plot title.
#'
#' @param col_sdt Colors for cases of 4 essential frequencies.
#' Default: \code{col_sdt = c(pal["hi"], pal["mi"], pal["fa"], pal["cr"])}.
#'
#' @examples
#' plot_mosaic()  # plot with default options
#'
#' @family visualization functions
#'
#' @seealso
#' \code{\link{plot_area}} is the new version of this function.
#'
#' @export

## plot_mosaic: Definition ----------

plot_mosaic <- function(prev = num$prev,             # probabilities
                        sens = num$sens, mirt = NA,
                        spec = num$spec, fart = NA,
                        N = num$N,                   # not needed in Mosaic plot (but used in comp_freq below)
                        ## Options:
                        by = "cddc",  # "cddc", "dccd", "cdac".
                        show_accu = TRUE, # compute and show accuracy metrics
                        w_acc = .50,      # weight w for wacc (from 0 to 1)
                        ## Text and color options: ##
                        title_lbl = txt$scen_lbl,
                        col_sdt = c(pal["hi"], pal["mi"], pal["fa"], pal["cr"])
) {

  ## (1) Handle deprecated function: ------

  message("Function 'plot_mosaic' is deprecated; using 'plot_area' instead.")

  ## (2) Pass to current function: ------

  plot_area(prev = prev, sens = sens, spec = spec, mirt = mirt, fart = fart,
            N = N, by = by,
            title_lbl = title_lbl)

} # plot_mosaic(...) end.

## eof. ---------
