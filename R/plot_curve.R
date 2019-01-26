## plot_curve.R | riskyr
## 2010 01 26
## plot_curve: Plots different probabilities
## (e.g., PPV, NPV, ppod, acc) as a function
## of prevalence (for given sens and spec).
## -----------------------------------------------

## plot_curve: Documentation ----------

#' Plot curves of selected values (e.g., PPV or NPV)
#' as a function of prevalence.
#'
#' \code{plot_curve} draws curves of selected values
#' (including \code{\link{PPV}}, \code{\link{NPV}})
#' as a function of the prevalence (\code{\link{prev}})
#' for given values of
#' sensitivity \code{\link{sens}} (or
#' miss rate \code{\link{mirt}}) and
#' specificity \code{\link{spec}} (or
#' false alarm rate \code{\link{fart}}).
#'
#' If no prevalence value is provided (i.e., \code{prev = NA}),
#' the desired probability curves are plotted without showing
#' specific points (i.e., \code{show_points = FALSE}).
#'
#' Note that a population size \code{\link{N}} is not needed for
#' computing probability information \code{\link{prob}}.
#' (An arbitrary value can be used when computing frequency information
#' \code{\link{freq}} from current probabilities \code{\link{prob}}.)
#'
#' \code{plot_curve} is a generalization of
#' \code{plot_PV} (see legacy code)
#' that allows plotting additional dependent values.
#'
#' @param prev  The condition's prevalence \code{\link{prev}}
#' (i.e., the probability of condition being \code{TRUE}).
#' If \code{prev = NA}, the curves in \code{what}
#' are plotted without points (i.e., \code{show_points = FALSE}).
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
#' @param spec  The decision's specificity \code{\link{spec}}
#' (i.e., the conditional probability
#' of a negative decision provided that the condition is \code{FALSE}).
#' \code{spec} is optional when its complement \code{fart} is provided.
#'
#' @param fart  The decision's false alarm rate \code{\link{fart}}
#' (i.e., the conditional probability
#' of a positive decision provided that the condition is \code{FALSE}).
#' \code{fart} is optional when its complement \code{spec} is provided.
#'
#' @param what  Vector of character codes that specify the
#' selection of curves to be plotted. Currently available
#' options are \code{c("prev", "PPV", "NPV", "ppod", "acc")}
#' (shortcut: \code{what = "all"}).
#' Default: \code{what = c("prev", "PPV", "NPV")}.
#'
#' @param p_lbl  Type of label for shown probability values,
#' with the following options:
#'   \enumerate{
#'   \item \code{"abb"}: show abbreviated probability names;
#'   \item \code{"def"}: show abbreviated probability names and values (default);
#'   \item \code{"nam"}: show only probability names (as specified in code);
#'   \item \code{"num"}: show only numeric probability values;
#'   \item \code{"namnum"}: show names and numeric probability values;
#'   \item \code{"no"}: hide labels (same for \code{p_lbl = NA} or \code{NULL}).
#'   }
#'
#' @param p_lwd  Line widths of probability curves plotted.
#' Default: \code{p_lwd = 2}.
#'
#' @param what_col  Vector of colors corresponding to the elements
#' specified in \code{what}.
#' Default: \code{what_col = \link{pal}}.
#'
#' @param uc  Uncertainty range, given as a percentage of the current
#' \code{\link{prev}}, \code{\link{sens}}, and \code{\link{spec}} values
#' (added in both directions).
#' Default: \code{uc = .00} (i.e., no uncertainty).
#' Plausible ranges are \code{0 < uc < .25}.
#'
#' @param show_points  Boolean value for showing the point of
#' intersection with the current prevalence \code{\link{prev}}
#' in all selected curves.
#' Default: \code{show_points = TRUE}.
#'
#' @param log_scale  Boolean value for switching from a linear
#' to a logarithmic x-axis.
#' Default: \code{log_scale = FALSE}.
#'
#' @param prev_range Range (minimum and maximum) of \code{\link{prev}} values
#' on x-axis (i.e., values in \code{c(0, 1)} range).
#' Default: \code{prev_range = c(0, 1)}.
#'
#' @param lbl_txt  Labels and text elements.
#' Default: \code{lbl_txt = \link{txt}}.
#'
#' @param title_lbl  Main plot title.
#' Default: \code{title_lbl = NA} (using \code{lbl_txt$scen_lbl}).
#'
#' @param cex_lbl  Scaling factor for the size of text labels
#' (e.g., on axes, legend, margin text).
#' Default: \code{cex_lbl = .85}.
#'
#' @param col_pal  Color palette (if what_col is unspecified).
#' Default: \code{col_pal = \link{pal}}.
#'
#' @param mar_notes  Boolean value for showing margin notes.
#' Default: \code{mar_notes = TRUE}.
#'
#' @param ... Other (graphical) parameters.
#'
#' @examples
#' # Basics:
#' plot_curve()  # default curve plot, same as:
#' # plot_curve(what = c("prev", "PPV", "NPV"), uc = 0, prev_range = c(0, 1))
#'
#' # Showing no/multiple prev values/points and uncertainty ranges:
#' plot_curve(prev = NA)  # default curves without prev value (and point) shown
#' plot_curve(show_points = FALSE, uc = .10)  # curves w/o points, 10% uncertainty range
#' plot_curve(prev = c(.10, .33, .75))  # 3 prev values, with numeric point labels
#' plot_curve(prev = c(.10, .33, .75), p_lbl = "no", uc = .10) # 3 prev, no labels, 10% uc
#'
#' # Provide local parameters and select curves:
#' plot_curve(prev = .2, sens = .8, spec = .6, what = c("PPV", "NPV", "acc"), uc = .2)
#'
#' # Selecting curves: what = ("prev", "PPV", "NPV", "ppod", "acc") = "all"
#' plot_curve(prev = .3, sens = .9, spec = .8, what = "all")  # all curves
#' # plot_curve(what = c("PPV", "NPV"))                  # PPV and NPV
#' plot_curve(what = c("prev", "PPV", "NPV", "acc"))     # prev, PPV, NPV, and acc
#' # plot_curve(what = c("prev", "PPV", "NPV", "ppod"))  # prev, PPV, NPV, and ppod
#'
#' # Visualizing uncertainty (uc as percentage range):
#' plot_curve(prev = .2, sens = .9, spec = .8, what = "all",
#'            uc = .10)  # all with a 10% uncertainty range
#' # plot_curve(prev = .3, sens = .9, spec = .8, what = c("prev", "PPV", "NPV"),
#' #            uc = .05)  # prev, PPV and NPV with a 5% uncertainty range
#'
#' # X-axis on linear vs. log scale:
#' plot_curve(prev = .01, sens = .9, spec = .8)                    # linear scale
#' plot_curve(prev = .01, sens = .9, spec = .8, log_scale = TRUE)  # log scale
#' # Several small prev values:
#' plot_curve(prev = c(.00001, .0001, .001, .01, .05),
#'            sens = .9, spec = .8, log_scale = TRUE)
#'
#' # Zooming in by setting prev_range (of prevalence values):
#' plot_curve(prev = c(.25, .33, .40), prev_range = c(.20, .50),
#'            what = "all", uc = .05)
#'
#' # Probability labels:
#' plot_curve(p_lbl = "abb", what = "all")     # abbreviated names
#' plot_curve(p_lbl = "nam", what = "all")     # names only
#' plot_curve(p_lbl = "num", what = "all")     # numeric values only
#' plot_curve(p_lbl = "namnum", what = "all")  # names and values
#'
#' # Text and color settings:
#' plot_curve(title_lbl = "Tiny text labels", p_lbl = "namnum", cex_lbl = .60)
#' plot_curve(title_lbl = "Specific colors", what = "all",
#'            uc = .1, what_col = c("grey", "red3", "green3", "blue3", "gold"))
#' plot_curve(title_lbl = "Black-and-white print version",
#'            what = "all", col_pal = pal_bwp)
#'
#'
#' @family visualization functions
#'
#' @seealso
#' \code{\link{comp_prob}} computes current probability information;
#' \code{\link{prob}} contains current probability information;
#' \code{\link{comp_freq}} computes current frequency information;
#' \code{\link{freq}} contains current frequency information;
#' \code{\link{num}} for basic numeric parameters;
#' \code{\link{txt}} for current text settings;
#' \code{\link{pal}} for current color settings.
#'
#' @importFrom graphics par
#' @importFrom graphics plot
#' @importFrom graphics axis
#' @importFrom graphics grid
#' @importFrom graphics abline
#' @importFrom graphics curve
#' @importFrom graphics polygon
#' @importFrom graphics points
#' @importFrom graphics text
#' @importFrom graphics title
#' @importFrom graphics mtext
#' @importFrom graphics legend
#'
#' @export

## plot_curve: Definition ----------

plot_curve <- function(prev = num$prev,  # probabilities (3 essential, 2 optional)
                       sens = num$sens, mirt = NA,
                       spec = num$spec, fart = NA,

                       # DVs:
                       what = c("prev", "PPV", "NPV"),  # what curves?  Options: "prev", "PPV", "NPV", "acc", "ppod", "all".

                       # Options:
                       p_lbl = "def",         # prob labels: "def", "nam"/"num"/"namnum", "abb"/"mix"/"min", or NA/NULL/"no" to hide prob labels
                       p_lwd = 2,             # line widths of what.
                       what_col = pal,        # colors for what.
                       uc = .00,              # Uncertainty range (as a percentage around current prev, sens, and spec values)
                       show_points = TRUE,    # show points at current prev?
                       log_scale = FALSE,     # x-axis on log scale?
                       prev_range = c(0, 1),  # Min and max prev values on x-axis (in c(0, 1)). Default: prev_range = c(0, 1).

                       # Text and color:
                       lbl_txt = txt,      # labels and text elements
                       title_lbl = NA,     # plot title
                       cex_lbl = .85,      # scale size of text labels (e.g., on axes, legend, margin text)
                       col_pal = pal,      # color palette

                       # Generic options:
                       mar_notes = TRUE,   # show margin notes?
                       ...                 # other (graphical) parameters
) {

  ## Increase robustness by anticipating and correcting common entry errors: ------

  # p_lbl:
  if ( is.null(p_lbl) || is.na(p_lbl) ) { p_lbl <- "no" }  # sensible zero/NA/NULL case

  # uc:
  if ( is.null(uc) || is.na(uc) ) { uc <- 0 }  # NA/NULL, to avoid error in (uc > 0) below
  if (uc > 0) { uc_dens <- NULL }  # OR: 20 # density of polygon lines (default = NULL: no lines)}

  ## Determine number of prevalence values:
  n_prev <- length(prev)  # == 1 for NA or 1 value; but also allowing vectors > 1.

  ## (0) Compute or collect current probabilities: ----------

  if ( (n_prev == 1) && !is.na(prev) &&  # Standard case: 1 non-NA prev value provided:
       (is_valid_prob_set(prev = prev, sens = sens, mirt = mirt, spec = spec, fart = fart, tol = .01)) ) {

    ## (1) A provided set of probabilities is valid:

    ## (a) Compute the complete quintet of probabilities:
    prob_quintet <- comp_complete_prob_set(prev, sens, mirt, spec, fart)
    sens <- prob_quintet[2] # gets sens (if not provided)
    mirt <- prob_quintet[3] # gets mirt (if not provided)
    spec <- prob_quintet[4] # gets spec (if not provided)
    fart <- prob_quintet[5] # gets fart (if not provided)

    ## (b) Compute LOCAL [freq and] prob based on current parameters (N and probabilities):
    # freq <- comp_freq(prev = prev, sens = sens, spec = spec, N = N, round = round)  # compute freq (default: round = TRUE)
    prob <- comp_prob_prob(prev = prev, sens = sens, spec = spec)

    ## Compute and assign current PVs:
    # cur_PPV <- comp_PPV(prev, sens, spec)  # compute PPV from probabilities
    # cur_NPV <- comp_NPV(prev, sens, spec)  # compute NPV from probabilities

    ## Use current PVs of prob:
    cur_PPV <- prob$PPV  # use PPV from prob
    cur_NPV <- prob$NPV  # use NPV from prob

  } else if ( (n_prev == 1) && is.na(prev) &&  # Case 2a: NO prev value provided:
              is_valid_prob_pair(sens, mirt, tol = .01) &&
              is_valid_prob_pair(spec, fart, tol = .01) ) {

    ## (2a) No prev value was provided, but 2 other probabilities are valid:

    message("No prevalence value provided: Plotting curves without points.")

    if (is.na(sens)) {sens <- comp_complement(mirt)}  # compute sens if only mirt was provided
    if (is.na(spec)) {spec <- comp_complement(fart)}  # compute spec if only fart was provided

    # No point probabilities:
    show_points <- FALSE

    cur_PPV <- NA
    cur_NPV <- NA

  } else if ( (n_prev > 1) && is_prob(prev) &&  # Case 2b: MULTIPLE prev values provided:
              is_valid_prob_pair(sens, mirt, tol = .01) &&
              is_valid_prob_pair(spec, fart, tol = .01) ) {

    ## (2b) Multiple prev values were provided, and 2 other probabilities are valid:

    if (show_points) {
      message("Multiple prevalence values provided: Using numeric values to label points.")
    }

    if (is.na(sens)) {sens <- comp_complement(mirt)}  # compute sens if only mirt was provided
    if (is.na(spec)) {spec <- comp_complement(fart)}  # compute spec if only fart was provided

    ## No point probabilities:
    # show_points <- FALSE

    # Point probabilities need to be determined for each prev value (below):
    cur_PPV <- NA
    cur_NPV <- NA

  } else {  # Case 3: Anything else:

    ## (3) NO valid set of probabilities is provided:

    message("No valid set of probabilities provided: Using global prob to plot curves.")

    ## Use current values of prob:
    prev <- prob$prev
    sens <- prob$sens
    spec <- prob$spec

    n_prev <- length(prev)  # n of prev values (here: length(prob$prev), typically == 1)

    ## Use current PVs of prob:
    cur_PPV <- prob$PPV  # use PPV from prob
    cur_NPV <- prob$NPV  # use NPV from prob

  }

  ## (1) Additional parameters (currently fixed): ----------

  x <- NULL  # "nulling out" to avoid NOTE (no visible binding for global variable ‘x’) in R CMD check!

  lbl_digits <- 1     # n_digits to which numeric probability values (prev, PPV, NPV, ppod, acc) are rounded
  p_lbl_sep <- " = "  # separator for probability point labels (p_lbl)

  ## (2) Define and interpret prev_range: ------

  ## Set prev_range (i.e., min and max values on x-axis) [NOW a function argument]:
  # prev_range <- c(0, 1)    # default prev_range
  # prev_range <- c(0, .25)  # custom prev_range (for zooming in)

  # Verify prev_range argument:
  if (is_prob_range(prev_range)) {
    if ( !is.na(prev) &&
         (any((min(prev) < min(prev_range))) || any((max(prev) > max(prev_range)))) ) {
      message("Some prev value(s) beyond current prev_range.")
    }
  } else {
    message("Using default prev_range = c(0, 1).")
    prev_range <- c(0, 1)  # set to default prev_range
  }

  # Interpret prev_range:
  x_min <- min(prev_range)  # minimum x value
  x_max <- max(prev_range)  # maximum x value

  # Avoid extreme x-values (for linear AND log scale):
  eps <- 10^-6  # some very small number
  if (x_min == 0) { x_min <- (0 + eps) }  # avoid 0
  if (x_max == 1) { x_max <- (1 - eps) }  # avoid 1

  ## Determine range of x-values used for plotting curves:

  if ( (min(prev_range) == 0) && (max(prev_range) == 1) ) {  # default prev_range:

    ## Set x-value (prevalence) range for plotting uncertainty polygons:
    if (uc > 0) {  # plot a polygon:

      ## Select x-value (prevalence) ranges based on current type of scale (log or linear):
      if (log_scale) {

        ## Ranges for x-values (prevalence) of polygon:
        x_lower <- c(10^-6, 10^-5, 10^-4, (1 * 10^-3), (2 * 10^-3), (5 * 10^-3),
                     .01, .02, .05, .10, .15, .25, .30, .50, .60, .70, .75, .80, .85, .90, .95, x_max)  # FIXED log steps (left to right)
        x_upper <- rev(x_lower)                                                # same steps (from right to left)

      } else {  # linear scale:

        ## (b) Define a FIXED vector of x-values for linear scale:
        x_left   <- c(x_min, .005, .010, .015, .020, .033, .040, .050, .060, .075, .100, .125, .150, .200, .250, .333, .400, .450)  # fixed steps (on left)
        x_right  <- rev(1 - x_left)           # reverse complements (from right to left)
        x_linear <- sort(c(x_left, .500, x_right))  # combine and add mid point

        # Ranges for x-values (prevalence) of uc polygon:
        x_lower <- x_linear       # use fixed steps defined in (b)
        x_upper <- rev(x_lower)  # same steps (from right to left)

      } # if (log_scale) etc.

    } # if (uc > 0) etc.

  } else {  # prev_range is NOT the default 0 to 1 range:

    ## Zoom message:
    # message(paste0("Zooming into custom prev_range (from ", min(prev_range), " to ", max(prev_range), ")."))

    ## Set x-value (prevalence) range for plotting uncertainty polygons:
    if (uc > 0) {  # plot a polygon:

      ## Select x-value (prevalence) ranges based on current type of scale (log or linear):
      if (log_scale) {

        # Define log values in range:
        x_lower <- sort(c(x_min, (x_min + 10^-5), (x_min + 10^-4), (x_min + 10^-3), (x_min + 10^-2),
                          (.10 * (x_max - x_min)), (.25 * (x_max - x_min)), (.50 * (x_max - x_min)), (.75 * (x_max - x_min)),
                          x_max))
        x_upper <- rev(x_lower)  # same steps (from right to left)

      } else {  # linear scale:

        # Define n_intervals in range:
        n_intervals <- 20
        x_linear <- seq(from = x_min, to = x_max, length.out = (n_intervals + 1))

        x_lower <- x_linear      # use fixed steps (from left to right)
        x_upper <- rev(x_lower)  # same steps (from right to left)

      } # if (log_scale) etc.

    } # if (uc > 0) etc.

  } # if ( (x_min == 0) && (x_max == 1) ) {  # default prev_range end.


  if (show_points) {

    ## Positional parameters (for raising and shifting p labels):
    if (log_scale) {
      if ( (n_prev == 1) && (!is.na(prev)) ) {
        h_shift <- prev * 2 * (max(prev_range) - min(prev_range))
      } else {
        h_shift <- 0  # ToDo
      }
    } else { # linear scale:
      if (p_lbl == "def" || p_lbl == "namnum" || p_lbl == "nam") {
        h_shift <- .110 * (max(prev_range) - min(prev_range)) # larger horizontal shift
      } else {  # p_lbl == "abb" || p_lbl == "num":
        h_shift <- .060 * (max(prev_range) - min(prev_range))  # smaller horizontal shift
      }
    }
    v_shift <-  3/100
    low_PV  <- 15/100  # threshold value for judging y-value (e.g., PPV or NPV) to be low
    v_raise <- min(c(cur_PPV, cur_NPV)) + .15  # vertical raise of y-prev when y-value (e.g., PPV or NPV) < low_PV

    ## Point appearance:
    pt_pch <- 21    # pch symbol of points
    pt_cex <- 1.6   # cex scaling of points
    pt_lwd <- 1.6   # lwd of point borders

  } # if (show_points) etc.


  ## Colors:

  # Set plot background color:
  par(bg = col_pal[["bg"]])  # col_pal[["bg"]] / "white" / NA (for transparent background)

  # Currently fixed parameters:
  uc_alpha <- .20                     # transparency of uncertainty polygons
  col_axes <- grey(.10, alpha = .99)  # axes
  col_bord <- grey(.10, alpha = .50)  # borders (also of points)


  ## Text labels:

  # Plot title:
  if (is.null(title_lbl)) { title_lbl <- "" }              # adjust NULL to "" (i.e., no title)
  if (is.na(title_lbl)) { title_lbl <- lbl_txt$scen_lbl }  # use scen_lbl as default plot title

  # Text label size:
  cex_lbl_sm <- if (cex_lbl > .50) {cex_lbl - .075} else {cex_lbl}  # slightly smaller than cex_lbl

  legend_lbls <- NULL  # initialize vector
  legend_cols <- NULL
  legend_ltys <- NULL


  ## (2) Define plotting area: ----------

  ## Record graphical parameters (par):
  opar <- par(no.readonly = TRUE)  # all par settings that can be changed.
  on.exit(par(opar))

  ## Define margin areas:
  if (nchar(title_lbl) > 0) { n_lines_top <- 4 } else { n_lines_top <- 3 }
  if (mar_notes) { n_lines_bot <- 4 } else { n_lines_bot <- 4 }

  par(mar = c(n_lines_bot, 4, n_lines_top, 2) + 0.1)  # margins; default: par("mar") = 5.1 4.1 4.1 2.1.
  par(oma = c(0, 0, 0, 0) + 0.1)                      # outer margins; default: par("oma") = 0 0 0 0.


  ## (a) Define steps and labels of x- and y-axes:

  ## x-axis:
  if ( (min(prev_range) == 0) && (max(prev_range) == 1) ) {  # default prev_range:

    if (log_scale) {
      x_seq <- c(10^-5, 10^-4, 10^-3, 10^-2, .10, .25, .50, 1)  # log steps
      x_lbl <- paste0(as_pc(x_seq, n_digits = 5), "%")          # log percentages (rounded to 5 decimals)
      x_ax_lbl <- "Prevalence (on logarithmic scale)"           # log x-axis label
    } else {
      x_seq <- seq(0, 1, by = .10)        # linear steps of 10%
      x_lbl <- paste0(as_pc(x_seq), "%")  # linear percentages
      x_ax_lbl <- "Prevalence"            # linear x-axis label
    }

  } else {  # prev_range is NOT the default 0 to 1 range:

    x_seq <- seq(x_min, x_max, length.out = 11)       # 10 equal intervals
    x_lbl <- paste0(as_pc(x_seq, n_digits = 1), "%")  # percentages
    x_ax_lbl <- "Prevalence (in range)"               # linear x-axis label

  }

  ## y-axis:
  y_seq <- seq(0, 1, by = .10)        # linear steps of 10%
  y_lbl <- paste0(as_pc(y_seq), "%")  # linear percentages
  y_ax_lbl <- "Probability"           # y-axis label

  ## (b) Initialize plot:
  if (log_scale) {
    plot(0, xlim = c(x_min, 1), ylim = c(0, 1), axes = FALSE,
         log = "x",
         ylab = y_ax_lbl, xlab = x_ax_lbl, cex.axis = cex_lbl, type = "n")
  } else { # linear scale:
    plot(0, xlim = c(x_min, x_max), ylim = c(0, 1), axes = FALSE,
         ylab = y_ax_lbl, xlab = x_ax_lbl, cex.axis = cex_lbl, type = "n")
  }

  ## (c) Axes (on 4 sides):
  axis(side = 1, at = x_seq, labels = x_lbl, cex.axis = cex_lbl, cex.lab = (cex_lbl),
       las = 1, pos = 0, tck = -.02, col.axis = col_axes, col.ticks = col_axes)       # x at bottom
  axis(side = 1, at = x_seq, labels = FALSE, cex.axis = cex_lbl, cex.lab = (cex_lbl),
       las = 1, pos = 1, tck = -.01, col.axis = col_axes, col.ticks = col_axes)       # x at top
  axis(side = 2, at = y_seq, labels = y_lbl, cex.axis = cex_lbl, cex.lab = (cex_lbl),
       las = 1, pos = max(min(prev_range), x_min), tck = -.02, col.axis = col_axes, col.ticks = col_axes)   # y at left
  axis(side = 4, at = y_seq, labels = y_lbl, cex.axis = cex_lbl, cex.lab = (cex_lbl),
       las = 1, pos = min(max(prev_range), x_max), tck = -.02, col.axis = col_axes, col.ticks = col_axes)   # y at right

  # print(paste0("y at left: max(min(prev_range), x_min) = ", max(min(prev_range), x_min)))  # debugging
  # print(paste0("y at right: min(max(prev_range), x_max) = ", min(max(prev_range), x_max))) # debugging

  ## (d) Grid:
  grid(col = grey(.80, .80))


  ## (3) Interpret what argument: ----------

  ## (a) handle NA and NULL cases (not needed to handle NA):
  # if ( is.null(what) || all(is.na(what)) ) { what <- NA }    # NA case: NA/"no"/"nil"/"nada" yield same result.

  # (b) express what in lowercase:
  what <- tolower(what)

  # (c) shortcuts for default what options:
  if ("def" %in% what || "default" %in% what ) {
    what <- c("prev", "ppv", "npv")  # default case (dropping non-default parts).
  }

  # (d) shortcuts for all/any what options:
  if ("all" %in% what || "any" %in% what || "else" %in% what )  {
    what <- c("prev", "ppv", "npv", "ppod", "acc")
  }


  ## (4) Plot elements of what: ----------

  ## (a) prev: ----------

  if ("prev" %in% what)  { # prev desired:

    if ( (n_prev == 1) && (!is.na(prev)) ) {  # 1. Standard case: 1 prev value (not NA) provided:

      ## 0. parameters:
      lty_prev <- 2  # prev line type

      ## color:
      if (length(what_col) == length(what)) { # a color vector was specified:
        pos_prev <- which(what == "prev")  # find position of "prev" in what
        col_prev <- what_col[pos_prev]     # use color specified for prev
      } else {
        col_prev <- grey(.50, alpha = .99) # use default color for prev
      }

      ## legend:
      legend_lbls <- c(legend_lbls, "prev")    # add prev label
      legend_cols <- c(legend_cols, col_prev)  # add prev color
      legend_ltys <- c(legend_ltys, lty_prev)  # add prev line type

      ## 0. Mark uncertainty about prev (as polygon/here: rectangle):

      if (uc > 0) {

        ## Color of uncertainty polygon (here: rectangle):
        uc_col  <- make_transparent(col_prev, alpha = uc_alpha)  # grey(.80, .33)

        ## Ranges for x-values (prev) of polygon (here: rectangle):
        x_lower_prev <- c(max(0, (prev - uc * prev)), min((prev + uc * prev), 1)) # only 2 points (left & right)
        x_upper_prev <- rev(x_lower_prev)  # only 2 points (right & left)

        ## Compute upper and lower y-values (0 and 1) corresponding to prev values:
        y_lower_prev <- c(0, 0)  # both points on minimum y-value
        y_upper_prev <- c(1, 1)  # both points on maximum y-value

        ## Plot polygon (here: rectangle):
        xx <- c(x_lower_prev, x_upper_prev)
        yy <- c(y_lower_prev, y_upper_prev)
        polygon(xx, yy, col = uc_col, border = NA, density = uc_dens)

      }

      # Special case: If p_lwd > 1:  Make prev_lwd <- p_lwd/2, else: Use altered value p_lwd:
      if (p_lwd > 1) { prev_lwd <- p_lwd/2 } else { prev_lwd <- p_lwd }

      ## 1. curve: prev as vline
      abline(v = prev, lty = lty_prev, lwd = prev_lwd, col = col_prev)  # prev curve/line

      ## 2. point:
      if (show_points) {

        if ((cur_NPV < low_PV) | (cur_PPV < low_PV)) { # y-pos at v_raise:
          points(x = prev, y = 0 + v_raise, pch = pt_pch, cex = pt_cex, lwd = pt_lwd, col = col_bord, bg = col_prev)  # prev point
        } else { # y-pos at bottom (y = 0):
          points(x = prev, y = 0,           pch = pt_pch, cex = pt_cex, lwd = pt_lwd, col = col_bord, bg = col_prev)  # prev point
        }

        ## 3. label:
        if (p_lbl != "no") {

          # prev_lbl <- paste0("prev = ", as_pc(prev, n_digits = lbl_digits), "%")  # prev label
          prev_lbl <- label_prob(pname = "prev", lbl_type = p_lbl, lbl_sep = p_lbl_sep, cur_prob = prob) # automatic label

          if ((cur_NPV < low_PV) | (cur_PPV < low_PV)) { # y at v_raise:
            if ( (prev < .50) || !(prev > 1 - h_shift) ) {
              text(x = prev + h_shift, y = 0 + v_raise,
                   labels = prev_lbl, col = col_prev, cex = cex_lbl_sm)  # prev on right
            } else {
              text(x = prev - h_shift, y = 0 + v_raise,
                   labels = prev_lbl, col = col_prev, cex = cex_lbl_sm)  # prev on left+
            }
          } else { # y at bottom (y = 0):
            # if ( (prev < h_shift) || !(prev > (1 - h_shift - .05)) ) {  # only depending on prev & h_shift:
            if ( !(prev > (1 - h_shift - .05)) ) {  # only depending on prev & h_shift:
              text(x = prev + h_shift, y = 0 + v_shift,
                   labels = prev_lbl, col = col_prev, cex = cex_lbl_sm)  # prev on right
            } else {
              text(x = prev - h_shift, y = 0 + v_shift,
                   labels = prev_lbl, col = col_prev, cex = cex_lbl_sm)  # prev on left+
            }
          }

        } # if (p_lbl != "no") etc.

      } # if (show_points) etc.

    } else if ( (n_prev == 1) && (is.na(prev)) ) {  # 2. Special case: prev = NA provided:

      ## Do nothing:
      # message("No prevalence value provided (prev = NA).")

    } else if (n_prev > 1) {  # 3. Special case: Multiple prev values provided:

      # message("Plot multiple prev lines:")  # debugging

      # lty range:
      lty_n_prev <- rep(2:6, length = n_prev)

      # color range:
      min_grey_val <- .50   # grey value (for lowest prev value): middle grey
      max_grey_val <- .00   # max. grey (for highest prev value): black
      min_prev <- min(prev)
      max_prev <- max(prev)
      prev_range <- (max_prev - min_prev)  # range of current prev values

      # Set horizontal shift (outside loop):
      if (show_points) {
        h_shift <- 3/100 * (max(prev_range) - min(prev_range))  # small correction factor (due to shorter label)
      }

      for (i in 1:n_prev) {  # Loop through all i prev values prev_i:

        prev_i <- prev[i]  # i-th prev value: prev_i

        # print(paste0("i = ", i, ", p_i = ", prev_i, ": "))  # debugging

        ## 0. parameters:
        lty_prev <- lty_n_prev[i]  # prev line type

        ## color:
        ## NEW: Map current prevalence value to color range:
        cur_prev_pos <- (prev_i - min_prev)/prev_range  # standardized prev position (0 to 1)
        cur_grey_val <- min_grey_val + (cur_prev_pos * (max_grey_val - min_grey_val))  # grey_val weighted by cur_prev_position
        col_prev <- grey(cur_grey_val, alpha = .99)  # use cur_grey_val

        ## legend:
        ## NEW: Provide an entry for each prev value:
        if (prev_i < .01) {
          prev_i_lbl <- paste0("prev = ", as_pc(prev_i, n_digits = 4), "%")  # more specific prev = prev_i label
        } else {
          prev_i_lbl <- paste0("prev = ", as_pc(prev_i), "%")  # specific prev = prev_i label
        }

        legend_lbls <- c(legend_lbls, prev_i_lbl)   # add specific prev label
        legend_cols <- c(legend_cols, col_prev)  # add prev color
        legend_ltys <- c(legend_ltys, lty_prev)  # add prev line type

        ## 0. Mark uncertainty about prev (as polygon/here: rectangle):

        if (uc > 0) {

          ## Color of uncertainty polygon (here: rectangle):
          uc_col  <- make_transparent(col_prev, alpha = uc_alpha)  # grey(.80, .33)

          ## Ranges for x-values (prev) of polygon (here: rectangle):
          x_lower_prev <- c(max(0, (prev_i - uc * prev_i)), min((prev_i + uc * prev_i), 1)) # only 2 points (left & right)
          x_upper_prev <- rev(x_lower_prev)  # only 2 points (right & left)

          ## Compute upper and lower y-values (0 and 1) corresponding to prev values:
          y_lower_prev <- c(0, 0)  # both points on minimum y-value
          y_upper_prev <- c(1, 1)  # both points on maximum y-value

          ## Plot polygon (here: rectangle):
          xx <- c(x_lower_prev, x_upper_prev)
          yy <- c(y_lower_prev, y_upper_prev)
          polygon(xx, yy, col = uc_col, border = NA, density = uc_dens)

        }

        # Special case: If p_lwd > 1:  Make prev_lwd <- p_lwd/2, else: Use altered value p_lwd:
        if (p_lwd > 1) { prev_lwd <- p_lwd/2 } else { prev_lwd <- p_lwd }

        ## 1. curve: prev at prev_i as a vertical line:
        abline(v = prev_i, lty = lty_prev, lwd = prev_lwd, col = col_prev)  # prev curve/line

        if (show_points) {

          ## 2. point:
          points(x = prev_i, y = 0, pch = pt_pch, cex = pt_cex, lwd = pt_lwd, col = col_bord, bg = col_prev)  # prev point prev_i

          ## 3. label:
          if (p_lbl != "no") {

            # Compose label:
            # prev_lbl <- label_prob(pname = "prev", lbl_type = p_lbl, lbl_sep = p_lbl_sep, cur_prob = prob) # automatic label
            prev_lbl <- paste0(as_pc(prev_i, n_digits = lbl_digits), "%")  # Shorter specific prev label for prev_i

            # Position of label:
            if (log_scale) {

              lbl_x <- (prev_i * 2)       # shift as a factor of prev_i
              lbl_y <- (0 + 1/2 * v_shift)  # increase by factor (to avoid low PPV values)

            } else {  # linear scale:

              if (prev_i < .91) {
                lbl_x <- (prev_i + h_shift)  # to right of prev_i
              } else {
                lbl_x <- (prev_i - h_shift)  # to left of prev_i
              }

              lbl_y <- (0 + 1 * v_shift)  # at bottom

            }


            # Print label:
            text(x = lbl_x, y = lbl_y, labels = prev_lbl, col = col_prev, cex = cex_lbl_sm)

          } # if (p_lbl != "no") etc.

        } # if (show_points) etc.

      } # for (i in 1:n_prev) loop.

    } else {  # 4. Unknown case:

      message("Unknown prevalence value (prev) provided.")

    } # if (n_prev == 1) etc.

  } # if ("prev" %in% what) etc.


  ## (b) PPV: ----------

  if ("ppv" %in% what) {

    ## 0. parameters:
    lty_ppv <- 1  # PPV line type (= default)

    ## colors:
    if (length(what_col) == length(what)) { # a color vector was specified:
      pos_ppv <- which(what == "ppv")  # find position of "PPV" in what
      col_ppv <- what_col[pos_ppv]     # use color specified for PPV
    } else {
      col_ppv <- col_pal["ppv"]  # use default color for PPV
    }

    legend_lbls <- c(legend_lbls, "PPV")    # add PPV label
    legend_cols <- c(legend_cols, col_ppv)  # add PPV color
    legend_ltys <- c(legend_ltys, lty_ppv)  # add PPV line type

    ## 0. Mark uncertainty about PPV based on vague values of sens and spec (as polygon):

    if (uc > 0) {

      ## Color of PPV uncertainty polygon:
      uc_col  <- make_transparent(col_ppv, alpha = uc_alpha)  # grey(.80, .33)

      ## Ranges for x-values (prev) of polygon:
      ## (See x_lower and x_upper above.)

      ## Compute upper and lower y-values (PPV) corresponding to prev values:
      y_lower <- comp_PPV(prev = x_lower, sens = max(0, (sens - uc * sens)), spec = max(0, (spec - uc * spec)))  # both sens & spec DEcreased by uc
      y_upper <- comp_PPV(prev = x_upper, sens = min(1, (sens + uc * sens)), spec = min(1, (spec + uc * spec)))  # both sens & spec INcreased by uc

      ## Correction: Limit values to range from 0 to 1:
      # y_lower[y_lower < 0] <- 0  # set y-values < 0 to 0.
      # y_upper[y_upper > 1] <- 1  # set y-values > 1 to 1.

      ## Plot polygon:
      xx <- c(x_lower, x_upper)
      yy <- c(y_lower, y_upper)
      polygon(xx, yy, col = uc_col, border = NA, density = uc_dens)

    }

    ## 1. PPV curve:
    curve(expr = comp_PPV(prev = x, sens, spec), from = x_min, to = x_max, add = TRUE, lty = lty_ppv, lwd = p_lwd, col = col_ppv)  # PPV curve

    ## 2. PPV point:
    if (show_points) {

      if (n_prev == 1) { # (a) plot current point only:

        points(x = prev, y = cur_PPV, pch = pt_pch, cex = pt_cex, lwd = pt_lwd, col = col_bord, bg = col_ppv)  # PPV point

        ## 3. label:
        if (p_lbl != "no") {

          # PPV_lbl <- paste0("PPV = ", as_pc(cur_PPV, n_digits = lbl_digits), "%")  # PPV label
          PPV_lbl <- label_prob(pname = "PPV", lbl_type = p_lbl, lbl_sep = p_lbl_sep, cur_prob = prob)  # automatic label

          if ( (cur_PPV < .75 & !(prev > 1 - h_shift)) || (prev < h_shift) ) {
            text(x = prev + h_shift, y = cur_PPV + v_shift,
                 labels = PPV_lbl, col = col_ppv, cex = cex_lbl_sm)  # PPV on right
          } else {
            text(x = prev - h_shift, y = cur_PPV + v_shift,
                 labels = PPV_lbl, col = col_ppv, cex = cex_lbl_sm)  # PPV on left+
          }

        } # if (p_lbl != "no") etc.

      } else if (n_prev > 1) { # (b) plot points for all prev values:

        # Set horizontal shift (outside loop):
        if (show_points) {
          h_shift <- 4/100 * (max(prev_range) - min(prev_range))  # small correction factor (due to shorter label)
        }

        for (i in 1:n_prev) {  # Loop through all i prev values prev_i:

          prev_i <- prev[i]  # i-th prev value: prev_i

          cur_PPV <- comp_PPV(prev = prev_i, sens = sens, spec = spec)  # compute current point: PPV

          points(x = prev_i, y = cur_PPV, pch = pt_pch, cex = pt_cex, lwd = pt_lwd, col = col_bord, bg = col_ppv)  # PPV point

          if (p_lbl != "no") {

            # Compose label:
            point_lbl <- paste0(as_pc(cur_PPV, n_digits = lbl_digits), "%")  # Shorter specific label for current point

            # Position of label:
            if (log_scale) {
              lbl_x <- (prev_i * 2)  # shift as a factor of prev_i
            } else {  # linear scale:
              lbl_x <- (prev_i + h_shift)  # to right of prev_i
            }
            lbl_y <- cur_PPV - (cur_PPV * v_shift)  # weighted y shift

            # Print label:
            text(x = lbl_x, y = lbl_y, labels = point_lbl, col = col_ppv, cex = cex_lbl_sm)

          } # if (p_lbl != "no") etc.

        } # for (i in 1:n_prev) loop.

      } # (n_prev etc.

    } # if (show_points) etc.

  } # if ("ppv" %in% what) etc.


  ## (c) NPV: ----------

  if ("npv" %in% what) {

    ## 0. parameters:
    lty_npv <- 1  # NPV line type

    ## color:
    if (length(what_col) == length(what)) { # a color vector was specified:
      pos_npv <- which(what == "npv")  # find position of "NPV" in what
      col_npv <- what_col[pos_npv]     # use color specified for NPV
    } else {
      col_npv <- col_pal["npv"]  # use default color for NPV
    }

    # Detect and handle special case of color equality (e.g., pal_bwp):
    if ( all_equal(c(col_pal[["npv"]], col_pal[["ppv"]])) && (length(what_col) != length(what)) ) {
      col_npv <- "grey35"  # distinct NPV color
      lty_npv <- 5         # unique NPV line type (5 = longer dashes)
    }

    legend_lbls <- c(legend_lbls, "NPV")    # add NPV label
    legend_cols <- c(legend_cols, col_npv)  # add NPV color
    legend_ltys <- c(legend_ltys, lty_npv)  # add NPV line type


    ## 0. Mark uncertainty about NPV based on vague values of sens and spec (as polygon):

    if (uc > 0) {

      ## Color of uncertainty polygon:
      uc_col  <- make_transparent(col_npv, alpha = uc_alpha)  # grey(.80, .33)

      ## Ranges for x-values (prev) of polygon:
      ## (See x_lower and x_upper above.)

      ## Compute upper and lower y-values (NPV) corresponding to prev values:
      y_lower <- comp_NPV(prev = x_lower, sens = max(0, (sens - uc * sens)), spec = max(0, (spec - uc * spec)))  # both sens & spec DEcreased by uc
      y_upper <- comp_NPV(prev = x_upper, sens = min(1, (sens + uc * sens)), spec = min(1, (spec + uc * spec)))  # both sens & spec INcreased by uc

      ## Correction: Limit values to range from 0 to 1:
      # y_lower[y_lower < 0] <- 0  # set y-values < 0 to 0.
      # y_upper[y_upper > 1] <- 1  # set y-values > 1 to 1.

      ## Plot polygon:
      xx <- c(x_lower, x_upper)
      yy <- c(y_lower, y_upper)
      polygon(xx, yy, col = uc_col, border = NA, density = uc_dens)

    }

    ## 1. NPV curve:
    curve(expr = comp_NPV(prev = x, sens, spec), from = x_min, to = x_max, add = TRUE, lty = lty_npv, lwd = p_lwd, col = col_npv)  # NPV curve

    ## 2. NPV point:
    if (show_points) {

      if (n_prev == 1) { # (a) plot current point only:

        points(x = prev, y = cur_NPV, pch = pt_pch, cex = pt_cex, lwd = pt_lwd, col = col_bord, bg = col_npv)  # NPV point

        ## 3. label:
        # NPV_lbl <- paste0("NPV = ", as_pc(cur_NPV, n_digits = lbl_digits), "%")  # NPV label
        NPV_lbl <- label_prob(pname = "NPV", lbl_type = p_lbl, lbl_sep = p_lbl_sep, cur_prob = prob) # automatic label

        if ( (cur_NPV > .75) || (prev < h_shift) ) {
          text(x = prev + h_shift, y = cur_NPV + v_shift,
               labels = NPV_lbl, col = col_npv, cex = cex_lbl_sm)  # NPV on right+
        } else {
          text(x = prev - h_shift, y = cur_NPV - v_shift,
               labels = NPV_lbl, col = col_npv, cex = cex_lbl_sm)  # NPV on left-
        }

      } else if (n_prev > 1) { # (b) plot points for all prev values:

        # Set horizontal shift (outside loop):
        if (show_points) {
          h_shift <- 4/100 * (max(prev_range) - min(prev_range))  # small correction factor (due to shorter label)
        }

        for (i in 1:n_prev) {  # Loop through all i prev values prev_i:

          prev_i <- prev[i]  # i-th prev value: prev_i

          cur_NPV <- comp_NPV(prev = prev_i, sens = sens, spec = spec)  # compute current point: NPV

          points(x = prev_i, y = cur_NPV, pch = pt_pch, cex = pt_cex, lwd = pt_lwd, col = col_bord, bg = col_npv)  # NPV point

          if (p_lbl != "no") {

            # Compose label:
            point_lbl <- paste0(as_pc(cur_NPV, n_digits = lbl_digits), "%")  # Shorter specific label for current point

            # Position of label:
            if (log_scale) {
              lbl_x <- (prev_i * 2)  # shift as a factor of prev_i
            } else {  # linear scale:
              lbl_x <- (prev_i + h_shift)  # to right of prev_i
            }
            lbl_y <- cur_NPV + (cur_NPV * v_shift)  # weighted y shift

            # Print label:
            text(x = lbl_x, y = lbl_y, labels = point_lbl, col = col_npv, cex = cex_lbl_sm)

          } # if (p_lbl != "no") etc.

        } # for (i in 1:n_prev) loop.

      } # (n_prev etc.

    } # if (show_points)...

  } # if ("npv" %in% what)...


  ## (d) ppod: ----------

  if ("ppod" %in% what) {

    ## 0. parameters:
    # cur_ppod <- comp_ppod(prev, sens, spec)  # compute current ppod
    cur_ppod <- prob$ppod                      # get ppod from prob
    lty_ppod <- 1                              # ppod line type

    ## color:
    if (length(what_col) == length(what)) { # a color vector was specified:
      pos_ppod <- which(what == "ppod")  # find position of "ppod" in what
      col_ppod <- what_col[pos_ppod]     # use color specified for ppod
    } else {
      col_ppod <- col_pal["dec_pos"]  # use default color for ppod (using "pos")
    }

    # Detect and handle special case of color equality (e.g., pal_bwp):
    if ( all_equal(c("white", col_ppod)) && (length(what_col) != length(what)) ) {
      col_ppod <- "grey50"  # distinct ppod color
      lty_ppod <- 4         # unique ppod line type (4 = dash-dot-dash)
    }

    legend_lbls <- c(legend_lbls, "ppod")    # add NPV label
    legend_cols <- c(legend_cols, col_ppod)  # add NPV color
    legend_ltys <- c(legend_ltys, lty_ppod)  # add NPV line type

    ## 0. Mark uncertainty about ppod based on vague values of sens and spec (as polygon):

    if (uc > 0) {

      ## Color of uncertainty polygon:
      uc_col  <- make_transparent(col_ppod, alpha = uc_alpha)  # grey(.80, .33)

      ## Ranges for x-values (prev) of polygon:
      ## (See x_lower and x_upper above.)

      ## Compute upper and lower y-values (ppod) corresponding to prev values:
      ## NOTE: ppod is INVERSELY related to spec!
      y_lower <- comp_ppod(prev = x_lower, sens = max(0, (sens - uc * sens)), spec = min(1, (spec + uc * spec)))  # NOTE: sens decrease & spec INcrease by uc
      y_upper <- comp_ppod(prev = x_upper, sens = min(1, (sens + uc * sens)), spec = max(0, (spec - uc * spec)))  # NOTE: sens increase & spec DEcrease by uc

      ## Correction: Limit values to range from 0 to 1:
      # y_lower[y_lower < 0] <- 0  # set y-values < 0 to 0.
      # y_upper[y_upper > 1] <- 1  # set y-values > 1 to 1.

      ## Plot polygon:
      xx <- c(x_lower, x_upper)
      yy <- c(y_lower, y_upper)
      polygon(xx, yy, col = uc_col, border = NA, density = uc_dens)

    }

    ## 1. curve:
    curve(expr = comp_ppod(prev = x, sens, spec), from = x_min, to = x_max, add = TRUE, lty = lty_ppod, lwd = p_lwd, col = col_ppod)  # ppod curve

    ## 2. point:
    if (show_points) {

      if (n_prev == 1) { # (a) plot current point only:

        points(x = prev, y = cur_ppod, pch = pt_pch, cex = pt_cex, lwd = pt_lwd, col = col_bord, bg = col_ppod)  # ppod point

        ## 3. label:
        # ppod_lbl <- paste0("ppod = ", as_pc(cur_ppod, n_digits = lbl_digits), "%")  # ppod label
        ppod_lbl <- label_prob(pname = "ppod", lbl_type = p_lbl, lbl_sep = p_lbl_sep, cur_prob = prob) # automatic label

        ## if ( (cur_ppod < .75) || (prev < h_shift) ) {
        if ( (prev < h_shift) || !(prev > (1 - h_shift - .05)) ) {  # only depending on prev & h_shift:
          text(x = prev + h_shift, y = cur_ppod + v_shift,
               labels = ppod_lbl, col = col_ppod, cex = cex_lbl_sm)  # ppod on right+
        } else {
          text(x = prev - h_shift, y = cur_ppod - v_shift,
               labels = ppod_lbl, col = col_ppod, cex = cex_lbl_sm)  # ppod on left-
        }

      } else if (n_prev > 1) { # (b) plot points for all prev values:

        # Set horizontal shift (outside loop):
        if (show_points) {
          h_shift <- 4/100 * (max(prev_range) - min(prev_range))  # small correction factor (due to shorter label)
        }

        for (i in 1:n_prev) {  # Loop through all i prev values prev_i:

          prev_i <- prev[i]  # i-th prev value: prev_i

          cur_ppod <- comp_ppod(prev = prev_i, sens = sens, spec = spec)  # compute current point: ppod

          points(x = prev_i, y = cur_ppod, pch = pt_pch, cex = pt_cex, lwd = pt_lwd, col = col_bord, bg = col_ppod)  # ppod point

          if (p_lbl != "no") {

            # Compose label:
            point_lbl <- paste0(as_pc(cur_ppod, n_digits = lbl_digits), "%")  # Shorter specific label for current point

            # Position of label:
            if (log_scale) {
              lbl_x <- (prev_i * 2)  # shift as a factor of prev_i
            } else {  # linear scale:
              lbl_x <- (prev_i + h_shift)  # to right of prev_i
            }
            lbl_y <- cur_ppod - (cur_ppod * v_shift)  # weighted y shift

            # Print label:
            text(x = lbl_x, y = lbl_y, labels = point_lbl, col = col_ppod, cex = cex_lbl_sm)

          } # if (p_lbl != "no") etc.

        } # for (i in 1:n_prev) loop.

      } # (n_prev etc.

    } # if (show_points)...

  } # if ("ppod" %in% what)...


  ## (e) Overall accuracy (acc): ----------

  if ("acc" %in% what) {

    ## 0. parameters:
    # cur_acc <- comp_acc(prev, sens, spec)  # compute current acc
    cur_acc <- prob$acc                      # get acc from prob
    lty_acc <- 1                             # acc line type

    ## color:
    if (length(what_col) == length(what)) { # a color vector was specified:
      pos_acc <- which(what == "acc")  # find position of "acc" in what
      col_acc <- what_col[pos_acc]     # use color specified for acc (using "hi")
    } else {
      col_acc <- col_pal["dec_cor"]  # use default color for acc (using "cor")
    }

    # Detect and handle special case of color equality (e.g., pal_bwp):
    if ( all_equal(c("white", col_acc)) && (length(what_col) != length(what)) ) {
      col_acc <- "grey20"  # distinct acc color
      lty_acc <- 3         # unique acc line type (3 = dotted)
    }

    legend_lbls <- c(legend_lbls, "acc")    # add acc label
    legend_cols <- c(legend_cols, col_acc)  # add acc color
    legend_ltys <- c(legend_ltys, lty_acc)  # add acc line type

    ## 0. Mark uncertainty about acc based on vague values of sens and spec (as polygon):

    if (uc > 0) {

      ## Color of uncertainty polygon:
      uc_col  <- make_transparent(col_acc, alpha = uc_alpha)  # grey(.80, .33)

      ## Ranges for x-values (prev) of polygon:
      ## (See x_lower and x_upper above.)

      ## Compute upper and lower y-values (acc) corresponding to prev values:
      y_lower <- comp_acc(prev = x_lower, sens = max(0, (sens - uc * sens)), spec = max(0, (spec - uc * spec)))  # both sens & spec DEcreased by uc
      y_upper <- comp_acc(prev = x_upper, sens = min(1, (sens + uc * sens)), spec = min(1, (spec + uc * spec)))  # both sens & spec INcreased by uc

      ## Correction: Limit values to range from 0 to 1:
      # y_lower[y_lower < 0] <- 0  # set y-values < 0 to 0.
      # y_upper[y_upper > 1] <- 1  # set y-values > 1 to 1.

      ## Plot polygon:
      xx <- c(x_lower, x_upper)
      yy <- c(y_lower, y_upper)
      polygon(xx, yy, col = uc_col, border = NA, density = uc_dens)

    }

    ## 1. acc curve:
    curve(expr = comp_acc(prev = x, sens, spec), from = x_min, to = x_max, add = TRUE, lty = lty_acc, lwd = p_lwd, col = col_acc)  # acc curve

    ## 2. acc point:
    if (show_points) {

      if (n_prev == 1) { # (a) plot current point only:

        points(x = prev, y = cur_acc, pch = pt_pch, cex = pt_cex, lwd = pt_lwd, col = col_bord, bg = col_acc)  # acc point

        ## 3. label:
        # acc_lbl <- paste0("acc = ", as_pc(cur_acc, n_digits = lbl_digits), "%")  # acc label
        acc_lbl <- label_prob(pname = "acc", lbl_type = p_lbl, lbl_sep = p_lbl_sep, cur_prob = prob) # automatic label

        if ( (prev < h_shift) || !(prev > (1 - h_shift - .05)) ) {  # only depending on prev & h_shift:
          text(x = prev + h_shift, y = cur_acc + v_shift,
               labels = acc_lbl, col = col_acc, cex = cex_lbl_sm)  # acc on right+
        } else {
          text(x = prev - h_shift, y = cur_acc - v_shift,
               labels = acc_lbl, col = col_acc, cex = cex_lbl_sm)  # acc on left-
        }

      } else if (n_prev > 1) { # (b) plot points for all prev values:

        # Set horizontal shift (outside loop):
        if (show_points) {
          h_shift <- 4/100 * (max(prev_range) - min(prev_range))  # small correction factor (due to shorter label)
        }

        for (i in 1:n_prev) {  # Loop through all i prev values prev_i:

          prev_i <- prev[i]  # i-th prev value: prev_i

          cur_acc <- comp_acc(prev = prev_i, sens = sens, spec = spec)  # compute current point: acc

          points(x = prev_i, y = cur_acc, pch = pt_pch, cex = pt_cex, lwd = pt_lwd, col = col_bord, bg = col_acc)  # ppod point

          if (p_lbl != "no") {

            # Compose label:
            point_lbl <- paste0(as_pc(cur_acc, n_digits = lbl_digits), "%")  # Shorter specific label for current point

            # Position of label:
            if (log_scale) {
              lbl_x <- (prev_i * 2)  # shift as a factor of prev_i
            } else {  # linear scale:
              lbl_x <- (prev_i + h_shift)  # to right of prev_i
            }
            lbl_y <- cur_acc + (cur_acc * v_shift)  # weighted y shift

            # Print label:
            text(x = lbl_x, y = lbl_y, labels = point_lbl, col = col_acc, cex = cex_lbl_sm)

          } # if (p_lbl != "no") etc.

        } # for (i in 1:n_prev) loop.

      } # (n_prev etc.


    } # if (show_points)...

  } # if ("acc" %in% what)...


  ## (5) Title: ----------

  # Define parts:
  if (nchar(title_lbl) > 0) { title_lbl <- paste0(title_lbl, ":\n") }  # put on top (in separate line)

  if (title_lbl == "") {  # if title has been set to "":
    type_lbl <- ""        # assume that no subtitle is desired either
  } else {
    type_lbl <- paste0("Probability curves by prevalence") #, "\n", cur_sens.spec_lbl)
  }

  # Compose label:
  cur_title_lbl <- paste0(title_lbl, type_lbl)

  # Plot title:
  title(cur_title_lbl, adj = 0, line = 1, font.main = 1, cex.main = 1.2)  # (left, raised by +1, normal font)



  ## (6) Margin notes: ----------

  if (mar_notes) {

    ## Text parameters:
    m_col <- grey(.33, .99)  # color
    m_cex <- .85             # size

    ##   (A) on left side (adj = 0): ----

    ## A1. Add freq label:
    show_freq <- FALSE

    if (show_freq) {

      if ( (n_prev == 1) && !is.na(prev) ) {

        N <- 1000  # HACK: compute freq values for some N:
        freq <- comp_freq_prob(prev = prev, sens = sens, spec = spec, N = N, round = TRUE)

        # Create freq label:
        freq_lbl <- make_freq_lbl(hi = freq$hi, mi = freq$mi, fa = freq$fa, cr = freq$cr) # use current freq values
        # mtext(freq_lbl, side = 1, line = 1, adj = 0, col = m_col, cex = m_cex)  # print freq label

      }

    } else {

      freq_lbl <- ""

    } # if (show_freq)...

    ## A2. Condition / p(cond) label:
    cur_cond_lbl <- make_cond_lbl(prev, sens, spec)  # use utility function to format label

    ## Combine 2 labels:
    cur_par_lbl <- paste0(freq_lbl, "\n", cur_cond_lbl)

    mtext(cur_par_lbl, side = 1, line = 2, adj = 0, col = m_col, cex = m_cex)  # print label


    ##   (B) on right side (adj = 1): ----

    ## B1. Note uncertainty:
    if (uc > 0) {

      ## (b) Note uncertainty signal and uc value:
      note <- paste0("Shading marks an uncertainty of ", as_pc(uc), "%")

      note_lbl <- paste0("Note:  ", note, ".")
      mtext(paste0("", note_lbl, ""), side = 1, line = 2, adj = 1, col = m_col, cex = m_cex)

    }

  } # if (mar_notes)


  ## (7) Legend: ----------

  if (length(legend_lbls) > 0) { # there is a curve:
    # legend("bottom", legend = c("PPV", "NPV"),
    #       col = c(col_ppv, col_npv), lty = 1, lwd = p_lwd, cex = 1, bty = "o", bg = "white")

    if (n_prev == 1) {  # Standard case: prev is NA or 1 value:

      add_legend("topright",
                 legend = legend_lbls, lty = legend_ltys, lwd = p_lwd, col = legend_cols,
                 cex = cex_lbl, horiz = FALSE, bty = 'n')

    } else {  # Special case: multiple prev values provided:

      # reduce size:
      p_lwd <- 1
      if (cex_lbl >= .60) (cex_lbl <- (cex_lbl - .10))

      add_legend("topright",
                 legend = legend_lbls, lty = legend_ltys, lwd = p_lwd, col = legend_cols,
                 cex = cex_lbl, horiz = FALSE, bty = 'n')

    } # if (n_prev == 1) etc.

  } # if (length(legend_lbls) > 0) etc.

  ## (8) Return what?: ----------
  # return(pp)     # returns plot
  # return()       # returns nothing
  # return("neat") # returns "..."

  ## Finish: ---------

  invisible()  # restores par(opar)

} # plot_curve etc. end.

## Check: ----------

# ## Basics:
# plot_curve()  # default curves: what = c("prev", "PPV", "NPV")
# plot_curve(what = "all")
# plot_curve(prev = .25, sens = .85, spec = .75, uc = .05)  # parameters with a 5% uc-range
#
# ## Selected curves:
# plot_curve(what = c("PPV", "NPV"))                  # => PPV and NPV
# plot_curve(what = c("prev", "PPV", "NPV", "acc"))   # => prev, PPV, NPV, and acc
# plot_curve(what = c("prev", "PPV", "NPV", "ppod"))  # => prev, PPV, NPV, and acc
#
# ## Visualizing uncertainty (as ranges):
# plot_curve(what = c("prev", "PPV", "NPV"), uc = .10)  # => prev, PPV and NPV, 10% uncertainty range
# plot_curve(prev = .2, sens = .8, spec = .7, what = "all", uc = .1)  # 10% uncertainty range
#
# ## Other options:
# plot_curve(show_points = FALSE)  # => default without points
# plot_curve(what = c("PPV", "NPV"), show_points = TRUE)  # prev not shown.
# plot_curve(col_pal = pal_kn, what = "all", uc = .05)  # color palette
#
# ## prev = NA or vector, prev_range other than c(0, 1):
# plot_curve(prev = NA, what = "all")
# plot_curve(prev = seq(0, 1, .20), what = "all")
# plot_curve(prev = seq(0, .20, .05), what = "all", prev_range = c(0, .25))
# plot_curve(prev = seq(0, .20, .05), what = "all", log_scale = TRUE, prev_range = c(0, .25))
#
# ## linear vs. log scale:
# plot_curve(prev = .01, sens = .9, spec = .8)                     # => linear scale
# plot_curve(prev = .01, sens = .9, spec = .8, log_scale = TRUE)   # => log scale
#
# plot_curve(prev = .0001, sens = .7, spec = .6)                   # => linear scale
# plot_curve(prev = .0001, sens = .7, spec = .6, log_scale = TRUE) # => log scale
#
# ## Probability labels:
# plot_curve(p_lbl = "abb", what = "all")     # abbreviated names
# plot_curve(p_lbl = "nam", what = "all")     # names only
# plot_curve(p_lbl = "num", what = "all")     # numeric values only
# plot_curve(p_lbl = "namnum", what = "all")  # names and values
#
# ## Text labels and colors:
# plot_curve(title_lbl = "Testing smaller text labels", cex_lbl = .60)
# plot_curve(title_lbl = "Testing specific colors", uc = .05,
#            what = "all", what_col = c("grey", "red3", "green3", "blue3", "gold"))
# plot_curve(title_lbl = "Testing color palette", uc = .05,
#            what = "all", col_pal = pal_org)


## Check~1: How do PPV/NPV depend on sens and spec? ------

# comp_NPV(prev = 1, sens = 1, spec = 1) # => NaN

## increasing sens: => increases PPV (by increasing hi):
# comp_PPV(prev = 1/4, sens = 2/4, spec = 3/4) # => 0.40
# comp_PPV(prev = 1/4, sens = 3/4, spec = 3/4) # => 0.50
# comp_PPV(prev = 1/4, sens = 7/8, spec = 3/4) # => 0.54

## increasing spec: => increases PPV (by reducing fa)
# comp_PPV(prev = 1/4, sens = 3/4, spec = 2/4) # => 0.33
# comp_PPV(prev = 1/4, sens = 3/4, spec = 3/4) # => 0.50
# comp_PPV(prev = 1/4, sens = 3/4, spec = 7/8) # => 0.67


## Check~2: How does ppod depend on spec? ------

# comp_ppod(prev = 1/4, sens = 3/4, spec = 2/4) # => 0.56
# comp_ppod(prev = 1/4, sens = 3/4, spec = 3/4) # => 0.38
# comp_ppod(prev = 1/4, sens = 3/4, spec = 7/8) # => 0.28

## increasing spec: => DEcreases ppod (as increasing cr decreases fa)!


## (*) Done: ----------

# Added prev_range argument (to allow zooming into prevalence ranges). [2019 01 25]

# Curves do not need a specific prevalence value: [2019 01 16]
# - allowed computing curves without a specific prevalence (prev = NA)
# - allowed supplying a vector of prevalences (and corresponding labels) to show
#   multiple vertical lines and points on curve(s).

# - Added option uc to show _ranges_ (polygons) to visualize uncertainty.
# - Compute and use local prob for all probability values.
# - Add p_lbl argument (to use label_prob helper).

## (+) ToDo: ----------

# - Revise function to:
#   1. Plot all desired curves (with uc-ranges, and allowing for multiple curves of 1 type)
#   2. Plot vertical prev lines for any prev specified (if desired)
#   3. Plot points on curves for any prev specified (if desired)
#   4. Label curves or points on curves (as specified)

# - Add option to sample multiple points from given _prob_ distributions.

# - Fine-tune positions of labels and legend (on linear vs. log scale)
#   [e.g., for plot_curve(what = "all", log_scale = T, p_lbl = "def", prev = .5) ]

## eof. ------------------------------------------
