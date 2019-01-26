## plot_plane.R | riskyr
## 2019 01 26
## Plot a 3d-plane of some prob (e.g., PPV or NPV)
## as a function of both sens and spec (for given prev).
## (i.e., generalization of the former plot_PV3d.R).
## -----------------------------------------------

## Plot a 3d-plane of what (e.g., PPV, NPV, ...)
## (using persp):

## plot_plane: Documentation ----------

#' Plot a plane of selected values (e.g., PPV or NPV)
#' as a function of sensitivity and specificity.
#'
#' \code{plot_plane} draws a 3D-plane of selected values
#' (e.g., predictive values \code{\link{PPV}}
#' or \code{\link{NPV}}) as a function of
#' a decision's sensitivity \code{\link{sens}} and
#' specificity value \code{\link{spec}}
#' for a given prevalence (\code{\link{prev}}).
#'
#' \code{plot_plane} is a generalization of
#' \code{plot_PV3d} (see legacy code)
#' that allows for additional dependent values.
#'
#' @param prev The condition's prevalence \code{\link{prev}}
#' (i.e., the probability of condition being \code{TRUE}).
#'
#' @param sens The decision's sensitivity \code{\link{sens}}
#' (i.e., the conditional probability of a positive decision
#' provided that the condition is \code{TRUE}).
#' \code{sens} is optional when its complement \code{mirt} is provided.
#' If \code{sens = NA}, then \code{show_points = FALSE}.
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
#' If \code{spec = NA}, then \code{show_points = FALSE}.
#'
#' @param fart The decision's false alarm rate \code{\link{fart}}
#' (i.e., the conditional probability
#' of a positive decision provided that the condition is \code{FALSE}).
#' \code{fart} is optional when its complement \code{spec} is provided.
#'
#' @param what A character code that specifies one metric
#' to be plotted as a plane. Currently available
#' options are \code{c("PPV", "NPV", "ppod", "acc")}.
#' Default: \code{what = "PPV"}.
#'
#' @param what_col Color for surface facets corresponding to the metric
#' specified in \code{what}.
#' Default: \code{what_col} uses color corresponding to \code{what}
#' in current \code{col_pal}.
#'
#' @param line_col Color for lines between surface facets.
#' Default: \code{line_col = "grey85"}.
#'
#' @param sens_range Range (minimum and maximum) of \code{\link{sens}} values
#' on x-axis (i.e., values in \code{c(0, 1)} range).
#' Default: \code{sens_range = c(0, 1)}.
#'
#' @param spec_range Range (minimum and maximum) of \code{\link{spec}} values
#' on y-axis (i.e., values in \code{c(0, 1)} range).
#' Default: \code{spec_range = c(0, 1)}.
#'
#' @param step_size  Sets the granularity of the
#' \code{\link{sens}}-by-\code{\link{spec}} grid.
#' (in range \code{.01 <= step_size <= 1}).
#' Default: \code{step_size = .05}.
#'
#' @param show_points Boolean option for showing the current value
#' of the selected metric for the current conditions
#' (\code{\link{prev}}, \code{\link{sens}}, \code{\link{spec}})
#' as a point on the plane.
#' Default: \code{show_points = TRUE}.
#'
#' @param point_col Fill color for showing current value on plane.
#' Default: \code{point_col = "yellow"}.
#'
#' @param theta Horizontal rotation angle (used by \code{\link{persp}}).
#' Default: \code{theta = -45}.
#'
#' @param phi Vertical rotation angle (used by \code{\link{persp}}).
#' Default: \code{phi = 0}.
#'
#' @param lbl_txt  Labels and text elements.
#' Default: \code{lbl_txt = \link{txt}}.
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
#' plot_plane()              # => default plot (what = "PPV")
#' # same as:
#' # plot_plane(what = "PPV")  # => plane of PPV
#'
#' plot_plane(what = "NPV")  # => plane of NPV
#' plot_plane(what = "ppod") # => plane of ppod
#' plot_plane(what = "acc")  # => plane of acc
#'
#' # Plane without points or multiple points:
#' plot_plane(prev = .5, sens = NA, spec = NA, what = "ppv")             # plane with 0 points
#' plot_plane(prev = .5, sens = c(.2, .5, .8), spec = .6, what = "npv")  # plane with 3 points
#'
#' # Zooming into specific sens and spec ranges:
#' plot_plane(prev = .02, sens = c(.8, .9), spec = c(.8, .8, .9, .9))  # default ranges
#' plot_plane(prev = .02, sens = c(.8, .9), spec = c(.8, .8, .9, .9),
#'            sens_range = c(.7, 1), spec_range = c(.7, 1), step_size = .02)  # zooming in
#'
#' # Other options:
#' plot_plane(title_lbl = "No point and smaller labels", show_points = FALSE, cex_lbl = .60)
#'
#' plot_plane(title_lbl = "Testing plot colors", what_col = "royalblue4", line_col = "sienna2")
#' plot_plane(title_lbl = "Testing b/w plot", what = "npv", what_col = "white", line_col = "black")
#' plot_plane(title_lbl = "Testing color pal_bwp", col_pal = pal_bwp)
#'
#' plot_plane(step_size = .333, what_col = "firebrick")    # => coarser granularity + color
#' plot_plane(step_size = .025, what_col = "chartreuse4")  # => finer granularity + color
#' plot_plane(what_col = "steelblue4", theta = -90, phi = 50)   # => rotated, from above
#'
#' @family visualization functions
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
#' @importFrom graphics par
#' @importFrom graphics persp
#' @importFrom graphics plot
#' @importFrom graphics axis
#' @importFrom graphics points
#' @importFrom graphics text
#' @importFrom graphics title
#' @importFrom graphics mtext
#'
#' @importFrom grDevices trans3d
#'
#' @export

## plot_plane: Definition ----------

plot_plane <- function(prev = num$prev,             # probabilities (3 essential, 2 optional)
                       sens = num$sens, mirt = NA,
                       spec = num$spec, fart = NA,

                       # DVs:
                       what = "PPV",  # what metric?  Options: "PPV", "NPV", "acc", "ppod".

                       # Options:
                       what_col = pal,       # color for facets of what (i.e., metric specified above)
                       line_col = "grey85",  # color for lines between facets

                       sens_range = c(0, 1), # range of sens values plotted on x. Default: sens_range = c(0, 1).
                       spec_range = c(0, 1), # range of spec values plotted on y. Default: spec_range = c(0, 1).
                       step_size = .05,      # resolution of matrix (sens_values and spec_values)

                       show_points = TRUE,   # show point(s) [resulting from all sens x spec combinations] on plane
                       point_col = "yellow", # fill color for showing current value on plane

                       # Main persp() options [adjustable]:
                       theta = -45,
                       phi = 0,

                       # Text and color:
                       lbl_txt = txt,   # labels and text elements
                       title_lbl = NA,  # plot title
                       p_lbl = "def",   # prob labels: "def", "nam"/"num"/"namnum", "abb"/"mix"/"min", or NA/NULL/"no" to hide prob labels
                       cex_lbl = .85,   # scale size of text labels (e.g., on axes, legend, margin text)
                       col_pal = pal,   # color palette

                       # Generic options:
                       mar_notes = TRUE,  # show margin notes?
                       ...                # other (graphical) parameters
) {

  ## Prepare parameters: ----------

  opar <- par(no.readonly = TRUE)  # all par settings that can be changed.
  on.exit(par(opar))  # par(opar)  # restore original settings

  ## Determine number of sens and spec values:
  n_sens <- length(sens)  # == 1 for NA or 1 value; but also allowing vectors > 1.
  n_spec <- length(spec)

  ## (0) Collect or compute current probabilities: ----------

  if ((n_sens == 1) && !is.na(sens) &&  # Standard case A: 1 non-NA sens value provided:
      (n_spec == 1) && !is.na(spec) &&  # Standard case B: 1 non-NA spec value provided:
      is_valid_prob_set(prev = prev, sens = sens, mirt = mirt, spec = spec, fart = fart, tol = .01)) {

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

  } else if ( ( ((n_sens == 1) && is.na(sens)) ||    # Case 2a_1: (NO sens value provided OR
                ((n_spec == 1) && is.na(spec)) ) &&  # Case 2a_2:  NO spec value provided)
              !is.na(prev) && is_prob(prev) ) {      #    AND a valid prev value provided:

    ## (2a) No sens or no spec value was provided, but prev value is valid:

    message("No sens or no spec value provided: Plotting plane without points.")

    # No point probabilities:
    show_points <- FALSE


  } else if ( ( ((n_sens > 1) && is_prob(sens) && is_prob(spec)) ||     # Case 2b_1: One or more sens values provided AND
                ((n_spec > 1) && is_prob(spec) && is_prob(sens))  ) &&  # Case 2b_2: One or more spec values provided:
              !is.na(prev) && is_prob(prev) ) {                         #    AND a valid prev value provided:

    ## (2b) Multiple sens/spec combinations were provided:

    if (show_points) {
      message("Multiple sens/spec values provided: Plotting multiple points on plane.")
    }


  } else {

    ## (3) NO valid set of probabilities is provided:

    message("No valid set of probabilities provided: Using global prob to plot plane.")

    ## Use current values of prob:
    prev <- prob$prev
    sens <- prob$sens
    spec <- prob$spec

  } # if (is_valid_prob_set(prev...

  ## Increase robustness by anticipating and correcting common entry errors: ------

  ## (1) Text labels: --------

  # Plot title:
  if (is.null(title_lbl)) { title_lbl <- "" }              # adjust NULL to "" (i.e., no title)
  if (is.na(title_lbl)) { title_lbl <- lbl_txt$scen_lbl }  # use scen_lbl as default plot title

  ## (+) Additional parameters (currently fixed):
  p_lbl_sep <- " = "  # separator for probability point labels (p_lbl)


  ## (2) Interpret what argument: ----------

  ## (a) handle NULL case (not needed to handle NA):
  if ( is.null(what) ) { what <- NA }  # NULL case: NA/"no"/"nil"/"nada" yield same result.

  # (b) express what in lowercase:
  what <- tolower(what)

  # (c) shortcuts for default what options:
  if ("def" %in% what || "default" %in% what ) {
    what <- c("ppv")  # default case (dropping non-default parts).
  }

  ## (d) shortcuts for all/any what options:
  # if ("all" %in% what || "any" %in% what || "else" %in% what )  {
  #   what <- c("ppv")
  # }

  ## (e) change invalid what options:
  if ((what %in%  c("ppv", "npv", "acc", "ppod")) == FALSE) {
    message("Invalid what argument: Plotting PPV plane instead.")
    what <- "ppv"
  }

  ## (3) Define plot and margin areas: ----------

  ## Colors: ------

  # Set plot background color:
  par(bg = col_pal[["bg"]])  # col_pal[["bg"]] / "white" / NA (for transparent background)

  ## (A) Define margin areas: ------

  if (nchar(title_lbl) > 0) { n_lines_top <- 2 } else { n_lines_top <- 0 }
  if (mar_notes) { n_lines_bot <- 4 } else { n_lines_bot <- 1 }

  par(mar = c(n_lines_bot, 1, n_lines_top, 1) + 0.1)  # margins; default: par("mar") = 5.1 4.1 4.1 2.1.
  par(oma = c(0, 0, 0, 0) + 0.1)                      # outer margins; default: par("oma") = 0 0 0 0.

  ## (B) Ranges on x- and y-axes: ------

  ## Ranges of sens and spec values:
  # sens_range <- c(0, 1)  # default sens range (on x axis)
  # spec_range <- c(0, 1)  # default spec range (on y axis)

  # Verify range arguments:
  # Verify sens_range:
  if (is_prob_range(sens_range)) {
    if ( !is.na(sens) &&
         (any((min(sens) < min(sens_range))) || any((max(sens) > max(sens_range)))) ) {
      message("Some sens value(s) beyond current sens_range.")
    }
  } else {
    message("Using default sens_range = c(0, 1).")
    sens_range <- c(0, 1)  # set to default sens_range
  }

  # Verify spec_range:
  if (is_prob_range(spec_range)) {
    if ( !is.na(spec) &&
         (any((min(spec) < min(spec_range))) || any((max(spec) > max(spec_range)))) ) {
      message("Some spec value(s) beyond current spec_range.")
    }
  } else {
    message("Using default spec_range = c(0, 1).")
    spec_range <- c(0, 1)  # set to default spec_range
  }

  # Set min/max of sens and spec:
  sens_min <- min(sens_range)
  sens_max <- max(sens_range)

  spec_min <- min(spec_range)
  spec_max <- max(spec_range)

  ## Ensure that step_size is a reasonable value in [0, 1] range:
  step_size_min <- .01
  step_size_max <- min(abs(sens_max - sens_min), abs(spec_max - spec_min))

  if (step_size < step_size_min) {
    message(paste0("Adjusting step_size to the minimum value of ", step_size_min, "."))
    step_size <- step_size_min }
  if (step_size > step_size_max) {
    message(paste0("Adjusting step_size to the maximum value of ", step_size_max, "."))
    step_size <- step_size_max }

  # Range of sens and spec values:
  sens_values <- seq(sens_min, sens_max, by = step_size)  # range of sens values (on x axis)
  spec_values <- seq(spec_min, spec_max, by = step_size)  # range of spec values (on y axis)


  ## (2) Determine current parameters and matrix for selected what metric: ----------

  ## (a) PPV: ------

  if (what == "ppv") {

    ## 1. Parameters:
    if ( (n_sens > 1) || (n_spec > 1) ) {  # multiple points:

      # compute multiple cur_val (PPV):
      cur_val <- comp_PPV(prev, sens, spec)

      # multiple labels:
      PPV_pcs <- paste0(as_pc(cur_val, n_digits = 1), collapse = "/")  # rounded percentages (without %)
      cur_lbl <- paste0("PPV = ", PPV_pcs, "%")  # cur_lbl

    } else { # only 1 point:

      # automatic PPV value:
      cur_val <- prob$PPV

      # automatic label:
      cur_lbl <- label_prob(pname = "PPV", lbl_type = p_lbl, lbl_sep = p_lbl_sep, cur_prob = prob) # automatic label
    }


    type_lbl <- "Probability plane of positive predictive values (PPV)"

    # cur_col color:
    if (length(what_col) == 1) { cur_col <- what_col } else { cur_col <- col_pal["ppv"] }  # cur_col for PPV

    # Detect and handle special case of color equality or similarity (e.g., pal_bwp OR pal_bw):
    if ( (all_equal(c("black", col_pal[["ppv"]])) || all_equal(c("#999999FC", col_pal[["ppv"]])) ) &&
         (length(what_col) != length(what)) ) {
      cur_col <- "grey55"  # distinct PPV color
    }

    z_lbl <- "PPV"    # label of z-axis
    z_lim <- c(0, 1)  # range of z-axis

    ## 2. Matrix:
    ## Hack fix: Prevent values of 0 from occurring:
    eps <- 10^-9  # some very small value
    if (sens_values[1] == 0) {sens_values[1] <- (sens_values[1] + eps)}  # prevent sens = 0
    if (spec_values[1] == 0) {spec_values[1] <- (spec_values[1] + eps)}  # prevent spec = 0

    cur_mat <- comp_prob_matrix(prev = prev, sens_values, spec_values, metric = "PPV", nan_adjust = FALSE)

  } # if (what == "ppv")...


  ## (b) NPV: ------

  if (what == "npv") {

    ## 1. Parameters:
    if ( (n_sens > 1) || (n_spec > 1) ) {  # multiple points:

      # compute multiple cur_val (NPV):
      cur_val <- comp_NPV(prev, sens, spec)

      # multiple labels:
      NPV_pcs <- paste0(as_pc(cur_val, n_digits = 1), collapse = "/")  # rounded percentages (without %)
      cur_lbl <- paste0("NPV = ", NPV_pcs, "%")  # cur_lbl

    } else { # only 1 point:

      # automatic NPV value:
      cur_val <- prob$NPV

      # automatic label:
      cur_lbl <- label_prob(pname = "NPV", lbl_type = p_lbl, lbl_sep = p_lbl_sep, cur_prob = prob) # automatic label
    }

    type_lbl <- "Probability plane of negative predictive values (NPV)"

    # cur_col color:
    if (length(what_col) == 1) { cur_col <- what_col } else { cur_col <- col_pal["npv"] }  # cur_col for NPV

    # Detect and handle special case of color equality or similarity (e.g., pal_bwp OR pal_bw):
    if ( (all_equal(c("black", col_pal[["npv"]])) || all_equal(c("#737373FC", col_pal[["npv"]])) ) &&
         (length(what_col) != length(what)) ) {
      cur_col <- "grey33"  # distinct NPV color
    }

    z_lbl <- "NPV"    # label of z-axis
    z_lim <- c(0, 1)  # range of z-axis

    ## 2. Matrix:
    ## Hack fix: Prevent values of 0 from occurring:
    eps <- 10^-9  # some very small value
    if (sens_values[1] == 0) {sens_values[1] <- (sens_values[1] + eps)}  # prevent sens = 0
    if (spec_values[1] == 0) {spec_values[1] <- (spec_values[1] + eps)}  # prevent spec = 0

    cur_mat <- comp_prob_matrix(prev = prev, sens_values, spec_values, metric = "NPV", nan_adjust = FALSE)

  } # if (what == "npv")...


  ## (c) ppod: ------

  if (what == "ppod") {

    ## 1. Parameters:
    if ( (n_sens > 1) || (n_spec > 1) ) {  # multiple points:

      # compute multiple cur_val (ppod):
      cur_val <- comp_ppod(prev, sens, spec)

      # multiple labels:
      ppod_pcs <- paste0(as_pc(cur_val, n_digits = 1), collapse = "/")  # rounded percentages (without %)
      cur_lbl <- paste0("ppod = ", ppod_pcs, "%")  # cur_lbl

    } else { # only 1 point:

      # automatic ppod value:
      cur_val <- prob$ppod

      # automatic label:
      cur_lbl <- label_prob(pname = "ppod", lbl_type = p_lbl, lbl_sep = p_lbl_sep, cur_prob = prob) # automatic label
    }

    type_lbl <- "Probability plane of the proportion of positive predictions (ppod)"

    # cur_col color:
    if (length(what_col) == 1) { cur_col <- what_col } else { cur_col <- col_pal["dec_pos"] }  # cur_col for ppod (using "pos")

    # Detect and handle special cases of lenient OR strict b+w color palettes (pal_bw OR pal_bwp):
    if ( (all(col_pal == pal_bwp) || all(col_pal == pal_bw)) && (length(what_col) != length(what)) ) {
      cur_col <- "grey44"  # unique ppod color
    }

    # Detect and handle special case of color equality or similarity (e.g., pal_bwp OR pal_bw):
    if ( (all_equal(c("white", col_pal[["dec_pos"]])) || all_equal(c("D9D9D9FC", col_pal[["dec_pos"]])) ) &&
         (length(what_col) != length(what)) ) {
      cur_col <- "grey44"  # distinct ppod color
    }

    z_lbl <- "ppod"   # label of z-axis
    z_lim <- c(0, 1)  # range of z-axis

    ## 2. Matrix:
    # ## Hack fix: Prevent values of 0 from occurring:
    # eps <- 10^-9  # some very small value
    # if (sens_values[1] == 0) {sens_values[1] <- (sens_values[1] + eps)}  # prevent sens = 0
    # if (spec_values[1] == 0) {spec_values[1] <- (spec_values[1] + eps)}  # prevent spec = 0

    cur_mat <- comp_prob_matrix(prev = prev, sens_values, spec_values, metric = "ppod", nan_adjust = FALSE)

  } # if (what == "ppod")...


  ## (d) acc: ------

  if (what == "acc") {

    ## 1. Parameters:
    if ( (n_sens > 1) || (n_spec > 1) ) {  # multiple points:

      # compute multiple cur_val (acc):
      cur_val <- comp_acc(prev, sens, spec)

      # multiple labels:
      acc_pcs <- paste0(as_pc(cur_val, n_digits = 1), collapse = "/")  # rounded percentages (without %)
      cur_lbl <- paste0("acc = ", acc_pcs, "%")  # cur_lbl

    } else { # only 1 point:

      # automatic acc value:
      cur_val <- prob$acc

      # automatic label:
      cur_lbl <- label_prob(pname = "acc", lbl_type = p_lbl, lbl_sep = p_lbl_sep, cur_prob = prob) # automatic label
    }

    type_lbl <- "Probability plane of accuracy values (acc)"

    # cur_col color:
    if (length(what_col) == 1) { cur_col <- what_col } else { cur_col <- col_pal["dec_cor"] }  # cur_col for acc (using "dec_cor")

    # Detect and handle special case of color equality or similarity (e.g., pal_bwp OR pal_bw):
    if ( (all_equal(c("white", col_pal[["dec_cor"]])) || all_equal(c("BFBFBFFC", col_pal[["dec_cor"]])) ) &&
         (length(what_col) != length(what)) ) {
      cur_col <- "grey66"  # distinct acc color
    }

    z_lbl <- "acc"    # label of z-axis
    z_lim <- c(0, 1)  # range of z-axis

    ## 2. Matrix:
    # ## Hack fix: Prevent values of 0 from occurring:
    # eps <- 10^-9  # some very small value
    # if (sens_values[1] == 0) {sens_values[1] <- (sens_values[1] + eps)}  # prevent sens = 0
    # if (spec_values[1] == 0) {spec_values[1] <- (spec_values[1] + eps)}  # prevent spec = 0

    cur_mat <- comp_prob_matrix(prev = prev, sens_values, spec_values, metric = "acc", nan_adjust = FALSE)

  } # if (what == "acc")...


  ## (3) Define persp parameters: ----------

  x <- sens_values
  y <- spec_values
  z <- as.matrix(cur_mat)

  ## Additional persp() parameters (currently fixed):
  d = 1.5
  expand = 1.1
  ltheta = 200
  shade = .300  # default was .25, NULL implies no shade

  line_wd = .60  # lwd parameter (for axes and lines between surface facets); default = 1.

  ## (4) Draw 3D plane (of z) with persp: ----------

  plane <- persp(x, y, z,
                 theta = theta, phi = phi, d = d, expand = expand,  # perspective
                 col = cur_col,     # color of surface facets
                 # border = NA,     # color of line between surface facets (NA disables borders)
                 border = line_col, # color of line between surface facets (NA disables borders)
                 ltheta = ltheta, shade = shade, # illumination
                 ticktype = "detailed",
                 nticks = 6, # at 20% intervals
                 xlab = "sens",
                 ylab = "spec",
                 zlab = z_lbl,
                 zlim = z_lim,
                 cex = cex_lbl,
                 cex.axis = cex_lbl,
                 cex.lab = cex_lbl,
                 # optional:
                 lwd = line_wd  # width of border and axes lines
  )


  ## (5) Add cur_val as point(s) to plot: ----------

  if ( show_points &&
       (sens >= sens_min) && (sens <= sens_max) &&   # sens in sens_range
       (spec >= spec_min) && (spec <= spec_max) ) {  # spec in spec_range

    ## Parameters:
    pt_pch <- 21         # symbol of point
    pt_cex <- 1.5        # scale point size
    pt_lwd <- 1.0        # line width of point border
    pt_col <- point_col  # point color
    bd_col <- grey(.01, alpha = .99)  # point border color

    # Detect and handle special case of color equality or similarity (e.g., pal_bwp OR pal_bw):
    if ( (all_equal(c("white", col_pal[["hi"]])) || all_equal(c("CCCCCCFC", col_pal[["hi"]])) ) &&
         (point_col == "yellow")  # point_col still is default point_col:
    ) {
      pt_col <- "white"  # change point_col to white
    }

    ## Add point(s) to plot:
    proj_pt <- trans3d(sens, spec, cur_val, plane)
    plane <- points(proj_pt, pch = pt_pch, col = bd_col, bg = pt_col, lwd = pt_lwd, cex = pt_cex)

  }


  ## (6) Title: ----------

  # Define parts:
  if (nchar(title_lbl) > 0) { title_lbl <- paste0(title_lbl, ":\n") }  # put on top (in separate line)

  if (title_lbl == "") {  # if title has been set to "":
    type_lbl <- ""        # assume that no subtitle is desired either
  } # else {
  # Use type_lbl defined above!
  # }

  # Compose label:
  cur_title_lbl <- paste0(title_lbl, type_lbl)

  # Plot title:
  title(cur_title_lbl, adj = 0, line = 0, font.main = 1, cex.main = 1.2)  # (left, not raised, normal font)



  ## (7) Margin text: ----------

  if (mar_notes) {

    ## (a) by condition: 3 basic probabilities
    cur_cond_lbl <- make_cond_lbl(prev, sens, spec)  # use utility function to format label
    mtext(cur_cond_lbl, side = 1, line = 3, adj = 1, col = grey(.33, .99), cex = cex_lbl)  # print label

    if (show_points) {
      mtext(cur_lbl, side = 1, line = 2, adj = 1, col = cur_col, cex = (cex_lbl + .05), font = 1)
    }

  } # if (mar_notes)


  ## (8) Return what?: ----------
  # return(p.pv)    # returns plot
  # return()        # returns nothing
  # return("wow!")  # returns "..."

  ##  Finish: ---------

  ## on.exit(par(opar))  # par(opar)  # restore original settings
  invisible() # restores par(opar)

} # plot_plane end.


## Check: ----------

# # Basics:
# plot_plane()  # => current defaults (what = "PPV")
# plot_plane(what = "PPV")  # => plane of PPV
# plot_plane(what = "NPV")  # => plane of NPV
# plot_plane(what = "ppod") # => plane of ppod
# plot_plane(what = "ppod", theta = 45) # => plane of ppod
# plot_plane(what = "acc")  # => plane of acc
#
# # Options:
# plot_plane(show_points = FALSE)  # => no point shown on plane
# plot_plane(step_size = .333, what_col = "firebrick")  # => coarser granularity + color
# plot_plane(step_size = .025, what_col = "chartreuse4")  # => finer granularity + color
# plot_plane(what_col = "steelblue4", theta = -90, phi = 45)  # => rotated, from above
# plot_plane(title_lbl = "Testing plot options")
# plot_plane(title_lbl = "Testing plot colors", what_col = "royalblue4", line_col = "sienna2")
# plot_plane(title_lbl = "Testing plot in b/w", what_col = "white", line_col = "black")

## Note: ----------

## The following persp() parameters are currently fixed:
#
# d = 1.5
# expand = 1.1
# ltheta = 200
# shade = .25
#
# From Documentation:
#
# #' @param d Strength of perspective transformation (used by \code{\link{persp}}).
# #' Values of \code{d > 1} will weaken perspective effects,  values \code{d <= 1} exaggerate it.
# #' Default: \code{d = 1.5}.
# #'
# #' @param expand Expansion factor applied to the z coordinates (used by \code{\link{persp}}).
# #' Default: \code{expand = 1.1}.
# #'
# #' @param ltheta Azimuth direction for shading (used by \code{\link{persp}}).
# #' Default: \code{ltheta = 200}.
# #'
# #' @param shade Shading value (used by \code{\link{persp}}).
# #' Default: \code{shade = .25}.


## OLDER function (2 in 1 plot): ----------

{

  # ## Plot both PPV and NPV planes in 2 adjacent plots
  # ## (combined into 1 plot):
  # plot.PV.planes <- function(env, show.PVpoints = TRUE,
  #                            theta = -45, phi = 0, # persp() parameters [adjustable by user inputs]
  #                            d = 1.5, expand = 1.1, ltheta = 200, shade = .25 # persp() parameters [fixed]
  # ) {
  #
  #   ## Current environment parameters:
  #   name <- env$name
  #   N    <- env$N
  #   prev <- env$prev
  #   sens <- env$sens
  #   spec <- env$spec
  #   source <- env$source
  #
  #   ## Current PPV and NPV values and labels:
  #   ## (a) from current data:
  #   # cur.PPV <- data$PPV # get.PPV(prev, sens, spec)
  #   # cur.NPV <- data$NPV # get.NPV(prev, sens, spec)
  #   # cur.PPV.label <- data$PPV.label # paste0("PPV = ", as_pc(cur.PPV), "%") # paste0("(", as_pc(prev), "%; ", as_pc(cur.PPV), "%)")
  #   # cur.NPV.label <- data$NPV.label # paste0("NPV = ", as_pc(cur.NPV), "%") # paste0("(", as_pc(prev), "%; ", as_pc(cur.NPV), "%)")
  #   ## (b) Compute from scratch:
  #   cur.PPV <- comp_PPV(prev, sens, spec) # data()$PPV
  #   cur.NPV <- comp_NPV(prev, sens, spec) # data()$NPV
  #   cur.PPV.label <- paste0("PPV = ", as_pc(cur.PPV), "%") # paste0("(", as_pc(prev), "%; ", as_pc(cur.PPV), "%)")
  #   cur.NPV.label <- paste0("NPV = ", as_pc(cur.NPV), "%") # paste0("(", as_pc(prev), "%; ", as_pc(cur.NPV), "%)")
  #
  #   ## Ranges on x- and y-axes:
  #   sens_values <- seq(0.0, 1.0, by = .05) # range of sensitivity values
  #   spec_values <- seq(0.0, 1.0, by = .05) # range of specificity values
  #
  #   ## Compute PPV and NPV matrices:
  #   PPV.mat <- comp_prob_matrix(prev, sens_values, spec_values, metric = "PPV")
  #   NPV.mat <- comp_prob_matrix(prev, sens_values, spec_values, metric = "NPV")
  #
  #   ## Graph parameters:
  #   x <- sens_values
  #   y <- spec_values
  #   z.ppv <- as.matrix(PPV.mat)
  #   z.npv <- as.matrix(NPV.mat)
  #   z_lim <- c(0, 1) # range of z-axis
  #   # cur.par.label <- paste0("(",
  #   #                         "prev = ", as_pc(prev), "%, ",
  #   #                         "sens = ", as_pc(sens), "%, ",
  #   #                         "spec = ", as_pc(spec), "%)")
  #   cur.par.label <- paste0("(prev = ", as_pc(prev), "%)")
  #
  #   # Plot 2 plots (adjacent to each other):
  #   {
  #
  #     ## Define special graphic settings:
  #     par(mfrow = c(1, 2)) # Combine 2 plots in 1 row x 2 columns:
  #     par(bg = "white")
  #
  #     ## 3D plot for PPV:
  #     p.ppv <- persp(x, y, z.ppv,
  #                    theta = theta, phi = phi,  d = d, expand = expand,
  #                    col = col.ppv, border = NA, # col.ppv, col.orange.1,
  #                    ltheta = ltheta, shade = shade,
  #                    ticktype = "detailed", nticks = 6,
  #                    xlab = "sens", ylab = "spec", zlab = "PPV", zlim = z_lim,
  #                    main = paste0(cur.PPV.label, "\n", cur.par.label)
  #     )
  #
  #     if (show.PVpoints) { # add cur.PPV to plot:
  #       pmat <- p.ppv
  #       add.PPV <- trans3d(sens, spec, cur.PPV, pmat)
  #       points(add.PPV, pch = 21, col = "grey88", bg = col.ppv, lwd = 1.0, cex = 1.3)
  #     }
  #
  #     ## 3D plot for NPV:
  #     p.npv <- persp(x, y, z.npv,
  #                    theta = theta, phi = phi,  d = d, expand = expand,
  #                    col = col.npv, border = NA, # col.npv, col.blue.1,
  #                    ltheta = ltheta, shade = shade,
  #                    ticktype = "detailed", nticks = 6,
  #                    xlab = "sens", ylab = "spec", zlab = "NPV", zlim = z_lim,
  #                    main = paste0(cur.NPV.label, "\n", cur.par.label)
  #     )
  #
  #     if (show.PVpoints) { # add cur.NPV to plot:
  #       pmat <- p.npv
  #       add.NPV <- trans3d(sens, spec, cur.NPV, pmat)
  #       points(add.NPV, pch = 21, col = "grey88", bg = col.npv, lwd = 1.0, cex = 1.3)
  #     }
  #
  #     ## Remove special graphic settings:
  #     par(mfrow = c(1, 1))
  #   }
  #
  # }
  #
  # ## Check:
  # # plot.PV.planes(env, show.PVpoints = TRUE)

}


## (*) Done: ----------

# - Update options (to use global variables col_pal, lbl_txt etc.)
# - Update title composition and mar_notes option.
# - Add point_col and min/max of step_size range.

# Planes no longer need specific sens+spec values:  [2019 01 22]
# - allowed computing planes without specific sens+spec values
# - allowed supplying a vector of sens+spec values (and corresponding labels) to show multiple points on plane.


## (+) ToDo: ----------

# - Add x_range and z_range arguments (to zoom into specific sens and spec ranges)
#   (as in plot_curve)

# - Add p_lbl option (as in plot_curve) to use label_prob for cur_lbl.
# - Use ... instead re-naming arguments passed on to persp?
# - Generalize to additional metrics (e.g., wacc, mcc, etc.)
# - Change labels for all axes to percentages (as in plot_curve)
# - Pimp plot (titles, axes, grid, colors, transparency)

## eof. ------------------------------------------
