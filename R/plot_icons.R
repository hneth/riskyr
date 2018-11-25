## plot_icons.R | riskyr
## 2018 11 25
## plot_icons: Plot a variety of icon arrays.
## -----------------------------------------------


# Preparation:------------------------------------

# Note: The final function only needs:
# - A vector of identities (colors.)
#   This can be obtained in different ways (e.g., calculation by respective function)
# - A vector of positions (to be generated according to version.)
# - A number of blocks.
# - A size for the icons (cex)
# - etc.?

# Plotting symbols: -----

# Important insight: an icon array is equivalent to an ordered (position constrained) scatterplot.
# All variants of it display the population concerning some property.

# (A) Four types:
#  1. Random position, random colors (typical scatterplot)
#  2. Random position, clustered colors (clustered scatterplot?)
#  3. Fixed positions (sample of positions constrained), random colors (random icon array)
#  4. Fixed positions, clustered colors (typical icon array)

# (B) Two dimensions:
#  1. Position
#  2. Identity

# (C) Translating these dimensions into code:


## plot_icons Documentation: ----------

#' Plot an icon array of a population.
#'
#' \code{plot_icons} plots a population of which individual's
#' condition has been classified correctly or incorrectly as icons
#' from a sufficient and valid set of 3 essential probabilities
#' (\code{\link{prev}}, and
#' \code{\link{sens}} or its complement \code{\link{mirt}}, and
#' \code{\link{spec}} or its complement \code{\link{fart}})
#' or existing frequency information \code{\link{freq}}
#' and a population size of \code{\link{N}} individuals.
#'
#' If probabilities are provided, a new list of
#' natural frequencies \code{\link{freq}} is computed by \code{\link{comp_freq}}.
#' By contrast, if no probabilities are provided,
#' the values currently contained in \code{\link{freq}} are used.
#' By default, \code{\link{comp_freq}} rounds frequencies to nearest integers
#' to avoid decimal values in \code{\link{freq}}.
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
#' A suitable value of \code{\link{N}} is computed, if not provided.
#' If N is 100,000 or greater it is reduced to 10,000
#' for the array types if the frequencies allow it.
#'
#' @param arr_type The icons can be arranged in different ways
#' resulting in different types of displays:
#'
#' \enumerate{
#'
#'   \item \code{arr_type = "array"}: Icons are plotted in a
#'   classical icon array (default).
#'   Icons can be arranged in blocks using \code{block_d}.
#'   The order of filling the array can be customized using
#'   \code{fill_array} and \code{fill_blocks}.
#'
#'   \item \code{arr_type = "shuffledarray"}: Icons are plotted in an
#'   icon array, but positions are shuffled (randomized).
#'   Icons can be arranged in blocks using \code{block_d}.
#'   The order of filling the array can be customized using
#'   \code{fill_array} and \code{fill_blocks}.
#'
#'   \item \code{arr_type = "mosaic"}: Icons are ordered like in a mosaic plot.
#'   The area size displays the relative proportions of their frequencies.
#'
#'   \item \code{arr_type = "fillequal"}: Icons are positioned into equally sized blocks.
#'   Thus, their density reflects the relative proportions of their frequencies.
#'
#'   \item \code{arr_type = "fillleft"}: Icons are randomly filled from the left.
#'
#'   \item \code{arr_type = "filltop"}: Icons are randomly filled from the top.
#'
#'   \item \code{arr_type = "scatter"}: Icons are randomly scattered into the plot.
#'
#' }
#'
#' @param ident_order  The order in which icon identities
#' (i.e., hi, mi, fa, and cr) are plotted.
#' Default: \code{ident_order = c("hi", "mi", "fa", "cr")}
#'
#' @param icon_types Specifies the appearance of the icons as a vector.
#' Accepts values from 1 to 25 (see \code{?points}).
#'
#' @param icon_size Manually specifies the size of the icons via \code{cex}
#' (calculated dynamically by default).
#'
#' @param icon_brd_lwd Specifies the border width of icons (if applicable).
#'
#' @param block_d  The distance between blocks
#' (does not apply to "filleft", "filltop", and "scatter")
#'
#' @param border_d  The distance of icons to the border.
#'
#' Additional options for controlling the arrangement of arrays
#' (for \code{arr_type = "array"} and \code{"shuffledarray"}):
#'
#' @param block_size_row specifies how many icons should be in each block row.
#'
#' @param block_size_col specifies how many icons should be in each block column.
#'
#' @param nblocks_row specifies how many blocks there are in each row.  Is calculated by default.
#'
#' @param nblocks_col specifies how many blocks are there in each column.  Is calculated by default.
#'
#' @param fill_array specifies how the blocks are filled into the array
#' (Options "left" (default) and "top").
#'
#' @param fill_blocks specifies how icons within blocks are filled
#' (Options: \code{fill_blocks = "rowwise"} (default) and \code{fill_blocks = "colwise"})
#'
#' Generic text and color options:
#'
#' @param lbl_txt  Default label set for text elements.
#' Default: \code{lbl_txt = \link{txt}}.
#'
#' @param title_lbl  Text label for current plot title.
#' Default: \code{title_lbl = txt$scen_lbl}.
#'
#' @param cex_lbl  Scaling factor for text labels.
#' Default: \code{cex_lbl = .90}.
#'
#' @param col_pal  Color palette.
#' Default: \code{col_pal = \link{pal}}.
#'
#' @param transparency Specifies the transparency for overlapping icons
#' (not for \code{arr_type = "array"} and \code{"shuffledarray"}).
#'
#' @param mar_notes  Boolean option for showing margin notes.
#' Default: \code{mar_notes = TRUE}.
#'
#' @param ...  Other (graphical) parameters.
#'
#'
#' @return Nothing (NULL).
#'
#' @examples
#' # ways to work:
#' plot_icons(N = 1000)  # icon array with default settings (arr_type = "array")
#' plot_icons(arr_type = "shuffledarray", N = 1000)  # icon array with shuffled IDs
#'
#' plot_icons(arr_type = "mosaic",    N = 1000)  # areas as in mosaic plot
#' plot_icons(arr_type = "fillequal", N = 1000)  # areas of equal size (probability as density)
#' plot_icons(arr_type = "fillleft",  N = 1000)  # icons filled from left to right (in columns)
#' plot_icons(arr_type = "filltop",   N = 1000)  # icons filled from top to bottom (in rows)
#' plot_icons(arr_type = "scatter",   N = 1000)  # icons randomly scattered
#'
#' # Icon symbols:
#' plot_icons(N = 100, icon_types = c(21, 23, 24, 23),
#'                block_size_row = 5, block_size_col = 5, #nblocks_row = 2, nblocks_col = 2,
#'                block_d = 0.5, border_d = 0.9)
#'
#' # Variants:
#' plot_icons(N = 800, arr_type = "array", icon_types = c(21, 22, 23, 24),
#'            block_d = 0.5, border_d = 0.5)
#'
#' plot_icons(N = 1250, sens = 0.9, spec = 0.9, prev = 0.9,
#'                icon_types = c(21, 23, 24, 23),
#'                block_size_row = 10, block_size_col = 5,
#'                nblocks_row = 5, nblocks_col = 5,
#'                block_d = 0.8,
#'                border_d = 0.2,
#'                fill_array = "top")
#'
#' plot_icons(N = 800, arr_type = "shuffledarray", icon_types = c(21, 23, 24, 22),
#'            block_d = 0.5, border_d = 0.5)
#'
#' plot_icons(N = 800, arr_type = "shuffledarray", icon_types = c(21, 23, 24, 22),
#'            icon_brd_col = grey(.33, .99), icon_brd_lwd = 3, cex_lbl = 1.2)
#'
#' plot_icons(N = 800, arr_type = "fillequal", icon_types = c(21, 22, 22, 21),
#'            icon_brd_lwd = .5, cex = 1, cex_lbl = 1.1)
#'
#' # Text and color options:
#' plot_icons(N = 1000, prev = .5, sens = .5, spec = .5, arr_type = "shuffledarray",
#'            title_lbl = "", lbl_txt = txt_TF, col_pal = pal_vir, mar_notes = FALSE)
#'
#' plot_icons(N = 1000, prev = .5, sens = .5, spec = .5, arr_type = "shuffledarray",
#'            title_lbl = "Green vs. red", col_pal = pal_4c, transparency = .5)
#'
#' plot_icons(N = 1000, prev = .5, sens = .5, spec = .5, arr_type = "shuffledarray",
#'            title_lbl = "Shades of blue", col_pal = pal_kn, transparency = .3)
#'
#' @family visualization functions
#'
#' @importFrom graphics par
#' @importFrom graphics plot
#' @importFrom graphics axis
#' @importFrom graphics grid
#' @importFrom graphics points
#' @importFrom graphics text
#' @importFrom graphics title
#' @importFrom graphics mtext
#' @importFrom graphics legend
#'
#' @export

## plot_icons Definition: ----------

plot_icons <- function(prev = num$prev,             # probabilities
                       sens = num$sens, mirt = NA,
                       spec = num$spec, fart = NA,  # was: num$fart,
                       N = freq$N,                  # ONLY freq used

                       # Key option:
                       arr_type = "array",  # needs to be specified if random position but nonrandom ident.
                                            # valid types include: array, shuffled array, mosaic, equal, fillleft, filltop, scatter.

                       # Icon settings:
                       ident_order = c("hi", "mi", "fa", "cr"),
                       icon_types = 22,    # plotting symbols; default: square with border
                       icon_size = NULL,   # size of icons
                       icon_brd_lwd = 1.5, # line width of icons
                       block_d = NULL,     # distance between blocks (where applicable).
                       border_d = 0.1,     # distance of icons to border.

                       # Classic icon arrays only:
                       block_size_row = 10,
                       block_size_col = 10,
                       nblocks_row = NULL,
                       nblocks_col = NULL,

                       fill_array = "left",
                       fill_blocks = "rowwise",

                       # Text and color:
                       lbl_txt = txt,  # labels and text elements
                       title_lbl = txt$scen_lbl,  # main plot title
                       # type_lbls = lbl_txt[c("hi_lbl", "mi_lbl", "fa_lbl", "cr_lbl")],  # 4 SDT cases/combinations
                       cex_lbl = .90,        # size of text labels

                       col_pal = pal,        # color palette
                       transparency = .50,   # alpha level for icons and icon_brd_col
                       # icon_col = col_pal[ident_order], # use one color for each usual arr_type.
                       # icon_brd_col = col_pal["brd"],   # border color of icons [was: grey(.10, .50)]

                       # Generic options:
                       mar_notes = TRUE,   # show margin notes?
                       # show_accu = TRUE,   # Option for showing current accuracy metrics.
                       # w_acc = 0.50,
                       ...                 # other (graphical) parameters (passed to plot_link and plot_ftype_label)

) {

  ## (1) Prepare parameters: ----------

  opar <- par(no.readonly = TRUE)  # all par settings that can be changed.
  on.exit(par(opar))  # par(opar)  # restore original settings

  ## (2) Define plot and margin areas: ----------

  ## Define margin areas:
  n_lines_mar <- 3 + 2  # to accommodate legend
  n_lines_oma <- 0
  par(mar = c(n_lines_mar, 1, 3, 1) + 0.1)  # margins; default: par("mar") = 5.1 4.1 4.1 2.1.
  par(oma = c(n_lines_oma, 0, 0, 0) + 0.1)  # outer margins; default: par("oma") = 0 0 0 0.

  ## (3) Key options and parameters: ----------

  # (a) Get current SDT case labels from lbl_txt:
  type_lbls = lbl_txt[c("hi_lbl", "mi_lbl", "fa_lbl", "cr_lbl")]  # 4 SDT cases/combinations

  # (b) Get current colors from col_pal:
  icon_col     <- col_pal[ident_order]  # use one color for each usual arr_type.
  icon_brd_col <- col_pal["brd"]        # border color of icons [was: grey(.10, .50)]
  icon_brd_col <- make_transparent(icon_brd_col, alpha = (1 - transparency))  # OR: alpha = 2/3

  ## Increase robustness by anticipating and correcting common entry errors: ------

  if ( !is.null(arr_type) && !is.na(arr_type) ) {
    arr_type <- tolower(arr_type)  # express arr_type in lowercase
  }
  if ( arr_type == "def" || arr_type == "default" || is.null(arr_type) || is.na(arr_type) ) { arr_type <- "array" }  # default/null
  if ( arr_type == "shuffled" || arr_type == "shuffle" ) { arr_type <- "shuffledarray" }
  if ( arr_type == "scattered" ) { arr_type <- "scatter" }
  if ( arr_type == "left" ) { arr_type <- "fillleft" }
  if ( arr_type == "top" ) { arr_type <- "filltop" }
  if ( arr_type == "equal" ) { arr_type <- "fillequal" }


  ## Currently fixed parameters:
  xlim = c(0, 1)  # xlim and ylim should currently remain fixed
  ylim = c(0, 1)
  cex = icon_size      # if NULL, cex will be calculated on demand

  # #' @param cex Size of the icons (calculated by default).

  ## Reconstruct logical values from arr_type:
  if (arr_type %in% c("mosaic", "fillequal", "fillleft", "filltop", "scatter")) {

    random.position <-  TRUE
  } else {
    if (arr_type %in% c("array", "shuffledarray")) {
      random.position <- FALSE
    } else {
      stop('Invalid "arr_type" argument in plot_icons. ')
    }
  }

  if (arr_type %in% c("mosaic", "fillequal", "fillleft", "filltop", "array")) {
    random.identities <- FALSE
  } else {
    random.identities <- TRUE
  }


  ## A0.1: Check entered parameters for plausibility ------

  # Check whether random.position and random.identities are logical:
  if ( !(is.logical(random.position) | is.logical(random.identities)) ) {
    stop("random.position and random.identities must be logical!")
  }

  ## A0.2: Check entered parameters for usabililty ------

  # TODO: Either check for missing N or use other comparison.

  ## A0.3: Different routes to col_vec and pch.vec  ------

  # A0.3.1: Calculation from probabilities  ------

  ## (A) If a valid set of probabilities was provided:
  if (is_valid_prob_set(prev = prev, sens = sens, mirt = mirt,
                        spec = spec, fart = fart, tol = .01)) {

    ## (a) Compute the complete quintet of probabilities:
    prob_quintet <- comp_complete_prob_set(prev, sens, mirt, spec, fart)
    sens <- prob_quintet[2] # gets sens (if not provided)
    mirt <- prob_quintet[3] # gets mirt (if not provided)
    spec <- prob_quintet[4] # gets spec (if not provided)
    fart <- prob_quintet[5] # gets fart (if not provided)

    ## (b) Compute cur_freq and popu based on current parameters (N and probabilities):
    cur_freq <- comp_freq(prev = prev, sens = sens, spec = spec, N = N, round = TRUE) # compute cur_freq (with round = TRUE).

  }
  else { # A0.3.2: Using existing frequencies:

    cur_freq <- freq

  }

  # Check size of N.Ist it needed?  Scale down if not needed and greater 100.000:
  ## Specify N:
  N <- cur_freq$N
  ind_lbl <- ""

  if (N >= 100000) {
    # get the minimal N:
    min_N <- riskyr::comp_min_N(prev = prev, sens = sens, spec = spec)

    if (min_N <= 10000) {  # only, if 10000 icons are sufficient:

      exponent <- ((N %/% 100000) %/% 10) + 1  # get exponent dependent on size.
      ind_per_icon <- 10 ^ exponent  # individuals per icon.
      # ind_lbl <- paste0("Icons have been scaled: Each icon represents ", ind_per_icon, " individuals.")
      ind_lbl <- paste0("(Each icon represents ", ind_per_icon, " individuals.)")

      N <- N / (10^exponent)
      cur_freq <- lapply(cur_freq,  function(x) {x / (10^exponent)})  # adjust cur_freq and N.

    }

  }

  # DO SOME CHECKS HERE!?
  ## Determine order:
  if (is.null(names(icon_col))) {
    names(icon_col) <- ident_order
  }

  if (is.null(names(icon_types))) {

    if (length(icon_types) < length(icon_col)) {

      if (length(icon_types) > 1) {warning("Icon types are recycled to number of colors.")}

      icon_types <- rep(icon_types, length.out = length(icon_col))
    }
    names(icon_types) <- names(icon_col)
  }

  ## (c) Compute icon_col from frequencies:
  col_vec <- rep(icon_col[ident_order], times = cur_freq[ident_order])

  ## (d) Compute pch.vec from frequencies:
  pch.vec <- rep(icon_types[ident_order], times = cur_freq[ident_order])



  ## A1 Random position, random colors ------
  if (random.position & random.identities) {

    # 1) Define positions:
    # 1a) draw random positions within plot dimensions:
    posx_vec <- runif(n = N, min = xlim[1], max = xlim[2])
    posy_vec <- runif(n = N, min = ylim[1], max = ylim[2])

    # 2) Randomize vectors:
    rand_ix <- sample(1:length(col_vec), replace = FALSE)  # create random vector.
    col_vec <- col_vec[rand_ix]  # randomize colors and

    if (length(pch.vec) == length(posx_vec)) {
      pch.vec <- pch.vec[rand_ix]  # characters accordingy.
    } else {
      pch.vec <- pch.vec[1]
      warning("pch.vec was not of length N.  Only first element used. ")
    }


  }  # end A1: (random position & random colors)


  ## A2 Random position, clustered colors ------
  if (random.position & !random.identities) {

    # 1b) sort dependent on parameter:
    # options:
    # right: from left to right, top: from top to bottom,
    # equal: in equal spaces of the plot, mosaic: relative to area.

    if (arr_type %in% c("fillleft", "filltop")) {

      # 1a) draw random positions:
      posx_vec <- runif(n = N, min = xlim[1], max = xlim[2])
      posy_vec <- runif(n = N, min = ylim[1], max = ylim[2])

      # Then sort one of the vectors accordingly (presupposes ordered color vector).
      # arr_type: from left to right:
      if (arr_type == "fillleft") {
        posx_vec <- sort(posx_vec)
      }

      # arr_type: from top to bottom:
      if(arr_type == "filltop"){
        posy_vec <- sort(posy_vec)
      }
    } else {  # if in equal or mosaic:

      # Initialize positions:
      posx_vec <- NULL
      posy_vec <- NULL

      # create n = "ident_type" compartments of the plot:
      block_n <- length(unique(col_vec))  # number of blocks for x and y.
      # TODO: not final; they should be distributed.

      # calculate number of observations in each block retaining original order:
      type_n <- sapply(unique(col_vec), function(x) sum(col_vec == x))


      # equal compartments:
      if (arr_type == "fillequal") {  # density varies, area is constant.


        if (is.null(block_d)) {
          block_d <- 0.05
        }

        # determine breakpoints:
        # !!!Currently for square numbers only:
        # TODO: include non-square points (e.g., by enlarging the plot area).
        block_sq <- sqrt(block_n)  # take square root.

        # create list of breakpoints including color types:
        seq_min <- (0:(block_sq - 1)) / block_sq  # of minimal coordinates.
        seq_max <- (1:block_sq) / block_sq

        min_ranges <- expand.grid(x_min = seq_min, y_min = seq_min)  # all combinations of minima.
        max_ranges <- expand.grid(x_max = seq_max, y_max = seq_max)  # all combinations of maxima.

        # add distance between icon blocks:
        global_min <- min(min_ranges)  # get global minimum of minima.
        global_max <- max(max_ranges)  # get global maximum of maxima.
        min_ranges[min_ranges != global_min] <- min_ranges[min_ranges != global_min] + block_d
        # we don't want distance at the global minima nad maxima.
        max_ranges[max_ranges != global_max] <- max_ranges[max_ranges != global_max] - block_d

        # TODO: flipping by swapping x and y or by changing vector of frequencies?
        # TODO: Bind ranges into one object?
        # TODO: notice the overlap!  Use cut?


        # sample the coordinates from the deterimined ranges:
        for(i in 1:nrow(min_ranges)){  # TODO: avoid for-loop!

          minx <- min_ranges$x_min[i]
          maxx <- max_ranges$x_max[i]
          miny <- min_ranges$y_min[i]
          maxy <- max_ranges$y_max[i]
          # TODO: This only holds for equal compartments.

          # sample vectors from compartments:
          posx_vec_i <- runif(n = type_n[i], min = minx, max = maxx)
          posy_vec_i <- runif(n = type_n[i], min = miny, max = maxy)

          posx_vec <- c(posx_vec, posx_vec_i)
          posy_vec <- c(posy_vec, posy_vec_i)
        }

      }

      # mosaic style:
      if (arr_type == "mosaic") {

        block_prop <- type_n / sum(type_n)  # proportion in each compartment.

        prev <- block_prop[1] + block_prop[2]  # TODO: Does this still hold for switched types?
        # define boundaries:
        b1 <- block_prop[1] / (block_prop[1] + block_prop[2])
        b2 <- block_prop [4] / (block_prop[4] + block_prop[3])
        # TODO: This depends on our typical order!  Might be made more transparent and customizable.

        # Quadrant dimensions (with prevalence in y-direction):
        block1 <- c(0, b1, 0, prev)
        block2 <- c(b1, 1, 0, prev)
        block3 <- c(b2, 1, prev, 1)
        block4 <- c(0, b2, prev, 1)

        # TODO: Allow to shuffle components around (using a list?).
        # TODO: not general yet!  How to make it general?  Calculation of area proportions?

        # bind vectors together.
        blocks <- rbind(block1, block2, block3, block4)

        # set distance parameter:
        # block_d may not be half the size of the distance between min and max.
        # for the example of prevalence == 0.15 it may not exceed 0.075.
        diff_dx <- apply(X = blocks[, c(1, 2)], MARGIN = 1, FUN = diff)
        diff_dy <- apply(X = blocks[, c(3, 4)], MARGIN = 1, FUN = diff)

        boundary_d <- min(c(abs(diff_dx), abs(diff_dy))) / 2

        if (is.null(block_d)){

          block_d <- 0.01  # a little messy though...

        }

        if ( block_d >= boundary_d ) {

          block_d <- boundary_d - 0.0001  # a little messy though...

        }

        blocks[, c(1, 3)] <- blocks[, c(1, 3)] + block_d
        blocks[, c(2, 4)] <- blocks[, c(2, 4)] - block_d
        block_n <- sapply(unique(col_vec), function(x) sum(col_vec == x))
        # calculate number of observations in each compartment.
        blocks <- cbind(blocks, block_n)  # bind to matrix.

        for(i in 1:nrow(blocks)){
          minx <- blocks[i, 1]
          maxx <- blocks[i, 2]
          miny <- blocks[i, 3]
          maxy <- blocks[i, 4]
          # TODO: This only holds for equal blocks.

          # sample vectors from blocks:
          posx_vec_i <- runif(n = blocks[i, 5], min = minx, max = maxx)
          posy_vec_i <- runif(n = blocks[i, 5], min = miny, max = maxy)

          posx_vec <- c(posx_vec, posx_vec_i)
          posy_vec <- c(posy_vec, posy_vec_i)

        }
      }
    }

  }  # end: valid arr_type
  # end A2: (random.position & !random.identities)

  if (random.position) {

    if (is.null(cex)) {
      # TODO: How to covary cex with device size & point number?


      cex1 <- ((par("pin")[1] * 10) + 3) / sqrt(length(posx_vec))  # ad hoc formula.
      cex2 <- ((par("pin")[2] * 10) + 3) / sqrt(length(posx_vec))  # ad hoc formula.
      cex <- min(c(cex1, cex2))

    }
  }  # end (random.position)


  ## A3 and A4: Fixed positions ----------
  if (!random.position) {

    # 0. Check arrangement parameters ------
    if (is.null(block_d)) {

      block_d <- 0.4  # set to a default value.
    }

    transparency <- NULL  # set transparency to zero.

    #given:
    # block_size_row
    # block_size_col

    # calculate icons per block:
    icons_per_block <- block_size_row * block_size_col

    # If no number of blocks or cols is given:

    if (is.null(nblocks_row) & is.null(nblocks_col)) {

      # calculate number of blocks required:
      n_blocks <- ceiling(N / icons_per_block)

      blocking_dim <- factors_min_diff(n_blocks)  # get the dimensions.

      # dependent on pin:
      dim_in <- par("pin")  # get dimensions of plotting region.

      if( dim_in[1] >= dim_in[2] ) {  # if x greater y:

        nblocks_row <- blocking_dim[2]  # larger in x dimension (cols).
        nblocks_col <- blocking_dim[1]  # smaller in y dimension (rows).

      } else {
        nblocks_row <- blocking_dim[1]  # smaller in x dimension (cols).
        nblocks_col <- blocking_dim[2]  # larger in y dimension (rows).
      }

    } else {  # if at least one of both is given:

      # TODO: Provide some testing, whether given numbers of blocks are valid!
      test_N <- nblocks_row * nblocks_col * icons_per_block

      if ( test_N < N) {
        stop("The number of blocks and columns is too small to accomodate the population.")
      }


      if (is.null(nblocks_row)) {  # if nblocks_row is not given:

        nblocks_row <- n_blocks / nblocks_col  # calculate number of blocks per row.

      }

      if (is.null(nblocks_col)) {  # if ncol_rows is not given:

        nblocks_col <- n_blocks / nblocks_row  # calculate number of blocks per column.

        # TODO: Change naming scheme!

      }

      # calculate number of blocks required:
      n_blocks <- nblocks_row * nblocks_col

    }

    # calculate total ncols and nrows:
    ncols <- block_size_row * nblocks_row
    nrows <- block_size_col * nblocks_col

    # Given a default of 10x10 blocks:
    #N / (block_size_row * block_size_col)


    # 1. Define positions:

    # find maximum for the positions given the units icons are moved:
    # find a monotonically increasing sequence, resulting in exactly the endpoint of xlim/ylim.
    # For x:
    max_posx <- ((ncols - 1) * xlim[2]) - (block_d * (nblocks_row - 1)) - border_d
    min_posx <- xlim[1] + border_d
    adj_posx <- seq(min_posx, max_posx, length.out = ncols)

    # For y:
    max_posy <- ((nrows - 1) * ylim[2]) - (block_d * (nblocks_col - 1)) - border_d
    min_posy <- ylim[1] + border_d
    adj_posy <- seq(max_posy, min_posy, length.out = nrows)

    # create position matrices:
    pos_mx <- matrix(adj_posx, nrow = nrows, ncol = ncols, byrow = TRUE)
    pos_my <- matrix(adj_posy, nrow = nrows, ncol = ncols)

    # add  a sequence to the x matrix:
    # For x:
    seqx_off <- seq(0, (nblocks_row - 1) * block_d, by = block_d)
    # get the sequence of offsets for icons in each block.
    seqx <- rep(seqx_off, each = block_size_row)
    # repeat this sequence by block size so every icon is affected.
    pos_mx <- pos_mx + rep(seqx, each = nrow(pos_mx))
    # do so for every row in the matrix.

    # For y:
    seqy_off <- seq((nblocks_col - 1) * block_d, 0, by = -block_d)  # create sequence of number to add.
    seqy <- rep(seqy_off, each = block_size_col)  # repeat to number of rows.
    pos_my <- pos_my + seqy  # will be repeated for each column anyways.


    # Plotting preparations: ------

    # save into respective vectors and norm on 0,1 space.
    posx_vec <- pos_mx / (ncols - 1)
    posy_vec <- pos_my / (nrows - 1)

    ## TODO: Not in region anymore --> change plot dimensions or decrease standard distance.

    ## Plotting dimensions for testing:
    # plotx_dim <- c(-0.1, 1.1)
    # ploty_dim <- c(-0.1, 1.1)


    if (!random.identities) {  # sort colors according to input.

      ## For A4 (fixed positions and clustered identities) only:
      ## 2. Color sorting:
      ## Create block information:
      seq_block <- 1:n_blocks  # create sequence of block positions.

      ## Determine, whether blocks are used colwise or rowwise:
      # fill_array <- "left"  # alternatively: "rowwise"

      ## If blocks are to be filled in x direction:
      if (fill_array == "left"){

        seq_blockx <- rep(seq_block, each = block_size_row)
        # create sequence repeted to the number of cols (can be changed to number of rows).
        mat_block <- matrix(seq_blockx, ncol = ncols, byrow = TRUE)
        # create a matrix from it.
        ind_block <- rep(1:nrow(mat_block), each = block_size_col)  # create index to repeat matrix.
        mat_block <- mat_block[ind_block, ]
        # repeat each row of the matrix to the number of rows.
      }

      ## If blocks are to be filled in y direction:
      if (fill_array == "top"){

        seq_blocky <- rep(seq_block, each = block_size_col)
        # create sequence repeted to the number of cols (can be changed to number of rows).
        mat_block <- matrix(seq_blocky, nrow = nrows, byrow = FALSE)
        # create a matrix from it.
        ind_block <- rep(1:ncol(mat_block), each = block_size_row)  # create index to repeat matrix.
        mat_block <- mat_block[ , ind_block]
        # repeat each row of the matrix to the number of columns.
      }

      ## Determine, whether blocks (within) are filled col- or rowwise:
      # fill_blocks <- "colwise"

      ## Sort colors accordingly:
      ## TODO: Find out WHY ON EARTH order(order()) works!

      if (fill_blocks == "rowwise"){
        order_mat <- order(order(t(mat_block)))  # matrix has to be transposed to get the rows.
        m <- matrix(order_mat, nrow = nrows, ncol = ncols, byrow = TRUE)  # This is the "rowwise witin blocks" version.

        # if (fill_array == "top") { m <- t(m) }
      }

      if (fill_blocks == "colwise") {
        order_mat <- order(order(mat_block))
        m <- matrix(order_mat, nrow = nrows, ncol = ncols, byrow = FALSE)  # This is the "colwise within blocks" version.

        # if (fill_array == "top") { m <- t(m) }
      }

      ## If the color vector already has the appropriate length:
      if (length(col_vec) == length(m)) {
        col_vec <- col_vec[m]  # order the color vector.
      }

      if (length(pch.vec) == length(m)) {
        pch.vec <- pch.vec[m]  # order the character vector.
      }


      ## If the color vector is too short:
      ## TODO: what to do if too long?
      if (length(col_vec) < length(m)) {

        len_diff <- length(pos_mx) - length(col_vec)  # calculate difference.
        col_vec <- c(col_vec, rep(NA, len_diff))  # enlarge the color vector.
        col_vec <- col_vec[m]  # order this color vector.

        ## Mute the respective positions:
        posx_vec[is.na(col_vec)] <- NA  # set NA x positions...
        posy_vec[is.na(col_vec)] <- NA  # ... and y positions.

      }

      if (length(pch.vec) < length(m) & length(pch.vec) > 1) {

        len_diff <- length(pos_mx) - length(pch.vec)  # calculate difference.
        pch.vec <- c(pch.vec, rep(NA, len_diff))  # enlarge the color vector.

        ## Mute the respective positions:
        pch.vec <- pch.vec[m]  # order character vector.
        posx_vec[is.na(pch.vec)] <- NA  # set NA x positions...
        posy_vec[is.na(pch.vec)] <- NA  # ... and y positions.

      }

    }  # end fixed identities (A4).


    ## For A3:
    if (random.identities) {

      rand_ix <- sample(1:length(col_vec), replace = FALSE)  # random index.
      col_vec <- col_vec[rand_ix]  # sample from the vector of colors.

      if (length(pch.vec) > 1) {
        pch.vec <- pch.vec[rand_ix]  # analog sample from character vector.
      }


      ## If the color vector is too short.
      if (length(col_vec) < (ncols * nrows) & length(col_vec) > 1) {

        ## for colors:
        len_diff <- (ncols * nrows) - length(col_vec)
        if (length(icon_brd_col) > 1) {
          icon_brd_col <- c(icon_brd_col, rep(NA, len_diff))
        }

        col_vec <- c(col_vec, rep(NA, len_diff))

      }

      ## If the character vector is too short:
      if (length(pch.vec) < (ncols * nrows) & length(pch.vec) > 1) {

        ## for colors:
        len_diff <- (ncols * nrows) - length(pch.vec)
        pch.vec <- c(pch.vec, rep(NA, len_diff))

      }


    }  # end random identities (A3).

    ## Adjust cex dynamically:
    if (is.null(cex)) {
      ## TODO: How to covary cex with device size & point number?

      cex1 <- ((par("pin")[1] * 10) + 3) / ncols
      cex2 <- ((par("pin")[2] * 10) + 3) / nrows
      cex <- min(c(cex1, cex2))

      # still not optimal...

      # cex <- 1.5 - (min(par("pin")) / N)  # this latter term likely plays some role...
    }


  }  # end A3 and A4 (fixed positions).


  ## B. Plotting ------

  ## TODO: Add text!

  if (any(!pch.vec %in% c(NA, 21:25))) {
    # if any of the plotting characters is not in the ones with border,
    # omit border and color accordingly.

    icon_brd_col <- col_vec

  }

  ## 3) Plot:
  plot(x = 1,
       xlim = xlim, ylim = ylim,
       type = "n", xlab = "", ylab = "", xaxt = "n", yaxt = "n",
       bty = "o", fg = "grey")

  ## 3a) Set plotting character:
  # pch <- 22  # filled square as default.
  # cex <- 0.5

  if (!is.null(transparency)) {
    col_vec <- adjustcolor(col_vec, alpha.f = 1 - transparency)
  }

  ## Additional information:
  points(x = posx_vec, y = posy_vec,  # positions.
         ## visual details:
         pch = pch.vec, col = icon_brd_col, bg = col_vec, lwd = icon_brd_lwd, cex = cex)


  ## Legend: -----

  if (sum(nchar(type_lbls)) > 0) {
    # reorder labels:
    names(type_lbls) <- c("hi", "mi", "fa", "cr")
    type_lbls <- type_lbls[ident_order]
  }

  legend(x = xlim[2] / 2, y = ylim[1] - (ylim[2] / 20),
         legend = type_lbls,
         horiz = TRUE, bty = "n",
         pt.bg = icon_col, pch = icon_types,
         cex = cex_lbl, xjust = 0.5, xpd = TRUE)
  ## TODO: fixed order of legend?


  ## Title: -----

  # Define parts:
  if (is.null(title_lbl)) { title_lbl <- "" }  # adjust NULL to "" (i.e., no title)
  if (is.na(title_lbl)) { title_lbl <- lbl_txt$scen_lbl }  # use scen_lbl as default plot title
  if (nchar(title_lbl) > 0) { title_lbl <- paste0(title_lbl, ":\n") }  # put on top (in separate line)

  if (title_lbl == "") {  # if title has been set to "":
    type_lbl <- ""        # assume that no subtitle is desired either
  } else {
    type_lbl <- paste0("Icon array") # , "(N = ", N, ")") # plot name: icon array.
  }

  # Compose label:
  cur_title_lbl <- paste0(title_lbl, type_lbl)

  # Plot title:
  title(cur_title_lbl, adj = 0, line = +1, font.main = 1, cex.main = 1.2)  # (left, raised by +1, normal font)


  ## Margins: ------

  ## OLD code:
  # if (mar_notes) {
  #
  #   ## (a) by condition: 3 basic probabilities
  #   cur_cond_lbl <- make_cond_lbl(prev, sens, spec)  # use utility function to format label
  #   mtext(cur_cond_lbl, side = 1, line = 2, adj = 0, col = grey(.33, .99), cex = .85)  # print label
  #
  #   # (b) by decision:
  #   ppod <- comp_ppod(prev, sens, spec)  # compute ppod etc.
  #   PPV <- comp_PPV(prev, sens, spec)
  #   NPV <- comp_NPV(prev, sens, spec)
  #
  #   cur_dec_lbl <- make_dec_lbl(ppod, PPV, NPV)  # use utility function to format label
  #   mtext(cur_dec_lbl, side = 1, line = 3, adj = 0, col = grey(.33, .99), cex = .85)  # print label
  #
  #   ## (c) Accuracy: Compute and show accuracy metrics
  #
  #   if (show_accu) {
  #
  #     # (0) Get 4 essential freq from cur_freq (computed above with round = TRUE):
  #     n_hi <- cur_freq$hi
  #     n_mi <- cur_freq$mi
  #     n_fa <- cur_freq$fa
  #     n_cr <- cur_freq$cr
  #
  #     # (1) Compute accuracy info based on current freq (which may be rounded OR not rounded):
  #     cur_accu <- comp_accu_freq(hi = n_hi, mi = n_mi, fa = n_fa, cr = n_cr, w = w_acc)
  #
  #     # Note: If freq are NOT rounded, then
  #     #       cur_accu <- comp_accu_prob(prev = prev, sens = sens, spec = spec, w = w_acc)
  #     #       would yield the same results.
  #
  #     # (2) Make label:
  #     cur_accu_lbl <- make_accu_lbl(acc = cur_accu$acc, w = w_acc, wacc = cur_accu$wacc, mcc = cur_accu$mcc)  # use utility function
  #
  #     # (3) Mark IF accu was based on rounded freq:
  #     # if (round) {  # freq were rounded to compute cur_freq above:
  #     cur_accu_lbl <- paste0("*", cur_accu_lbl, " (rounded)")
  #     # }
  #
  #     # (4) Plot label:
  #     mtext(cur_accu_lbl, side = 1, line = 2, adj = 1, col = grey(.33, .99), cex = .85)  # print label
  #
  #   } # if (show_accu)...
  #
  #
  #   ## (d) Note scaling:
  #   mtext(ind_lbl, side = 1, line = 3, adj = 1, col = grey(.11, .99), cex = .85)  # print label
  #
  # } # if (mar_notes) etc.

  ## NEW code:
  if (mar_notes) {

    # Note:
    note_lbl <- ""  # initialize
    # if ( (area != "no") && (scale == "f") ) { # Note area type and scaling by f:
    #   note_lbl <- label_note(area = area, scale = scale)
    # }

    plot_mar(show_freq = TRUE, show_cond = TRUE, show_dec = TRUE,
             show_accu = TRUE, accu_from_freq = FALSE, # TRUE,
             note = note_lbl,
             cur_freq = freq, cur_prob = prob, cur_txt = lbl_txt)

  } # if (mar_notes)


  ##   Finish: ---------

  # on.exit(par(opar))  # par(opar)  # restore original settings
  invisible() # restores par(opar)

} # plot_icons end.


## Check: ----------

# # ways to work:
# plot_icons()  # => plots icon array for default population (with default arr_type = "array")
# plot_icons(arr_type = "shuffledarray")  # => icon array with shuffled IDs
#

# plot_icons(icon_types = c(21, 23, 24, 23),
#                block_size_row = 5, block_size_col = 5, #nblocks_row = 2, nblocks_col = 2,
#                block_d = 0.5, border_d = 0.9)

# plot_icons(arr_type = "mosaic",    N = 1000)  # => areas as in mosaic plot
# plot_icons(arr_type = "fillequal", N = 1000)  # => areas of equal size (density reflects probability)
# plot_icons(arr_type = "fillleft",  N = 1000)  # => icons filled from left to right (in columns)
# plot_icons(arr_type = "filltop",   N = 1000)  # => icons filled from top to bottom (in rows)
#
# plot_icons(arr_type = "scatter",   N = 1000)  # => icons randomly scattered.
#

# plot_icons(N = 1250, sens = 0.9, spec = 0.9, prev = 0.9,
#                icon_types = c(21,23,24,23),
#                block_size_row = 10, block_size_col = 5,
#                nblocks_row = 5, nblocks_col = 5,
#                block_d = 0.8,
#                border_d = 0.2,
#                fill_array = "top")
# # pretty variants:
# plot_icons(N = 800, arr_type = "array", icon_types = c(21,22,23,24),
#            block_d = 0.5, border_d = 0.5)
#
# plot_icons(N = 800, arr_type = "shuffledarray", icon_types = c(21,23,24,22),
#            block_d = 0.5, border_d = 0.5)
#
# plot_icons(N = 800, arr_type = "shuffledarray", icon_types = c(21,23,24,22),
#            icon_brd_col = grey(.33, .99), icon_brd_lwd = 3)
#
# plot_icons(N = 800, arr_type = "fillequal", icon_types = c(21,22,22,21),
#            icon_brd_lwd = .5, cex = 2)

## -----------------------------------------------
## (+) ToDo:

## - Use default txt and pal arguments.
## - Use by = "cd", "cddc", etc. argument.
## - Provide area = "icons" functionality to other plots.

## - Show as 4 distinct clusters (rectangles?) of icons.
## - Hybrid plots: Combine icons with fnet/ftree/prism.
## - Add borders to left and top type of sorting.
## - More modular: Different plot types as separate (sub-)functions?
##
## - Better understand cex --> how does it work, when does it change sizes, when not?

## -----------------------------------------------
## eof.
