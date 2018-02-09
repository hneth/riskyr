## plot_iconarray.R | riskyr
## 2018 02 08
## -----------------------------------------------
##  This function plots an icon array in many ways,
##  dependent on population size
## -----------------------------------------------

#' Plot an icon array of a population.
#'
#' \code{plot_iconarray} draws an icon array of
#' a population of which individual's condition has been classified
#' correctly or incorrectly
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
#'
#' @param type.sort Bla bla bla... with the following options:
#'
#' \enumerate{
#'
#'   \item \code{type.sort = "mosaic"} Order in some way.
#'
#'   \item \code{type.sort = "xyz"} Order in some other way.#'
#'
#' }
#'
#'
#' @param ident.order Specifies the order in which icon identities
#' (hits, misses, false alarms, and correct rejections) are plotted.
#' Default: \code{ident.order = c("hi", "mi", "fa", "cr")}
#'
#' @param random.position Are positions randomly drawn?
#' Default: \code{random.position = FALSE}.
#'
#' @param random.identities Are identities randomly assigned to positions?
#' Default: \code{random.identities = FALSE}
#'
#'
#' Various other options allow the customization of text labels and colors:
#'
#' @param title.lbl Text label to set plot title.
#'
#' @examples
#' # ways to work:
#' plot_iconarray()  # => plots icon array for default population
#'
#' # standard icon arrays:
#' plot_iconarray(N = 800, icon.types = c(21,23,24,23), block.d = 0.5, border.d = 0.5)
#'
#' plot_iconarray(N = 800, icon.types = c(21,23,24,23), block.d = 0.5, border.d = 0.5,
#'                random.identities = TRUE)
#'
#' plot_iconarray(N = 10000, icon.types = c(21,23,24,23), block.d = 0.5, border.d = 0.5)
#'
#' plot_iconarray(N = 10000, icon.types = c(21,23,24,23),
#'                random.identities = TRUE)
#'
#' plot_iconarray(icon.types = c(21,23,24,23),
#'                block_size_col = 5, block_size_row = 5, #ncol_blocks = 2, nrow_blocks = 2,
#'                block.d = 0.5, border.d = 0.9)
#'
#' plot_iconarray(N = 1000, icon.types = c(21,23,24,23), block.d = 0.4)
#'
#' plot_iconarray(N = 1250, sens = 0.9, spec = 0.9, prev = 0.9,
#'                icon.types = c(21,23,24,23),
#'                block_size_col = 10, block_size_row = 5,
#'                ncol_blocks = 5, nrow_blocks = 5,
#'                block.d = 0.8,
#'                border.d = 0.2,
#'                fill_array = "top")
#'
#'
#' plot_iconarray(N = 10000, sens = 0.9, spec = 0.6, prev = 0.3,
#'                icon.types = c(21,23,21,23),
#'                ident.order = c("hi", "mi", "cr", "fa"),
#'                block.d = 0.8,
#'                border.d = 0.01,
#'                cex = 0.7,
#'                random.position = FALSE,
#'                random.identities = FALSE)
#'
#'
#' # Mosaic like and randomized arrays:
#' plot_iconarray(N = 1000, icon.types = c(22,23,22,23), #cex = 3,
#'                random.position = TRUE, type.sort = "mosaic", block.d = 0.05)
#'
#' plot_iconarray(icon.types = c(22,23,21,23), #cex = 10,
#'                ident.order = c("mi", "hi", "cr", "fa"),
#'                random.position = TRUE, type.sort = "equal", block.d = 0.05)
#'
#' plot_iconarray(icon.types = c(21,23,22,23), #cex = 10,
#'                random.position = TRUE, random.identities = TRUE)
#'
#' plot_iconarray(N = 10000, sens = 0.9, spec = 0.9, prev = 0.9,
#'                icon.types = c(21,23,21,23),
#'                ident.order = c("hi", "mi", "cr", "fa"),
#'                block.d = 0.1,
#'                border.d = 0.01,
#'                cex = 0.7,
#'                random.position = TRUE,
#'                random.identities = FALSE,
#'                type.sort = "mosaic")
#'
#'
#' @family visualization functions
#'
#' @export

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# Preparation:------------------------------------
#
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Note: The function finally only needs:
# A vector of identities (colors.)
# This can be obtained in different ways (e.g., calculation by respective function)
# A vector of positions (to be generated according to version.)
# A number of blocks.
# A size for the icons (cex)
# ...?

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# Plotting symbols:-------------------------------
#
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Important insight: an icon array is equivalent to an ordered (position constrained) scatterplot.
# All variants of it display the population concerning some property.

# (A) Four types:
# 1. Random position, random colors (typical scatterplot)
# 2. Random position, clustered colors (clustered scatterplot?)
# 3. Fixed positions (sample of positions constrained), random colors (random icon array)
# 4. Fixed positions, clustered colors (typical icon array)

# (B) Two dimensions:
# 1. Position
# 2. Identity

# (C) Translating these dimensions into code:

plot_iconarray <- function(prev = num$prev,             # probabilities
                           sens = num$sens, mirt = NA,
                           spec = num$spec, fart = NA,  # was: num$fart,
                           N = freq$N,    # ONLY freq used (so far)
                           ## Key options: ##
                           type.sort = "mosaic",  # needs to be given if random position but nonrandom ident.
                           ident.order = c("hi", "mi", "fa", "cr"),
                           random.position = FALSE,    # are positions randomly drawn?
                           random.identities = FALSE,  # are identities randomly assigned to positions?
                           ## defaults to classic icon array!
                           ## TODO: rather name these?
                           icon.colors = pal[c("hi", "mi", "fa", "cr")],  # use one color for each usual type.
                           icon.types = 22,  # plotting characters; default square with border
                           pch.border = grey(.10, .50),  # border color of icons
                           pch.lwd = 1.5, # line width of icons
                           transparency = .50,
                           # one can also enter a full vector of length N.
                           block.d = NULL,  # distance between blocks (where applicable).
                           border.d = 0.1,  # distance of icons to border.

                           # for classic icon arrays only:
                           # TODO: Allow to calculate defaults in the function!
                           # ncols = NULL,
                           # nrows = NULL,
                           # blocks = 1,
                           block_size_col = 10,
                           block_size_row = 10,
                           ncol_blocks = NULL,
                           nrow_blocks = NULL,

                           # TODO: Do I need them all the information is pretty redundant?

                           fill_array = "left",
                           fill_blocks = "rowwise",

                           # labelling:
                           title.lbl = txt$scen.lbl,
                           type.lbls = txt[c("sdt.hi.lbl", "sdt.mi.lbl", "sdt.fa.lbl", "sdt.cr.lbl")],

                           # (currently) fixed parameters:
                           xlim = c(0, 1),
                           ylim = c(0, 1),  # xlim and ylim should currently remain fixed!
                           cex = NULL,  # if NULL, cex will be calculated on demand!

                           ...  #additional parameters for plot()
) {

  # TODO: Checking of parameters!
  ## A0.1: Check entered parameters for plausibility!--------------------------------------------

  # Check whether random.position and random.identities are logical:
  if ( !(is.logical(random.position) | is.logical(random.identities)) ) {
    stop("random.position and random.identities must be logical!")
  }

  ## A0.2: Check entered parameters for usabililty:------------------------------------------

  # TODO: Either check for missing N or use other comparison.

  ## A0.3: Different routes to col.vec and pch.vec ----------------------------------------

  # A0.3.1: Calculation from probabilities ----------------------------------------------

  ## (A) If a valid set of probabilities was provided:
  if (is_valid_prob_set(prev = prev, sens = sens, mirt = mirt,
                        spec = spec, fart = fart, tol = .01)) {

    ## (a) Compute the complete quintet of probabilities:
    prob_quintet <- comp_complete_prob_set(prev, sens, mirt, spec, fart)
    sens <- prob_quintet[2] # gets sens (if not provided)
    mirt <- prob_quintet[3] # gets mirt (if not provided)
    spec <- prob_quintet[4] # gets spec (if not provided)
    fart <- prob_quintet[5] # gets fart (if not provided)

    ## (b) Compute cur.freq and popu based on current parameters (N and probabilities):
    cur.freq <- comp_freq(prev = prev, sens = sens, spec = spec, N = N, round = TRUE) # compute cur.freq (with round = TRUE).

  }
  else { # A0.3.2: Using existing frequencies:

    cur.freq <- freq

  }

  # Check size of N.Ist it needed?  Sccale down if not needed and greater 100.000:
  ## Specify N:
  N <- cur.freq$N
  ind.lbl <- NULL

  if (N >= 100000) {
    # get the minimal N:
    min_N <- riskyr::comp_min_N(prev = prev, sens = sens, spec = spec)

    if (min_N <= 10000) {  # only, if 10000 icons are sufficient:

      exponent <- ((N %/% 100000) %/% 10) + 1  # get exponent dependent on size.
      ind_per_icon <- 10 ^ exponent  # individuals per icon.
      ind.lbl <- paste0("Note: Icons have been scaled. Each icon represents ", ind_per_icon, " individuals")

      N <- N / (10^exponent)
      cur.freq <- lapply(cur.freq,  function(x) {x / (10^exponent)})  # adjust cur.freq and N.

    }

  }


  # DO SOME CHECKS HERE!?
  ## Determine order:
  if (is.null(names(icon.colors))) {
    names(icon.colors) <- ident.order
  }

  if (is.null(names(icon.types))) {

    if (length(icon.types) < length(icon.colors)) {

      if (length(icon.types) > 1) {warning("Icon types are recycled to number of colors.")}

      icon.types <- rep(icon.types, length.out = length(icon.colors))
    }
    names(icon.types) <- names(icon.colors)
  }

  ## (c) Compute icon.colors from frequencies:
  col.vec <- rep(icon.colors[ident.order], times = cur.freq[ident.order])

  ## (d) Compute pch.vec from frequencies:
  pch.vec <- rep(icon.types[ident.order], times = cur.freq[ident.order])



  ## A1 Random position, random colors:---------------------------------------
  if (random.position & random.identities) {

    # 1) Define positions:
    # 1a) draw random positions within plot dimensions:
    posx_vec <- runif(n = N, min = xlim[1], max = xlim[2])
    posy_vec <- runif(n = N, min = ylim[1], max = ylim[2])

    # 2) Randomize vectors:
    rand_ix <- sample(1:length(col.vec), replace = FALSE)  # create random vector.
    col.vec <- col.vec[rand_ix]  # randomize colors and

    if (length(pch.vec) == length(posx_vec)) {
      pch.vec <- pch.vec[rand_ix]  # characters accordingy.
    } else {
      pch.vec <- pch.vec[1]
      warning("pch.vec was not of length N.  Only first element used. ")
    }


  }  # end A1: (random position & random colors)


  ## A2 Random position, clustered colors: ---------------------------------------
  if (random.position & !random.identities) {

    # 1b) sort dependent on parameter:
    # options:
    # right: from left to right, top: from top to bottom,
    # equal: in equal spaces of the plot, mosaic: relative to area.

    if (!type.sort %in% c("right", "top", "equal", "mosaic")) {
      stop('type_sort must be either "right", "top", "equal", or "mosaic"')
      # maybe add stop and error message?
    } else {

      if (type.sort %in% c("right", "top")) {

        # 1a) draw random positions:
        posx_vec <- runif(n = N, min = xlim[1], max = xlim[2])
        posy_vec <- runif(n = N, min = ylim[1], max = ylim[2])

        # Then sort one of the vectors accordingly (presupposes ordered color vector).
        # type: from left to right:
        if (type.sort == "right") {
          posx_vec <- sort(posx_vec)
        }

        # type: from top to bottom:
        if(type.sort == "top"){
          posy_vec <- sort(posy_vec)
        }
      } else {  # if in equal or mosaic:

        # Initialize positions:
        posx_vec <- NULL
        posy_vec <- NULL

        # create n = "ident_type" compartments of the plot:
        block_n <- length(unique(col.vec))  # number of blocks for x and y.
        # TODO: not final; they should be distributed.

        # calculate number of observations in each block retaining original order:
        type_n <- sapply(unique(col.vec), function(x) sum(col.vec == x))

        # equal compartments:
        if (type.sort == "equal") {  # density varies, area is constant.

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
          min_ranges[min_ranges != global_min] <- min_ranges[min_ranges != global_min] + block.d
          # we don't want distance at the global minima nad maxima.
          max_ranges[max_ranges != global_max] <- max_ranges[max_ranges != global_max] - block.d

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
        if (type.sort == "mosaic") {

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
          # block.d may not be half the size of the distance between min and max.
          # for the example of prevalence == 0.15 it may not exceed 0.075.
          diff_dx <- apply(X = blocks[, c(1, 2)], MAR = 1, FUN = diff)
          diff_dy <- apply(X = blocks[, c(3, 4)], MAR = 1, FUN = diff)

          boundary_d <- min(c(abs(diff_dx), abs(diff_dy))) / 2

          if (is.null(block.d)){

            block.d <- 0.01  # a little messy though...

          } else {

            if ( block.d >= boundary_d ) {

              block.d <- boundary_d - 0.0001  # a little messy though...

            }
          }


          blocks[, c(1, 3)] <- blocks[, c(1, 3)] + block.d
          blocks[, c(2, 4)] <- blocks[, c(2, 4)] - block.d
          block_n <- sapply(unique(col.vec), function(x) sum(col.vec == x))
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

    }  # end: valid type.sort
  }  # end A2: (random.position & !random.identities)

  if (random.position) {

    if (is.null(cex)) {
      # TODO: How to covary cex with device size & point number?


      cex1 <- ((par("pin")[1] * 10) + 3) / sqrt(length(posx_vec))  # ad hoc formula.
      cex2 <- ((par("pin")[2] * 10) + 3) / sqrt(length(posx_vec))  # ad hoc formula.
      cex <- min(c(cex1, cex2))

    }
  }  # end (random.position)


  ## A3 and A4: Fixed positions:  --------------------------------------
  if (!random.position) {

    # 0. Check arrangement parameters: ----------------------------------
    if (is.null(block.d)) {

      block.d <- 0.4  # set to a default value.
    }

    transparency <- NULL  # set transparency to zero.

    #given:
    # block_size_col
    # block_size_row

    # calculate icons per block:
    icons_per_block <- block_size_col * block_size_row

    # If no number of blocks or cols is given:

    if (is.null(ncol_blocks) & is.null(nrow_blocks)) {

      # calculate number of blocks required:
      n_blocks <- ceiling(N / icons_per_block)

      blocking_dim <- factors_min_diff(n_blocks)  # get the dimensions.

      # dependent on pin:
      dim_in <- par("pin")  # get dimensions of plotting region.

      if( dim_in[1] >= dim_in[2] ) {  # if x greater y:

        ncol_blocks <- blocking_dim[2]  # larger in x dimension (cols).
        nrow_blocks <- blocking_dim[1]  # smaller in y dimension (rows).

      } else {
        ncol_blocks <- blocking_dim[1]  # smaller in x dimension (cols).
        nrow_blocks <- blocking_dim[2]  # larger in y dimension (rows).
      }

    } else {  # if at least one of both is given:

      # TODO: Provide some testing, whether given numbers of blocks are valid!
      test_N <- ncol_blocks * nrow_blocks * icons_per_block

      if ( test_N < N) {
        stop("The number of blocks and columns is too small to accomodate the population.")
      }


      if (is.null(ncol_blocks)) {  # if ncol_blocks is not given:

        ncol_blocks <- n_blocks / nrow_blocks  # calculate number of blocks per row.

      }

      if (is.null(nrow_blocks)) {  # if ncol_rows is not given:

        nrow_blocks <- n_blocks / ncol_blocks  # calculate number of blocks per column.

        # TODO: Change naming scheme!

      }

      # calculate number of blocks required:
      n_blocks <- ncol_blocks * nrow_blocks

    }

    # calculate total ncols and nrows:
    ncols <- block_size_col * ncol_blocks
    nrows <- block_size_row * nrow_blocks

    # Given a default of 10x10 blocks:
    #N / (block_size_col * block_size_row)


    # 1. Define positions:

    # find maximum for the positions given the units icons are moved:
    # find a monotonically increasing sequence, resulting in exactly the endpoint of xlim/ylim.
    # For x:
    max_posx <- ((ncols - 1) * xlim[2]) - (block.d * (ncol_blocks - 1)) - border.d
    min_posx <- xlim[1] + border.d
    adj_posx <- seq(min_posx, max_posx, length.out = ncols)

    # For y:
    max_posy <- ((nrows - 1) * ylim[2]) - (block.d * (nrow_blocks - 1)) - border.d
    min_posy <- ylim[1] + border.d
    adj_posy <- seq(max_posy, min_posy, length.out = nrows)

    # create position matrices:
    pos_mx <- matrix(adj_posx, nrow = nrows, ncol = ncols, byrow = TRUE)
    pos_my <- matrix(adj_posy, nrow = nrows, ncol = ncols)

    # add  a sequence to the x matrix:
    # For x:
    seqx_off <- seq(0, (ncol_blocks - 1) * block.d, by = block.d)
    # get the sequence of offsets for icons in each block.
    seqx <- rep(seqx_off, each = block_size_col)
    # repeat this sequence by block size so every icon is affected.
    pos_mx <- pos_mx + rep(seqx, each = nrow(pos_mx))
    # do so for every row in the matrix.

    # For y:
    seqy_off <- seq((nrow_blocks - 1) * block.d, 0, by = -block.d)  # create sequence of number to add.
    seqy <- rep(seqy_off, each = block_size_row)  # repeat to number of rows.
    pos_my <- pos_my + seqy  # will be repeated for each column anyways.


    # Plotting preparations:

    # save into respective vectors and norm on 0,1 space.
    posx_vec <- pos_mx / (ncols - 1)
    posy_vec <- pos_my / (nrows - 1)

    # TODO: Not in region anymore --> change plot dimensions or decrease standard distance.

    # Plotting dimensions for testing:
    # plotx_dim <- c(-0.1, 1.1)
    # ploty_dim <- c(-0.1, 1.1)


    if (!random.identities) {  # sort colors according to input.

      # For A4 (fixed positions and clustered identities) only:
      # 2. Color sorting:
      # Create block information:
      seq_block <- 1:n_blocks  # create sequence of block positions.

      # Determine, whether blocks are used colwise or rowwise:
      #fill_array <- "left"  # alternatively: "rowwise"

      # If blocks are to be filled in x direction:
      if (fill_array == "left"){

        seq_blockx <- rep(seq_block, each = block_size_col)
        # create sequence repeted to the number of cols (can be changed to number of rows).
        mat_block <- matrix(seq_blockx, ncol = ncols, byrow = TRUE)
        # create a matrix from it.
        ind_block <- rep(1:nrow(mat_block), each = block_size_row)  # create index to repeat matrix.
        mat_block <- mat_block[ind_block, ]
        # repeat each row of the matrix to the number of rows.
      }

      # If blocks are to be filled in y direction:
      if (fill_array == "top"){

        seq_blocky <- rep(seq_block, each = block_size_row)
        # create sequence repeted to the number of cols (can be changed to number of rows).
        mat_block <- matrix(seq_blocky, nrow = nrows, byrow = FALSE)
        # create a matrix from it.
        ind_block <- rep(1:ncol(mat_block), each = block_size_col)  # create index to repeat matrix.
        mat_block <- mat_block[ , ind_block]
        # repeat each row of the matrix to the number of columns.
      }

      # Determine, whether blocks (within) are filled col- or rowwise:
      #fill_blocks <- "colwise"

      # sort colors accordingly:
      # TODO: Find out WHY ON EARTH order(order()) works!

      if (fill_blocks == "rowwise"){
        order_mat <- order(order(t(mat_block)))  # matrix has to be transposed to get the rows.
        m <- matrix(order_mat, nrow = nrows, ncol = ncols, byrow = TRUE)  # This is the "rowwise witin blocks" version.

        #if (fill_array == "top") { m <- t(m) }
      }

      if (fill_blocks == "colwise") {
        order_mat <- order(order(mat_block))
        m <- matrix(order_mat, nrow = nrows, ncol = ncols, byrow = FALSE)  # This is the "colwise within blocks" version.

        #if (fill_array == "top") { m <- t(m) }
      }


      # if the color vector already has the appropriate length:
      if (length(col.vec) == length(m)) {
        col.vec <- col.vec[m]  # order the color vector.
      }

      if (length(pch.vec) == length(m)) {
        pch.vec <- pch.vec[m]  # order the character vector.
      }


      # if the color vector is too short:
      # TODO: what to do if too long?
      if (length(col.vec) < length(m)) {

        len_diff <- length(pos_mx) - length(col.vec)  # calculate difference.
        col.vec <- c(col.vec, rep(NA, len_diff))  # enlarge the color vector.
        col.vec <- col.vec[m]  # order this color vector.

        # mute the respective positions:
        posx_vec[is.na(col.vec)] <- NA  # set NA x positions...
        posy_vec[is.na(col.vec)] <- NA  # ... and y positions.

      }

      if (length(pch.vec) < length(m) & length(pch.vec) > 1) {

        len_diff <- length(pos_mx) - length(pch.vec)  # calculate difference.
        pch.vec <- c(pch.vec, rep(NA, len_diff))  # enlarge the color vector.

        # mute the respective positions:
        pch.vec <- pch.vec[m]  # order character vector.
        posx_vec[is.na(pch.vec)] <- NA  # set NA x positions...
        posy_vec[is.na(pch.vec)] <- NA  # ... and y positions.

      }

    }  # end fixed identities (A4).


    # For A3:
    if (random.identities) {

      rand_ix <- sample(1:length(col.vec), replace = FALSE)  # random index.
      col.vec <- col.vec[rand_ix]  # sample from the vector of colors.

      if (length(pch.vec) > 1) {
        pch.vec <- pch.vec[rand_ix]  # analog sample from character vector.
      }


      # if the color vector is too short.
      if (length(col.vec) < (ncols * nrows) & length(col.vec) > 1) {

        # for colors:
        len_diff <- (ncols * nrows) - length(col.vec)
        if (length(pch.border) > 1) {
          pch.border <- c(pch.border, rep(NA, len_diff))
        }

        col.vec <- c(col.vec, rep(NA, len_diff))

      }

      # if the character vector is too short:
      if (length(pch.vec) < (ncols * nrows) & length(pch.vec) > 1) {

        # for colors:
        len_diff <- (ncols * nrows) - length(pch.vec)
        pch.vec <- c(pch.vec, rep(NA, len_diff))

      }


    }  # end random identities (A3).

    # Adjust cex dynamically:
    if (is.null(cex)) {
      # TODO: How to covary cex with device size & point number?

      cex1 <- ((par("pin")[1] * 10) + 3) / ncols
      cex2 <- ((par("pin")[2] * 10) + 3) / nrows
      cex <- min(c(cex1, cex2))

      # still not optimal...

      # cex <- 1.5 - (min(par("pin")) / N)  # this latter term likely plays some role...
    }


  }  # end A3 and A4 (fixed positions).


  # B. Plotting

  # TODO: Add text!

  if (any(!pch.vec %in% c(NA, 21:25))) {
    # if any of the plotting characters is not in the ones with border,
    # omit border and color accordingly.
    pch.border <- col.vec
  }

  # 3) Plot:
  plot(x = 1,
       xlim = xlim, ylim = ylim,
       type = "n", xlab = "", ylab = "", xaxt = "n", yaxt = "n",
       bty = "o", fg = "grey"
  )

  # 3a) set plotting character:
  # pch <- 22  # filled square as default.
  # cex <- 0.5

  if (!is.null(transparency)) {
    col.vec <- adjustcolor(col.vec, alpha.f = 1 - transparency)
  }

  points(x = posx_vec, y = posy_vec, # positions.
         # visual details:
         pch = pch.vec, col = pch.border, bg = col.vec, lwd = pch.lwd, cex = cex)


  ## Additional information:

  ## Define labels:
  if (nchar(title.lbl) > 0) { title.lbl <- paste0(title.lbl, ":\n") }  # put on top (in separate line)
  cur.title.lbl <- paste0(title.lbl, "Icon array") # , "(N = ", N, ")")
  cur.par.lbl <-  paste0("Conditions: ",
                         "prev = ", as_pc(prev), "%, ",
                         "sens = ", as_pc(sens), "%, ",
                         "spec = ", as_pc(spec), "%")

  if (sum(nchar(type.lbls)) > 0) {
    # reorder lables:
    names(type.lbls) <- c("hi", "mi", "fa", "cr")
    type.lbls <- type.lbls[ident.order]
  }


  # Plot additional information:
  title(cur.title.lbl, adj = 0.5, line = 1.0, font.main = 1)  # (centered, raised, normal font)

  legend(x = xlim[2] / 2, y = ylim[1] - (ylim[2] / 20), legend = type.lbls,
         horiz = TRUE, bty = "n",
         pt.bg = icon.colors, pch = icon.types, cex = 1.2,
         xjust = 0.5, xpd = TRUE)
  # TODO: fixed order of legend?

  mtext(cur.par.lbl, side = 1, line = 3)
  mtext(ind.lbl, side = 1, line = 2)

}  # end of function.

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# END OF FUNCTION!!!
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Testing ground:-----------------------------------------------------

## Check:

{
  # # ways to work:
  # plot_iconarray()  # => plots icon array for default population
  #
  # # standard icon arrays:
  # plot_iconarray(N = 800, icon.types = c(21,23,24,23), block.d = 0.5, border.d = 0.5)
  #
  # plot_iconarray(N = 800, icon.types = c(21,23,24,23), block.d = 0.5, border.d = 0.5,
  #                random.identities = TRUE)
  #
  # plot_iconarray(N = 10000, icon.types = c(21,23,24,23), block.d = 0.5, border.d = 0.5)
  #
  # plot_iconarray(N = 10000, icon.types = c(21,23,24,23),
  #                random.identities = TRUE)
  #
  # plot_iconarray(icon.types = c(21,23,24,23),
  #                block_size_col = 5, block_size_row = 5, #ncol_blocks = 2, nrow_blocks = 2,
  #                block.d = 0.5, border.d = 0.9)
  #
  # plot_iconarray(N = 1000, icon.types = c(21,23,24,23), block.d = 0.4)
  #
  # plot_iconarray(N = 1250, sens = 0.9, spec = 0.9, prev = 0.9,
  #                icon.types = c(21,23,24,23),
  #                block_size_col = 10, block_size_row = 5,
  #                ncol_blocks = 5, nrow_blocks = 5,
  #                block.d = 0.8,
  #                border.d = 0.2,
  #                fill_array = "top")
  #
  #
  # plot_iconarray(N = 10000, sens = 0.9, spec = 0.6, prev = 0.3,
  #                icon.types = c(21,23,21,23),
  #                ident.order = c("hi", "mi", "cr", "fa"),
  #                block.d = 0.8,
  #                border.d = 0.01,
  #                cex = 0.7,
  #                random.position = FALSE,
  #                random.identities = FALSE)
  #
  # # TODO: Here it messes things up!
  #
  # # Mosaic like and randomized arrays:
  # plot_iconarray(N = 1000, icon.types = c(22,23,22,23), #cex = 3,
  #                random.position = TRUE, type.sort = "mosaic", block.d = 0.05)
  #
  # plot_iconarray(icon.types = c(22,23,21,23), #cex = 10,
  #                ident.order = c("mi", "hi", "cr", "fa"),
  #                random.position = TRUE, type.sort = "equal", block.d = 0.05)
  #
  # plot_iconarray(icon.types = c(21,23,22,23), #cex = 10,
  #                random.position = TRUE, random.identities = TRUE)
  #
  # plot_iconarray(N = 10000, sens = 0.9, spec = 0.9, prev = 0.9,
  #                icon.types = c(21,23,21,23),
  #                ident.order = c("hi", "mi", "cr", "fa"),
  #                block.d = 0.1,
  #                border.d = 0.01,
  #                cex = 0.7,
  #                random.position = TRUE,
  #                random.identities = FALSE,
  #                type.sort = "mosaic")

}

## -----------------------------------------------
## (+) ToDo:

## - add borders to left and top type of sorting.
##
## - Each of the plot types may be a potential function.  Then it is more modular!
##
## - Understand cex --> how does it work, when does it change size, when not?

## -----------------------------------------------
## eof.
