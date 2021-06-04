## comp_mlm.R (based on comp_metrics.R) | riskyr
## 2021 06 04
## -------------------------------------------

## (A) Matrix lens model: ----------

# Frame a 2x2 matrix (as contingency table). Distinguish 3 use cases:
#  1. from raw data: data frame with multiple variables (non-binary, 1 criterion) and individual cases
#  2. from contingency data: data frame with multiple variables (...) and 1 numeric column (frequency counts)
#  3. from description: 4-tuple of frequency values (vector abcd, in by-row direction)
#     and corresponding layout specification (dimension names and names/order of levels)

## ad 1.: From raw data:


## Filtering: ------

# Tasks:
# - binarize a variable with given cut-off value
# - optimize cut-off value given some criterion and metric
# - binarize an entire dataset


## Framing: ------

# (a) Given raw data of cases: Assume that data contains individual cases
#     (i.e., not a contingency table with a column of frequency counts)

## Documentation:

# Inputs: data and description of desired 2x2 matrix

# The input data can be:
# 1. a raw data with binary variables x and y (and z)
# 2. a contingency table
# 3. a vector of 4 basic frequency counts

# Output: Returns a 2x2 matrix (as a contingency table).

## frame Documentation: ------

#' Frame a 2x2 matrix (from data or description).
#'
#' \code{frame} creates a 2x2 matrix from data or description.
#'
#' \code{frame} allows for 3 basic use cases:
#'
#' 1. From raw data with
#' each row of a data frame \code{data} denoting an individual case, and
#' its columns containing at least 2 binary variables \code{x} and \code{y}.
#'
#' 2. From a contingency table with
#' each row of a data frame \code{data} denoting a case description,
#' a numeric variable/column \code{freq_var} denoting each case's frequency count,
#' and other columns containing at least 2 binary variables \code{x} and \code{y}.
#'
#' 3. From a description that provides
#' a numeric vector \code{data} as 4 basic frequency counts (abcd, read in by-row direction),
#' and a description of the matrix layout (names of dimensions and names/orders of category levels).
#'
#' Use cases 2. and 3. allow conditionalizing \code{data} on a value \code{z_val} of a variable \code{z}.
#'
#' @return A 2x2 matrix (as a contingency \code{table} of integer values).
#'
#' @param data A data frame (with at least 2 binary variables \code{x} and \code{y},
#' denoting individual cases or a contingency table with frequency counts)
#' or a vector with 4 basic frequency counts (abcd, in by-row direction).
#'
#' @param x Variable of \code{data} used for x-dimension of 2x2 matrix.
#' @param y Variable of \code{data} used for y-dimension of 2x2 matrix.
#'
#' @param z Variable of \code{data} used for conditionalizing \code{data}.
#' @param z_val Value of \code{z} used for conditionalizing \code{data}.
#'
#' @param freq_var Variable containing the frequency count of each case
#' when \code{data} provides a contingency table
#' (i.e., each row provides a case description).
#'
#' @param x_name Name of x-dimension in 2x2 matrix.
#' @param y_name Name of y-dimension in 2x2 matrix.
#'
#' @param x_levels Order of category levels on x-dimension
#' (values must be present in \code{data}).
#' @param y_levels Order of category levels on y-dimension
#' (values must be present in \code{data}).
#'
#' @examples
#' # 1. From raw data (denoting individual cases):
#' s <- riskyr(hi = 1, mi = 2, fa = 3, cr = 4)
#' d <- write_popu(s)
#' frame(d, x = "True condition", y = "Outcome")
#'
#' # 2. From a contingency table (df) with a variable denoting
#' #    the frequency counts of each case:
#' c_tab <- as.data.frame(Titanic)
#' frame(c_tab, x = "Sex", y = "Survived", freq_var = "Freq")
#' frame(c_tab, x = "Sex", y = "Survived", freq_var = "Freq",
#'       x_name = "Gender", y_name = "Alive",
#'       x_levels = c("Female", "Male"), y_levels = c("Yes", "No"))
#'
#' # 3. From 4 frequency counts and layout description:
#' frame(data = 1:4, x = "Condition", y = "Outcome")
#'
#' abcd <- c(344, 367, 126, 1364)
#' frame(data = abcd, x = "sex", y = "survived",
#'       x_levels = c("female", "male"), y_levels = c(1, 0))
#' frame(data = abcd, x = "Gender", y = "Alive",
#'       x_levels = c("Female", "Male"), y_levels = c("Yes", "No"))
#'
#' @importFrom stats aggregate
#'
#' @family matrix lens model functions
#'
#' @seealso
#' \code{\link{comp_popu}} creates data (as df) from description (frequencies);
#' \code{\link{read_popu}} creates a scenario (description) from data (as df);
#' \code{\link{riskyr}} initializes a \code{riskyr} scenario.
#'
#' @export

frame <- function(data, x, y,
                  z = NA,  z_val = NA,
                  freq_var = NA,
                  x_name = NA, y_name = NA,
                  x_levels = NA, y_levels = NA){

  # Initialize:
  mx   <- NA
  abcd <- NA

  # Case 1: From binary raw data: ----

  if (is.data.frame(data)){

    # message("Cases 1+2: Creating mx from data:")  # 4debugging

    # 0. Verify that
    # a. x, y (and z) are variables in data
    # b. variables are binary

    # Conditionalize data (on z == z_val):
    if (!is.na(z) & !is.na(z_val)){

      ix_z  <- which(names(data) == z)
      vec_z <- data[ , ix_z]
      ToF_z <- (vec_z == z_val)

      message(paste0("Conditionalizing data on ", z, " == ", z_val))  # 4debugging

      data <- data[ToF_z, ]  # Filter: Only cases/rows for which condition z == z_val.

    } # conditionalize end.

    # Column indices:
    ix_x <- which(names(data) == x)
    ix_y <- which(names(data) == y)

    # Dimension names:
    if (is.na(x_name)){
      name_x <- names(data)[ix_x]  # variable name
    } else {
      name_x <- x_name  # provided name
    }

    if (is.na(y_name)){
      name_y <- names(data)[ix_y]  # variable name
    } else {
      name_y <- y_name  # provided name
    }

    # Vectors from data:
    vec_x <- data[ , ix_x]
    vec_y <- data[ , ix_y]

    # Note non-binary variables:
    nval_x <- length(unique(vec_x))
    if (nval_x != 2){
      message(paste0("frame: x is non-binary (", nval_x, " unique values)"))
    }
    nval_y <- length(unique(vec_y))
    if (nval_y != 2){
      message(paste0("frame: y is non-binary (", nval_y, " unique values)"))
    }

    # Vectors as factors:
    if (!all(is.na(x_levels))){
      vec_x <- factor(vec_x, levels = x_levels, ordered = FALSE)
    }
    if (!all(is.na(y_levels))){
      vec_y <- factor(vec_y, levels = y_levels, ordered = FALSE)
    }

    # Distinguish two sub-cases:
    if (is.na(freq_var)){ # Case 1: Raw data with rows of cases:

      message("Case 1: Creating mx from RAW data")  # 4debugging

      # Cross-tabulate vectors:
      mx <- table(vec_y, vec_x, dnn = c(name_y, name_x))


    } else { # Case 2: From aggregated/contingency data with frequency counts (freq_var): ----

      message("Case 2: Creating df from aggregated data")  # 4debugging

      ix_fv  <- which(names(data) == freq_var)
      vec_fv <- data[ , ix_fv]

      # Aggregate df:
      agg_df <- aggregate(x = vec_fv, list(vec_y, vec_x), FUN = "sum")
      names(agg_df) <- c(name_y, name_x, "freq")

      # print(agg_df)  # 4debugging

      # # Turn IVs into factors:
      # agg_df[ , 1] <- as.factor(agg_df[ , 1])
      # agg_df[ , 2] <- as.factor(agg_df[ , 2])

      # Get levels from agg_df (iff NA):
      if (all(is.na(x_levels))){
        x_factor <- as.factor(agg_df[, 2])
        x_levels <- levels(x_factor)
        # print(x_levels)  # 4debugging
      }
      if (all(is.na(y_levels))){
        y_factor <- as.factor(agg_df[, 1])
        y_levels <- levels(y_factor)
        # print(y_levels)  # 4debugging
      }

      # HACK: Pass vector agg$freq (in abcd order) with description (Case 3):
      acbd <- agg_df$freq          # 2x2 cell values (in by-column direction)
      abcd <- acbd[c(1, 3, 2, 4)]  # 2x2 cell values (in by-row direction)

      # mx <- frame(data = abcd, x = name_x, y = name_y) # basics only
      mx <- frame(data = abcd, x = name_x, y = name_y,   # basics and
                  x_name = x_name, x_levels = x_levels,  # change names and level order
                  y_name = y_name, y_levels = y_levels)

    } # Case 2: From aggregated/contingency data end.

  } # Cases 1+2: From raw/contingency data end.


  # Case 3: From description: ----

  if (is.vector(data, mode = "numeric") && length(data == 4)) {

    message("Case 3: Creating mx from 4 frequency counts and layout description")  # 4debugging

    # Coerce data to integer:
    data <- as.integer(data)

    # Level names:
    if (all(is.na(x_levels))){ x_levels <- c("c_1", "c_2") }
    if (all(is.na(y_levels))){ y_levels <- c("r_1", "r_2") }
    dim_list <- list(y_levels, x_levels)

    # Dimension names:
    if (is.na(x_name)){
      name_x <- x  # variable name
    } else {
      name_x <- x_name  # provided name
    }

    if (is.na(y_name)){
      name_y <- y  # variable name
    } else {
      name_y <- y_name  # provided name
    }

    names(dim_list) <- c(name_y, name_x)

    # Define matrix:
    mx <- matrix(data, nrow = 2, ncol = 2, byrow = TRUE, dimnames = dim_list)

    # Coerce into table:
    mx <- as.table(mx)

  } # Case 3: From description end.


  # Output: ----
  return(mx)

} # frame().


## Check: ------

# ## Frame a 2x2 matrix: Distinguish three general use cases:
#
# ## Case 1: From raw data (rows contain individual cases): ----
# #  AE: data = data with binary variables (after filter step)
# df_raw <- FFTrees::titanic  # binary variables with individual cases
# # df_raw
#
# # (a) Basics:
# m1_a <- frame(data = df_raw, x = "sex", y = "survived")
# m1_a
# sum(m1_a)
#
# # (b) Adding dimension names, and arrange rows/columns (by order of levels):
# m1_b <- frame(data = df_raw, x = "sex", y = "survived",
#               x_name = "Gender", y_name = "Alive",
#               x_levels = c("female" ,"male"),
#               y_levels = c(1, 0))
# m1_b
#
# # (c) Conditionalize on z:
# m1_c <- frame(df_raw, x = "sex", y = "survived",
#               z = "age", z_val = "child",
#               x_levels = c("female", "male"), y_levels = c(1, 0),
#               x_name = "Gender", y_name = "Alive")
# m1_c
# sum(m1_c)
#
# # (d) Note: Non-binary variables:
# frame(df_raw, x = "class", y = "survived")
# frame(df_raw, y = "class", x = "survived", x_name = "Survival",
#       y_levels = c("first", "second", "third"))
#
#
# ## Case 2: From a data that provides a contingency table: ----
# # AE: data = a contingency table (with a variable/column containing frequency counts):
# df_con <- as.data.frame(Titanic)
# df_con
#
# # (a) Basics:
# (m2_a <- frame(df_con, x = "Sex", y = "Survived", freq_var = "Freq"))
# sum(m2_a)
#
# # (b) Add dimension names and re-arrange rows/columns:
# m2_b <- frame(df_con, x = "Sex", y = "Survived", freq_var = "Freq",
#               x_name = "Gender", y_name = "Alive",
#               x_levels = c("Female", "Male"), y_levels = c("Yes", "No"))
# m2_b
# sum(m2_b)
#
# # (c) with conditionalization:
# m2_c <- frame(df_con, x = "Sex", y = "Survived", freq_var = "Freq",
#               x_name = "Gender", y_name = "Alive",
#               x_levels = c("Female", "Male"), y_levels = c("Yes", "No"),
#               z = "Age", z_val = "Child")
# m2_c
# sum(m2_c)
#
# ## Snippets (from aggregated/contingency table > summary table > 2x2 table):
#
# # ## (a) Tidyverse solution:
# # library(tidyverse)
# #
# # df_con %>%
# #   group_by(Sex, Survived) %>%
# #   summarise(n = n(),
# #             freq = sum(Freq))
# #
# # ## (b) Base R solution:
# # # ?aggregate()
# #
# #
# # agg <- aggregate(x = df_con$Freq, list(df_con$Survived, df_con$Sex), FUN = "sum")
# # names(agg) <- c("dim_y", "dim_x", "freq")
# # agg
# #
# # agg[, 1]
# # is.factor(agg[, 1])
# #
# # frame(agg$freq, x = names(agg$dim_x), y = names(agg$dim_y),
# #       x_name = "x_name", x_levels = unique(agg$dim_x),
# #       y_name = "y_name", y_levels = unique(agg$dim_y))
# #
# # # Other stuff:
# # by1 <- df_con$Sex
# # by2 <- df_con$Survived
# # aggregate(x = df_con$Freq, by = list(by2, by1), FUN = "sum")
# #
# # fby1 <- factor(df_con$Sex)
# # fby2 <- factor(df_con$Survived)
# # aggregate(x = df_con$Freq, by = list(fby2, fby1), FUN = "sum")
#
#
# ## Case 3: From 4 freq values and layout description: ----
# # AE: data = Vector of 4 basic values (abcd, read in by-row direction):
# abcd <- c(344, 367, 126, 1364)
#
# # Basics:
# (m3_a <- frame(data = abcd, x = "sex", y = "survived"))
#
# # add level labels:
# (m3_a <- frame(data = abcd, x = "sex", y = "survived",
#                x_levels = c("female", "male"), y_levels = c(1, 0)))
#
# # Note: Levels must match abcd structure:
# m1_a  # original to recreate:
# m3_a <- frame(data = c(126, 1364, 344, 367),
#               x = "sex", y = "survived",
#               x_levels = c("female", "male"), y_levels = c(0, 1))
# # m3_a
# all.equal(m3_a, m1_a)
#
# # Recreate m1_c from description:
# m1_c  # original to recreate:
# m3_c <- frame(data = c(28, 29, 17, 35),
#               x = "sex", y = "survived",
#               x_name = "Gender", y_name = "Alive",
#               x_levels = c("female", "male"), y_levels = c(1, 0))
# # m3_c
# all.equal(m1_c, m3_c)
#
# # Recreate m2_c from description:
# m2_c  # original to recreate:
# m3_d <- frame(data = c(28, 29, 17, 35),
#               x = "sex", y = "survived",
#               x_name = "Gender", y_name = "Alive",
#               x_levels = c("Female", "Male"), y_levels = c("Yes", "No"))
# all.equal(m2_c, m3_d)



## Transformations: ------

# # Infos:
# m1_a
# dim(m1_a)
#
# is.matrix(m1_a)
# is.table(m1_a)
# typeof(m1_a)
#
# dimnames(m1_a)
# dimnames(m1_a)[[2]]
#
# # Sums:
# sum(m1_a)
# rowSums(m1_a)
# colSums(m1_a)
#
# summary(m1_a)
#
# # Get four basic values:
# (abcd <- c(m1_a[1, 1], m1_a[1, 2], m1_a[2, 1], m1_a[2, 2]))
#
# # Probabilities and marginal probabilities:
# prop.table(m1_a, margin = NULL) * 100  # by cells
# prop.table(m1_a, margin = 1) * 100     # by rows
# prop.table(m1_a, margin = 2) * 100     # by cols
# # ToDo: Diagonal (margin = 3)


## Focusing: ------

## Test:
# chisq.test(m1_a)
# chisq.test(m2_a)
# chisq.test(m3_a)


## Visualizations: ------

## (a) Mosaic plot:
# mosaicplot(t(m1_b), color = c("skyblue1", "grey75"))
# mosaicplot(t(m1_c), color = c("skyblue1", "grey75"))

## (b) Tile plot:
# (see ggplot code below)


## (B) comp_metrix: Compute key metrics from 4 basic frequencies: ----------

# (0) 2x2 matrices: ------

mx <- c(4, 2, 1, 3)

# even distribution:
mx_01 <- c(25, 25, 25, 25)

# 1 cell with low frequency:
mx_02 <- c(10, 30, 30, 30)
mx_03 <- c(30, 10, 30, 30)
mx_04 <- c(30, 30, 10, 30)
mx_05 <- c(30, 30, 30, 10)

# 1 cell with high frequency:
mx_06 <- c(70, 10, 10, 10)
mx_07 <- c(10, 70, 10, 10)
mx_08 <- c(10, 10, 70, 10)
mx_09 <- c(10, 10, 10, 70)

# 2 low vs. 2 high frequency cells:
mx_10 <- c(10, 10, 40, 40)
mx_11 <- c(10, 40, 10, 40)
mx_12 <- c(10, 40, 40, 10)
mx_13 <- c(40, 10, 10, 40)
mx_14 <- c(40, 10, 40, 10)
mx_15 <- c(40, 40, 10, 10)

# 4 different values:
mx_16 <- c(10, 20, 30, 40)
mx_17 <- c(10, 20, 40, 30)
mx_18 <- c(10, 30, 20, 40)
mx_19 <- c(10, 30, 40, 20)
mx_20 <- c(10, 40, 20, 30)
mx_21 <- c(10, 40, 30, 20)
# etc. (swap 10 with 20, 30, 40, ...)


# Plot 2x2 matrix as ggplot tile plot: ------

mx_data <- function(m = mx){
  # turn mx into data for ggplot2:

  out <- NA
  xs <- c(0, 1, 0, 1)
  ys <- c(1, 1, 0, 0)

  out <- data.frame(x = xs, y = ys, n = m)

  return(out)
}

## Check:
# mx_data(mx)

# plot_tbt <- function(m = mx){
#
#   d <- mx_data(m)
#
#   n_max <- sum(m)  # for frequency matrices.
#   # Note: n_max for conditional probability matrices:
#   # Max. value p(cell | row/col/diagonal)!
#
#   # parameters:
#   lbl_sz <- 5
#   col_hi  <- unikn::pal_petrol[[5]] # "black" # unikn::pal_karpfenblau[[5]]
#   col_brd <- grey(.33, 1)
#   brd_sz <- .5
#
#     ggplot2::ggplot(d, aes(x = x, y = y)) +
#       geom_tile(aes(fill = n), color = col_brd, size = brd_sz) +
#       geom_text(aes(label = n), size = lbl_sz) +
#       scale_fill_gradient(low = "white", high = col_hi, limits = c(0, n_max)) +
#       coord_fixed() +
#       theme_void() +
#       theme(legend.position = "none")
#
# }

## Check:
# plot_tbt()
# plot_tbt(mx_12)
# plot_tbt(m = c(0, 0, 0, 100))

# (1) Frequencies: ------

# get_a: -----

get_a <- function(a = mx[1], b = mx[2], c = mx[3], d = mx[4]){

  out <- NA

  out <- (a)

  return(out)

}


# get_b: -----

get_b <- function(a = mx[1], b = mx[2], c = mx[3], d = mx[4]){

  out <- NA

  out <- (b)

  return(out)

}


# get_c: -----

get_c <- function(a = mx[1], b = mx[2], c = mx[3], d = mx[4]){

  out <- NA

  out <- (c)

  return(out)

}


# get_d: -----

get_d <- function(a = mx[1], b = mx[2], c = mx[3], d = mx[4]){

  out <- NA

  out <- (d)

  return(out)

}


# get_N: -----

get_N <- function(a = mx[1], b = mx[2], c = mx[3], d = mx[4]){

  out <- NA

  out <- (a + b + c + d)

  return(out)

}



# (2) Probabilities: ------

# get_prev: -----

get_prev <- function(a = mx[1], b = mx[2], c = mx[3], d = mx[4]){

  out <- NA

  out <- (a + c)/(a + b + c + d)

  return(out)

}


# get_bias: -----

get_bias <- function(a = mx[1], b = mx[2], c = mx[3], d = mx[4]){

  out <- NA

  out <- (a + b)/(a + b + c + d)

  return(out)

}


# get_ACC: -----

get_ACC <- function(a = mx[1], b = mx[2], c = mx[3], d = mx[4]){

  out <- NA

  out <- (a + d)/(a + b + c + d)

  return(out)

}


# (3) Conditional Probabilities: ------

# (3a) normalized by column:

# get_sens: -----

get_sens <- function(a = mx[1], b = mx[2], c = mx[3], d = mx[4]){

  out <- NA

  out <- (a)/(a + c)

  return(out)

}


# get_TPR: -----

get_TPR <- function(a = mx[1], b = mx[2], c = mx[3], d = mx[4]){

  out <- NA

  out <- get_sens(a = a, b = b, c = c, d = d)  # TPR = sens

  return(out)

}


# get_FPR: -----

get_FPR <- function(a = mx[1], b = mx[2], c = mx[3], d = mx[4]){

  out <- NA

  out <- (b)/(b + d)

  return(out)

}


# get_FNR: -----

get_FNR <- function(a = mx[1], b = mx[2], c = mx[3], d = mx[4]){

  out <- NA

  out <- (c)/(a + c)

  return(out)

}


# get_spec: -----

get_spec <- function(a = mx[1], b = mx[2], c = mx[3], d = mx[4]){

  out <- NA

  out <- (d)/(b + d)

  return(out)

}


# get_TNR: -----

get_TNR <- function(a = mx[1], b = mx[2], c = mx[3], d = mx[4]){

  out <- NA

  out <- get_spec(a = a, b = b, c = c, d = d)  # TNR = spec

  return(out)

}

# (3b) normalized by row:

# get_PPV: -----

get_PPV <- function(a = mx[1], b = mx[2], c = mx[3], d = mx[4]){

  out <- NA

  out <- (a)/(a + b)

  return(out)

}


# get_FDR: -----

get_FDR <- function(a = mx[1], b = mx[2], c = mx[3], d = mx[4]){

  out <- NA

  out <- (b)/(a + b)

  return(out)

}


# get_FOR: -----

get_FOR <- function(a = mx[1], b = mx[2], c = mx[3], d = mx[4]){

  out <- NA

  out <- (c)/(c + d)

  return(out)

}


# get_NPV: -----

get_NPV <- function(a = mx[1], b = mx[2], c = mx[3], d = mx[4]){

  out <- NA

  out <- (d)/(c + d)

  return(out)

}



# (4) Likelihood/odds ratios: ------

# get_LRp: -----

get_LRp <- function(a = mx[1], b = mx[2], c = mx[3], d = mx[4]){

  out <- NA
  sens <- NA
  FPR <- NA

  sens <- get_sens(a, b, c, d)
  FPR  <- get_FPR(a, b, c, d)

  out <- sens/FPR

  return(out)

}


# get_LRn: -----

get_LRn <- function(a = mx[1], b = mx[2], c = mx[3], d = mx[4]){

  out <- NA
  spec <- NA
  FNR <- NA

  FNR  <- get_FNR(a, b, c, d)
  spec <- get_spec(a, b, c, d)

  out <- FNR/spec

  return(out)

}


# get_DOR: -----

get_DOR <- function(a = mx[1], b = mx[2], c = mx[3], d = mx[4]){

  out <- NA
  LRp <- NA
  LRn <- NA
  sens <- NA
  spec <- NA
  PPV <- NA
  NPV <- NA

  out <- (a * d)/(b * c)

  # Checks:
  LRp <- get_LRp(a, b, c, d)
  LRn <- get_LRn(a, b, c, d)
  alt <- LRp/LRn

  sens <- get_sens(a, b, c, d)
  spec <- get_spec(a, b, c, d)
  alt2 <- (sens * spec)/((1 - sens) * (1 - spec))

  PPV <- get_PPV(a, b, c, d)
  NPV <- get_NPV(a, b, c, d)
  alt3 <- (PPV * NPV)/((1 - PPV) * (1 - NPV))

  if ((round(out, 9) == round(alt, 9)) &
      (round(out, 9) == round(alt2, 9)) &
      (round(out, 9) == round(alt3, 9))){
    return(out)
  } else {
    message(paste0("get_DOR: out = ", out, " differs from some alt = ", alt))
  }

}



# (5) Integrated measures: ------


# (a) Contingencies: ------

# get_dPr: Row contingency -----

get_dPr <- function(a = mx[1], b = mx[2], c = mx[3], d = mx[4]){

  out <- NA

  out <- ((a)/(a + b)) - ((c)/(c + d))  # PPV - FOR

  return(out)

}


# get_dPc: Column contingency -----

get_dPc <- function(a = mx[1], b = mx[2], c = mx[3], d = mx[4]){

  out <- NA

  out <- ((a)/(a + c)) - ((b)/(b + d))  # sens - FPR

  return(out)

}


# get_chi2: -----

get_chi2 <- function(a = mx[1], b = mx[2], c = mx[3], d = mx[4]){

  out <- NA
  out_num <- NA
  out_den <- NA

  # Compute chi2:
  out_num <- (a + b + c + d) * ((a * d) - (b * c))^2
  out_den <- (a + b) * (c + d) * (a + c) * (b + d)
  out <- out_num/out_den

  # Check: chi2 == N * dPc * dPr
  alt <- get_N(a, b, c, d) *
    get_dPc(a, b, c, d) *
    get_dPr(a, b, c, d)

  if (round(out, 9) == round(alt, 9)){
    return(out)
  } else {
    message(paste0("get_chi2: out = ", out, " differs from alt = ", alt))
  }

}



# (b) Other integrated measures:

# get_MCC: -----

get_MCC <- function(a = mx[1], b = mx[2], c = mx[3], d = mx[4]){

  out <- NA
  out_num <- NA
  out_den <- NA

  # Compute MCC:
  out_num <- ((a * d) - (b * c))
  out_den <- sqrt((a + b) * (c + d) * (a + c) * (b + d))
  out <- out_num/out_den

  # Check: chi2 == N * MCC^2
  alt <- sqrt(get_chi2(a, b, c, d)/get_N(a, b, c, d))

  if (round(out, 9) == round(alt, 9)){
    return(out)
  } else {
    message(paste0("get_MCC: out = ", out, " differs from alt = ", alt))
  }

}


# get_F1s: -----

get_F1s <- function(a = mx[1], b = mx[2], c = mx[3], d = mx[4], beta = 1){

  out <- NA
  precision <- NA
  recall <- NA

  # Compute F1s:
  precision <- get_PPV(a, b, c, d)
  recall    <- get_sens(a, b, c, d)
  out <- (1 + beta^2) * (precision * recall)/((beta^2 * precision) + recall)

  return(out)

}


# (6) Risk measures: ------

# AR: Absolute risk (i.e., 2 conditional probabilities)
#     - correspondence of AR+ to sens/PPV and
#     -                of AR- to FPR/1-spec/1-NPV (depending on matrix dimensions/direction).

# AR+: Same as sens -----

get_ARp <- function(a = mx[1], b = mx[2], c = mx[3], d = mx[4]){

  get_sens(a, b, c, d)

}

# AR-: Same as FPR -----

get_ARm <- function(a = mx[1], b = mx[2], c = mx[3], d = mx[4]){

  get_FPR(a, b, c, d)

}

# ARR (absolute risk reduction/increase): Same as Column Contingency (delta Pc) -----

get_ARR <- function(a = mx[1], b = mx[2], c = mx[3], d = mx[4]){

  get_dPc(a, b, c, d)

}


# RR:  Relative risk
# RRR: Relative risk reduction/increase


## (+) ToDo: ----------

## - Implement matrix lens model in 3 steps:
##   1. filtering: binarize variables, based on criterion variable and metric to maximize
##   2. framing: 2 dimensions, map category levels
##   3. focusing: provide a metric specified

## eof. ------------------------------------------
