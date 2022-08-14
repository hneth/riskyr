## comp_mlm.R (based on comp_metrics.R) | riskyr
## 2022 08 14
## -------------------------------------------

# See new R package MLM.

## (A) Matrix lens model: ----------

# Frame a 2x2 matrix (as contingency table). Distinguish 3 use cases:
#  1. from raw data: data frame with multiple variables (non-binary, 1 criterion) and individual cases
#  2. from contingency data: data frame with multiple variables (...) and 1 numeric column (frequency counts)
#  3. from description: 4-tuple of frequency values (vector abcd, in by-row direction)
#     and corresponding layout specification (dimension names and names/order of levels)

## ad 1.: From raw data:


## (1) Filtering: ------

# Tasks:
# - binarize a variable with given cut-off value
# - optimize cut-off value given some criterion and metric
# - binarize an entire dataset


## (2) Framing: ------

# (a) Given raw data of cases: Assume that data contains individual cases
#     (i.e., not a contingency table with a column of frequency counts)

## Documentation:

# Inputs: data and description of desired 2x2 matrix

# The input data can be:
# 1. a raw data with binary variables x and y (and z)
# 2. a contingency table
# 3. a vector of 4 basic frequency counts

# Output: Returns a 2x2 matrix (as a contingency table).


# mlm_frame: Frame a 2x2 matrix (from data or description): ------

#' Frame a 2x2 matrix (from data or description).
#'
#' \code{mlm_frame} creates a 2x2 matrix
#' (i.e., a numeric contingency table of frequency counts)
#' from data or a description of its contents and layout.
#'
#' \code{mlm_frame} supports 4 use cases:
#'
#' \enumerate{
#'
#' \item From raw data (as data frame):
#' Each row of \code{data} denotes an individual case,
#' and the columns contain at least 2 binary variables \code{x} and \code{y}.
#'
#' \item From a contingency table (as data frame):
#' Each row of \code{data} denotes a case description,
#' a numeric variable \code{freq_var} denotes the frequency count per case,
#' and other columns contain at least 2 binary variables \code{x} and \code{y}.
#'
#' \item From a description:
#' A numeric vector \code{data} provides 4 basic frequency counts
#' (i.e., abcd, read in by-row direction),
#' and a specification of the matrix layout
#' (i.e., names of dimensions \code{x} and \code{y},
#' and the names/orders of category levels).
#'
#' \item From a table (as text):
#' A character string \code{data} that can be interpreted by
#' \code{\link{read.ftable}} of the \strong{stats} package.
#'
#' }
#'
#' Use cases 1. and 2. allow conditionalizing \code{data}
#' on a value \code{z_val} of a variable \code{z}.
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
#' s <- riskyr(hi = 8, mi = 2, fa = 95, cr = 895)
#' d <- write_popu(s)
#' mlm_frame(d, x = "True condition", y = "Outcome")
#'
#' # 2. From a contingency table (df) with a variable denoting
#' #    the frequency counts of each case:
#' c_tab <- as.data.frame(Titanic)
#' mlm_frame(c_tab, x = "Sex", y = "Survived", freq_var = "Freq")
#' mlm_frame(c_tab, x = "Sex", y = "Survived", freq_var = "Freq",
#'           x_name = "Gender", y_name = "Alive",
#'           x_levels = c("Female", "Male"), y_levels = c("Yes", "No"))
#'
#' # 3. From a description of 4 frequency counts and matrix layout:
#' # a. Basics:
#' mlm_frame(data = 1:4, x = "Condition", y = "Outcome")
#'
#' # b. The mammography problem:
#' abcd <- c(8, 95, 2, 895)  # 4 frequencies (Gigerenzer & Hoffrage, 1995)
#' mlm_frame(data = abcd, x = "Condition", y = "Test",
#'           x_levels = c("cancer", "no cancer"),
#'           y_levels = c("positive", "negative"))
#'
#' # c. Titanic passengers:
#' abcd <- c(344, 367, 126, 1364)
#' mlm_frame(data = abcd, x = "sex", y = "survived",
#'           x_levels = c("female", "male"), y_levels = c(1, 0))
#' mlm_frame(data = abcd, x = "Gender", y = "Alive",
#'           x_levels = c("Female", "Male"), y_levels = c("Yes", "No"))
#'
#' # 4. From a table (as text):
#' tbl_txt <-
#' "   Condition Cancer no_Cancer
#' Test
#' positive      8        95
#' negative      2       895"
#' mlm_frame(tbl_txt)
#'
#' @importFrom stats aggregate
#' @importFrom stats read.ftable
#'
#' @family matrix lens model functions
#'
#' @seealso
#' \code{\link{is_matrix}} for verifying a 2x2 matrix;
#' \code{\link{mlm_trans}} converts a 2x2 matrix (into a table of probabilities/conditional probabilities);
#' \code{\link{mlm_focus}} computes measures for a 2x2 matrix;
#' \code{\link{comp_popu}} creates data (as df) from description (frequencies);
#' \code{\link{read_popu}} creates a scenario (description) from data (as df);
#' \code{\link{riskyr}} initializes a \code{riskyr} scenario.
#'
#' @export

mlm_frame <- function(data, x, y,
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
      message(paste0("mlm_frame: x is non-binary (", nval_x, " unique values)"))
    }
    nval_y <- length(unique(vec_y))
    if (nval_y != 2){
      message(paste0("mlm_frame: y is non-binary (", nval_y, " unique values)"))
    }

    # Vectors as factors:
    if (!all(is.na(x_levels))){
      vec_x <- factor(vec_x, levels = x_levels, ordered = FALSE)
    }
    if (!all(is.na(y_levels))){
      vec_y <- factor(vec_y, levels = y_levels, ordered = FALSE)
    }

    # Distinguish two sub-cases:
    if (is.na(freq_var)){ # Case 1: Raw data with rows of individual cases:

      message("mlm_frame (case 1): Creating mx from raw data (individual cases)")  # 4debugging

      # Cross-tabulate vectors:
      mx <- table(vec_y, vec_x, dnn = c(name_y, name_x))


    } else { # Case 2: From aggregated/contingency data with frequency counts (freq_var): ----

      message("mlm_frame (case 2): Creating df from aggregated data (contingency table)")  # 4debugging

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

      # HACK: Pass vector agg$freq (as abcd values, by-row) with description (Case 3):
      acbd <- agg_df$freq          # 2x2 cell values (in by-column direction)
      abcd <- acbd[c(1, 3, 2, 4)]  # 2x2 cell values (in by-row direction)

      # mx <- mlm_frame(data = abcd, x = name_x, y = name_y) # basics only
      mx <- mlm_frame(data = abcd, x = name_x, y = name_y,   # basics and
                      x_name = x_name, x_levels = x_levels,  # change names and level order
                      y_name = y_name, y_levels = y_levels)

    } # Case 2: From aggregated/contingency data end.

  } # Cases 1+2: From raw/contingency data end.


  # Case 3: From description: ----

  if (is.vector(data, mode = "numeric") && length(data == 4)) {

    message("mlm_frame (case 3): Creating mx from frequency counts (abcd) and layout description")  # 4debugging

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


  # Case 4: From table as text: ----

  if (is.character(data)) {

    message("mlm_frame (case 4): Creating mx from text data")  # 4debugging

    mx <- as.table(stats::read.ftable(textConnection(data, encoding = "UTF-8")))

  } # Case 4: From table as text end.


  # Output: ----
  return(mx)

} # mlm_frame().


## Check:

# ## Frame a 2x2 matrix: Distinguish three general use cases:
#
# ## Case 1: From raw data (rows contain individual cases):
# #  AE: data = data with binary variables (after filter step)
# df_raw <- FFTrees::titanic  # binary variables with individual cases
# # df_raw
#
# # (a) Basics:
# m1_a <- mlm_frame(data = df_raw, x = "sex", y = "survived")
# m1_a
# sum(m1_a)
#
# # (b) Adding dimension names, and arrange rows/columns (by order of levels):
# m1_b <- mlm_frame(data = df_raw, x = "sex", y = "survived",
#               x_name = "Gender", y_name = "Alive",
#               x_levels = c("female" ,"male"),
#               y_levels = c(1, 0))
# m1_b
#
# # (c) Conditionalize on z:
# m1_c <- mlm_frame(df_raw, x = "sex", y = "survived",
#               z = "age", z_val = "child",
#               x_levels = c("female", "male"), y_levels = c(1, 0),
#               x_name = "Gender", y_name = "Alive")
# m1_c
# sum(m1_c)
#
# # (d) Note: Non-binary variables:
# mlm_frame(df_raw, x = "class", y = "survived")
# mlm_frame(df_raw, y = "class", x = "survived", x_name = "Survival",
#       y_levels = c("first", "second", "third"))
#
#
# ## Case 2: From a data that provides a contingency table:
# # AE: data = a contingency table (with a variable/column containing frequency counts):
# df_con <- as.data.frame(Titanic)
# df_con
#
# # (a) Basics:
# (m2_a <- mlm_frame(df_con, x = "Sex", y = "Survived", freq_var = "Freq"))
# sum(m2_a)
#
# # (b) Add dimension names and re-arrange rows/columns:
# m2_b <- mlm_frame(df_con, x = "Sex", y = "Survived", freq_var = "Freq",
#                   x_name = "Gender", y_name = "Alive",
#                   x_levels = c("Female", "Male"), y_levels = c("Yes", "No"))
# m2_b
# sum(m2_b)
#
# # (c) with conditionalization:
# m2_c <- mlm_frame(df_con, x = "Sex", y = "Survived", freq_var = "Freq",
#                   x_name = "Gender", y_name = "Alive",
#                   x_levels = c("Female", "Male"), y_levels = c("Yes", "No"),
#                   z = "Age", z_val = "Child")
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
# # mlm_frame(agg$freq, x = names(agg$dim_x), y = names(agg$dim_y),
# #           x_name = "x_name", x_levels = unique(agg$dim_x),
# #           y_name = "y_name", y_levels = unique(agg$dim_y))
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
# ## Case 3: From 4 freq values and layout description:
# # AE: data = Vector of 4 basic values (abcd, read in by-row direction):
#
# # 1. The mammography problem:
# abcd <- c(8, 95, 2, 895)  # Frequencies (Gigerenzer & Hoffrage, 1995)
# (mp <- mlm_frame(data = abcd, x = "Condition", y = "Test",
#                  x_levels = c("cancer", "no cancer"),
#                  y_levels = c("positive", "negative")))
#
# # 2. Titanic passengers:
# abcd <- c(344, 367, 126, 1364)
#
# # Basics:
# (m3_a <- mlm_frame(data = abcd, x = "sex", y = "survived"))
#
# # add level labels:
# (m3_a <- mlm_frame(data = abcd, x = "sex", y = "survived",
#                    x_levels = c("female", "male"), y_levels = c(1, 0)))
#
# # Note: Levels must match abcd structure:
# m1_a  # original to recreate:
# m3_a <- mlm_frame(data = c(126, 1364, 344, 367),
#                   x = "sex", y = "survived",
#                   x_levels = c("female", "male"), y_levels = c(0, 1))
# # m3_a
# all.equal(m3_a, m1_a)
#
# # Recreate m1_c from description:
# m1_c  # original to recreate:
# m3_c <- mlm_frame(data = c(28, 29, 17, 35),
#                   x = "sex", y = "survived",
#                   x_name = "Gender", y_name = "Alive",
#                   x_levels = c("female", "male"), y_levels = c(1, 0))
# # m3_c
# all.equal(m1_c, m3_c)
#
# # Recreate m2_c from description:
# m2_c  # original to recreate:
# m3_d <- mlm_frame(data = c(28, 29, 17, 35),
#                   x = "sex", y = "survived",
#                   x_name = "Gender", y_name = "Alive",
#                   x_levels = c("Female", "Male"), y_levels = c("Yes", "No"))
# all.equal(m2_c, m3_d)

# # Case 4: From a table (as text):
#
# mam_pro <-
# "   Condition Cancer no_Cancer
# Test
# positive      8        95
# negative      2       895"
# #
# # as.table(stats::read.ftable(textConnection(mam_pro, encoding = "UTF-8")))
#
# (m4a <- mlm_frame(mam_pro))
#
# titanic_sx_sv <-
#   "Sex Female Male
# Survival
# TRUE    344  367
# FALSE   126 1364"
#
# (m4b <- mlm_frame(titanic_sx_sv))



# # ReDo Figure 3: Frame 3 matrices of Titanic passengers (from data):
# # <https://www.frontiersin.org/files/Articles/567817/fpsyg-11-567817-HTML-r4/image_m/fpsyg-11-567817-g003.jpg>
#
# df_con <- as.data.frame(Titanic)  # data as contingency table
# head(df_con)
# # A: x = Age, y = Sex:
# fig_3a <- mlm_frame(df_con, x = "Age", y = "Sex", freq_var = "Freq",
#                     x_levels = c("Adult", "Child"), y_levels = c("Female", "Male"))
# # B: x = Age, y = Survival:
# fig_3b <- mlm_frame(df_con, x = "Age", y = "Survived", freq_var = "Freq",
#                     x_levels = c("Adult", "Child"), y_levels = c("Yes", "No"))
# # C: x = Sex, y = Survival:
# fig_3c <- mlm_frame(df_con, x = "Sex", y = "Survived", freq_var = "Freq",
#                     x_levels = c("Female", "Male"), y_levels = c("Yes", "No"))
# # Show:
# fig_3a
# fig_3b
# fig_3c




# riskyr_mx: Convert a 2x2 matrix (as contingency table) into a riskyr scenario: ------

# ToDo: Integrate riskyr_mx() into riskyr() function.
# (Currently NOT used or exported.)

riskyr_mx <- function(mx, ...){

  # 0. Initialize: ----
  out <- NA

  if (!is_matrix(mx)){  # verify mx:

    # message("riskyr_mx: mx is not a valid matrix.")  # 4debugging

    return(NA)

  } else {


    # 1. Extract info from mx: ----
    acbd <- as.vector(mx)  # 4 essential frequency counts (by-column: hi, mi, fa, cr)

    dim_list <- dimnames(mx)  # Names/labels: Dimension y before x in table!

    dim_names <- names(dim_list)
    y_name <- dim_names[1]
    x_name <- dim_names[2]

    y_levels <- dim_list[[1]]
    x_levels <- dim_list[[2]]


    # 2. Use info to create riskyr scenario: ----
    out <- riskyr(hi = acbd[1], mi = acbd[2], fa = acbd[3], cr = acbd[4],
                  cond_lbl = x_name, cond_true_lbl = x_levels[1], cond_false_lbl = x_levels[2],
                  dec_lbl = y_name, dec_pos_lbl = y_levels[1], dec_neg_lbl = y_levels[2],
                  ...)

  } # else end.

  return(out)

} # riskyr_mx().

## Check:
# riskyr_mx(mx = 1:4)
# plot(riskyr_mx(mlm_frame(1:4, x = "X dim", y = "Y dim")))
#
# # 1. The mammography problem:
# abcd <- c(8, 95, 2, 895)  # Frequencies (Gigerenzer & Hoffrage, 1995)
# mp <- mlm_frame(data = abcd, x = "Condition", y = "Test",
#             x_levels = c("cancer", "no cancer"),
#             y_levels = c("positive", "negative"))
#
# smp <- riskyr_mx(mp)
# plot(smp, type = "table")


## (+) Transformations: ------

# # 1. The mammography problem:
# abcd <- c(8, 95, 2, 895)  # Frequencies (Gigerenzer & Hoffrage, 1995)
# mp <- mlm_frame(data = abcd, x = "Condition", y = "Test",
#             x_levels = c("cancer", "no cancer"),
#             y_levels = c("positive", "negative"))
#
# # Infos:
# mp
# dim(mp)
#
# is.matrix(mp)
# is.table(mp)
# typeof(mp)
#
# dimnames(mp)
# dimnames(mp)[[1]]
#
# # Sums and summary:
# sum(mp)
# rowSums(mp)
# colSums(mp)
#
# summary(mp)
#
# # # Probabilities and marginal probabilities:
# prop.table(mp, margin = NULL)  # by cells
# prop.table(mp, margin = 1)     # by rows
# prop.table(mp, margin = 2)     # by cols
# # # ToDo: Diagonal (margin = 3)


# diaSums: Diagonal sums of a 2x2 matrix: ------

# (Currently NOT used or exported.)

diaSums <- function(mx){

  out <- NA

  if (!is_matrix(mx)){  # verify mx:

    # message("diaSums: mx is not a valid matrix.")  # 4debugging

    return(NA)

  } else { # transform mx:

    # 4 essential frequency values:
    acbd <- as.vector(mx)  # values in by-col direction

    a <- acbd[1]
    c <- acbd[2]  # mx[2, 1]
    b <- acbd[3]  # mx[1, 2]
    d <- acbd[4]

    sum_ad <- (a + d)
    sum_bc <- (b + c)

  } # else end.

  # Output:
  out <- c(sum_ad, sum_bc)
  names(out) <- c("a+d", "b+c")

  return(out)

} # diaSums().

## Check:
# diaSums(1:4)
# diaSums(mlm_frame(c(1, 3, 5, 9), x = "X", y = "Y"))



# mlm_trans: Transform a 2x2 matrix (into a table of probabilities/conditional probabilities): ------

#' Transform a 2x2 matrix (into a table of probabilities/conditional probabilities).
#'
#' \code{mlm_trans} converts a 2x2 matrix \code{mx}
#' (i.e., a numeric contingency table of frequency counts)
#' into a table of probabilities or marginal probabilities.
#'
#' By default, \code{mlm_trans} converts a 2x2 matrix of frequency counts
#' into the corresponding probability values (\code{margin = 0}).
#'
#' Setting \code{margin} to 1 to 3 computes
#' conditional probabilities in a
#' by row (\code{margin = 1}),
#' by column (\code{margin = 2}), or
#' by diagonal (\code{margin = 3}) fashion.
#'
#' The tables for \code{margin = 0} to \code{margin = 2}
#' are computed by the \strong{base} R function \code{\link{prop.table}}.
#'
#' @param mx A 2x2 matrix (as numeric contingency table, required).
#'
#' @param margin Margin to conditionalize table values (numeric, from 0 to 3).
#' Default: \code{margin = 0} (i.e., unconditional probabilities).
#'
#' @param as_pc Boolean: Convert probabilities into percentages?
#' Default: \code{as_pc = FALSE}.
#'
#' @param n_digits Number of decimal places to which result is rounded.
#' Default: \code{n_digits = 3}.
#'
#' @return A numeric 2x2 table.
#'
#' @examples
#' # The mammography problem:
#' abcd <- c(8, 95, 2, 895)  # Frequencies (Gigerenzer & Hoffrage, 1995)
#' mp <- mlm_frame(data = abcd, x = "Condition", y = "Test",
#'                 x_levels = c("cancer", "no cancer"),
#'                 y_levels = c("positive", "negative"))
#'
#' mlm_trans(mp)
#' mlm_trans(mp, margin = 0)  # by-cell: probabilities
#' mlm_trans(mp, margin = 1)  # by-row: conditional prob
#' mlm_trans(mp, margin = 2)  # by-col
#' mlm_trans(mp, margin = 3)  # by-diagonals
#'
#' mlm_trans(mp, as_pc = TRUE, n_digits = 2)  # as percentages
#'
#' # The following must sum to 1:
#' sum(mlm_trans(mp, margin = 0))      # 4 cell values
#' rowSums(mlm_trans(mp, margin = 1))  # 2 row sums
#' colSums(mlm_trans(mp, margin = 2))  # 2 col sums
#'
#'
#' @family matrix lens model functions
#'
#' @seealso
#' \code{\link{mlm_frame}} creates a 2x2 matrix;
#' \code{\link{mlm_focus}} computes measures for a 2x2 matrix;
#' \code{\link{is_matrix}} verifies a 2x2 matrix.
#'
#' @export

mlm_trans <- function(mx,
                      margin = 0, as_pc = FALSE, n_digits = 3){

  # 0. Initialize: ----
  out <- NA
  if (is.null(margin)) { margin <- 0 }  # use default

  if (!is_matrix(mx)){  # verify mx:

    # message("mlm_trans: mx is not a valid matrix.")  # 4debugging

    return(NA)

  } else { # transform mx:

    # Distinguish cases by margin value:
    if (margin == 0){ # cell proportions/probabilities:

      out <- prop.table(mx, margin = NULL)

    } else if (margin == 1){ # conditionalize by-row:

      out <- prop.table(mx, margin = 1)

    } else if (margin == 2){ # conditionalize by-col:

      out <- prop.table(mx, margin = 2)

    } else if (margin == 3){ # conditionalize by-diagonal:

      # 4 essential frequency values:
      acbd <- as.vector(mx)  # values in by-col direction

      a <- acbd[1]
      c <- acbd[2]  # mx[2, 1]
      b <- acbd[3]  # mx[1, 2]
      d <- acbd[4]

      sum_ad <- (a + d)
      sum_bc <- (b + c)

      out <- mx  # re-initialize out

      # Change cell values:
      out[1, 1] <- a/sum_ad
      out[1, 2] <- b/sum_bc
      out[2, 1] <- c/sum_bc
      out[2, 2] <- d/sum_ad

    } else {

      message("mlm_trans: Unknown margin value.")

    } # if (margin) end.

  } # else end.


  # Output: ----

  if (is.table(out)){  # more relaxed than is_matrix():

    if (as_pc){  # as percentage:
      out <- as_pc(out, n_digits = n_digits)
    } else {  # rounding:
      out <- round(out, digits = n_digits)
    }

  }

  return(out)

} # mlm_trans().

## Check:
# # 1. The mammography problem:
# abcd <- c(8, 95, 2, 895)  # Frequencies (Gigerenzer & Hoffrage, 1995)
# mp <- mlm_frame(data = abcd, x = "Condition", y = "Test",
#                 x_levels = c("cancer", "no cancer"),
#                 y_levels = c("positive", "negative"))
#
# mp
# mlm_trans(mp)
# mlm_trans(mp, margin = 0)  # by-cell: probabilities
# mlm_trans(mp, margin = 1)  # by-row
# mlm_trans(mp, margin = 2)  # by-col
# mlm_trans(mp, margin = 3)  # by-diagonal
#
# # Note:
# mlm_trans(mp, as_pc = TRUE, n_digits = 2)  # percentages
# mlm_trans(NA)
# mlm_trans(1:4)
#
# # The following must sum to 1:
# sum(mlm_trans(mp, margin = 0))      # 4 cell values
# rowSums(mlm_trans(mp, margin = 1))  # 2 row sums
# colSums(mlm_trans(mp, margin = 2))  # 2 col sums
# diaSums(mlm_trans(mp, margin = 3))  # 2 diagonal sums

# # ReDo Figure 4: Define a 2x2 matrix (from description) and 4 typical transformations:
# # <https://www.frontiersin.org/files/Articles/567817/fpsyg-11-567817-HTML-r4/image_m/fpsyg-11-567817-g004.jpg>
#
# # Frame matrix (from description):
# fig_4_1 <- mlm_frame(data = c(8, 95, 2, 895),  # 4 frequencies
#                      x = "True condition", y = "Test outcome",
#                      x_levels = c("C+", "C-"),
#                      y_levels = c("T+", "T-"))
# # Transformations:
# fig_4_2  <- mlm_trans(fig_4_1, margin = 0)  # as probabilities
# fig_4_3a <- mlm_trans(fig_4_1, margin = 1)  # conditionalize (by row)
# fig_4_3b <- mlm_trans(fig_4_1, margin = 2)  # conditionalize (by col)
# fig_4_3c <- mlm_trans(fig_4_1, margin = 3)  # conditionalize (by diagonal)
#
# # Show:
# fig_4_1
# fig_4_2
# fig_4_3a
# fig_4_3b
# fig_4_3c



## (3) Focusing: ------

# mlm_focus: Focus on a 2x2 matrix to compute some measure(s): ------

#' Focus on a 2x2 matrix to compute some measure(s).
#'
#' \code{mlm_focus} adopts particular perspectives on a 2x2 matrix \code{mx}
#' to compute various scientific measures.
#'
#' If no subset of \code{measures} is specified,
#' \code{mlm_focus} computes the 36 measures for a 2x2 matrix \code{mx}
#' that are contained in Table 3 of Neth et al., (2021)
#' (\url{https://doi.org/10.3389/fpsyg.2020.567817}).
#'
#' @param mx A 2x2 matrix (as numeric contingency table, required).
#'
#' @param measures A vector of the measure(s) to compute.
#' Default: \code{measures = c("all")} (i.e., all available measures).
#'
#' @param as_pc Boolean: Convert probabilities into percentages?
#' Default: \code{as_pc = FALSE}.
#'
#' @param n_digits Number of decimal places to which result is rounded.
#' Default: \code{n_digits = 3}.
#'
#' @return A named numeric vector.
#'
#' @examples
#' # 1. The mammography problem:
#' abcd <- c(8, 95, 2, 895)  # Frequencies (Gigerenzer & Hoffrage, 1995)
#' mp <- mlm_frame(data = abcd, x = "Condition", y = "Test",
#'             x_levels = c("cancer", "no cancer"),
#'             y_levels = c("positive", "negative"))
#'
#' # 1. Computing ALL measures at once (and selecting afterwards):
#' mlm_focus(mp)
#' mlm_focus(mp)["PPV"]
#' mlm_focus(mp)[c("dPc", "dPr", "Chi")]
#'
#' # 2. Computing individual measures:
#' # 4 frequencies:
#' mlm_focus(mp, measures = c("TP", "fp", "fn", "TN"))
#' sum(mlm_focus(mp, measures = c("TP", "fp", "fn", "TN")))  # N
#'
#' # 3 marginal probabilities:
#' mlm_focus(mp, measures = c("prev", "bias", "Acc"))
#'
#' # 8 conditional probabilities:
#' mlm_focus(mp, measures = c("sens", "fpr", "fnr", "spec"))  # X/by-col
#' mlm_trans(mp, margin = 2)
#' mlm_focus(mp, measures = c("PPV", "FDR", "FOR", "NPV"))    # Y/by-row
#' mlm_trans(mp, margin = 1)
#'
#' # 3 triangular measures:
#' mlm_focus(mp, measures = c("Jaccard", "F1", "G2"))
#' # 2 mixed scores:
#' mlm_focus(mp, measures = c("lift", "RI"))
#'
#' # 8 difference-based measures:
#' mlm_focus(mp, measures = c("dPc", "NNT", "BACC", "RRR",
#'                        "dPr", "kappa", "MCC", "Chi"))
#'
#' # 3 simple odds:
#' mlm_focus(mp, measures = c("pre-test odds", "post-test odds+", "post-test odds-"))
#'
#' # 5 odds ratios:
#' mlm_focus(mp, measures = c("LR+", "LR-", "DOR", "Q", "Y"))
#'
#' # Typical uses:
#' mlm_focus(mp, measures = c("sens", "spec", "ppv", "npv"))
#'
#' @family matrix lens model functions
#'
#' @seealso
#' \code{\link{mlm_frame}} creates a 2x2 matrix;
#' \code{\link{mlm_trans}} converts a 2x2 matrix (into a table of probabilities/conditional probabilities);
#' \code{\link{is_matrix}} verifies a 2x2 matrix.
#'
#' @export

mlm_focus <- function(mx,
                      measures = c("all"),
                      as_pc = FALSE, n_digits = 3){

  # 0. Initialize: ----

  # Constant (of all current measure acronyms):
  all_measures <-
    c("TP", "FP", "FN", "TN",          # 4 frequencies
      "prev", "bias", "Acc",           # 3 marginal probabilities
      "sens", "fpr", "fnr", "spec",    # 4 conditional probabilities (by-col)
      "PPV", "FDR", "FOR", "NPV",      # 4 conditional probabilities (by-row)
      "Jaccard", "F1", "G2",           # 3 triangular measures
      "lift", "RI",                    # 2 mixed scores
      "dPc", "NNT", "BACC", "RRR",     # 8 difference-based measures
      "dPr", "kappa", "MCC", "Chi",
      "pre-test odds",                 # 3 simple odds
      "post-test odds+", "post-test odds-",
      "LR+", "LR-", "DOR", "Q", "Y")   # 5 odds ratios

  if ("all" %in% tolower(measures)){ # reset user inputs:
    measures_ORG <- all_measures       # provide ALL measures
    measures <- tolower(all_measures)  # (in lowercase below)
  } else { # use user inputs:
    measures_ORG <- measures           # store user inputs
    measures <- tolower(measures)      # (in lowercase below)
  }


  if (!is_matrix(mx)){  # verify mx:

    message("mlm_focus: mx is not a valid matrix.")  # 4debugging

    return(NA)

  } else {

    # 0. Initialize output:
    out <- rep(NA, length(measures))

    # 1. Get 4 essential frequency values:
    acbd <- as.vector(mx)  # values in by-col direction

    a <- acbd[1]
    c <- acbd[2]  # mx[2, 1]
    b <- acbd[3]  # mx[1, 2]
    d <- acbd[4]

    # Key combinations:
    N <- sum(acbd)            # population
    ad_bc <- ((a*d) - (b*c))  # determinant

  } # else end.


  # 2. Main: Compute desired measures ----

  # (a) Frequencies: ----

  if ("tp" %in% measures) { # tp/hi/a:

    ix  <- which(measures == "tp")
    out[ix] <- a

  }

  if ("fp" %in% measures) { # fp/fa/b:

    ix  <- which(measures == "fp")
    out[ix] <- b

  }

  if ("fn" %in% measures) { # fn/mi/c:

    ix  <- which(measures == "fn")
    out[ix] <- c

  }

  if ("tn" %in% measures) { # tn/cr/d:

    ix  <- which(measures == "tn")
    out[ix] <- d

  }

  # (b) Marginal probabilities: ----

  if ("prev" %in% measures) {

    ix  <- which(measures == "prev")
    prev <- (a+c)/N
    out[ix] <- prev

  }

  if ("bias" %in% measures) {

    ix  <- which(measures == "bias")
    bias <- (a+b)/N
    out[ix] <- bias

  }

  if ("acc" %in% measures) {

    ix  <- which(measures == "acc")
    acc <- (a+d)/N
    out[ix] <- acc

  }

  # (c) Conditional probabilities: ----

  # on X (by-col):

  if ("sens" %in% measures) { # sens/TPR/hit rate/recall/1-beta/power/AR+/EER:

    ix  <- which(measures == "sens")
    sens <- a/(a+c)
    out[ix] <- sens

  }

  if ("fpr" %in% measures) { # FPR/FAR/fart/fallout/alpha/AR-/CER:

    ix  <- which(measures == "fpr")
    fpr <- b/(b+d)
    out[ix] <- fpr

  }

  if ("fnr" %in% measures) { # FNR/miss rate/beta:

    ix  <- which(measures == "fnr")
    fnr <- c/(a+c)
    out[ix] <- fnr

  }

  if ("spec" %in% measures) { # spec/TNR/inverse recall/1-alpha:

    ix  <- which(measures == "spec")
    spec <- d/(b+d)
    out[ix] <- spec

  }

  # on Y (by-row):

  if ("ppv" %in% measures) { # PPV/precision/confidence/PPP:

    ix  <- which(measures == "ppv")
    ppv <- a/(a+b)
    out[ix] <- ppv

  }

  if ("fdr" %in% measures) { # FDR:

    ix  <- which(measures == "fdr")
    fdr <- b/(a+b)
    out[ix] <- fdr

  }

  if ("for" %in% measures) { # FOR:

    ix  <- which(measures == "for")
    FOR <- c/(c+d)
    out[ix] <- FOR

  }

  if ("npv" %in% measures) { # NPV/inverse precision/spec_y:

    ix  <- which(measures == "npv")
    npv <- d/(c+d)
    out[ix] <- npv

  }


  # (d) Triangular scores: ----

  if ("jaccard" %in% measures) { # Jaccard score/TS/CSI:

    ix  <- which(measures == "jaccard")
    jacc <- a/(a+b+c)
    out[ix] <- jacc

  }

  if ("f1" %in% measures) { # F1 score/dice coefficient/PS+:

    ix  <- which(measures == "f1")
    F1 <- (2 * a)/((2 * a) + b + c)
    out[ix] <- F1

  }

  if ("g2" %in% measures) { # G(^2) score/cosine:

    ix  <- which(measures == "g2")
    G2 <- a/sqrt((a+b) * (a+c))
    out[ix] <- G2

  }


  # (e) Mixed scores:

  if ("lift" %in% measures) { # lift/interest:

    ix  <- which(measures == "lift")
    lift <- (N * a)/((a+b) * (a+c))
    out[ix] <- lift

  }

  if ("ri" %in% measures) { # RI/Piatetsky-Shapiro's rule-interest:

    ix  <- which(measures == "ri")
    RI <- ad_bc/(N^2)
    out[ix] <- RI

  }


  # (f) Difference-based scores:

  if ("dpc" %in% measures) { # delta Pc/contingency (columns)/BI/Bookmaker informedness/J/Youden's index/ARR/ARI/Attributable risk/Risk difference/Uplift:

    ix  <- which(measures == "dpc")
    dPc <- ad_bc/((a+c) * (b+d))  # cols in denominator
    out[ix] <- dPc

  }

  if ("nnt" %in% measures) { # NNT/number needed to treat/NNH/number needed to harm:

    ix  <- which(measures == "nnt")
    NNT <- ((a+c) * (b+d))/ad_bc  # inverse of dPc
    out[ix] <- NNT

  }

  if ("bacc" %in% measures) { # BACC/balanced accuracy:

    ix  <- which(measures == "bacc")
    BACC <- (ad_bc/((a+c) * (b+d)) + 1)/2
    out[ix] <- BACC

  }

  if ("rrr" %in% measures) { # RRR/relative risk reduction/RRI/relative risk increase:

    ix  <- which(measures == "rrr")
    RRR <- ad_bc/((a*b) + (b*c))
    out[ix] <- RRR

  }

  if ("dpr" %in% measures) { # delta Pr/contingency (rows)/MK/Markedness/E/Difference coefficient

    ix  <- which(measures == "dpr")
    dPr <- ad_bc/((a+d) * (c+d))  # rows in denominator
    out[ix] <- dPr

  }

  if ("kappa" %in% measures) { # Cohen's kappa:

    ix  <- which(measures == "kappa")
    kappa <- (2 * ad_bc)/((a+b) * (c+d) * (a+c) * (b+d))   # (rows * cols) in denominator
    out[ix] <- kappa

  }

  if ("mcc" %in% measures) { # MCC/Matthews correlation coefficient/r/Correlation coefficient/Root mean square contingency/Phi coefficient:

    ix  <- which(measures == "mcc")
    MCC <- ad_bc/sqrt((a+b) * (c+d) * (a+c) * (b+d))   # sqrt(rows * cols) in denominator
    out[ix] <- MCC

  }

  if ("chi" %in% measures) { # chi^2/Chi-square contingency/Test for independence:

    ix  <- which(measures == "chi")
    chi2 <- (N * (ad_bc^2))/((a+b) * (c+d) * (a+c) * (b+d))   # (rows * cols) in denominator
    out[ix] <- chi2

  }


  # (g) Odds/simple odds:

  if ("pre-test odds" %in% measures) { # pre-test/prior odds/class ratio/skew/odds:

    ix  <- which(measures == "pre-test odds")
    pre_odds <- (a+c)/(b+d)  # ratio of columns
    out[ix] <- pre_odds

  }

  if ("post-test odds+" %in% measures) { # post-test odds + (pos.):

    ix  <- which(measures == "post-test odds+")
    post_odds_p <- a/b  # ratio of row+
    out[ix] <- post_odds_p

  }

  if ("post-test odds-" %in% measures) { # post-test odds - (neg.):

    ix  <- which(measures == "post-test odds-")
    post_odds_n <- c/d  # ratio of row-
    out[ix] <- post_odds_n

  }


  # (h) Odds ratios:

  if ("lr+" %in% measures) { # LR+/positive likelihood ratio/Neyman-Pearson test/RR+/relative risk/risk ratio:

    ix  <- which(measures == "lr+")
    lr_p <- (a/(a + c))/(b/(b + d))  # by col, numerator+
    out[ix] <- lr_p

  }

  if ("lr-" %in% measures) { # LR-/negative likelihood ratio:

    ix  <- which(measures == "lr-")
    lr_n <- (c/(a + c))/(d/(b + d))  # by col, numerator-
    out[ix] <- lr_n

  }

  if ("dor" %in% measures) { # DOR/diagnostic odds ratio/OR/odds ratio/cross ratio/approximate relative risk:

    ix  <- which(measures == "dor")
    DOR <- (a*d)/(b*c)  # ratio of diagonal products
    out[ix] <- DOR

  }

  if ("q" %in% measures) { # Q/Yule's Q:

    ix  <- which(measures == "q")
    Yule_Q <- ad_bc/((a*d) + (b*c))
    out[ix] <- Yule_Q

  }

  if ("y" %in% measures) { # Y/Yule's Y:

    ix  <- which(measures == "y")
    Yule_Y <- (sqrt(a*d) - sqrt(b*c))/(sqrt(a*d) + sqrt(b*c))
    out[ix] <- Yule_Y

  }

  # Output: ----

  if (!all(is.na((out)))){

    if (as_pc){  # as percentage:
      out <- as_pc(out, n_digits = n_digits)
    } else {  # rounding:
      out <- round(out, digits = n_digits)
    }

  }

  # sep <- "="
  # out <- paste(measures_ORG, sep, out)

  names(out) <- measures_ORG

  return(out)

} # mlm_focus().


# +++ here now +++


## Check:
# # 1. The mammography problem:
# abcd <- c(8, 95, 2, 895)  # Frequencies (Gigerenzer & Hoffrage, 1995)
# # abcd <- c(0, 0, 0, 0)  # test
# mp <- mlm_frame(data = abcd, x = "Condition", y = "Test",
#             x_levels = c("cancer", "no cancer"),
#             y_levels = c("positive", "negative"))
#
# # 1. Computing ALL measures at once (and selecting afterwards):
# mlm_focus(mp)
# mlm_focus(mp)["PPV"]
# mlm_focus(mp)[c("dPc", "dPr", "Chi")]
#
# # 2. Computing individual measures:
# # 4 frequencies:
# mlm_focus(mp, measures = c("TP", "fp", "fn", "TN"))
# sum(mlm_focus(mp, measures = c("TP", "fp", "fn", "TN")))  # N
#
# # 3 marginal probabilities:
# mlm_focus(mp, measures = c("prev", "bias", "Acc"))
#
# # 8 conditional probabilities:
# mlm_focus(mp, measures = c("sens", "fpr", "fnr", "spec"))  # X/by-col
# mlm_trans(mp, margin = 2)
# mlm_focus(mp, measures = c("PPV", "FDR", "FOR", "NPV"))    # Y/by-row
# mlm_trans(mp, margin = 1)
#
# # 3 triangular measures:
# mlm_focus(mp, measures = c("Jaccard", "F1", "G2"))
# # 2 mixed scores:
# mlm_focus(mp, measures = c("lift", "RI"))
#
# # 8 difference-based measures:
# mlm_focus(mp, measures = c("dPc", "NNT", "BACC", "RRR", "dPr", "kappa", "MCC", "Chi"))
#
# # 3 simple odds:
# mlm_focus(mp, measures = c("pre-test odds", "post-test odds+", "post-test odds-"))
#
# # 5 odds ratios:
# mlm_focus(mp, measures = c("LR+", "LR-", "DOR", "Q", "Y"))
#
# # # Typical uses:
# # mlm_focus(mp, measures = c("sens", "spec", "ppv", "npv"))


## Test:
# chisq.test(mp)
# chisq.test(mp, correct = FALSE)
# chisq.test(m2_a)
# chisq.test(m3_a)


## (4) Presenting/Visualizing: ------

## (a) Mosaic plot:
# mosaicplot(t(mp), color = c("indianred2", "grey75"), main = "The mammography problem")
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

} # get_a().


# get_b: -----

get_b <- function(a = mx[1], b = mx[2], c = mx[3], d = mx[4]){

  out <- NA

  out <- (b)

  return(out)

} # get_b().


# get_c: -----

get_c <- function(a = mx[1], b = mx[2], c = mx[3], d = mx[4]){

  out <- NA

  out <- (c)

  return(out)

} # get_c().


# get_d: -----

get_d <- function(a = mx[1], b = mx[2], c = mx[3], d = mx[4]){

  out <- NA

  out <- (d)

  return(out)

} # get_d().



# get_N: -----

get_N <- function(a = mx[1], b = mx[2], c = mx[3], d = mx[4]){

  out <- NA

  out <- (a + b + c + d)

  return(out)

} # get_N().



# (2) Probabilities: ------

# get_prev: -----

get_prev <- function(a = mx[1], b = mx[2], c = mx[3], d = mx[4]){

  out <- NA

  out <- (a + c)/(a + b + c + d)

  return(out)

} # get_prev().


# get_bias: -----

get_bias <- function(a = mx[1], b = mx[2], c = mx[3], d = mx[4]){

  out <- NA

  out <- (a + b)/(a + b + c + d)

  return(out)

} # get_bias().



# get_ACC: -----

get_ACC <- function(a = mx[1], b = mx[2], c = mx[3], d = mx[4]){

  out <- NA

  out <- (a + d)/(a + b + c + d)

  return(out)

} # get_ACC().



# (3) Conditional Probabilities: ------

# (3a) normalized by column:

# get_sens: -----

get_sens <- function(a = mx[1], b = mx[2], c = mx[3], d = mx[4]){

  out <- NA

  out <- (a)/(a + c)

  return(out)

} # get_sens().



# get_TPR: -----

get_TPR <- function(a = mx[1], b = mx[2], c = mx[3], d = mx[4]){

  out <- NA

  out <- get_sens(a = a, b = b, c = c, d = d)  # TPR = sens

  return(out)

} # get_TPR().



# get_FPR: -----

get_FPR <- function(a = mx[1], b = mx[2], c = mx[3], d = mx[4]){

  out <- NA

  out <- (b)/(b + d)

  return(out)

} # get_FPR().



# get_FNR: -----

get_FNR <- function(a = mx[1], b = mx[2], c = mx[3], d = mx[4]){

  out <- NA

  out <- (c)/(a + c)

  return(out)

} # get_FNR().



# get_spec: -----

get_spec <- function(a = mx[1], b = mx[2], c = mx[3], d = mx[4]){

  out <- NA

  out <- (d)/(b + d)

  return(out)

} # get_spec().



# get_TNR: -----

get_TNR <- function(a = mx[1], b = mx[2], c = mx[3], d = mx[4]){

  out <- NA

  out <- get_spec(a = a, b = b, c = c, d = d)  # TNR = spec

  return(out)

} # get_TNR().


# (3b) normalized by row:


# get_PPV: -----

get_PPV <- function(a = mx[1], b = mx[2], c = mx[3], d = mx[4]){

  out <- NA

  out <- (a)/(a + b)

  return(out)

} # get_PPV().


# get_FDR: -----

get_FDR <- function(a = mx[1], b = mx[2], c = mx[3], d = mx[4]){

  out <- NA

  out <- (b)/(a + b)

  return(out)

} # get_FDR().


# get_FOR: -----

get_FOR <- function(a = mx[1], b = mx[2], c = mx[3], d = mx[4]){

  out <- NA

  out <- (c)/(c + d)

  return(out)

} # get_FOR().


# get_NPV: -----

get_NPV <- function(a = mx[1], b = mx[2], c = mx[3], d = mx[4]){

  out <- NA

  out <- (d)/(c + d)

  return(out)

} # get_NPV().



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

} # get_LRp().



# get_LRn: -----

get_LRn <- function(a = mx[1], b = mx[2], c = mx[3], d = mx[4]){

  out <- NA
  spec <- NA
  FNR <- NA

  FNR  <- get_FNR(a, b, c, d)
  spec <- get_spec(a, b, c, d)

  out <- FNR/spec

  return(out)

} # get_LRn().


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

} # get_DOR().



# (5) Integrated measures: ------


# (a) Contingencies: ------

# get_dPr: Row contingency -----

get_dPr <- function(a = mx[1], b = mx[2], c = mx[3], d = mx[4]){

  out <- NA

  out <- ((a)/(a + b)) - ((c)/(c + d))  # PPV - FOR

  return(out)

} # get_dPr().


# get_dPc: Column contingency -----

get_dPc <- function(a = mx[1], b = mx[2], c = mx[3], d = mx[4]){

  out <- NA

  out <- ((a)/(a + c)) - ((b)/(b + d))  # sens - FPR

  return(out)

} # get_dPc().


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

} # get_chi2().



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

} # get_MCC().


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

} # get_F1s().


# (6) Risk measures: ------

# AR: Absolute risk (i.e., 2 conditional probabilities)
#     - correspondence of AR+ to sens/PPV and
#     -                of AR- to FPR/1-spec/1-NPV (depending on matrix dimensions/direction).

# AR+: Same as sens -----

get_ARp <- function(a = mx[1], b = mx[2], c = mx[3], d = mx[4]){

  get_sens(a, b, c, d)

} # get_ARp().



# AR-: Same as FPR -----

get_ARm <- function(a = mx[1], b = mx[2], c = mx[3], d = mx[4]){

  get_FPR(a, b, c, d)

} # get_ARm().



# ARR (absolute risk reduction/increase): Same as Column Contingency (delta Pc) -----

get_ARR <- function(a = mx[1], b = mx[2], c = mx[3], d = mx[4]){

  get_dPc(a, b, c, d)

} # get_ARR().


# RR:  Relative risk
# RRR: Relative risk reduction/increase


## (+) ToDo: ----------

## - Implement matrix lens model in 3 steps:
##   1. filtering: binarize variables, based on criterion variable and metric to maximize
##   2. framing: Select 2 dimensions, map category levels
##   3. focusing: provide a metric specified

## eof. ------------------------------------------
