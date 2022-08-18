## riskyr_sims.R | riskyr
## 2022 08 09
## Enable riskyr simulations (i.e., data from descriptions)
## -----------------------------------------------

# Goals: Interact/translate between data <--> description. ------

# Move from descriptions to data, and from data to description:
# (1) comp_popu(): description: 4 freq   --> data/cases
# (2) comp_simu(): description: scenario --> data/cases
# (3) read_popu(): data/cases --> description/scenario


## (1) Create data (popu, as df) from a description: 4 essential freq ------

# See comp_popu() (in file comp_popu.R).
# Note: 4 freq fully define popu (i.e., allow for no variation due to rounding/sampling).


## (2) Create popu (data, as df) from a riskyr scenario (description) ------

# Goal: Create/expand data from a riskyr scenario (i.e., allowing for a
#       probabilistic description and random sampling!):
#  - input a riskyr scenario (i.e., allowing for random sampling)
#  - use comp_popu() to create and return popu from freq (as df)
# Note: Freq values are rounded to nearest integers.


## write_popu Documentation: ------

#' Write a population table (data) from a riskyr scenario (description).
#'
#' \code{write_popu} computes (or expands) a table \code{\link{popu}}
#' (as an R data frame) from a \code{\link{riskyr}} scenario (description),
#' using its 4 essential frequencies.
#'
#' \code{write_popu} expects a \code{\link{riskyr}} scenario as input
#' and passes its 4 essential frequencies (rounded to integers)
#' to \code{\link{comp_popu}}.
#'
#' By default, \code{write_popu} uses the text settings
#' contained in \code{\link{txt}}, but labels can be changed
#' by passing arguments to \code{\link{comp_popu}} (via \code{...}).
#'
#' @format An object of class \code{data.frame}
#' with \code{\link{N}} rows and 3 columns
#' (e.g., \code{"X/truth/cd", "Y/test/dc", "SDT/cell/class"}).
#'
#' @return A data frame \code{popu}
#' containing \code{\link{N}} rows (individual cases)
#' and 3 columns (e.g., \code{"X/truth/cd", "Y/test/dc", "SDT/cell/class"}).
#' encoded as ordered factors (with 2, 2, and 4 levels, respectively).
#'
#' @param x  A \code{\link{riskyr}} scenario (description).
#'
#' @param ... Additional parameters (text labels, passed to \code{\link{comp_popu}}).
#'
#' @examples
#' # Define scenarios (by description):
#' s1 <- riskyr(prev = .5, sens = .5, spec = .5, N = 10)  # s1: define by 3 prob & N
#' s2 <- riskyr(hi = 2, mi = 3, fa = 2, cr = 3)           # s2: same scenario by 4 freq
#'
#' # Create data (from descriptions):
#' write_popu(s1)  # data from (prob) description
#' write_popu(s2,  # data from (freq) description & change labels:
#'            cond_lbl = "Disease (X)",
#'            cond_true_lbl = "sick", cond_false_lbl = "healthy",
#'            dec_lbl = "Test (Y)")
#'
#' # Rounding:
#' s3 <- riskyr(prev = 1/3, sens = 2/3, spec = 6/7, N = 10, round = FALSE)  # s3: w/o rounding
#' write_popu(s3, cond_lbl = "X", dec_lbl = "Y", sdt_lbl = "class")  # rounded to nearest integers
#'
#' # Sampling:
#' s4 <- riskyr(prev = 1/3, sens = 2/3, spec = 6/7, N = 10, sample = TRUE)  # s4: with sampling
#' write_popu(s4, cond_lbl = "X", dec_lbl = "Y", sdt_lbl = "class")  # data from sampling
#'
#' @family functions converting data/descriptions
#'
#' @seealso
#' \code{\link{comp_popu}} creates data (as df) from description (frequencies);
#' \code{\link{read_popu}} creates a scenario (description) from data (as df);
#' \code{\link{popu}} for data format;
#' \code{\link{txt}} for current text settings;
#' \code{\link{riskyr}} initializes a \code{riskyr} scenario.
#'
#' @export

## write_popu: Definition ------

write_popu <- function(x = NULL,  # a riskyr scenario
                       ...        # other parameters (passed to comp_popu)
) {

  # 1. extract freq from scenario x (and round to integers):
  n_hi <- round(x$hi, 0)
  n_mi <- round(x$mi, 0)
  n_fa <- round(x$fa, 0)
  n_cr <- round(x$cr, 0)

  # 2. pass to comp_popu():
  comp_popu(hi = n_hi, # 4 essential freq
            mi = n_mi,
            fa = n_fa,
            cr = n_cr,
            # use labels from txt
            ... # other parameters
  )

} # write_popu() end.


## Check: ------
# # Define scenarios (by description):
# s1 <- riskyr(prev = .5, sens = .5, spec = .5, N = 10)  # s1: define by 3 prob & N
# s2 <- riskyr(hi = 2, mi = 3, fa = 2, cr = 3)           # s2: same scenario by 4 freq
#
# # Create data (from descriptions):
# write_popu(s1)  # data from (prob) description
# write_popu(s2,  # data from (freq) description & change some labels:
#            cond_lbl = "Disease (X)", cond_true_lbl = "sick", cond_false_lbl = "healthy")
#
# # Rounding:
# s3 <- riskyr(prev = 1/3, sens = 2/3, spec = 6/7, N = 10, round = FALSE)  # s3: w/o rounding
# write_popu(s3, cond_lbl = "X", dec_lbl = "Y", sdt_lbl = "class")  # rounded to nearest integers
#
# # Sampling:
# s4 <- riskyr(prev = 1/3, sens = 2/3, spec = 6/7, N = 10, sample = TRUE)  # s4: with sampling
# write_popu(s4, cond_lbl = "X", dec_lbl = "Y", sdt_lbl = "class")  # data from sampling


## (3) Read riskyr scenario (description) from binary data (popu, as df) ------

## read_popu: Documentation: ------

#' Read population data (from df) into a riskyr scenario (description).
#'
#' \code{read_popu} reads a data frame \code{df}
#' (containing observations of some population
#' that are cross-classified on two binary variables)
#' and returns a \code{riskyr} scenario
#' (i.e., a description of the data).
#'
#' Note that \code{df} needs to be structured (cross-classified)
#' according to the data frame \code{\link{popu}},
#' created by \code{\link{comp_popu}}.
#'
#' @return A \code{riskyr} object describing a risk-related scenario.
#'
#' @param df A data frame providing a population \code{\link{popu}}
#' of individuals, which are identified on at least
#' 2 binary variables and cross-classified into 4 cases in a 3rd variable.
#' Default: \code{df = \link{popu}} (as data frame).
#'
#' @param ix_by_top Index of variable (column) providing the 1st (X/top) perspective (in df).
#' Default: \code{ix_by_top = 1} (1st column).
#'
#' @param ix_by_bot Index of variable (column) providing the 2nd (Y/bot) perspective (in df).
#' Default: \code{ix_by_bot = 2} (2nd column).
#'
#' @param ix_sdt Index of variable (column) providing
#' a cross-classification into 4 cases (in df).
#' Default: \code{ix_by_bot = 3} (3rd column).
#'
#' @param hi_lbl Label of cases classified as hi (TP).
#' @param mi_lbl Label of cases classified as mi (FN).
#' @param fa_lbl Label of cases classified as fa (FP).
#' @param cr_lbl Label of cases classified as cr (TN).
#'
#' @param ... Additional parameters (passed to \code{\link{riskyr}}).
#'
#' @examples
#' # Generating and interpreting different scenario types:
#'
#' # (A) Diagnostic/screening scenario (using default labels): ------
#' popu_diag <- comp_popu(hi = 4, mi = 1, fa = 2, cr = 3)
#' # popu_diag
#' scen_diag <- read_popu(popu_diag, scen_lbl = "Diagnostics", popu_lbl = "Population tested")
#' plot(scen_diag, type = "prism", area = "no", f_lbl = "namnum")
#'
#' # (B) Intervention/treatment scenario: ------
#' popu_treat <- comp_popu(hi = 80, mi = 20, fa = 45, cr = 55,
#'                         cond_lbl = "Treatment", cond_true_lbl = "pill", cond_false_lbl = "placebo",
#'                         dec_lbl = "Health status", dec_pos_lbl = "healthy", dec_neg_lbl = "sick")
#' # popu_treat
#' s_treat <- read_popu(popu_treat, scen_lbl = "Treatment", popu_lbl = "Population treated")
#' plot(s_treat, type = "prism", area = "sq", f_lbl = "namnum", p_lbl = "num")
#' plot(s_treat, type = "icon", lbl_txt = txt_org, col_pal = pal_org)
#'
#' # (C) Prevention scenario (e.g., vaccination): ------
#' popu_vacc <- comp_popu(hi = 960, mi = 40, fa = 880, cr = 120,
#'                        cond_lbl = "Vaccination", cond_true_lbl = "yes", cond_false_lbl = "no",
#'                        dec_lbl = "Disease", dec_pos_lbl = "no flu", dec_neg_lbl = "flu")
#' # popu_vacc
#' s_vacc <- read_popu(popu_vacc, scen_lbl = "Vaccination effects", popu_lbl = "RCT population")
#' plot(s_vacc, type = "prism", area = "sq", f_lbl = "namnum", col_pal = pal_rgb, p_lbl = "num")
#'
#' @family functions converting data/descriptions
#'
#' @seealso
#' \code{\link{comp_popu}} creates data (as df) from description (frequencies);
#' \code{\link{write_popu}} creates data (as df) from a riskyr scenario (description);
#' \code{\link{popu}} for data format;
#' \code{\link{riskyr}} initializes a \code{riskyr} scenario.
#'
#' @export

## read_popu: Definition ------

read_popu <- function(df = popu,  # df (as population data with 3+ columns, see comp_popu)
                      ix_by_top = 1, ix_by_bot = 2, ix_sdt = 3,  # indices of by_top, by_bot, and sdt cols in df
                      # text labels (from txt):
                      hi_lbl = txt$hi_lbl, mi_lbl = txt$mi_lbl, fa_lbl = txt$fa_lbl, cr_lbl = txt$cr_lbl,
                      ...) {

  # 0. Verify inputs: ----
  # ToDo: Ensure that popu is df with appropriate shape/contents

  # 1. Values from cross-classified variable: ----
  sdt_cases <- df[ , ix_sdt]
  sdt_lbl <- names(df)[ix_sdt]

  n_hi <- length(sdt_cases[sdt_cases == hi_lbl])
  n_mi <- length(sdt_cases[sdt_cases == mi_lbl])
  n_fa <- length(sdt_cases[sdt_cases == fa_lbl])
  n_cr <- length(sdt_cases[sdt_cases == cr_lbl])

  # 2. Use other variables only for labels: ----
  cond_lbl <- names(df)[ix_by_top]  # X-dim/top
  cond_true_lbl  <- levels(df[ , ix_by_top])[1]
  cond_false_lbl <- levels(df[ , ix_by_top])[2]

  dec_lbl <- names(df)[ix_by_bot]  # Y-dim/bot
  dec_pos_lbl <- levels(df[ , ix_by_bot])[1]
  dec_neg_lbl <- levels(df[ , ix_by_bot])[2]

  # 3. Create riskyr scenario: ----
  scen <- riskyr(hi = n_hi, mi = n_mi, fa = n_fa, cr = n_cr,
                 cond_lbl = cond_lbl, cond_true_lbl = cond_true_lbl, cond_false_lbl = cond_false_lbl,
                 dec_lbl = dec_lbl, dec_pos_lbl = dec_pos_lbl, dec_neg_lbl = dec_neg_lbl,
                 sdt_lbl = sdt_lbl,
                 ...)

  # 4. Return description: ----
  return(scen)

} # read_popu().

## Check: ----------

# ## Generating and interpreting different scenario types:
#
# # (A) Diagnostic/screening scenario (using default labels): ------
# popu_diag <- comp_popu(hi = 4, mi = 1, fa = 2, cr = 3)
# # popu_diag
# scen_diag <- read_popu(popu_diag, scen_lbl = "Diagnostics", popu_lbl = "Population tested")
# plot(scen_diag, type = "prism", area = "no", f_lbl = "namnum")
#
# # (B) Intervention/treatment scenario: ------
# popu_treat <- comp_popu(hi = 80, mi = 20, fa = 45, cr = 55,
#                         cond_lbl = "Treatment", cond_true_lbl = "pill", cond_false_lbl = "placebo",
#                         dec_lbl = "Health status", dec_pos_lbl = "healthy", dec_neg_lbl = "sick")
# # popu_treat
# s_treat <- read_popu(popu_treat, scen_lbl = "Treatment", popu_lbl = "Population treated")
# plot(s_treat, type = "prism", area = "hr", f_lbl = "namnum", col_pal = "whitesmoke", f_lwd = 1)
# plot(s_treat, type = "icon")
#
# # (C) Prevention scenario (e.g., vaccination): ------
# popu_vacc <- comp_popu(hi = 960, mi = 40, fa = 880, cr = 120,
#                        cond_lbl = "Vaccination", cond_true_lbl = "yes", cond_false_lbl = "no",
#                        dec_lbl = "Disease", dec_pos_lbl = "no flu", dec_neg_lbl = "flu")
# # popu_vacc
# s_vacc <- read_popu(popu_vacc, scen_lbl = "Prevention", popu_lbl = "Population vaccinated")
# plot(s_vacc, type = "prism", area = "sq", f_lbl = "namnum", col_pal = pal_bw, p_lbl = "num")


## (*) Done: ----------

## - etc.

## (+) ToDo: ----------

## - etc.

## eof. ------------------------------------------
