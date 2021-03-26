## riskyr_sims.R | riskyr
## 2021 03 26
## Enable riskyr simulations (i.e., data from descriptions)
## -----------------------------------------------

# Goals: Interact/translate between data <--> description. ------

# Move from descriptions to data, and from data to description:
# (1) comp_popu(): description: 4 freq   --> data/cases
# (2) comp_simu(): description: scenario --> data/cases
# (3) read_popu(): data/cases --> description/scenario


## (1) Create data (popu, as df) from description: 4 essential freq ------

# See comp_popu() (in file comp_popu.R).
# Note: 4 freq fully define popu (i.e., allow for no variation due to rounding/sampling).


## (2) Create data (popu, as df) from description: A riskyr scenario ------

# ToDo: Create data from a riskyr scenario (i.e., allowing for a
#       probabilistic description and random sampling!)

# +++ here now +++


## (3) Read riskyr scenario from binary data (popu, as df) ------

## read_popu: Documentation: ------

#' Read a population (given as data frame) into a riskyr scenario.
#'
#' \code{read_popu} interprets a data frame \code{df}
#' (containing observations of some population
#' that are cross-classified on two binary variables)
#' and returns a scenario of class \code{"riskyr"}.
#'
#' Note that \code{df} needs to be structured (cross-classified)
#' according to the data frame \code{\link{popu}},
#' created by \code{\link{comp_popu}}.
#'
#' @return An object of class "riskyr" describing a risk-related scenario.
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
#' @param ... Additional parameters (passed to \code{\link{riskyr}} function).
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
#' scen_treat <- read_popu(popu_treat, scen_lbl = "Treatment", popu_lbl = "Population treated")
#' plot(scen_treat, type = "prism", area = "sq", f_lbl = "namnum", p_lbl = "num")
#' plot(scen_treat, type = "icon", lbl_txt = txt_org, col_pal = pal_org)
#'
#' # (C) Prevention scenario (e.g., vaccination): ------
#' popu_vacc <- comp_popu(hi = 960, mi = 40, fa = 880, cr = 120,
#'                        cond_lbl = "Vaccination", cond_true_lbl = "yes", cond_false_lbl = "no",
#'                        dec_lbl = "Disease", dec_pos_lbl = "no flu", dec_neg_lbl = "flu")
#' # popu_vacc
#' scen_vacc <- read_popu(popu_vacc, scen_lbl = "Prevention", popu_lbl = "Population vaccinated")
#' plot(scen_vacc, type = "prism", area = "sq", f_lbl = "namnum", col_pal = pal_bw, p_lbl = "num")
#'
#' @family riskyr scenario functions
#'
#' @seealso
#' the corresponding data frame \code{\link{popu}};
#' the corresponding generating function \code{\link{comp_popu}};
#' \code{\link{riskyr}} initializes a \code{riskyr} scenario.
#'
#' @export

## read_popu: Definition ------

read_popu <- function(df = popu,  # df (as population with 3+ columns, see comp_popu)
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

}

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
# scen_treat <- read_popu(popu_treat, scen_lbl = "Treatment", popu_lbl = "Population treated")
# plot(scen_treat, type = "prism", area = "hr", f_lbl = "namnum", col_pal = "whitesmoke", f_lwd = 1)
# plot(scen_treat, type = "icon")
#
# # (C) Prevention scenario (e.g., vaccination): ------
# popu_vacc <- comp_popu(hi = 960, mi = 40, fa = 880, cr = 120,
#                        cond_lbl = "Vaccination", cond_true_lbl = "yes", cond_false_lbl = "no",
#                        dec_lbl = "Disease", dec_pos_lbl = "no flu", dec_neg_lbl = "flu")
# # popu_vacc
# scen_vacc <- read_popu(popu_vacc, scen_lbl = "Prevention", popu_lbl = "Population vaccinated")
# plot(scen_vacc, type = "prism", area = "sq", f_lbl = "namnum", col_pal = pal_bw, p_lbl = "num")


## (*) Done: ----------

## - etc.

## (+) ToDo: ----------

## - etc.

## eof. ------------------------------------------
