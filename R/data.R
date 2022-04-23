## data.R | riskyr
## 2022 04 23
## Document the scenarios of ./data
## -----------------------------------------------

# (1) riskyr scenarios: -------

#' A collection of riskyr scenarios from various sources (as df).
#'
#' \code{df_scenarios} is an R data frame that
#' contains a collection of scenarios from the
#' scientific literature and other sources.
#'
#' When loading \code{riskyr}, all scenarios contained in
#' \code{df_scenarios} are converted into a list of
#' \code{riskyr} objects \code{\link{scenarios}}.
#'
#' @format A data frame with currently 25 rows (i.e., scenarios)
#' and 21 columns (variables describing each scenario):
#'
#' See \code{\link{scenarios}} for a list of scenarios
#' and the variables currently contained in \code{df_scenarios}.
#'
#' Note that names of variables (columns)
#' correspond to a subset of \code{\link{init_txt}} (to initialize \code{\link{txt}})
#' and \code{\link{init_num}} (to initialize \code{\link{num}}).
#'
#' The variables \code{scen_src} and \code{scen_apa}
#' provide a scenario's source information.
#'
#' @family datasets
#'
#' @seealso
#' \code{\link{scenarios}} contains all scenarios as \code{riskyr} objects;
#' \code{\link{riskyr}} initializes a \code{riskyr} scenario;
#' \code{\link{txt}} contains basic text information;
#' \code{\link{init_txt}} initializes text information;
#' \code{\link{num}} contains basic numeric parameters;
#' \code{\link{init_num}} initializes basic numeric parameters;
#' \code{\link{pal}} contains current color information;
#' \code{\link{init_pal}} initializes color information.

"df_scenarios"


# (2) Cumulative risks: -------

# 1. BRCA1: ----

#' Cumulative risk of breast cancer in women with the BRCA1 mutation.
#'
#' \code{BRCA1} provides the cumulative risk of breast cancer
#' in a population of women with the BRCA1 mutation
#' as a function of their age (in years).
#'
#' @format A data frame (11 x 2).
#'
#' \code{x}: age (in years).
#'
#' \code{y}: cumulative risk of developing breast
#' cancer in this (BRCA1) population.
#'
#' @family datasets
#'
#' @seealso
#' \code{\link{plot_crisk}} plots cumulative risk curves.

"BRCA1"


# 2. BRCA2: ----

#' Cumulative risk of breast cancer in women with the BRCA2 mutation.
#'
#' \code{BRCA2} provides the cumulative risk of breast cancer
#' in a population of women with the BRCA2 mutation
#' as a function of their age (in years).
#'
#' @format A data frame (11 x 2).
#'
#' \code{x}: age (in years).
#'
#' \code{y}: cumulative risk of developing breast
#' cancer in this (BRCA2) population.
#'
#' @family datasets
#'
#' @seealso
#' \code{\link{plot_crisk}} plots cumulative risk curves.

"BRCA2"


# 3a. mamRiskViz: Introductory task ----

#' Cumulative risk curve I (introductory task).
#'
#' \code{t_I} provides the cumulative risk of
#' some genetic risk factor for developing a disease
#' in some target population as a function of age.
#'
#' @format A data frame (17 x 2).
#'
#' \code{age}: age (in years).
#'
#' \code{crisk_I}: cumulative risk of developing
#' some disease in the target population.
#'
#' @family datasets
#'
#' @seealso
#' \code{\link{plot_crisk}} plots cumulative risk curves.

"t_I"


# 3b. mamRiskViz: Main/transfer Task A ----

#' Cumulative risk curve A (main/transfer task A).
#'
#' \code{t_A} provides the cumulative risk of
#' some genetic risk factor for developing disease A
#' in some target population as a function of age.
#'
#' @format A data frame (17 x 2).
#'
#' \code{age}: age (in years).
#'
#' \code{crisk_A}: cumulative risk of developing
#' some disease A in the target population.
#'
#' @family datasets
#'
#' @seealso
#' \code{\link{plot_crisk}} plots cumulative risk curves.

"t_A"


# 3c. mamRiskViz: Main/transfer Task B ----

#' Cumulative risk curve B (main/transfer task B).
#'
#' \code{t_B} provides the cumulative risk of
#' some genetic risk factor for developing disease B
#' in some target population as a function of age.
#'
#' @format A data frame (17 x 2).
#'
#' \code{age}: age (in years).
#'
#' \code{crisk_B}: cumulative risk of developing
#' some disease B in the target population.
#'
#' @family datasets
#'
#' @seealso
#' \code{\link{plot_crisk}} plots cumulative risk curves.

"t_B"


# 4a. Breast cancer risk with BRCA1: ------

#' Cumulative risk of breast cancer in women with the BRCA1 mutation.
#'
#' \code{BRCA1_mam} provides the cumulative risk of breast cancer
#' in a population of women with the BRCA1 mutation
#' as a function of their age (in years).
#'
#' @source Based on Figure 2 (p. 2408) of
#' Kuchenbaecker, K. B., Hopper, J. L., Barnes, D. R., Phillips, K. A., Mooij, T. M., Roos-Blom, M. J., ... & BRCA1 and BRCA2 Cohort Consortium (2017).
#' Risks of breast, ovarian, and contralateral breast cancer for BRCA1 and BRCA2 mutation carriers. \emph{JAMA}, \emph{317} (23), 2402--2416.
#' doi: 10.1001/jama.2017.7112
#'
#' @format A data frame (63 x 2).
#'
#' \code{age}: age (in years).
#'
#' \code{cumRisk}: cumulative risk of developing breast
#' cancer in this (BRCA1) population.
#'
#' @family datasets
#'
#' @seealso
#' \code{\link{plot_crisk}} plots cumulative risk curves.

"BRCA1_mam"



## eof. ------------------------------------------
