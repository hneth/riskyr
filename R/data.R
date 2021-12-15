## data.R | riskyr
## 2021 12 15
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



## eof. ------------------------------------------
