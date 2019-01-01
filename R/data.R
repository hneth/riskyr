## data.R | riskyr
## 2018 12 20
## Document the scenarios of ./data
## -----------------------------------------------

#' A collection of riskyr scenarios from various sources.
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

## eof. ------------------------------------------
