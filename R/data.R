## data.R | riskyr
## 2018 02 14
## -----------------------------------------------
## Document scenarios in ./data
## -----------------------------------------------

#' A collection of riskyr scenarios from various sources.
#'
#' \code{df.scenarios} is an R data frame that
#' contains a collection of scenarios from the
#' scientific literature and other sources.
#'
#' When loading \code{riskyr}, all scenarios contained in
#' \code{df.scenarios} are also converted into a list of
#' \code{riskyr} objects \code{scenarios}.
#'
#'
#' @format A data frame with currently 26 rows (i.e., scenarios)
#' and 21 columns (variables describing each scenario):
#'
#' \enumerate{
#'
#'   \item \code{scen.lbl} Text label for current scenario.
#'   \item \code{scen.lng} Language of current scenario.
#'   \item \code{scen.txt} Description text of current scenario.
#'
#'   \item \code{popu.lbl} Text label for current population.
#'
#'   \item \code{cond.lbl} Text label for current condition.
#'   \item \code{cond.true.lbl} Text label for \code{\link{cond.true}} cases.
#'   \item \code{cond.false.lbl} Text label for \code{\link{cond.false}} cases.
#'
#'   \item \code{dec.lbl} Text label for current decision.
#'   \item \code{dec.pos.lbl} Text label for \code{\link{dec.pos}} cases.
#'   \item \code{dec.neg.lbl} Text label for \code{\link{dec.neg}} cases.
#'
#'   \item \code{hi.lbl} Text label for cases of hits \code{\link{hi}}.
#'   \item \code{mi.lbl} Text label for cases of misses \code{\link{mi}}.
#'   \item \code{fa.lbl} Text label for cases of false alarms \code{\link{fa}}.
#'   \item \code{cr.lbl} Text label for cases of correct rejections \code{\link{cr}}.
#'
#'   \item \code{prev} Value of current prevalence \code{\link{prev}}.
#'   \item \code{sens} Value of current sensitivity \code{\link{sens}}.
#'   \item \code{spec} Value of current specificity \code{\link{spec}}.
#'   \item \code{fart} Value of current false alarm rate \code{\link{fart}}.
#'
#'   \item \code{N} Current population size \code{\link{N}}.
#'
#'   \item \code{scen.src} Source information for current scenario.
#'   \item \code{scen.apa} Source information in APA format.
#'
#' }
#'
#' Note that names of variables (columns)
#' correspond to \code{\link{init_txt}} (to initialize \code{\link{txt}})
#' and \code{\link{init_num}} (to initialize \code{\link{num}}).
#'
#'
#' @source See columns \code{scen.src} and \code{scen.apa}
#' for a scenario's source information.

"df.scenarios"

## -----------------------------------------------
## eof.
