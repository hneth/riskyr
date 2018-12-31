## data.R | riskyr
## 2018 12 16
## -----------------------------------------------
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
#' Scenarios:
#'
#' \code{df_scenarios} currently contains the following scenarios
#' (n1 to n12 in English language, n13 to n25 in German language):
#'
#' In English language:
#'
#' \enumerate{
#'
#'   \item Bowel cancer screening
#'   \item Cab problem
#'   \item Hemoccult test
#'   \item Mammography screening
#'   \item Mammography (freq)
#'   \item Mammography (prob)
#'   \item Mushrooms
#'   \item Musical town
#'   \item PSA test (baseline)
#'   \item PSA test (patients)
#'   \item Psylicraptis screening
#'   \item Sepsis
#'
#' In German language:
#'
#'   \item Amniozentese (in German)
#'   \item HIV-Test 1
#'   \item HIV-Test 2
#'   \item HIV-Test 3
#'   \item HIV-Test 4
#'   \item Mammografie 1
#'   \item Mammografie 2
#'   \item Mammografie 3
#'   \item Mammografie 4
#'   \item Nackenfaltentest (NFT) 1
#'   \item Nackenfaltentest (NFT) 2
#'   \item Sigmoidoskopie 1
#'   \item Sigmoidoskopie 2
#'
#' }
#'
#' Variables describing a scenario:
#'
#' \enumerate{
#'
#'   \item \code{scen_lbl}: Text label for the current scenario.
#'   \item \code{scen_lng}: Language of the current scenario (en/de).
#'   \item \code{scen_txt}: Description text of the current scenario.
#'
#'   \item \code{popu_lbl}: Text label for the current population.
#'
#'   \item \code{cond_lbl}: Text label for the current condition.
#'   \item \code{cond_true_lbl}:  Text label for \code{\link{cond_true}} cases.
#'   \item \code{cond_false_lbl}: Text label for \code{\link{cond_false}} cases.
#'
#'   \item \code{dec_lbl}: Text label for the current decision.
#'   \item \code{dec_pos_lbl}: Text label for \code{\link{dec_pos}} cases.
#'   \item \code{dec_neg_lbl}: Text label for \code{\link{dec_neg}} cases.
#'
#'   \item \code{hi_lbl}: Text label for cases of hits \code{\link{hi}}.
#'   \item \code{mi_lbl}: Text label for cases of misses \code{\link{mi}}.
#'   \item \code{fa_lbl}: Text label for cases of false alarms \code{\link{fa}}.
#'   \item \code{cr_lbl}: Text label for cases of correct rejections \code{\link{cr}}.
#'
#'   \item \code{prev}: Value of the current prevalence \code{\link{prev}}.
#'   \item \code{sens}: Value of the current sensitivity \code{\link{sens}}.
#'   \item \code{spec}: Value of the current specificity \code{\link{spec}}.
#'   \item \code{fart}: Value of the current false alarm rate \code{\link{fart}}.
#'
#'   \item \code{N}: Current population size \code{\link{N}}.
#'
#'   \item \code{scen_src}: Source information for the current scenario.
#'   \item \code{scen_apa}: Source information in APA format.
#'
#' }
#'
#' Note that names of variables (columns)
#' correspond to a subset of \code{\link{init_txt}} (to initialize \code{\link{txt}})
#' and \code{\link{init_num}} (to initialize \code{\link{num}}).
#'
#' See the columns \code{scen_src} and \code{scen_apa}
#' for a scenario's source information.
#'
#' @seealso
#' \code{\link{riskyr}} initializes a \code{riskyr} scenario;
#' \code{\link{txt}} contains basic text information;
#' \code{\link{init_txt}} initializes text information;
#' \code{\link{num}} contains basic numeric parameters;
#' \code{\link{init_num}} initializes basic numeric parameters;
#' \code{\link{pal}} contains current color information;
#' \code{\link{init_pal}} initializes color information.

"df_scenarios"

## -----------------------------------------------
## eof.
