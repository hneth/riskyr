## riskyr_class.R | riskyr
## 2018 12 20
## Define riskyr class and corresponding methods
## -----------------------------------------------

## - Re-define df_scenarios as a list of riskyr objects.

## (A) Create riskyr objects: ---------------

## Get some exemplary scenarios with "riskyr" class attribute -----

# scenario1 <- df_scenarios[1, ]  # get scenario 3 of df_scenarios
# class(scenario1) <- "riskyr"

# scenario2 <- df_scenarios[2, ]  # get scenario 2 of df_scenarios
# class(scenario2) <- "riskyr"

## (1) Function to create diagnostic riskyr scenarios: ------

## riskyr Documentation: ------

#' Create a riskyr scenario.
#'
#' \code{riskyr} creates a scenario of class "riskyr",
#' which can be visualized by the \code{plot} method \code{\link{plot.riskyr}}
#' and summarized by the \code{summary} method \code{\link{summary.riskyr}}.
#'
#' Beyond basic scenario information (i.e., text elements describing a scenario)
#' only the population size \code{\link{N}} and the essential probabilities
#' \code{\link{prev}}, \code{\link{sens}}, \code{\link{spec}}, and \code{\link{fart}}
#' are used and returned.
#'
#' Note:
#'
#' \itemize{
#'
#'   \item Basic text information and some numeric parameters
#'   (see \code{\link{num}} and \code{\link{init_num}})
#'   are integral parts of a \code{riskyr} scenario.
#'
#'   \item By contrast, basic \emph{color} information
#'   (see \code{\link{pal}} and \code{\link{init_pal}})
#'   is not an integral part, but independently defined.
#'
#'   \item The names of \emph{probabilities}
#'   (see \code{\link{prob}}) are currently
#'   not an integral part of \code{txt} and \code{riskyr} scenarios
#'   (but defined in \code{prob_lbl_def} and \code{label_prob}).
#' }
#'
#' @format An object of class "riskyr" with textual and numeric information
#' describing a risk-related scenario.
#'
#' @return An object of class "riskyr" describing a risk-related scenario.
#'
#' Scenario-specific titles and text labels (see \code{\link{txt}}:
#'
#' @param scen_lbl The current scenario title (sometimes in Title Caps).
#'
#' @param popu_lbl A brief description of the current population or sample.
#' @param N_lbl A label for the current population \code{\link{popu}} or sample.
#'
#' @param cond_lbl A label for the \emph{condition} or feature (e.g., some disease) currently considered.
#' @param cond.true_lbl A label for the \emph{presence} of the current condition
#' or \code{\link{cond.true}} cases (the condition's true state of TRUE).
#' @param cond.false_lbl A label for the \emph{absence} of the current condition
#' or \code{\link{cond.false}} cases (the condition's true state of FALSE).
#'
#' @param dec_lbl A label for the \emph{decision} or judgment (e.g., some diagnostic test) currently made.
#' @param dec.pos_lbl A label for \emph{positive} decisions
#' or \code{\link{dec.pos}} cases (e.g., predicting the presence of the condition).
#' @param dec.neg_lbl A label for \emph{negative} decisions
#' or \code{\link{dec.neg}} cases (e.g., predicting the absence of the condition).
#'
#' @param acc_lbl A label for \emph{accuracy} (i.e., correspondence between condition and decision or judgment).
#' @param dec.cor_lbl A label for \emph{correct} (or accurate) decisions or judgments.
#' @param dec.err_lbl A label for \emph{incorrect} (or erroneous) decisions or judgments.
#'
#' @param sdt_lbl A label for the combination of \emph{condition} and \emph{decision} currently made.
#' @param hi_lbl A label for \emph{hits} or \emph{true positives} \code{\link{hi}}
#' (i.e., correct decisions of the presence of the condition, when the condition is actually present).
#' @param mi_lbl A label for \emph{misses} or \emph{false negatives} \code{\link{mi}}
#' (i.e., incorrect decisions of the absence of the condition when the condition is actually present).
#' @param fa_lbl A label for \emph{false alarms} or \emph{false positives} \code{\link{fa}}
#' (i.e., incorrect decisions of the presence of the condition when the condition is actually absent).
#' @param cr_lbl A label for \emph{correct rejections} or \emph{true negatives} \code{\link{cr}}
#' (i.e., a correct decision of the absence of the condition, when the condition is actually absent).
#'
#' Essential probabilities:
#'
#' @param prev The condition's prevalence \code{\link{prev}}
#' (i.e., the probability of condition being \code{TRUE}).
#'
#' @param sens The decision's sensitivity \code{\link{sens}}
#' (i.e., the conditional probability of a positive decision
#' provided that the condition is \code{TRUE}).
#' \code{sens} is optional when its complement \code{mirt} is provided.
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
#' Essential frequencies:
#'
#' @param N The number of individuals in the scenario's population.
#' A suitable value of \code{\link{N}} is computed, if not provided.
#'
#' @param hi The number of hits \code{\link{hi}} (or true positives).
#'
#' @param mi The number of misses \code{\link{mi}} (or false negatives).
#'
#' @param fa The number of false alarms \code{\link{fa}} (or false positives).
#'
#' @param cr The number of correct rejections \code{\link{cr}} (or true negatives).
#'
#' Details and source information:
#'
#' @param scen_lng Language of the current scenario (as character code).
#' Options: \code{"en"} for English, \code{"de"} for German.
#'
#' @param scen_txt A longer text description of the current scenario
#' (which may extend over several lines).
#'
#' @param scen_src Source information for the current scenario.
#'
#' @param scen_apa Source information for the current scenario
#' according to the American Psychological Association (APA style).
#'
#' @examples
#' # Defining scenarios: -----
#' # (a) minimal information:
#' hustosis <- riskyr(scen_lbl = "Screening for hustosis",
#'                    N = 1000, prev = .04, sens = .80, spec = .95)
#'
#' # (2) detailed information:
#' scen_reoffend <- riskyr(scen_lbl = "Identify reoffenders",
#'                         cond_lbl = "being a reoffender",
#'                         popu_lbl = "Prisoners",
#'                         cond.true_lbl = "has reoffended",
#'                         cond.false_lbl = "has not reoffended",
#'                         dec_lbl = "test result",
#'                         dec.pos_lbl = "will reoffend",
#'                         dec.neg_lbl = "will not reoffend",
#'                         sdt_lbl = "combination",
#'                         hi_lbl = "reoffender found", mi_lbl = "reoffender missed",
#'                         fa_lbl = "false accusation", cr_lbl = "correct release",
#'                         prev = .45,  # prevalence of being a reoffender.
#'                         sens = .98,
#'                         spec = .46, fart = NA,  # (provide 1 of 2)
#'                         N = 753,
#'                         scen_src = "Example scenario")
#'
#' # Using scenarios: -----
#' summary(hustosis)
#' plot(hustosis)
#'
#' summary(scen_reoffend)
#' plot(scen_reoffend)
#'
#' # 2 ways of defining the same scenario: -----
#' s1 <- riskyr(prev = .5, sens = .5, spec = .5, N = 100)  # s1: define by 3 prob & N
#' s2 <- riskyr(hi = 25, mi = 25, fa = 25, cr = 25)        # s2: same scenario by 4 freq
#' all.equal(s1, s2)  # should be TRUE
#'
#' # Ways to work: -----
#' riskyr(prev = .5, sens = .5, spec = .5, hi = 25, mi = 25, fa = 25, cr = 25)  # works (consistent)
#' riskyr(prev = .5, sens = .5, spec = .5, hi = 25, mi = 25, fa = 25)           # works (ignores freq)
#'
#' ## Watch out for:
#' # riskyr(hi = 25, mi = 25, fa = 25, cr = 25, N = 101)  # warns, uses actual sum of freq
#' # riskyr(prev = .4, sens = .5, spec = .5, hi = 25, mi = 25, fa = 25, cr = 25)  # warns, uses freq
#'
#' @family riskyr scenario functions
#' @family functions initializing scenario information
#'
#' @seealso
#' \code{\link{init_num}} and \code{\link{num}} for basic numeric parameters;
#' \code{\link{init_txt}} and \code{\link{txt}} for current text settings;
#' \code{\link{init_pal}} and \code{\link{pal}} for current color settings.
#'
#' @export

## riskyr Definition: ------

riskyr <- function(#
  # (1) Scenario label:
  scen_lbl = txt$scen_lbl,  # OR: "" (to hide header/plot titles)
  # Population:
  popu_lbl = txt$popu_lbl,
  N_lbl    = txt$N_lbl,
  # (2) Three perspectives:
  # a. by condition:
  cond_lbl = txt$cond_lbl,
  cond.true_lbl = txt$cond.true_lbl, cond.false_lbl = txt$cond.false_lbl,
  # b. by decision:
  dec_lbl = txt$dec_lbl,
  dec.pos_lbl = txt$dec.pos_lbl, dec.neg_lbl = txt$dec.neg_lbl,
  # c. by accuracy:
  acc_lbl = txt$acc_lbl,
  dec.cor_lbl = txt$dec.cor_lbl, dec.err_lbl = txt$dec.err_lbl,
  # (3) 4 SDT cases:
  sdt_lbl = txt$sdt_lbl,
  hi_lbl = txt$hi_lbl, mi_lbl = txt$mi_lbl,
  fa_lbl = txt$fa_lbl, cr_lbl = txt$cr_lbl,
  # (4) Essential probabilities:
  prev = NA,
  sens = NA,
  spec = NA, fart = NA,
  # (5) Essential frequencies:
  N = NA,  # WAS: freq$N,
  hi = NA, mi = NA,
  fa = NA, cr = NA,
  # (6) Scenario details:
  scen_lng = txt$scen_lng,
  scen_txt = txt$scen_txt,
  scen_src = txt$scen_src,
  scen_apa = txt$scen_apa
) {

  ## (0): Initialize some stuff: ------
  freqs <- NA
  probs <- NA
  prob_quintet <- NA

  ## Case_1: Using 4 frequencies: -------

  if (!any(is.na(c(hi, mi, fa, cr)))) {  # all four frequencies are provided:

    ## (a) Checking consistency of N: -----

    if (is.na(N)) {  # check, whether N is NA.

      N <- sum(c(hi, mi, fa, cr))  # set N to sum of frequencies.

    } else {  # N is provided:

      ## check, whether N matches the sum of frequencies:
      N.sum <- sum(c(hi, mi, fa, cr))

      if (N != N.sum) {

        msg <- paste0("N = ", N, ", but the sum of frequencies = ", N.sum, ". Using the latter...")
        warning(msg)

        N <- N.sum  # use sum of frequencies as N

      }
    }

    ## (b) Calculate the probabilities: -----

    probs_calc <- comp_prob_freq(hi, mi, fa, cr)
    probs      <- c(probs_calc$prev, probs_calc$sens, probs_calc$mirt, probs_calc$spec, probs_calc$fart)
    need_probs <- FALSE  # set flag that probs are no longer needed

    ## (c) Calculate ALL frequencies from 4 essential frequencies:
    freqs <- comp_freq_freq(hi, mi, fa, cr)

  } else {  # if not all 4 essential frequencies are provided:

    probs <- NA         # set probs to NA (and use inputs below).
    need_probs <- TRUE  # set flag that probs are still needed

  } # if (!any(is.na(c(hi, mi, fa, cr))))...


  ## Case_2: Using 3 essential probabilities: ------

  if (is_valid_prob_set(prev = prev,
                        sens = sens, mirt = NA,
                        spec = spec, fart = fart,
                        tol = .01)) {  # a valid set of probabilities is provided:

    ## (a) Compute the complete quintet of probabilities:
    prob_quintet <- comp_complete_prob_set(prev, sens, mirt = NA, spec, fart)
    # sens <- prob_quintet[2] # gets sens (if not provided)
    # mirt <- prob_quintet[3] # gets mirt (if not provided)
    # spec <- prob_quintet[4] # gets spec (if not provided)
    # fart <- prob_quintet[5] # gets fart (if not provided)

    ## (b) If frequencies have been provided, test whether the probabilities match:
    if (!any(is.na(probs))) { # if probs has been calculated:

      if (!is.null(prob_quintet)) {  # check, whether prob_quintet has been calculated.

        ## Do they equal the provided probabilities?
        if (any(abs(probs - prob_quintet) > .01)) {

          warning("Probabilities provided differ from probabilities calculated from frequencies.\nUsing probabilities from frequencies...")

        }
      }  # end check prob_quintet.

    } else {  # if no frequencies have been provided (probs is NA): ------

      ## (c) Set probs to the computed prob quintet:
      probs <- prob_quintet

      ## (d) if no N has been provided:
      if (is.na(N)) {

        N <- comp_min_N(prev = probs[1], sens = probs[2], spec = probs[4],
                        min.freq = 1)  # calculate a suitable N.
      }

      ## (e) Calculate the frequencies from probabilities:
      freqs <- comp_freq_prob(prev = probs[1], sens = probs[2], mirt = probs[3],
                              spec = probs[4], probs[5], N = N)

    }
  } # if (is_valid_prob_set(...

  else { # no valid set of probabilities was provided:

    if (need_probs) {  # not all 4 essential frequencies were provided above:

      ## Case_3: Not all frequencies OR a valid set of probabilities were provided:
      warning("Neither a full set of frequencies nor a valid set of probabilities was provided.")

    }
  }

  ## Case_4: Something is missing: ------

  # if (is.na(freqs)) {
  #   warning("Frequencies were not provided or could not be computed.")
  # }
  #
  # if (is.na(probs)) {
  #   warning("Probabilities were not provided or could not be computed.")
  # }

  ## prob_quintet <- probs  # both should be the same by now (not needed?).

  ## Define object (scenario) as a list: ------

  object <- list(#
    # (1) Scenario text:
    # Title label:
    scen_lbl = scen_lbl,
    # population:
    popu_lbl = popu_lbl,
    N_lbl = N_lbl,
    # a. by condition:
    cond_lbl = cond_lbl,
    cond.true_lbl = cond.true_lbl, cond.false_lbl = cond.false_lbl,
    # b. by decision:
    dec_lbl = dec_lbl,
    dec.pos_lbl = dec.pos_lbl, dec.neg_lbl = dec.neg_lbl,
    # c. by accuracy:
    acc_lbl = acc_lbl,
    dec.cor_lbl = dec.cor_lbl, dec.err_lbl = dec.err_lbl,
    # 4 SDT cases:
    sdt_lbl = sdt_lbl,
    hi_lbl = hi_lbl, mi_lbl = mi_lbl, fa_lbl = fa_lbl, cr_lbl = cr_lbl,
    # (2) Numeric info:
    # Probabilities:
    prev = probs[1],
    sens = probs[2],
    spec = probs[4], fart = probs[5],
    # Frequencies:
    N = N,
    hi = freqs$hi, mi = freqs$mi,
    fa = freqs$fa, cr = freqs$cr,
    # (+) Scenario details:
    scen_lng = scen_lng, scen_txt = scen_txt,
    scen_src = scen_src, scen_apa = scen_apa
  )

  ## Add class riskyr:
  class(object) <- "riskyr"

  return(object)

} # riskyr end.


## Check: ----------
# test.obj <- riskyr()  # initialize with default parameters
# names(test.obj)

## 2 ways to define the same scenario:
# s1 <- riskyr(prev = .5, sens = .5, spec = .5, N = 100)  # define s1
# s2 <- riskyr(hi = 25, mi = 25, fa = 25, cr = 25)        # s2: same in terms of freq
# all.equal(s1, s2)  # should be TRUE

## Ways to work:
# riskyr(prev = .5, sens = .5, spec = .5, hi = 25, mi = 25, fa = 25, cr = 25)  # works (consistent)
# riskyr(prev = .5, sens = .5, spec = .5, hi = 25, mi = 25, fa = 25)           # works (ignores freq)

## Watch out for:
# riskyr(hi = 25, mi = 25, fa = 25, cr = 25, N = 101)  # warn: use sum of freq
# riskyr(prev = .4, sens = .5, spec = .5, hi = 25, mi = 25, fa = 25, cr = 25)  # warn: use freq

## Check incomplete or inconsistent entries:
## riskyr(prev = NA, hi = NA)

## Compare with df_scenarios:
# names(df_scenarios)
# all.equal(names(test.obj), names(df_scenarios))

# # cat(
# #   paste0(
# #     paste0(names(scenarios$scen1), " = ", names(scenarios$scen1)),
# #     collapse = ", "))


## 2. scenarios: Define scenarios as a list of riskyr objects -----------

## Note: Convert the data frame df_scenarios into a list "scenarios"
##       of riskyr objects:

scenarios <- NULL # initialize

## Note helper stuff:
# cat(paste0("#'   \\item ", df_scenarios$scen_lbl[-1], "\n#'\n"))

## scenarios: Documentation ------

#' A collection of riskyr scenarios from various sources.
#'
#' \code{scenarios} is a list of scenarios of class "riskyr"
#' collected from the scientific literature and other sources
#' and to be used by visualization and summary functions.
#'
#' \code{scenarios} currently contains the following scenarios
#' (n1 to n12 in English language, n13 to n25 in German language):
#'
#' \enumerate{
#'
#' In English language:
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
#'    \item Amniozentese
#'    \item HIV-Test 1
#'    \item HIV-Test 2
#'    \item HIV-Test 3
#'    \item HIV-Test 4
#'    \item Mammografie 1
#'    \item Mammografie 2
#'    \item Mammografie 3
#'    \item Mammografie 4
#'    \item Nackenfaltentest (NFT) 1
#'    \item Nackenfaltentest (NFT) 2
#'    \item Sigmoidoskopie 1
#'    \item Sigmoidoskopie 2
#'
#' }
#'
#' Variables describing a scenario:
#'
#' \enumerate{
#'
#'   \item \code{scen_lbl} Text label for current scenario.
#'   \item \code{scen_lng} Language of current scenario (en/de).
#'   \item \code{scen_txt} Description text of current scenario.
#'
#'   \item \code{popu_lbl} Text label for current population.
#'
#'   \item \code{cond_lbl} Text label for current condition.
#'   \item \code{cond.true_lbl} Text label for \code{\link{cond.true}} cases.
#'   \item \code{cond.false_lbl} Text label for \code{\link{cond.false}} cases.
#'
#'   \item \code{dec_lbl} Text label for current decision.
#'   \item \code{dec.pos_lbl} Text label for \code{\link{dec.pos}} cases.
#'   \item \code{dec.neg_lbl} Text label for \code{\link{dec.neg}} cases.
#'
#'   \item \code{hi_lbl} Text label for cases of hits \code{\link{hi}}.
#'   \item \code{mi_lbl} Text label for cases of misses \code{\link{mi}}.
#'   \item \code{fa_lbl} Text label for cases of false alarms \code{\link{fa}}.
#'   \item \code{cr_lbl} Text label for cases of correct rejections \code{\link{cr}}.
#'
#'   \item \code{prev} Value of current prevalence \code{\link{prev}}.
#'   \item \code{sens} Value of current sensitivity \code{\link{sens}}.
#'   \item \code{spec} Value of current specificity \code{\link{spec}}.
#'   \item \code{fart} Value of current false alarm rate \code{\link{fart}}.
#'
#'   \item \code{N} Current population size \code{\link{N}}.
#'
#'   \item \code{scen_src} Source information for current scenario.
#'   \item \code{scen_apa} Source information in APA format.
#'
#' }
#'
#' Note that names of variables (columns)
#' correspond to \code{\link{init_txt}} (to initialize \code{\link{txt}})
#' and \code{\link{init_num}} (to initialize \code{\link{num}}).
#'
#' See columns \code{scen_src} and \code{scen_apa}
#' for a scenario's source information.
#'
#' The information of \code{scenarios} is also contained in an
#' R data frame \code{\link{df_scenarios}} (and generated from
#' the corresponding \code{.rda} file in \code{/data/}).
#'
#' @format A list with currently 25 objects of class "riskyr" (i.e., scenarios)
#' which are each described by 21 variables:
#'
#' @export

## scenarios: Definition ------

scenarios <- vector("list", nrow(df_scenarios))  # initialize scenarios as a list (from df_scenarios)
names(scenarios) <- paste0("n", 1:nrow(df_scenarios))

for (i in 1:nrow(df_scenarios)) {  # for each scenario i in df_scenarios:

  ## (1) define scenario s:
  s <- df_scenarios[i, ]

  ## (2) pass scenario s to riskyr function:
  cur_scen <- riskyr(#
    # Scenario label:
    scen_lbl = s$scen_lbl,
    # Population:
    popu_lbl = s$popu_lbl,
    N_lbl    = txt$N_lbl,  # use txt default (as currently not set in data)
    # a. by condition:
    cond_lbl = s$cond_lbl,
    cond.true_lbl = s$cond.true_lbl, cond.false_lbl = s$cond.false_lbl,
    # b. by decision:
    dec_lbl = s$dec_lbl,
    dec.pos_lbl = s$dec.pos_lbl, dec.neg_lbl = s$dec.neg_lbl,
    # c. by accuracy:
    acc_lbl = txt$acc_lbl,  # use txt default (as currently not set in data)
    dec.cor_lbl = txt$dec.cor_lbl, dec.err_lbl = txt$dec.err_lbl,
    # 4 SDT cases:
    sdt_lbl = txt$sdt_lbl,  # use txt default (as currently not set in data)
    hi_lbl = s$hi_lbl, mi_lbl = s$mi_lbl, fa_lbl = s$fa_lbl, cr_lbl = s$cr_lbl,
    # Probabilities:
    prev = s$prev,
    sens = s$sens,
    spec = s$spec, fart = s$fart,
    # Frequencies:
    N = s$N,
    # Scenario details:
    scen_lng = s$scen_lng, scen_txt = s$scen_txt,
    scen_src = s$scen_src, scen_apa = s$scen_apa
  )

  # (3) Add cur_scen (riskyr object) as i-th element of scenarios
  scenarios[[i]] <- cur_scen

} # end for ...


## Check: --------
# length(scenarios)
# scenarios$n25  # => shows elements of a scenario

## (B) Handle riskyr objects: ------------------

## 1. plot.riskyr function: -------

## Testing dots:
# test_fun <- function(...) {
#   plot_icons(...)
# }
#
# test_fun(N = 100, blubb = 5, prev = .7)
#
## ok.

## plot.riskyr Documentation: ------

#' Plot information of a riskyr object.
#'
#' \code{plot.riskyr} is a method that allows to generate
#' different plot types from a \code{"riskyr"} object.
#'
#' \code{plot.riskyr} also uses the text settings
#' specified in the "riskyr" object.
#'
#' @param x An object of class "riskyr", usually a result of a call to \code{\link{riskyr}}.
#' Pre-defined \code{\link{scenarios}} are also of type "riskyr".
#'
#' @param type The type of plot to be generated.
#'
#' The following plot types are currently available:
#'
#' \enumerate{
#'
#'   \item \code{type = "prism"} or \code{type = "net"} or \code{type = "tree"}:
#'   Risk information is plotted in a network diagram of frequencies and probabilities (default).
#'   See \code{\link{plot_prism}} for further options.
#'
#'   \item \code{type = "tab"} or \code{type = "ftab"}:
#'   Risk information is plotted as a 2-by-2 frequency or contingency table.
#'   See \code{\link{plot_tab}} for further options.
#'
#'   \item \code{type = "area"} or \code{type = "mosaic"}:
#'   Risk information is plotted as a mosaic plot (scaled area).
#'   See \code{\link{plot_area}} for further options.
#'
#'   \item \code{type = "bar"} or \code{type = "fbar"}:
#'   Risk information is plotted as a bar chart.
#'   See \code{\link{plot_bar}} for further options.
#'
#'   \item \code{type = "icons"} or \code{type = "iconarray"}:
#'   The underlying population is plotted as an array of icons.
#'   See \code{\link{plot_icons}} for further options.
#'
#'   \item \code{type = "curve"} or \code{type = "curves"}:
#'   Draws curves of selected values (including \code{\link{PPV}}, \code{\link{NPV}})
#'   See \code{\link{plot_curve}} for further options.
#'
#'   \item \code{type = "plane"} or \code{type = "planes"}:
#'   Draws a 3D-plane of selected values (e.g., predictive values \code{\link{PPV}} or \code{\link{NPV}})
#'   See \code{\link{plot_plane}} for further options.
#'
#' }
#'
#' @param ... Additional parameters to be passed to the underlying plotting functions.
#'
#' @examples
#' # Select a scenario (from list of scenarios):
#' s1 <- scenarios$n1  # select scenario 1 from scenarios
#' plot(s1)  # default plot (type = "prism")
#'
#' # Plot types currently available:
#' plot(s1, type = "prism")                # prism/network diagram (default)
#' plot(s1, type = "tree", by = "cd")      # tree diagram (only 1 perspective)
#' plot(s1, type = "area")                 # area/mosaic plot
#' plot(s1, type = "tab")                  # 2x2 frequency/contingency table
#' plot(s1, type = "bar", dir = 2)         # bar plot
#' plot(s1, type = "icons")                # icon array
#' plot(s1, type = "curve", what = "all")  # curves as fn. of prev
#' plot(s1, type = "plane", what = "NPV")  # plane as function of sens & spec
#' plot(s1, type = "default")              # unknown type: use default plot
#'
#' @family visualization functions
#' @family riskyr scenario functions
#'
#' @export

## plot.riskyr Definition: ------

plot.riskyr <- function(x = NULL,        # require riskyr scenario
                        type = "prism",  # default type
                        # by = "cddc",   # default perspective
                        ...              # other type and display parameters in plot_xxx functions
) {

  ## Note: Most other functions (except for plot_icons) currently lack the ellipsis.
  ## Therefore, these functions will throw an exception when unnecessary parameters are passed.

  ## (1) Increase robustness: ----------

  type <- tolower(type)  # ensure lowercase

  # Test type argument:
  if (!type %in% c(#
    # plot_prism:
    "prism", "fprism", "tree", "ftree", "net", "fnet", "network",
    # plot_area:
    "area", "farea", "mosaic",
    # plot_tab:
    "tab", "table", "ftab", "ctab",
    # plot_icons:
    "icon", "icons", "iconarray",
    # plot_bar:
    "bar", "bars", "barplot", "fbar",
    # plot_curve:
    "curve", "curves",
    # plot_plane:
    "plane", "planes", "cube")) {

    message("Unknown plot type (in plot.riskyr): Using type = 'prism'.")
    type <- "prism"

  }

  # # If type == "tree" and current by contains more than 1 perspective:
  # if ( (substr(type, 1, 4) == "tree") || (substr(type, 1, 5) == "ftree") ) {
  #   if ( !is.null(by) ) {
  #     if ( is.character(by) && (nchar(by) > 2) ) {
  #
  #       # print(paste0("1. by = ", by))  # debugging
  #       by <- substr(by, 1, 2)           # use only 1st perspective of by
  #       # print(paste0("2. by = ", by))  # debugging
  #
  #     }
  #   }
  # }

  ## (2) Use text info of scenario x for current txt information: ----------

  x_txt <- init_txt(scen_lbl = x$scen_lbl,

                    popu_lbl = x$popu_lbl,
                    N_lbl = x$N_lbl,

                    cond_lbl = x$cond_lbl,
                    cond.true_lbl = x$cond.true_lbl,
                    cond.false_lbl = x$cond.false_lbl,

                    dec_lbl  = x$dec_lbl,
                    dec.pos_lbl = x$dec.pos_lbl,
                    dec.neg_lbl = x$dec.neg_lbl,

                    acc_lbl = x$acc_lbl,
                    dec.cor_lbl = x$dec.cor_lbl,
                    dec.err_lbl = x$dec.err_lbl,

                    sdt_lbl = x$sdt_lbl,
                    hi_lbl = x$hi_lbl,
                    mi_lbl = x$mi_lbl,
                    fa_lbl = x$fa_lbl,
                    cr_lbl = x$cr_lbl,

                    scen_txt = x$scen_txt,
                    scen_src = x$scen_src,
                    scen_apa = x$scen_apa,
                    scen_lng = x$scen_lng
                    )

  ## (3) Call plotting functions: ----------

  ## 1. Table / contingency/confusion/frequency table / tab plot:
  if ((substr(type, 1, 3) == "tab") || (type == "ftab") || (type == "ctab")) {

    plot_tab(prev = x$prev,
             sens = x$sens, mirt = NA,
             spec = x$spec, fart = NA,
             N = x$N,
             # Options:
             lbl_txt = x_txt,
             title_lbl = x$scen_lbl,
             ...
    )

  } # if (type == "tab")

  ## 2. Area / mosaic plot:
  if ((substr(type, 1, 4) == "area") || (type == "farea") ||
      (substr(type, 1, 6) == "mosaic")) {  # "mosaic"

    plot_area(prev = x$prev,
              sens = x$sens, mirt = NA,
              spec = x$spec, fart = NA,
              N = x$N,
              # Options:
              lbl_txt = x_txt,
              title_lbl = x$scen_lbl,
              ...
    )

  } # if (type == "area")

  ## 3. Icon array:
  if (substr(type, 1, 4) == "icon") {

    plot_icons(prev = x$prev,             # probabilities
               sens = x$sens, mirt = NA,
               spec = x$spec, fart = NA,  # was: num$fart,
               N = x$N,    # ONLY freq used (so far)
               # Options:
               title_lbl = x$scen_lbl,
               type_lbls = x[c("hi_lbl", "mi_lbl", "fa_lbl", "cr_lbl")],
               ...
    )

  } #  if (type == "icon")

  ## 4. Prism plot:
  if ((substr(type, 1, 5) == "prism") || (substr(type, 1, 6) == "fprism") ||
      (substr(type, 1, 3) == "net")   || (substr(type, 1, 4) == "fnet")   ||
      (substr(type, 1, 4) == "tree")  || (substr(type, 1, 5) == "ftree")) {

    plot_prism(prev = x$prev,
               sens = x$sens, mirt = NA,
               spec = x$spec, fart = NA,
               N = x$N,
               # Options:
               lbl_txt = x_txt,
               title_lbl = x$scen_lbl,
               ...
    )

  } # if (type == "prism")

  ## 5. Bar plot / frequency bars:
  if ((substr(type, 1, 3) == "bar") || (substr(type, 1, 4) == "fbar")) {

    plot_bar(prev = x$prev,
             sens = x$sens, mirt = NA,
             spec = x$spec, fart = NA,
             N = x$N,
             # Options:
             lbl_txt = x_txt,
             title_lbl = x$scen_lbl,
             ...
    )

  } # if (type == "bar")

  ## 6. Curve of probabilities:
  if (substr(type, 1, 5) == "curve") {

    plot_curve(prev = x$prev,             # probabilities (3 essential, 2 optional)
               sens = x$sens, mirt = NA,
               spec = x$spec, fart = NA,
               # Options:
               title_lbl = x$scen_lbl,
               ...
    )
  } # if (type == "curve")

  ## 7. Plane/cube of probabilities:
  if ((substr(type, 1, 5) == "plane") || (substr(type, 1, 4) == "cube")) {

    plot_plane(prev = x$prev,             # probabilities (3 essential, 2 optional)
               sens = x$sens, mirt = NA,
               spec = x$spec, fart = NA,
               # Options:
               title_lbl = x$scen_lbl, # plot title label
               ...
    )
  } # if (type == "plane")

} # plot.riskyr end.


## Check: ------
## (A) with example scenarios (defined above):
# plot(scenario2, type = "icons")
# plot(scenario3, type = "tree")

## (B) with scenarios from scenarios (defined BELOW):
#
# s25 <- scenarios$n25  # select Scenario 25 from scenarios
#
# plot(s25)  # => default plot (prism/net)
# plot(s25, type = "ofnet")  # => old network diagram (old default)
# plot(s25, type = "tree", area = "vr") # => tree diagram (with vertical rectangles)
# plot(s25, type = "icons")
# plot(s25, type = "icons", type = "mosaic")  # passing on additional parameters.
# plot(s25, type = "curve", what = "all")
# plot(s25, type = "plane", what = "npv")
# plot(s25, type = "wetwork")
#
# New plots:
# plot(s25, type = "prism", by = "cddc", f_lbl = "num")
# plot(s25, type = "tree", by = "ac", f_lbl = "num")
# plot(s25, type = "area", by = "cddc")
# plot(s25, type = "tab", by = "cddc", f_lwd = 2)
# plot(s25, type = "bar", dir = 2)
#
# # Plot types currently available:
# plot(s25, type = "prism")                # prism/network diagram (default)
# plot(s25, type = "tree", by = "cd")      # tree diagram (only 1 perspective)
# plot(s25, type = "area")                 # area/mosaic plot
# plot(s25, type = "tab")                  # 2x2 frequency/contingency table
# plot(s25, type = "bar", dir = 2)         # bar plot
# plot(s25, type = "icons")                # icon array
# plot(s25, type = "curve", what = "all")  # curves as fn. of prev
# plot(s25, type = "plane", what = "NPV")  # plane as function of sens & spec
#
# # Older plot types (obsolete and RETIRED):
# plot(s25, type = "onet")     # plot_fnet (replaced by plot_prism)
# plot(s25, type = "otree")    # plot_tree (replaced by plot_prism)
# plot(s25, type = "omosaic")  # plot_mosaic (replaced by plot_area)



## Legacy code snippets: ------

## From documentation of older plot.riskyr:
# Older plot types (replaced by the above, retired since version 0.1.0.950):
#
# \item \code{type = "onet"} or \code{type = "ofnet"}:
# Risk information is plotted in a network diagram of frequencies and probabilities (default).
# Replaced by \code{\link{plot_prism}}, but see \code{\link{plot_fnet}} for further options.
#
# \item \code{type = "otree"} or \code{type = "oftree"}:
# Risk information is plotted in a frequency tree.
# Replaced by \code{\link{plot_prism}}, but see \code{\link{plot_tree}} for further options.
#
# \item \code{type = "omosaic"} or \code{type = "omosaicplot"}:
# Risk information is plotted as a mosaicplot.
# Replaced by \code{\link{plot_area}}, but see \code{\link{plot_mosaic}} for further options.


## 2. summary.riskyr function: ------------------

## (a) Create a summary object:

## summary.riskyr Documentation: ------

#' Summarizing a riskyr scenario.
#'
#' \code{summary.riskyr} provides a \code{summary} method for objects of class "riskyr".
#'
#' @format An object of class \code{summary.riskyr} with up to 9 entries.
#'
#' @return A summary list \code{obj.sum}
#' with up to 9 entries, dependent on which information is requested by \code{summarize}.
#'
#' Scenario name, relevant condition \code{}, and \code{N}
#' are summarized by default.
#'
#' @param object  An object of class "riskyr", usually a result of a call to \code{\link{riskyr}}.
#' Inbuilt \code{scenarios} are also of type "riskyr".
#'
#' @param summarize What is summarized as a vector consisting of \code{c("freq", "prob", "accu")}
#' for frequencies, probabilities, and accuracy respectively.
#' The default "all" is an alias to all three.
#'
#' @param ... Additional parameters (to be passed to summary functions).
#'
#' @examples
#' summary(scenarios$n4)
#'
#' @family summary functions
#' @family riskyr scenario functions
#'
#' @export

## summary.riskyr Definition: ------

summary.riskyr <- function(object = NULL, summarize = "all", ...) {

  obj.sum <- list()  # initialize as list

  obj.sum$scen_lbl <- object$scen_lbl

  obj.sum$cond_lbl <- object$cond_lbl  # condition
  obj.sum$dec_lbl <- object$dec_lbl    # decision
  obj.sum$popu_lbl <- object$popu_lbl  # population
  obj.sum$N <- object$N                # N
  obj.sum$scen_src <- object$scen_src  # source (short)

  ## (0) If all should be summarized: ----------

  if (summarize == "all") summarize <- c("prob", "freq", "accu")


  ## (A) Probability information: ----------

  if (("prob" %in% summarize) || ("probs" %in% summarize) || ("probabilities" %in% summarize)) {

    # calculate all probabilities:
    probs <- comp_prob_prob(prev = object$prev, sens = object$sens, spec = object$spec)

    probs.ess <- unlist(probs[c("prev", "sens", "mirt", "spec", "fart")])  # essential probabilities.

    probs.ness <- unlist(probs[c("ppod", "PPV", "NPV", "FDR", "FOR", "acc")])  # non-essential probabilities.

    obj.sum$probs <- list(probs.ess = probs.ess, probs.ness = probs.ness)

  } # if "prob"


  ## (B) Frequency information: ----------

  if (("freq" %in% summarize) || ("freqs" %in% summarize) || ("frequencies" %in% summarize)) {

    # calculate frequencies:
    freqs <- comp_freq(prev = object$prev, sens = object$sens, spec = object$spec,
                       N = object$N)

    ## (a) Frequencies by condition:
    cond.freqs <- unlist(freqs[c("cond.true", "cond.false")])

    ## (b) Frequencies by decision:
    dec.freqs <- unlist(freqs[c("dec.pos", "dec.neg")])

    ## (c) Frequencies by accuracy (i.e., correspondence of decision to condition):
    acc.freqs <- unlist(freqs[c("dec.cor", "dec.err")])

    ## (d) SDT frequencies:
    sdt.freqs <- unlist(freqs[c("hi", "mi", "fa", "cr")])  # == "essential" frequencies.

    ## (+) Add to summary object:
    obj.sum$freqs <- list(cond.freqs = cond.freqs,
                          dec.freqs = dec.freqs,
                          acc.freqs = acc.freqs,
                          sdt.freqs = sdt.freqs)

  } # if "freq"


  ## (C) Accuracy information: ----------

  if (("acc" %in% summarize) || ("accu" %in% summarize) || ("accuracy" %in% summarize)) {

    ## Overall accuracy acc:
    obj.sum$acc <- comp_acc(prev = object$prev, sens = object$sens, spec = object$spec)

    ## ToDo: ALL accuracy metrics:
    # accu <- comp_accu_prob(prev = obj$prev, sens = obj$sens, spec = obj$spec)

  } # if "acc"



  ## Add class to summary object: ----------

  class(obj.sum) <- c("summary.riskyr")

  return(obj.sum)

}


## 4. print.summary.riskyr function: ------------------

## Create print function corresponding to summary object:

## print.summary.riskyr Documentation: ------

#' Printing summarized risk information.
#'
#' \code{print.summary.riskyr} provides a \code{print} method for objects of class "summary.riskyr".
#'
#' @format Printed output of a "summary.riskyr" object.
#'
#' @param x An object of class "summary.riskyr", usually a result of a call to \code{summary.riskyr}.
#'
#' @param ... Additional parameters (to be passed to generic print function).
#'
#' @examples
#' summary(scenarios$n4)
#'
#' @family print functions
#'
#' @export

## print.summary.riskyr Definition: ------

print.summary.riskyr <- function(x = NULL, ...) {

  ## 1. Always print header: ----------

  cat("Scenario: ",   x$scen_lbl, "\n\n")  # always show scenario name.

  cat("Condition: ",  x$cond_lbl, "\n")  # always show current condition.
  cat("Decision: ",   x$dec_lbl,  "\n")  # always show current decision.
  cat("Population: ", x$popu_lbl, "\n")  # always show current condition.
  cat("N = ", x$N, "\n")                 # always show population size N.
  cat("Source: ", x$scen_src, "\n")      # always show (short) source info

  ## 2. Print only on demand: ----------

  n <- names(x)  # save names.

  ## (A) Probabilities: ----------

  if ("probs" %in% n) {

    cat("\nProbabilities:\n\n")

    cat(" Essential probabilities:\n")
    # names(x$probs$probs.ess) <- c("Prevalence", "Sensitivity", "Miss rate", "Specificity", "False alarm rate")  # explicit
    names(x$probs$probs.ess) <- c("prev", "sens", "mirt", "spec", "fart")  # shorter
    print(x$probs$probs.ess)

    cat("\n Other probabilities:\n")
    print(round(x$probs$probs.ness, 3))  # no naming for non-essential probs.

  }

  ## (B) Frequencies: ----------

  if ("freqs" %in% n) {

    cat("\nFrequencies:\n")

    cat("\n by conditions:\n")
    # names(x$freqs$cond.freqs) <- c("True", "False")  # explicit
    names(x$freqs$cond.freqs) <- c("cond.true", "cond.false")  # more explicit
    print(x$freqs$cond.freqs)

    cat("\n by decision:\n")
    names(x$freqs$dec.freqs) <- c("Positive", "Negative")  # explicit
    names(x$freqs$dec.freqs) <- c("dec.pos", "dec.neg")  # more explicit
    print(x$freqs$dec.freqs)

    cat("\n by correspondence (of decision to condition):\n")
    # names(x$freqs$acc.freqs) <- c("Correct cases", "Incorrect cases")  # explicit
    names(x$freqs$acc.freqs) <- c("dec.cor", "dec.err")  # implicit
    print(x$freqs$acc.freqs)

    cat("\n 4 essential (SDT) frequencies:\n")
    # names(x$freqs$sdt.freqs) <- c("Hits", "Misses", "False alarms", "Correct rejections")  # explicit
    names(x$freqs$sdt.freqs) <- c("hi", "mi", "fa", "cr")  # implicit
    print(x$freqs$sdt.freqs)

  }

  ## (C) Accuracy: ----------

  if (("acc" %in% n) || ("accu" %in% n) || ("accuracy" %in% n)) {

    cat("\nAccuracy:\n\n")

    cat(" acc:\n")
    cat(x$acc)  # overall accuracy acc only!

    ## ToDo: Include ALL other accuracy metrics (accu).

  }

}


## Check: ------
# scenario2 <- df_scenarios[2, ]  # get scenario 2 of df_scenarios
# summary(scenario2)  # => all summaries
# summary(scenario2, summarize = "freq")
# summary(scenario2, summarize = "prob")
# summary(scenario2, summarize = "accu")


## (C) Demo: Typical user interaction / session: ----------

## 1. Defining and viewing a scenario: -----------
## see Vignette of riskyr primer

## 2. Exploring pre-defined scenarios: -----------

## Example 1: Bowel cancer (FOB screening): ------
## Source: https://en.wikipedia.org/wiki/Positive_and_negative_predictive_values#Worked_example

## Select scenario 1:
# s1 <- scenarios$n1   # select by name:   $ndd

# summary(s1)
# summary(s1, summarize = "freq")

# plot(s1, type = "tree", area = "hr") # => tree diagram (with vertical rectangles)
# plot(s1, type = "curve", what = "all", uc = .05)
# plot(s1, type = "icons")
# plot(s1, type = "icons", arr_type = "mosaic")  # passing on additional parameters.
# plot(s1, type = "mosaic")
# plot(s1, type = "plane", what = "NPV")
# plot(s1, type = "wetwork")


## Example 2: Mammography screening (standard example) -----
## Source: Hoffrage et al. (2015), p. 3

## (a) Select scenario 6:
# s6 <- scenarios[[6]] # select by number: [[dd]]
# s6 <- scenarios$n6   # select by name:   $ndd

## (b) Summary:
# summary(s6)

## (c) Visualization:
# plot(s6)  # => default plot (fnet)
# plot(s6, type = "icons")
# plot(s6, type = "curve")


## Example 3: PSA screening ------
## Source: Arkes & Gaissmaier (2012), p. 550

## A. Low prevalence version (Scenario 9: prev = 6.3%):

# summary(scenarios$n9)

# plot(scenarios$n9, type = "tree", area = "sq")
# plot(scenarios$n9, type = "icons")
# plot(scenarios$n9, type = "curves", what = "all")
# plot(scenarios$n9, type = "planes", what = "PPV")

## B. Contrast with high prevalence version (Scenario 10: prev = 50%):

# summary(scenarios$n10)

# plot(scenarios$n10, type = "tree", area = "sq")
# plot(scenarios$n10, type = "icons")
# plot(scenarios$n10, type = "curves", what = "all")
# plot(scenarios$n10, type = "planes", what = "PPV")


## (3) Read riskyr scenario from a population (popu, given as data frame) ----------

## read_popu: Documentation: ------

#' Read a population (given as data frame) into a riskyr scenario.
#'
#' \code{read_popu} interprets a data frame \code{df}
#' (that contains individual observations of some population)
#' and returns a scenario of class \code{"riskyr"}.
#'
#' Note that \code{df} needs to be structured according to
#' the \code{\link{popu}} created by \code{\link{comp_popu}}.
#'
#' @return An object of class "riskyr" describing a risk-related scenario.
#'
#' @param df A data frame providing a population \code{\link{popu}}
#' of individuals, which are identified on at least
#' 2 binary variables and classified into 4 cases in a 3rd variable.
#' Default: \code{df = \link{popu}} (as data frame).
#'
#' @param ix_by_top Index of variable (column) providing the 1st (top) perspective (in df).
#' Default: \code{ix_by_top = 1} (1st column).
#' @param ix_by_bot Index of variable (column) providing the 2nd (bot) perspective (in df).
#' Default: \code{ix_by_bot = 2} (2nd column).
#' @param ix_sdt Index of variable (column) providing a classification into 4 cases (in df).
#' Default: \code{ix_by_bot = 3} (3rd column).
#'
#' @param hi_lbl Variable label of cases classified as hi (TP).
#' @param mi_lbl Variable label of cases classified as mi (FN).
#' @param fa_lbl Variable label of cases classified as fa (FP).
#' @param cr_lbl Variable label of cases classified as cr (TN).
#'
#' @param ... Additional parameters (to be passed to \code{\link{riskyr}} function).
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
#'                         cond_lbl = "Treatment", cond.true_lbl = "pill", cond.false_lbl = "placebo",
#'                         dec_lbl = "Health status", dec.pos_lbl = "healthy", dec.neg_lbl = "sick")
#' # popu_treat
#' scen_treat <- read_popu(popu_treat, scen_lbl = "Treatment", popu_lbl = "Population treated")
#' plot(scen_treat, type = "prism", area = "sq", f_lbl = "namnum", p_lbl = "num")
#' plot(scen_treat, type = "icon", lbl_txt = txt_org, col_pal = pal_org)
#'
#' # (C) Prevention scenario (e.g., vaccination): ------
#' popu_vacc <- comp_popu(hi = 960, mi = 40, fa = 880, cr = 120,
#'                        cond_lbl = "Vaccination", cond.true_lbl = "yes", cond.false_lbl = "no",
#'                        dec_lbl = "Disease", dec.pos_lbl = "no flu", dec.neg_lbl = "flu")
#' # popu_vacc
#' scen_vacc <- read_popu(popu_vacc, scen_lbl = "Prevention", popu_lbl = "Population vaccinated")
#' plot(scen_vacc, type = "prism", area = "sq", f_lbl = "namnum", col_pal = pal_bw, p_lbl = "num")
#'
#' @family riskyr scenario functions
#'
#' @seealso
#' the corresponding data frame \code{\link{popu}};
#' the corresponding generating function \code{\link{comp_popu}};
#' main \code{\link{riskyr}} function.
#'
#' @export

## read_popu: Definition ----------

read_popu <- function(df = popu,  # df (as population with 3+ columns, see comp_popu)
                      ix_by_top = 1, ix_by_bot = 2, ix_sdt = 3,  # indices of by_top, by_bot, and sdt cols in df
                      # text labels (from txt):
                      hi_lbl = txt$hi_lbl, mi_lbl = txt$mi_lbl, fa_lbl = txt$fa_lbl, cr_lbl = txt$cr_lbl,
                      ...) {

  sdt_cases <- df[ , ix_sdt]

  n_hi <- length(sdt_cases[sdt_cases == hi_lbl])
  n_mi <- length(sdt_cases[sdt_cases == mi_lbl])
  n_fa <- length(sdt_cases[sdt_cases == fa_lbl])
  n_cr <- length(sdt_cases[sdt_cases == cr_lbl])

  # Labels:
  cond_lbl <- names(df)[ix_by_top]
  cond.true_lbl  <- levels(df[ , ix_by_top])[1]
  cond.false_lbl <- levels(df[ , ix_by_top])[2]

  dec_lbl <- names(df)[ix_by_bot]
  dec.pos_lbl <- levels(df[ , ix_by_bot])[1]
  dec.neg_lbl <- levels(df[ , ix_by_bot])[2]

  sdt_lbl <- names(df)[ix_sdt]

  # Create riskyr scenario:
  scen <- riskyr(hi = n_hi, mi = n_mi, fa = n_fa, cr = n_cr,
                 cond_lbl = cond_lbl, cond.true_lbl = cond.true_lbl, cond.false_lbl = cond.false_lbl,
                 dec_lbl = dec_lbl, dec.pos_lbl = dec.pos_lbl, dec.neg_lbl = dec.neg_lbl,
                 sdt_lbl = sdt_lbl,
                 ...)

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
#                         cond_lbl = "Treatment", cond.true_lbl = "pill", cond.false_lbl = "placebo",
#                         dec_lbl = "Health status", dec.pos_lbl = "healthy", dec.neg_lbl = "sick")
# # popu_treat
# scen_treat <- read_popu(popu_treat, scen_lbl = "Treatment", popu_lbl = "Population treated")
# plot(scen_treat, type = "prism", area = "hr", f_lbl = "namnum", col_pal = "whitesmoke", f_lwd = 1)
# plot(scen_treat, type = "icon")
#
# # (C) Prevention scenario (e.g., vaccination): ------
# popu_vacc <- comp_popu(hi = 960, mi = 40, fa = 880, cr = 120,
#                        cond_lbl = "Vaccination", cond.true_lbl = "yes", cond.false_lbl = "no",
#                        dec_lbl = "Disease", dec.pos_lbl = "no flu", dec.neg_lbl = "flu")
# # popu_vacc
# scen_vacc <- read_popu(popu_vacc, scen_lbl = "Prevention", popu_lbl = "Population vaccinated")
# plot(scen_vacc, type = "prism", area = "sq", f_lbl = "namnum", col_pal = pal_bw, p_lbl = "num")

## (*) Done: ----------

## - Clean up code.  [2018 08 22].
## - Remove retired functions (otree, fnet/ofnet, omosaic). [2018 12 21]
## - Update scenario data (and order). [2018 12 20]

## (+) ToDo: ----------

## - allow riskyr() to take different kinds of inputs (prob, freq, ...),
##   until a full object is created.
## - allow incomplete riskyr scenarios (e.g., 1 branch of tree)

## eof. ------------------------------------------
