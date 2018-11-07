## riskyr_class.R | riskyr
## 2018 11 05
## Define riskyr class and corresponding methods
## -----------------------------------------------
## Note:
## - Re-define df.scenarios as a list of riskyr objects.

## (A) Create riskyr objects: ---------------

## Get some exemplary scenarios with "riskyr" class attribute -----

# scenario2 <- df.scenarios[2, ]  # get scenario 2 of df.scenarios
# class(scenario2) <- "riskyr"

# scenario3 <- df.scenarios[3, ]  # get scenario 3 of df.scenarios
# class(scenario3) <- "riskyr"


## (1) Function to create diagnostic riskyr scenarios: ------

## riskyr Documentation: ------

#' Create riskyr scenarios.
#'
#' The instantiation function \code{riskyr} is used to create
#' scenarios of class "riskyr",
#' which can then be visualized by the \code{plot} method \code{\link{plot.riskyr}}
#' and summarized by the \code{summary} method \code{\link{summary.riskyr}}.
#'
#' Beyond basic scenario information
#' only the population size \code{\link{N}} and the essential probabilities
#' \code{\link{prev}}, \code{\link{sens}}, \code{\link{spec}}, and \code{\link{fart}}
#' are used and returned.
#'
#' @format An object of class "riskyr"
#' with 21 entries on textual and numeric information on
#' a riskyr scenario.
#'
#' @return A list \code{object} of class "riskyr"
#' containing information on a risky scenario.
#'
#' Text elements (all elements of \code{\link{txt}}:
#'
#' @param scen.lbl The current scenario title (sometimes in Title Caps).
#' @param scen.txt A longer text description of the current scenario
#' (which may extend over several lines).
#'
#' @param scen.lng Language of the current scenario (as character code).
#' Options: \code{"en"} for English, \code{"de"} for  German.
#'
#' @param popu.lbl A brief description of the current target population \code{\link{popu}} or sample.
#'
#' @param cond.lbl A name for the \emph{condition} or feature (e.g., some disease) currently considered.
#' @param cond.true.lbl A label for the \emph{presence} of the current condition
#' or \code{\link{cond.true}} cases (the condition's true state of TRUE).
#' @param cond.false.lbl A label for the \emph{absence} of the current condition
#' or \code{\link{cond.false}} cases (the condition's true state of FALSE).
#'
#' @param dec.lbl A name for the \emph{decision} or judgment (e.g., some diagnostic test) currently made.
#' @param dec.pos.lbl A label for \emph{positive} decisions
#' or \code{\link{dec.pos}} cases (e.g., predicting the presence of the condition).
#' @param dec.neg.lbl A label for \emph{negative} decisions
#' or \code{\link{dec.neg}} cases (e.g., predicting the absence of the condition).
#'
#' @param hi.lbl A label for \emph{hits} or \emph{true positives} \code{\link{hi}}
#' (i.e., correct decisions of the presence of the condition, when the condition is actually present).
#' @param mi.lbl A label for \emph{misses} or \emph{false negatives} \code{\link{mi}}
#' (i.e., incorrect decisions of the absence of the condition when the condition is actually present).
#' @param fa.lbl A label for \emph{false alarms} or \emph{false positives} \code{\link{fa}}
#' (i.e., incorrect decisions of the presence of the condition when the condition is actually absent).
#' @param cr.lbl A label for \emph{correct rejections} or \emph{true negatives} \code{\link{cr}}
#' (i.e., a correct decision of the absence of the condition, when the condition is actually absent).
#'
#' Numeric elements:
#'
#' @param N The number of individuals in the scenario's population.
#' A suitable value of \code{\link{N}} is computed, if not provided.
#'
#' @param prev The condition's prevalence \code{\link{prev}}
#' (i.e., the probability of condition being \code{TRUE}).
#' @param sens The decision's sensitivity \code{\link{sens}}
#' (i.e., the conditional probability of a positive decision
#' provided that the condition is \code{TRUE}).
#' \code{sens} is optional when its complement \code{mirt} is provided.
#' @param spec The decision's specificity value \code{\link{spec}}
#' (i.e., the conditional probability
#' of a negative decision provided that the condition is \code{FALSE}).
#' \code{spec} is optional when its complement \code{fart} is provided.
#' @param fart The decision's false alarm rate \code{\link{fart}}
#' (i.e., the conditional probability
#' of a positive decision provided that the condition is \code{FALSE}).
#' \code{fart} is optional when its complement \code{spec} is provided.
#'
#' @param hi The number of hits \code{\link{hi}} (or true positives).
#' @param mi The number of misses \code{\link{mi}} (or false negatives).
#' @param fa The number of false alarms \code{\link{fa}} (or false positives).
#' @param cr The number of correct rejections \code{\link{cr}} (or true negatives).
#'
#' Source information:
#'
#' @param scen.src Source information for the current scenario.
#' @param scen.apa Source information for the current scenario
#' in the style of the American Psychological Association (APA style).
#'
#' @examples
#' # Defining a scenario:
#' scen.reoffend <- riskyr(scen.lbl = "Identify reoffenders",
#'                         cond.lbl = "being a reoffender",
#'                         popu.lbl = "Prisoners",
#'                         cond.true.lbl = "has reoffended",
#'                         cond.false.lbl = "has not reoffended",
#'                         dec.lbl = "test result",
#'                         dec.pos.lbl = "will reoffend",
#'                         dec.neg.lbl = "will not reoffend",
#'                         hi.lbl = "reoffender found", mi.lbl = "reoffender missed",
#'                         fa.lbl = "false accusation", cr.lbl = "correct release",
#'                         prev = .45,  # prevalence of being a reoffender.
#'                         sens = .98,
#'                         spec = .46, fart = NA,  # (provide 1 of 2)
#'                         N = 753,
#'                         scen.src = "Example scenario")
#'
#' # Using a scenario:
#' summary(scen.reoffend)
#' plot(scen.reoffend)
#'
#' # 2 ways of defining the same scenario:
#' s1 <- riskyr(prev = .5, sens = .5, spec = .5, N = 100)  # s1: define by 3 prob & N
#' s2 <- riskyr(hi = 25, mi = 25, fa = 25, cr = 25)        # s2: same scenario by 4 freq
#' all.equal(s1, s2)  # should be TRUE
#'
#' # Ways to work:
#' riskyr(prev = .5, sens = .5, spec = .5, hi = 25, mi = 25, fa = 25, cr = 25)  # works (consistent)
#' riskyr(prev = .5, sens = .5, spec = .5, hi = 25, mi = 25, fa = 25)           # works (ignores freq)
#'
#' ## Watch out for:
#' # riskyr(hi = 25, mi = 25, fa = 25, cr = 25, N = 101)  # warns, uses actual sum of freq
#' # riskyr(prev = .4, sens = .5, spec = .5, hi = 25, mi = 25, fa = 25, cr = 25)  # warns, uses freq
#'
#' @export

## riskyr Definition: ------

riskyr <- function(scen.lbl = "",  ## WAS: txt$scen.lbl,
                   scen.lng = txt$scen.lng,
                   scen.txt = txt$scen.txt, popu.lbl = txt$popu.lbl,
                   cond.lbl = txt$cond.lbl,
                   cond.true.lbl = txt$cond.true.lbl, cond.false.lbl = txt$cond.false.lbl,
                   dec.lbl = txt$dec.lbl,
                   dec.pos.lbl = txt$dec.pos.lbl, dec.neg.lbl = txt$dec.neg.lbl,
                   hi.lbl = txt$hi.lbl, mi.lbl = txt$mi.lbl,
                   fa.lbl = txt$fa.lbl, cr.lbl = txt$cr.lbl,
                   prev = NA,
                   sens = NA,
                   spec = NA, fart = NA,
                   N = NA,  ## WAS: freq$N,
                   hi = NA, mi = NA,
                   fa = NA, cr = NA,
                   scen.src = txt$scen.src,
                   scen.apa = txt$scen.apa) {

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
    probs <- c(probs_calc$prev, probs_calc$sens, probs_calc$mirt, probs_calc$spec, probs_calc$fart)
    need.probs <- FALSE  # set flag that probs are no longer needed

    ## (c) Calculate ALL frequencies from 4 essential frequencies:
    freqs <- comp_freq_freq(hi, mi, fa, cr)

  } else {  # if not all 4 essential frequencies are provided:

    probs <- NA         # set probs to NA (and use inputs below).
    need.probs <- TRUE  # set flag that probs are still needed

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

    if (need.probs) {  # not all 4 essential frequencies were provided above:

      ## Case_3: Not all frequencies OR a valid set of probabilities were provided:
      warning("Neither a full set of frequencies nor a valid set of probabilities was provided.")

    }

  }

  ## Case_4: Something is missing: ------

  ## +++ here now +++

  # if (is.na(freqs)) {
  #   warning("Frequencies were not provided or could not be computed.")
  # }
  #
  # if (is.na(probs)) {
  #   warning("Probabilities were not provided or could not be computed.")
  # }

  ## prob_quintet <- probs  # both should be the same by now (not needed?).

  ## Define object (scenario) as a list: ------

  object <- list(scen.lbl = scen.lbl, scen.lng = scen.lng, scen.txt = scen.txt,
                 popu.lbl = popu.lbl, cond.lbl = cond.lbl,
                 cond.true.lbl = cond.true.lbl, cond.false.lbl = cond.false.lbl,
                 dec.lbl = dec.lbl, dec.pos.lbl = dec.pos.lbl, dec.neg.lbl = dec.neg.lbl,
                 hi.lbl = hi.lbl, mi.lbl = mi.lbl, fa.lbl = fa.lbl, cr.lbl = cr.lbl,
                 prev = probs[1],
                 sens = probs[2],
                 spec = probs[4], fart = probs[5],
                 N = N,
                 hi = freqs$hi, mi = freqs$mi,
                 fa = freqs$fa, cr = freqs$cr,
                 scen.src = scen.src, scen.apa = scen.apa)

  ## Add class riskyr:
  class(object) <- "riskyr"

  return(object)

}


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
## +++ here now +++
## riskyr(prev = NA, hi = NA)

## Compare with df.scenarios:
# names(df.scenarios)
# all.equal(names(test.obj), names(df.scenarios))

# # cat(
# #   paste0(
# #     paste0(names(scenarios$scen1), " = ", names(scenarios$scen1)),
# #     collapse = ", "))


## 2. scenarios: Define scenarios as a list of riskyr objects -----------

## Note: Convert the data frame df.scenarios into a list "scenarios"
##       of riskyr objects:

scenarios <- NULL # initialize

## Note helper stuff:
# cat(paste0("#'   \\item ", df.scenarios$scen.lbl[-1], "\n#'\n"))

## scenarios: Documentation ------

#' A collection of riskyr scenarios from various sources.
#'
#' \code{scenarios} is a list  that
#' contains a collection of scenarios of class "riskyr" from the
#' scientific literature and other sources that can be used directly
#' in the visualization and summary functions.
#'
#' \code{scenarios} currently contains the following scenarios:
#'
#' \enumerate{
#'
#'   \item Mammografie 1
#'
#'   \item Nackenfaltentest (NFT)
#'
#'   \item HIV 1 (f)
#'   \item HIV 2 (f)
#'
#'   \item Mammography 2
#'
#'   \item Sepsis
#'
#'   \item Cab problem
#'
#'   \item Sigmoidoskopie 1
#'   \item Sigmoidoskopie 2
#'
#'   \item Brustkrebs 1
#'   \item Brustkrebs 2 (BRCA1)
#'   \item Brustkrebs 3 (BRCA1 + pos. Mam.)
#'   \item HIV 3 (m)
#'   \item HIV 4 (m)
#'   \item Nackenfaltentest 2 (NFT)
#'   \item Amniozentese (pos. NFT)
#'
#'   \item Musical town
#'   \item Mushrooms
#'
#'   \item Bowel cancer (FOB screening)
#'
#'   \item PSA test 1 (high prev)
#'   \item PSA test 2 (low prev)
#'
#'   \item Colorectal cancer
#'
#'   \item Psylicraptis screening
#'
#'   \item Mammography 6 (prob)
#'   \item Mammography 6 (freq)
#'
#' }
#'
#' Variables describing each scenario:
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
#' See columns \code{scen.src} and \code{scen.apa}
#' for a scenario's source information.
#'
#' The information of \code{scenarios} is also contained in an
#' R data frame \code{\link{df.scenarios}} (and generated from
#' the corresponding \code{.rda} file in \code{/data/}).
#'
#' @format A list with currently 26 objects of class "riskyr" (i.e., scenarios)
#' which are each described by 21 variables:
#'
#' @export

## scenarios: Definition ------

scenarios <- vector("list", nrow(df.scenarios))  # initialize scenarios as a list (from df.scenarios)
names(scenarios) <- paste0("n", 1:nrow(df.scenarios))

for (i in 1:nrow(df.scenarios)) {  # for each scenario i in df.scenarios:

  ## (1) define scenario s:
  s <- df.scenarios[i, ]

  ## (2) pass scenario s to riskyr function:
  cur.scen <- riskyr(scen.lbl = s$scen.lbl, scen.lng = s$scen.lng, scen.txt = s$scen.txt,
                     popu.lbl = s$popu.lbl, cond.lbl = s$cond.lbl,
                     cond.true.lbl = s$cond.true.lbl, cond.false.lbl = s$cond.false.lbl,
                     dec.lbl = s$dec.lbl, dec.pos.lbl = s$dec.pos.lbl, dec.neg.lbl = s$dec.neg.lbl,
                     hi.lbl = s$hi.lbl, mi.lbl = s$mi.lbl, fa.lbl = s$fa.lbl, cr.lbl = s$cr.lbl,
                     prev = s$prev,
                     sens = s$sens,
                     spec = s$spec, fart = s$fart,
                     N = s$N,
                     scen.src = s$scen.src, scen.apa = s$scen.apa)  # use initialization function.

  # (3) Add cur.scen (riskyr object) as i-th element of scenarios
  scenarios[[i]] <- cur.scen

} # end for ...


## Check: --------
# length(scenarios)
# scenarios$n25  # => shows elements of a scenario

## (B) Handle riskyr objects: ------------------

## 1. plot.riskyr function: -----

## Testing dots:
# test_fun <- function(...) {
#   plot_icons(...)
# }
#
# test_fun(N = 100, blubb = 5, prev = 0.7)
#
## ok...

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
#' @param plot.type The type of plot to be generated.
#'
#' \enumerate{
#'
#'   \item \code{plot.type = "fnet"} or \code{plot.type = "network"}:
#'   Risk information is plotted in a network diagram of frequencies and probabilities (default).
#'   See \code{\link{plot_fnet}} for further options.
#'
#'   \item \code{plot.type = "ftree"} or \code{plot.type = "ftree"}:
#'   Risk information is plotted in a frequency tree.
#'   See \code{\link{plot_tree}} for further options.
#'
#'   \item \code{plot.type = "icons"} or \code{plot.type = "iconarray"}:
#'   The underlying population is plotted as icons.
#'   See \code{\link{plot_icons}} for further options.
#'
#'   \item \code{plot.type = "mosaic"} or \code{plot.type = "mosaicplot"}:
#'   Risk information is plotted as a mosaicplot.
#'   See \code{\link{plot_mosaic}} for further options.
#'
#'   \item \code{plot.type = "curve"} or \code{plot.type = "curves"}:
#'   Draws curves of selected values (including \code{\link{PPV}}, \code{\link{NPV}})
#'   See \code{\link{plot_curve}} for further options.
#'
#'   \item \code{plot.type = "plane"} or \code{plot.type = "planes"}:
#'   Draws a 3D-plane of selected values (e.g., predictive values \code{\link{PPV}} or \code{\link{NPV}})
#'   See \code{\link{plot_plane}} for further options.
#' }
#'
#' @param ... Additional parameters to be passed to the
#' underlying plotting functions.
#'
#' @examples
#' # Select a scenario from list of scenarios:
#' s25 <- scenarios$n25  # select scenario 25 from scenarios
#'
#' # Plot different types:
#' plot(s25)  # => default plot (fnet)
#' plot(s25, plot.type = "fnet")  # => network diagram (same as default)
#' plot(s25, plot.type = "tree", area = "vr") # => tree diagram (with vertical rectangles)
#' plot(s25, plot.type = "icons", type = "mosaic")
#' plot(s25, plot.type = "curve", what = "all")
#' plot(s25, plot.type = "plane", what = "NPV")
#'
#' plot(s25, plot.type = "area", by = "cddc")
#' plot(s25, plot.type = "tab", by = "cddc", f_lwd = 3)
#'
#' @family visualization functions
#'
#' @export

## plot.riskyr Definition: ------

plot.riskyr <- function(x = NULL,  # require riskyr scenario
                        plot.type = "network",  # default plot.type
                        ...        # other type and display parameters in plot_XXX().
) {

  ## Note: Most other functions (except for plot_icons) currently lack the ellipsis.
  ## Therefore, these functions will throw an exception when unnecessary parameters are passed.

  ## Test plot.type argument:
  if (!plot.type %in% c("fnet", "network", "net",
                        "prism", "fprism",
                        "area", "farea",
                        "tab", "table", "ftab", "ctab",
                        "tree", "ftree",
                        "icons", "iconarray", "icon",
                        "mosaic", "mosaicplot",
                        "curve", "curves",
                        "plane", "planes", "cube")) {
    stop("Unknown plot.type specified in plot.riskyr.")
  }

  ## Increase robustness: ----------

  plot.type <- tolower(plot.type)  # ensure lowercase

  ## Plotting functions: ----------

  ## frequency net/fnet (default):
  if ((plot.type == "fnet") || (plot.type == "network") || (plot.type == "net")) {

    plot_fnet(prev = x$prev,
              sens = x$sens, mirt = NA,
              spec = x$spec, fart = NA,
              N = x$N,
              ## Options:
              title.lbl = x$scen.lbl,
              popu.lbl = x$popu.lbl,
              cond.true.lbl = x$cond.true.lbl,
              cond.false.lbl = x$cond.false.lbl,
              dec.pos.lbl = x$dec.pos.lbl,
              dec.neg.lbl = x$dec.neg.lbl,
              hi.lbl = x$hi.lbl, mi.lbl = x$mi.lbl,
              fa.lbl = x$fa.lbl, cr.lbl = x$cr.lbl,
              ...
    )

  } # if (plot.type == "network")

  ## prism plot:
  if ((plot.type == "prism") || (plot.type == "fprism")) {  # "prism"

    plot_prism(prev = x$prev,
               sens = x$sens, mirt = NA,
               spec = x$spec, fart = NA,
               N = x$N,
               ## Options:
               # title_lbl = x$scen.lbl,
               ...
    )

  } # if (plot.type == "prism")

  ## area / mosaic plot:
  if ((plot.type == "area") || (plot.type == "farea")) {  # "mosaic"

    plot_area(prev = x$prev,
              sens = x$sens, mirt = NA,
              spec = x$spec, fart = NA,
              N = x$N,
              ## Options:
              # title_lbl = x$scen.lbl,
              ...
    )

  } # if (plot.type == "area")

  ## Contingency / frequency table / tab plot:
  if ((plot.type == "tab") || (plot.type == "table") || (plot.type == "ftab") || (plot.type == "ctab")) {

    plot_tab(prev = x$prev,
             sens = x$sens, mirt = NA,
             spec = x$spec, fart = NA,
             N = x$N,
             ## Options:
             # title_lbl = x$scen.lbl,
             ...
    )

  } # if (plot.type == "tab")

  ## frequency tree:
  if ((plot.type == "tree") || (plot.type == "ftree")) {

    plot_tree(prev = x$prev,             # probabilities
              sens = x$sens, mirt = NA,
              spec = x$spec, fart = NA,  # was: num$fart,
              N = x$N,    # ONLY freq used (so far)
              ## Options:
              title.lbl = x$scen.lbl,     # custom text labels
              popu.lbl = x$popu.lbl,
              cond.true.lbl = x$cond.true.lbl,
              cond.false.lbl = x$cond.false.lbl,
              dec.pos.lbl = x$dec.pos.lbl,
              dec.neg.lbl = x$dec.neg.lbl,
              hi.lbl = x$hi.lbl, mi.lbl = x$mi.lbl,
              fa.lbl = x$fa.lbl, cr.lbl = x$cr.lbl,
              ...
    )

  } #  if (plot.type == "tree")

  ## Mosaic plot:
  if ((plot.type == "mosaic") || (plot.type == "mosaicplot")) {
    plot_mosaic(prev = x$prev,
                sens = x$sens, mirt = NA,
                spec = x$spec, fart = NA,
                N = x$N,
                ## Options:
                title.lbl = x$scen.lbl,
                ...)

  } # if (plot.type == "mosaicplot")

  ## Icon array
  if ((plot.type == "icons") || (plot.type == "iconarray") || (plot.type == "icon")) {

    plot_icons(prev = x$prev,             # probabilities
               sens = x$sens, mirt = NA,
               spec = x$spec, fart = NA,  # was: num$fart,
               N = x$N,    # ONLY freq used (so far)
               ## Options:
               title.lbl = x$scen.lbl,
               type.lbls = x[c("hi.lbl", "mi.lbl", "fa.lbl", "cr.lbl")],
               ...
    )

  } #  if (plot.type == "iconarray")

  ## Curve of probabilities:
  if ((plot.type == "curve") || (plot.type == "curves")) {

    plot_curve(prev = x$prev,             # probabilities (3 essential, 2 optional)
               sens = x$sens, mirt = NA,
               spec = x$spec, fart = NA,
               ## Options:
               title.lbl = x$scen.lbl,
               ...
    )
  } # if (plot.type == "curve")


  ## Plane/cube of probabilities:
  if ((plot.type == "plane") || (plot.type == "planes") || (plot.type == "cube")) {

    plot_plane(prev = x$prev,             # probabilities (3 essential, 2 optional)
               sens = x$sens, mirt = NA,
               spec = x$spec, fart = NA,
               ## Options:
               title.lbl = x$scen.lbl, # plot title label
               ...
    )
  } # if (plot.type == "plane")

}


## Check: ------
## (A) with example scenarios (defined above):
# plot(scenario2, plot.type = "icons")
# plot(scenario3, plot.type = "tree")

## (B) with scenarios from scenarios (defined BELOW):
#
# s25 <- scenarios$n25  # select Scenario 25 from scenarios
#
# plot(s25)  # => default plot (fnet)
# plot(s25, plot.type = "fnet")  # => network diagram (same as default)
# plot(s25, plot.type = "tree", area = "vr") # => tree diagram (with vertical rectangles)
# plot(s25, plot.type = "icons")
# plot(s25, plot.type = "icons", type = "mosaic")  # passing on additional parameters.
# plot(s25, plot.type = "mosaic")
# plot(s25, plot.type = "curve", what = "all")
# plot(s25, plot.type = "plane", what = "npv")
# # plot(s25, plot.type = "wetwork")
## Newer plots:
# plot(s25, plot.type = "prism", by = "cddc", f_lbl = "num")
# plot(s25, plot.type = "area", by = "cddc")
# plot(s25, plot.type = "tab", by = "cddc", f_lwd = 3)

## 2. summary.riskyr function: ------------------

## (a) Create a summary object:

## summary.riskyr Documentation: ------

#' Summarizing a riskyr scenario.
#'
#' \code{summary.riskyr} provides a \code{summary} method for objects of class "riskyr".
#'
#' @format An object of class \code{summary.riskyr}
#' with up to 9 entries.
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
#' @param ... Additional parameters to be passed to the
#' underlying summary functions.
#'
#' @examples
#' summary(scenarios$n4)
#'
#' @family summary functions
#'
#' @export

## summary.riskyr Definition: ------

summary.riskyr <- function(object = NULL, summarize = "all", ...) {

  obj.sum <- list()  # initialize as list

  obj.sum$scen.lbl <- object$scen.lbl

  obj.sum$cond.lbl <- object$cond.lbl  # condition
  obj.sum$dec.lbl <- object$dec.lbl    # decision
  obj.sum$popu.lbl <- object$popu.lbl  # population
  obj.sum$N <- object$N                # N
  obj.sum$scen.src <- object$scen.src  # source (short)

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

## 3. print.summary.riskyr function: ------------------

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
#' @param ... Additional parameters to be passed to the
#' generic print function.
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

  cat("Scenario: ",   x$scen.lbl, "\n\n")  # always show scenario name.

  cat("Condition: ",  x$cond.lbl, "\n")  # always show current condition.
  cat("Decision: ",   x$dec.lbl,  "\n")  # always show current decision.
  cat("Population: ", x$popu.lbl, "\n")  # always show current condition.
  cat("N = ", x$N, "\n")                 # always show population size N.
  cat("Source: ", x$scen.src, "\n")      # always show (short) source info

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
# summary(scenario2)  # => all summaries
# summary(scenario2, summarize = "freq")
# summary(scenario2, summarize = "prob")
# summary(scenario2, summarize = "accu")


## (C) Demo: Typical user interaction / session: -------

## 1. Defining and viewing your own scenario: -----
## see Vignette of riskyr primer

## 2. Exporing pre-defined scenarios: ------------

## Example 1: Mammography screening (standard example) ------
## Source: Hoffrage et al. (2015), p. 3

# ## (a) Choosing a scenario
# s25 <- scenarios[[25]] # select by number: [[dd]]
# s25 <- scenarios$n25   # select by name:   $ndd

# ## (b) Summary info:
# summary(s25)

# ## (c) Visualization:
# plot(s25)  # => default plot (fnet)
# plot(s25, plot.type = "icons")
# plot(s25, plot.type = "curve")


## Example 2: PSA screening ----------------
## Source: Arkes & Gaissmaier (2012), p. 550

## Overview:
# summary(scenarios$n21)

## Visualization:
# plot(scenarios$n21, plot.type = "tree", area = "sq")
# plot(scenarios$n21, plot.type = "icons")
# plot(scenarios$n21, plot.type = "curves", what = "all")
# plot(scenarios$n21, plot.type = "planes", what = "PPV")

## Contrast with lower prevalence version:

## Overview:
# summary(scenarios$n22)

## Visualization:
# plot(scenarios$n22, plot.type = "tree", area = "sq")
# plot(scenarios$n22, plot.type = "icons")
# plot(scenarios$n22, plot.type = "curves", what = "all")
# plot(scenarios$n22, plot.type = "planes", what = "PPV")


## Example 3: Bowel cancer (FOB screening): ------
## Source: https://en.wikipedia.org/wiki/Positive_and_negative_predictive_values#Worked_example

# s20 <- scenarios$n20   # select by name:   $ndd

## Overview:
# summary(s20)
# summary(s20, summarize = "freq")

## Visualization:
# plot(s20, plot.type = "tree", area = "vr") # => tree diagram (with vertical rectangles)
# plot(s20, plot.type = "curve", what = "all")
# plot(s20, plot.type = "icons")
# plot(s20, plot.type = "icons", type = "mosaic")  # passing on additional parameters.
# plot(s20, plot.type = "mosaic")
# plot(s20, plot.type = "plane", what = "NPV")
## plot(s20, plot.type = "wetwork")

## (*) Done: ----------

## - Clean up code.  [2018 08 22].

## (+) ToDo: ----------

## - allow riskyr() to take all kinds of inputs,
##   so that a full object is created.

## eof. ------------------------------------------
