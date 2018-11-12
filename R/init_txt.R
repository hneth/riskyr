## init_txt.R | riskyr
## 2018 11 12
## Define defaults and initialize the
## current set of all text elements (txt):
## -----------------------------------------------

## (A) Set defaults for all titles and labels (txt) ----------

txt_def <- list(

  # (a) Scenario:
  scen.lbl = "Scenario", # provide scenario name/label in Title Caps (if desired in plot titles)
  scen.txt = "Description of the current scenario in a short paragraph of text. This description may include several sentences.",
  scen.src = "Source information",
  scen.apa = "Source information in APA format",
  scen.lng = "en/de", # language

  # (b) Population:
  ## Distinguish between 2 different population labels:
  ## 1. Dimension: general ftype_label ("Population") vs.
  popu.lbl = "Population",  # Dimension: current population label: "Population", "Sample", "N", etc.
  ## 2. Frequency box: current label for fbox of ftype N ("Current sample"/"N").
  N.lbl = "N",  # Label for fbox of ftype N.
  ## Note: Consider a 3rd population label that provides a longer description of the current population
  # popu.txt  # Longer description of the current population (as in s21: PSA test example)

  # (c) Condition:
  cond.lbl = "Condition",   # Dimension label: "Condition X", "disease", "Breast cancer", "HIV", "Sepsis", etc.
  cond.true.lbl  = "True",  # "Condition true",   # "has condition", "is affected"
  cond.false.lbl = "False", # "Condition false",  # "does not have condition", "is unaffected"

  # (d) Decision/prediction/predicted condition:
  dec.lbl = "Decision",  # Dimension label: "Prediction", "Diagnostic decision", "Mammography", "HIV test"
  dec.pos.lbl  = "Positive", # "Decision positive",  # "Prediction positive", "called", "is judged to have condition", "is classified as affected"
  dec.neg.lbl = "Negative",  # "Decision negative",   # "Prediction negative", "not called", "is judged to not have condition", "is classified as unaffected"

  # (e) Accuracy/correspondence of decision to condition:
  acc.lbl = "Accuracy",   # Dimension label: accuracy/correspondence
  dec.cor.lbl = "Correct", # Decision correct", "accurate"  # acc/cor  # accurate decision
  # Note: "dec.cor" should better be called "dec.acc" (for consistency with probabilities "acc" vs. "err")!
  dec.err.lbl = "Erroneous", # "Decision error", "inaccurate"  # err   # inaccurate decision

  # (f) Labels for the 4 SDT cases/cells in 2x2 contingency table (combinations):
  sdt.lbl = "Cases",  # Dimension label: "Classification" or cell/case/SDT 2x2 table, etc.
  hi.lbl = "TP", # "True positive",  # "hit", "true positive", "has condition and is detected/predicted as such", "set(dec POS & cond TRUE)"
  mi.lbl = "FN", # "False negative", # "miss", "false negative", "omission", "has condition and is NOT detected/predicted as such", "set(dec NEG & cond TRUE)"
  fa.lbl = "FP", # "False positive", # "false alarm",       "false positive", "set(dec POS & cond FALSE)"
  cr.lbl = "TN"  # "True negative"   # "correct rejection", "true negative",  "set(dec NEG & cond FALSE)"

)

## Check:
# length(txt_def)  # 21 text elements


## (B) Initialization of all text elements (titles and labels) ----------

#' Initialize basic text elements.
#'
#' \code{init_txt} initializes basic text elements
#' (i.e., all titles and labels corresponding to the current scenario
#' and used throughout the \code{riskyr} package).
#'
#' All textual elements that specify titles and details of the current scenario
#' are stored as named elements (of type character) in a list \code{txt}.
#' \code{\link{init_txt}} allows changing elements by assigning new character
#' objects to existing names.
#'
#' @param scen.lbl The current scenario title (sometimes in Title Caps).
#' @param scen.txt A longer text description of the current scenario
#' (which may extend over several lines).
#' @param scen.src The source information for the current scenario.
#' @param scen.apa Source information in APA format.
#' @param scen.lng Language of the current scenario (as character code).
#' Options: \code{"en"}...English, \code{"de"}...German.
#'
#' @param popu.lbl A general name for \emph{population} dimension.
#' @param N.lbl A brief description of the current target population \code{\link{popu}} or sample.
#'
#' @param cond.lbl A general name for the \emph{condition} (e.g., some disease) currently considered.
#' @param cond.true.lbl A short label for the \emph{presence} of the current condition
#' or \code{\link{cond.true}} cases (the condition's true state of TRUE).
#' @param cond.false.lbl A short label for the \emph{absence} of the current condition
#' or \code{\link{cond.false}} cases (the condition's true state of FALSE).
#'
#' @param dec.lbl A general name for the \emph{decision} dimension (e.g., some diagnostic test) currently made.
#' @param dec.pos.lbl A short label for \emph{positive} decisions
#' or \code{\link{dec.pos}} cases (e.g., predicting the presence of the condition).
#' @param dec.neg.lbl A short label for \emph{negative} decisions
#' or \code{\link{dec.neg}} cases (e.g., predicting the absence of the condition).
#'
#' @param acc.lbl A general name for the \emph{accuracy} dimension (e.g., correspondence of decision to condition).
#' @param dec.cor.lbl A short label for \emph{correct} decisions
#' or \code{\link{dec.cor}} cases (e.g., accurately predicting the condition).
#' @param dec.err.lbl A short label for \emph{erroneous} decisions
#' or \code{\link{dec.err}} cases (e.g., inaccurately predicting the condition).
#'
#' @param sdt.lbl A name for the case/category/cell dimension in the 2x2 contingency table (SDT: condition x decision).
#' @param hi.lbl A short label for \emph{hits} or \emph{true positives} \code{\link{hi}}
#' (i.e., correct decisions of the presence of the condition, when the condition is actually present).
#' @param mi.lbl A short label for \emph{misses} or \emph{false negatives} \code{\link{mi}}
#' (i.e., incorrect decisions of the absence of the condition when the condition is actually present).
#' @param fa.lbl A short label for \emph{false alarms} or \emph{false positives} \code{\link{fa}}
#' (i.e., incorrect decisions of the presence of the condition when the condition is actually absent).
#' @param cr.lbl A short label for \emph{correct rejections} or \emph{true negatives} \code{\link{cr}}
#' (i.e., a correct decision of the absence of the condition, when the condition is actually absent).
#'
#' @examples
#' init_txt()          # defines a list of (default) text elements
#' length(init_txt())  # 21
#'
#' # Customizing current text elements:
#' txt <- init_txt(scen.lbl = "My scenario",
#'                 scen.src = "My source",
#'                 N.lbl = "My population")
#'
#' @family functions initializing scenario information
#'
#' @seealso
#' \code{\link{txt}} for current text settings;
#' \code{\link{pal}} for current color settings;
#' \code{\link{num}} for basic numeric parameters
#'
#' @export

init_txt <- function(scen.lbl = txt_def$scen.lbl,  # Scenario title
                     scen.txt = txt_def$scen.txt,  # text with scenario description
                     scen.src = txt_def$scen.src,  # scenario source
                     scen.apa = txt_def$scen.apa,  # scenario source in APA format
                     scen.lng = txt_def$scen.lng,  # language
                     ## (+) Population:
                     popu.lbl = txt_def$popu.lbl,  # Population dimension label
                     N.lbl = txt_def$N.lbl,        # Current target population label
                     ## (a) Condition:
                     cond.lbl = txt_def$cond.lbl,              # Condition dimension label
                     cond.true.lbl  = txt_def$cond.true.lbl,   # Condition true
                     cond.false.lbl = txt_def$cond.false.lbl,  # Condition false
                     ## (b) Decision/prediction/predicted condition:
                     dec.lbl = txt_def$dec.lbl,                # Decision dimension label
                     dec.pos.lbl  = txt_def$dec.pos.lbl,       # "Decision positive"
                     dec.neg.lbl = txt_def$dec.neg.lbl,        # "Decision negative"
                     ## (c) Accuracy/correspondence of decision to condition:
                     acc.lbl = txt_def$acc.lbl,                # Accuracy dimension label
                     dec.cor.lbl = txt_def$dec.cor.lbl,        # "Decision accurate"
                     dec.err.lbl = txt_def$dec.err.lbl,        # "Decision inaccurate"
                     ## (d) Labels for the 4 SDT cases (combinations):
                     sdt.lbl = txt_def$sdt.lbl,  # case/category/cell/SDT
                     hi.lbl = txt_def$hi.lbl,    # hits               = "True positive"
                     mi.lbl = txt_def$mi.lbl,    # misses             = "False negative"
                     fa.lbl = txt_def$fa.lbl,    # false alarms       = "False positive"
                     cr.lbl = txt_def$cr.lbl     # correct rejections = "True negative"
) {

  ## 1. Initialize txt:
  txt <- NULL

  ## 2. Pass arguments to list:
  txt <- list(
    ## Scenario:
    scen.lbl = scen.lbl,
    scen.txt = scen.txt,
    scen.src = scen.src,
    scen.apa = scen.apa,
    scen.lng = scen.lng,
    ## (+) Population:
    popu.lbl = popu.lbl,
    N.lbl = N.lbl,
    ## (a) Condition:
    cond.lbl = cond.lbl,
    cond.true.lbl  = cond.true.lbl,
    cond.false.lbl = cond.false.lbl,
    ## (b) Decision/prediction/predicted condition:
    dec.lbl = dec.lbl,
    dec.pos.lbl = dec.pos.lbl,
    dec.neg.lbl = dec.neg.lbl,
    ## (c) Accuracy:
    acc.lbl = acc.lbl,
    dec.cor.lbl = dec.cor.lbl,
    dec.err.lbl = dec.err.lbl,
    ## (d) Labels for the 4 SDT cases/cells (combinations):
    sdt.lbl = sdt.lbl,
    hi.lbl = hi.lbl,
    mi.lbl = mi.lbl,
    fa.lbl = fa.lbl,
    cr.lbl = cr.lbl
  )

  ## 3. Return entire list txt:
  return(txt)

}

## Check:
# init_txt()          # => defines a list of (default) text elements
# length(init_txt())  # => 21
#
# # Customizing current text elements:
# txt <- init_txt(scen.lbl = "US or Them",
#                 scen.src = "Some stable genius",
#                 N.lbl = "We, the people")



## (C) Initialize a list txt to contain all current text elements ---------

#' List current values of basic text elements.
#'
#' \code{txt} is initialized to a list of named elements
#' to define all titles and labels corresponding to the current scenario
#' and used throughout the \code{riskyr} package.
#'
#' All textual elements that specify titles and details of the current scenario
#' are stored as named elements (of type character) in a list \code{txt}.
#' To change an element, assign a new character object to an existing name.
#'
#' \code{txt} currently contains the following text labels:
#'
#' \enumerate{
#'
#' \item \code{scen.lbl} The current scenario title (sometimes in Title Caps).
#'
#' \item \code{scen.txt} A longer text description of the current scenario
#' (which may extend over several lines).
#'
#' \item \code{scen.src} The source information for the current scenario.
#'
#' \item \code{scen.apa} The source information in APA format.
#'
#' \item \code{scen.lng} The language of the current scenario (as character code).
#' Options: \code{"en"}...English, \code{"de"}... German.
#'
#' \item \code{popu.lbl} A brief description of the current target population \code{\link{popu}} or sample.
#'
#' \item \code{cond.lbl} A name for the \emph{condition} or feature (e.g., some disease) currently considered.
#'
#' \item \code{cond.true.lbl} A label for the \emph{presence} of the current condition
#' or \code{\link{cond.true}} cases (the condition's true state of TRUE).
#'
#' \item \code{cond.false.lbl} A label for the \emph{absence} of the current condition
#' or \code{\link{cond.false}} cases (the condition's true state of FALSE).
#'
#' \item \code{dec.lbl} A name for the \emph{decision} or judgment (e.g., some diagnostic test) currently made.
#'
#' \item \code{dec.pos.lbl} A label for \emph{positive} decisions
#' or \code{\link{dec.pos}} cases (e.g., predicting the presence of the condition).
#'
#' \item \code{dec.neg.lbl} A label for \emph{negative} decisions
#' or \code{\link{dec.neg}} cases (e.g., predicting the absence of the condition).
#'
#' \item \code{sdt.lbl} A name for the \emph{case/category/cell} in the 2x2 contingency table (condition x decision, SDT).
#'
#' \item \code{hi.lbl} A label for \emph{hits} or \emph{true positives} \code{\link{hi}}
#' (i.e., correct decisions of the presence of the condition, when the condition is actually present).
#'
#' \item \code{mi.lbl} A label for \emph{misses} or \emph{false negatives} \code{\link{mi}}
#' (i.e., incorrect decisions of the absence of the condition when the condition is actually present).
#'
#' \item \code{fa.lbl} A label for \emph{false alarms} or \emph{false positives} \code{\link{fa}}
#' (i.e., incorrect decisions of the presence of the condition when the condition is actually absent).
#'
#' \item \code{cr.lbl} A label for \emph{correct rejections} or \emph{true negatives} \code{\link{cr}}
#' (i.e., a correct decision of the absence of the condition, when the condition is actually absent).
#'
#' }
#'
#' @examples
#' txt           # Show  all current names and elements
#' txt$scen.lbl  # Show the current scenario label (e.g., used in plot titles)
#' txt$scen.lbl <- "My example"  # Set a new scenario title
#'
#'
#' @family lists containing current scenario information
#'
#'
#' @seealso
#' \code{\link{init_txt}} initializes text information;
#' \code{\link{num}} contains basic numeric parameters;
#' \code{\link{init_num}} initializes basic numeric parameters;
#' \code{\link{pal}} contains current color information;
#' \code{\link{init_pal}} initializes color information;
#' \code{\link{freq}} contains current frequency information;
#' \code{\link{comp_freq}} computes current frequency information;
#' \code{\link{prob}} contains current probability information;
#' \code{\link{comp_prob}} computes current probability information.
#'
#' @export

## Apply:
txt <- init_txt()

## Check:
# txt          # displays all current names and elements
# txt$scen.lbl # displays the current scenario label (e.g., used in plot titles)
# txt$scen.lbl <- "My favorite example" # sets a new scenario title


## txt_TF: Define alternative set of text labels: --------

#' Alternative text labels (TP, FN, FP, TN).
#'
#' \code{txt_TF} is initialized to alternative text labels
#' to define a frequency naming scheme in which
#' (hi, mi, fa, cr) are called (TP, FN, FP, TN).
#'
#' See \code{\link{txt}} for default text information.
#'
#' Assign \code{txt <- txt_TF} to use as default text labels
#' throughout the \code{riskyr} package.
#'
#' @examples
#' txt_TF       # shows all text labels
#' txt_TF["hi"]  # shows the current color for hits ("TP")
#' txt_TF["hi"] <- "hit" # defines a new label for hits (true positives, TP)
#'
#' @family lists containing current scenario information
#'
#' @seealso
#' \code{\link{txt}} contains current color information;
#' \code{\link{init_txt}} initializes color information.
#'
#' @export

txt_TF <- init_txt(scen.lbl = "",
                   cond.lbl = "Truth", cond.true.lbl = "True", cond.false.lbl = "False",
                   dec.lbl = "Test", dec.pos.lbl = "Positive", dec.neg.lbl = "Negative",
                   acc.lbl = "Accuracy", dec.cor.lbl = "Correct", dec.err.lbl = "Incorrect",
                   hi.lbl = "TP", mi.lbl = "FN", fa.lbl = "FP", cr.lbl = "TN")


## prob_lbl: List of probability labels ----------

## ToDo: Integrate prob_lbl into txt (to look up and define current set of probability names).

prob_lbl <- list(

  # (a) by condition:
  prev = "Prevalence",
  prev_c = "1 - prevalence",
  sens = "Sensitivity",       # aka. hit rate, recall
  mirt = "Miss rate",         # rate of type-2 errors, beta
  spec = "Specificity",       # aka. true negative rate, correct rejection rate
  fart = "False alarm rate",  # aka. false positive rate, rate of type-1 errors, alpha

  # (b) by decision:
  ppod = "Proportion positive",
  ppod_c = "Proportion negative",
  PPV = "Positive predictive value (PPV)",  # aka. precision
  FDR = "False detection rate",             # aka. false discovery rate
  NPV = "Negative predictive value (NPV)",  # aka. true omission rate
  FOR = "False omission rate",

  # (c) by accuracy:
  acc = "Rate correct",        # correct, accurate
  err = "Rate incorrect",      # error rate, inaccurate
  acc_hi = "p(hi | dec.cor)",  # "Proportion positive correct" (ppcor)
  acc_cr = "p(cr | dec.cor)",  # "Proportion negative correct" (pncor)
  err_mi = "p(mi | dec.err)",
  err_fa = "p(fa | dec.err)"

)


## (*) Done: -------------------------------------

## - name_prob: Add function to look up the prob that corresponds
##   to 2 freq    [2018 08 20].
## - label_freq + label_prob: Added functions to obtain labels
##   of freq and prob (from fname or pname).
## - Rename acc.cor and acc.err to dec.cor and dec.err
##   (to remain consistent with freq names).
## - Add text labels for accuracy/correspondence of decision to condition
##   (acc.lbl, dec.cor.lbl, dec.err.lbl) and for sdt.lbl.
## - Clean up code [2018 08 20].

## (+) ToDo: -------------------------------------

## - Add shorter, abbreviated versions of text elements (to use as labels)?
##   (e.g., hi/TP, mi/FN, fa/FN, cr/TN)
## - Add text elements for probabilities (to be used in labels)?

## eof. ------------------------------------------
