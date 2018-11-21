## init_txt.R | riskyr
## 2018 11 21
## Define defaults and initialize the
## current set of all text elements (txt):
## -----------------------------------------------

## (A) Set defaults for all titles and labels (txt) ----------

txt_def <- list(

  # (a) Scenario:
  scen_lbl = "Scenario", # provide scenario name/label in Title Caps (if desired in plot titles)
  scen_txt = "Description of the current scenario in a short paragraph of text. This description may include several sentences.",
  scen_src = "Source information",
  scen_apa = "Source information in APA format",
  scen_lng = "en/de", # language

  # (b) Population:
  ## Distinguish between 2 different population labels:
  ## 1. Dimension: general ftype_label ("Population") vs.
  popu_lbl = "Population",  # Dimension: current population label: "Population", "Sample", "N", etc.
  ## 2. Frequency box: current label for fbox of ftype N ("Current sample"/"N").
  N_lbl = "N",  # Label for fbox of ftype N.
  ## Note: Consider a 3rd population label that provides a longer description of the current population
  # popu.txt  # Longer description of the current population (as in s21: PSA test example)

  # (c) Condition:
  cond_lbl = "Condition",   # Dimension label: "Condition X", "disease", "Breast cancer", "HIV", "Sepsis", etc.
  cond.true_lbl  = "True",  # "Condition true",   # "has condition", "is affected"
  cond.false_lbl = "False", # "Condition false",  # "does not have condition", "is unaffected"

  # (d) Decision/prediction/predicted condition:
  dec_lbl = "Decision",  # Dimension label: "Prediction", "Diagnostic decision", "Mammography", "HIV test"
  dec.pos_lbl  = "Positive", # "Decision positive",  # "Prediction positive", "called", "is judged to have condition", "is classified as affected"
  dec.neg_lbl  = "Negative", # "Decision negative",  # "Prediction negative", "not called", "is judged to not have condition", "is classified as unaffected"

  # (e) Accuracy/correspondence of decision to condition:
  acc_lbl = "Accuracy",   # Dimension label: accuracy/correspondence
  dec.cor_lbl = "Correct", # Decision correct", "accurate"  # acc/cor  # accurate decision
  # Note: "dec.cor" should better be called "dec.acc" (for consistency with probabilities "acc" vs. "err")!
  dec.err_lbl = "Erroneous", # "Decision error", "inaccurate"  # err   # inaccurate decision

  # (f) Labels for the 4 SDT cases/cells in 2x2 contingency table (combinations):
  sdt_lbl = "Cases",  # Dimension label: "Classification" or cell/case/SDT 2x2 table, etc.
  hi_lbl = "TP", # "True positive",  # "hit", "true positive", "has condition and is detected/predicted as such", "set(dec POS & cond TRUE)"
  mi_lbl = "FN", # "False negative", # "miss", "false negative", "omission", "has condition and is NOT detected/predicted as such", "set(dec NEG & cond TRUE)"
  fa_lbl = "FP", # "False positive", # "false alarm",       "false positive", "set(dec POS & cond FALSE)"
  cr_lbl = "TN"  # "True negative"   # "correct rejection", "true negative",  "set(dec NEG & cond FALSE)"

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
#' @param scen_lbl The current scenario title (sometimes in Title Caps).
#' @param scen_txt A longer text description of the current scenario
#' (which may extend over several lines).
#' @param scen_src The source information for the current scenario.
#' @param scen_apa Source information in APA format.
#' @param scen_lng Language of the current scenario (as character code).
#' Options: \code{"en"}...English, \code{"de"}...German.
#'
#' @param popu_lbl A general name for \emph{population} dimension.
#' @param N_lbl A brief description of the current target population \code{\link{popu}} or sample.
#'
#' @param cond_lbl A general name for the \emph{condition} (e.g., some disease) currently considered.
#' @param cond.true_lbl A short label for the \emph{presence} of the current condition
#' or \code{\link{cond.true}} cases (the condition's true state of TRUE).
#' @param cond.false_lbl A short label for the \emph{absence} of the current condition
#' or \code{\link{cond.false}} cases (the condition's true state of FALSE).
#'
#' @param dec_lbl A general name for the \emph{decision} dimension (e.g., some diagnostic test) currently made.
#' @param dec.pos_lbl A short label for \emph{positive} decisions
#' or \code{\link{dec.pos}} cases (e.g., predicting the presence of the condition).
#' @param dec.neg_lbl A short label for \emph{negative} decisions
#' or \code{\link{dec.neg}} cases (e.g., predicting the absence of the condition).
#'
#' @param acc_lbl A general name for the \emph{accuracy} dimension (e.g., correspondence of decision to condition).
#' @param dec.cor_lbl A short label for \emph{correct} decisions
#' or \code{\link{dec.cor}} cases (e.g., accurately predicting the condition).
#' @param dec.err_lbl A short label for \emph{erroneous} decisions
#' or \code{\link{dec.err}} cases (e.g., inaccurately predicting the condition).
#'
#' @param sdt_lbl A name for the case/category/cell dimension in the 2x2 contingency table (SDT: condition x decision).
#' @param hi_lbl A short label for \emph{hits} or \emph{true positives} \code{\link{hi}}
#' (i.e., correct decisions of the presence of the condition, when the condition is actually present).
#' @param mi_lbl A short label for \emph{misses} or \emph{false negatives} \code{\link{mi}}
#' (i.e., incorrect decisions of the absence of the condition when the condition is actually present).
#' @param fa_lbl A short label for \emph{false alarms} or \emph{false positives} \code{\link{fa}}
#' (i.e., incorrect decisions of the presence of the condition when the condition is actually absent).
#' @param cr_lbl A short label for \emph{correct rejections} or \emph{true negatives} \code{\link{cr}}
#' (i.e., a correct decision of the absence of the condition, when the condition is actually absent).
#'
#' @examples
#' init_txt()          # defines a list of (default) text elements
#' length(init_txt())  # 21
#'
#' # Customizing current text elements:
#' txt <- init_txt(scen_lbl = "My scenario",
#'                 scen_src = "My source",
#'                 N_lbl = "My population")
#'
#' @family functions initializing scenario information
#'
#' @seealso
#' \code{\link{txt}} for current text settings;
#' \code{\link{pal}} for current color settings;
#' \code{\link{num}} for basic numeric parameters
#'
#' @export

init_txt <- function(scen_lbl = txt_def$scen_lbl,  # Scenario title
                     scen_txt = txt_def$scen_txt,  # text with scenario description
                     scen_src = txt_def$scen_src,  # scenario source
                     scen_apa = txt_def$scen_apa,  # scenario source in APA format
                     scen_lng = txt_def$scen_lng,  # language
                     ## (+) Population:
                     popu_lbl = txt_def$popu_lbl,  # Population dimension label
                     N_lbl = txt_def$N_lbl,        # Current target population label
                     ## (a) Condition:
                     cond_lbl = txt_def$cond_lbl,              # Condition dimension label
                     cond.true_lbl  = txt_def$cond.true_lbl,   # Condition true
                     cond.false_lbl = txt_def$cond.false_lbl,  # Condition false
                     ## (b) Decision/prediction/predicted condition:
                     dec_lbl = txt_def$dec_lbl,                # Decision dimension label
                     dec.pos_lbl  = txt_def$dec.pos_lbl,       # "Decision positive"
                     dec.neg_lbl = txt_def$dec.neg_lbl,        # "Decision negative"
                     ## (c) Accuracy/correspondence of decision to condition:
                     acc_lbl = txt_def$acc_lbl,                # Accuracy dimension label
                     dec.cor_lbl = txt_def$dec.cor_lbl,        # "Decision accurate"
                     dec.err_lbl = txt_def$dec.err_lbl,        # "Decision inaccurate"
                     ## (d) Labels for the 4 SDT cases (combinations):
                     sdt_lbl = txt_def$sdt_lbl,  # case/category/cell/SDT
                     hi_lbl = txt_def$hi_lbl,    # hits               = "True positive"
                     mi_lbl = txt_def$mi_lbl,    # misses             = "False negative"
                     fa_lbl = txt_def$fa_lbl,    # false alarms       = "False positive"
                     cr_lbl = txt_def$cr_lbl     # correct rejections = "True negative"
) {

  ## 1. Initialize txt:
  txt <- NULL

  ## 2. Pass arguments to list:
  txt <- list(
    ## Scenario:
    scen_lbl = scen_lbl,
    scen_txt = scen_txt,
    scen_src = scen_src,
    scen_apa = scen_apa,
    scen_lng = scen_lng,
    ## (+) Population:
    popu_lbl = popu_lbl,
    N_lbl = N_lbl,
    ## (a) Condition:
    cond_lbl = cond_lbl,
    cond.true_lbl  = cond.true_lbl,
    cond.false_lbl = cond.false_lbl,
    ## (b) Decision/prediction/predicted condition:
    dec_lbl = dec_lbl,
    dec.pos_lbl = dec.pos_lbl,
    dec.neg_lbl = dec.neg_lbl,
    ## (c) Accuracy:
    acc_lbl = acc_lbl,
    dec.cor_lbl = dec.cor_lbl,
    dec.err_lbl = dec.err_lbl,
    ## (d) Labels for the 4 SDT cases/cells (combinations):
    sdt_lbl = sdt_lbl,
    hi_lbl = hi_lbl,
    mi_lbl = mi_lbl,
    fa_lbl = fa_lbl,
    cr_lbl = cr_lbl
  )

  ## 3. Return entire list txt:
  return(txt)

}

## Check:
# init_txt()          # => defines a list of (default) text elements
# length(init_txt())  # => 21
#
# # Customizing current text elements:
# txt <- init_txt(scen_lbl = "US or Them",
#                 scen_src = "Some stable genius",
#                 N_lbl = "We, the people")



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
#' \item \code{scen_lbl} The current scenario title (sometimes in Title Caps).
#'
#' \item \code{scen_txt} A longer text description of the current scenario
#' (which may extend over several lines).
#'
#' \item \code{scen_src} The source information for the current scenario.
#'
#' \item \code{scen_apa} The source information in APA format.
#'
#' \item \code{scen_lng} The language of the current scenario (as character code).
#' Options: \code{"en"}...English, \code{"de"}... German.
#'
#' \item \code{popu_lbl} A brief description of the current target population \code{\link{popu}} or sample.
#'
#' \item \code{cond_lbl} A name for the \emph{condition} or feature (e.g., some disease) currently considered.
#'
#' \item \code{cond.true_lbl} A label for the \emph{presence} of the current condition
#' or \code{\link{cond.true}} cases (the condition's true state of TRUE).
#'
#' \item \code{cond.false_lbl} A label for the \emph{absence} of the current condition
#' or \code{\link{cond.false}} cases (the condition's true state of FALSE).
#'
#' \item \code{dec_lbl} A name for the \emph{decision} or judgment (e.g., some diagnostic test) currently made.
#'
#' \item \code{dec.pos_lbl} A label for \emph{positive} decisions
#' or \code{\link{dec.pos}} cases (e.g., predicting the presence of the condition).
#'
#' \item \code{dec.neg_lbl} A label for \emph{negative} decisions
#' or \code{\link{dec.neg}} cases (e.g., predicting the absence of the condition).
#'
#' \item \code{sdt_lbl} A name for the \emph{case/category/cell} in the 2x2 contingency table (condition x decision, SDT).
#'
#' \item \code{hi_lbl} A label for \emph{hits} or \emph{true positives} \code{\link{hi}}
#' (i.e., correct decisions of the presence of the condition, when the condition is actually present).
#'
#' \item \code{mi_lbl} A label for \emph{misses} or \emph{false negatives} \code{\link{mi}}
#' (i.e., incorrect decisions of the absence of the condition when the condition is actually present).
#'
#' \item \code{fa_lbl} A label for \emph{false alarms} or \emph{false positives} \code{\link{fa}}
#' (i.e., incorrect decisions of the presence of the condition when the condition is actually absent).
#'
#' \item \code{cr_lbl} A label for \emph{correct rejections} or \emph{true negatives} \code{\link{cr}}
#' (i.e., a correct decision of the absence of the condition, when the condition is actually absent).
#'
#' }
#'
#' @examples
#' txt           # Show  all current names and elements
#' txt$scen_lbl  # Show the current scenario label (e.g., used in plot titles)
#' txt$scen_lbl <- "My example"  # Set a new scenario title
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
# txt$scen_lbl # displays the current scenario label (e.g., used in plot titles)
# txt$scen_lbl <- "My favorite example" # sets a new scenario title


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

txt_TF <- init_txt(scen_lbl = "",
                   cond_lbl = "Truth", cond.true_lbl = "True", cond.false_lbl = "False",
                   dec_lbl = "Test", dec.pos_lbl = "Positive", dec.neg_lbl = "Negative",
                   acc_lbl = "Accuracy", dec.cor_lbl = "Correct", dec.err_lbl = "Incorrect",
                   hi_lbl = "TP", mi_lbl = "FN", fa_lbl = "FP", cr_lbl = "TN")


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
##   (acc_lbl, dec.cor_lbl, dec.err_lbl) and for sdt_lbl.
## - Clean up code [2018 08 20].

## (+) ToDo: -------------------------------------

## - Add shorter, abbreviated versions of text elements (to use as labels)?
##   (e.g., hi/TP, mi/FN, fa/FN, cr/TN)
## - Add text elements for probabilities (to be used in labels)?

## eof. ------------------------------------------
