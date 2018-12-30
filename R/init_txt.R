## init_txt.R | riskyr
## 2018 12 20
## Define defaults and initialize the
## current set of all text elements (txt):
## -----------------------------------------------

## (A) Set defaults for all titles and labels (txt & prob): ----------

## txt_lbl_def: Define a list of all scenario and freq labels: ----------

txt_lbl_def <- list(

  # (1) Scenario:
  scen_lbl = "Scenario", # provide scenario name/label in Title Caps (if desired in plot titles)
  scen_txt = "Description of the current scenario in a short paragraph of text. This description may include several sentences.",
  scen_src = "Source information",
  scen_apa = "Source information in APA format",
  scen_lng = "en/de", # language

  # (2) Population:
  ##   Distinguish between 2 different population labels:
  # 1. Dimension: general ftype_label ("Population") vs.
  popu_lbl = "Population",  # Dimension: current population label: "Population", "Sample", "N", etc.
  # 2. Frequency box: current label for fbox of ftype N ("Current sample"/"N").
  N_lbl = "N",              # Label for fbox of ftype N.
  ## Note: Consider a 3rd population label that provides a longer description of the current population
  # popu.txt  # Longer description of the current population (as in s21: PSA test example)

  # (3) 3 perspectives:
  # (A) by condition:
  cond_lbl = "Condition",   # Dimension label: "Condition X", "disease", "Breast cancer", "HIV", "Sepsis", etc.
  cond_true_lbl  = "True",  # "Condition true",   # "has condition", "is affected"
  cond_false_lbl = "False", # "Condition false",  # "does not have condition", "is unaffected"

  # (B) by decision/prediction/predicted condition:
  dec_lbl = "Decision",  # Dimension label: "Prediction", "Diagnostic decision", "Mammography", "HIV test"
  dec_pos_lbl  = "Positive", # "Decision positive",  # "Prediction positive", "called", "is judged to have condition", "is classified as affected"
  dec_neg_lbl  = "Negative", # "Decision negative",  # "Prediction negative", "not called", "is judged to not have condition", "is classified as unaffected"

  # (C) by accuracy/correspondence of decision to condition:
  acc_lbl = "Accuracy",   # Dimension label: accuracy/correspondence
  dec_cor_lbl = "Correct", # Decision correct", "accurate"  # acc/cor  # accurate decision
  # Note: "dec_cor" should better be called "dec_acc" (for consistency with probabilities "acc" vs. "err")!
  dec_err_lbl = "Erroneous", # "Decision error", "inaccurate"  # err   # inaccurate decision

  # (4) Labels for the 4 cases/cells/classes/SDT combinations in 2x2 contingency table:
  sdt_lbl = "Cases",  # Dimension label: "Classification" or cell/case/SDT 2x2 table, etc.
  hi_lbl  = "hi", # "TP", # "True positive",  # "hit", "true positive", "has condition and is detected/predicted as such", "set(dec POS & cond TRUE)"
  mi_lbl  = "mi", # "FN", # "False negative", # "miss", "false negative", "omission", "has condition and is NOT detected/predicted as such", "set(dec NEG & cond TRUE)"
  fa_lbl  = "fa", # "FP", # "False positive", # "false alarm",       "false positive", "set(dec POS & cond FALSE)"
  cr_lbl  = "cr"  # "TN"  # "True negative"   # "correct rejection", "true negative",  "set(dec NEG & cond FALSE)"

)

## Check:
# length(txt_lbl_def)  # 21
# txt_lbl_def$cond_true_lbl


## prob_lbl_def: Define a list of all probability labels: ----------

## ToDo: Integrate prob_lbl_def into txt (to consult current set of probability names)
##       and use in label_prob (to determine probability labels in plot_util.R).

prob_lbl_def <- list(

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
  acc_hi = "p(hi | dec_cor)",  # "Proportion positive correct" (ppcor)
  acc_cr = "p(cr | dec_cor)",  # "Proportion negative correct" (pncor)
  err_mi = "p(mi | dec_err)",
  err_fa = "p(fa | dec_err)"

)

## Check:
# length(prob_lbl_def)  # 18
# prob_lbl_def$PPV


## (B) Initialization of all text elements (titles and labels): ----------

#' Initialize basic text elements.
#'
#' \code{init_txt} initializes basic text elements \code{\link{txt}}
#' (i.e., all titles and labels corresponding to the current scenario)
#' that are used throughout the \code{riskyr} package.
#'
#' All textual elements that specify titles and details of the current scenario
#' are stored as named elements (of type character) in a list \code{\link{txt}}.
#' \code{\link{init_txt}} allows changing elements by assigning new character
#' objects to existing names.
#'
#' However, you can directly specify scenario-specific text elements
#' when defining a scenario with the \code{\link{riskyr}} function.
#'
#' @param scen_lbl The current scenario title (sometimes in Title Caps).
#' @param scen_txt A longer text description of the current scenario
#' (which may extend over several lines).
#' @param scen_src The source information for the current scenario.
#' @param scen_apa Source information in APA format.
#' @param scen_lng Language of the current scenario (as character code).
#' Options: \code{"en"}: English, \code{"de"}: German.
#'
#' @param popu_lbl A general name describing the current \emph{population}.
#' @param N_lbl A brief label for the current population \code{\link{popu}} or sample.
#'
#' @param cond_lbl A general name for the \emph{condition} dimension currently considered (e.g., some clinical condition).
#' @param cond_true_lbl A short label for the \emph{presence} of the current condition
#' or \code{\link{cond_true}} cases (the condition's true state of TRUE).
#' @param cond_false_lbl A short label for the \emph{absence} of the current condition
#' or \code{\link{cond_false}} cases (the condition's true state of FALSE).
#'
#' @param dec_lbl A general name for the \emph{decision} dimension (e.g., some diagnostic test) currently made.
#' @param dec_pos_lbl A short label for \emph{positive} decisions
#' or \code{\link{dec_pos}} cases (e.g., predicting the presence of the condition).
#' @param dec_neg_lbl A short label for \emph{negative} decisions
#' or \code{\link{dec_neg}} cases (e.g., predicting the absence of the condition).
#'
#' @param acc_lbl A general name for the \emph{accuracy} dimension (e.g., correspondence of decision to condition).
#' @param dec_cor_lbl A short label for \emph{correct} decisions
#' or \code{\link{dec_cor}} cases (e.g., accurately predicting the condition).
#' @param dec_err_lbl A short label for \emph{erroneous} decisions
#' or \code{\link{dec_err}} cases (e.g., inaccurately predicting the condition).
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
#' \code{\link{num}} for basic numeric parameters.
#'
#' @export

init_txt <- function(#
  # 1. Scenario:
  scen_lbl = txt_lbl_def$scen_lbl,  # Scenario title
  scen_txt = txt_lbl_def$scen_txt,  # text with scenario description
  scen_src = txt_lbl_def$scen_src,  # scenario source
  scen_apa = txt_lbl_def$scen_apa,  # scenario source in APA format
  scen_lng = txt_lbl_def$scen_lng,  # language
  # 2. Population:
  popu_lbl = txt_lbl_def$popu_lbl,  # Population dimension label
  N_lbl = txt_lbl_def$N_lbl,        # Current target population label
  # 3a. Condition:
  cond_lbl = txt_lbl_def$cond_lbl,              # Condition dimension label
  cond_true_lbl  = txt_lbl_def$cond_true_lbl,   # Condition true
  cond_false_lbl = txt_lbl_def$cond_false_lbl,  # Condition false
  # 3b. Decision/prediction/predicted condition:
  dec_lbl = txt_lbl_def$dec_lbl,                # Decision dimension label
  dec_pos_lbl  = txt_lbl_def$dec_pos_lbl,       # "Decision positive"
  dec_neg_lbl = txt_lbl_def$dec_neg_lbl,        # "Decision negative"
  # 3c. Accuracy/correspondence of decision to condition:
  acc_lbl = txt_lbl_def$acc_lbl,                # Accuracy dimension label
  dec_cor_lbl = txt_lbl_def$dec_cor_lbl,        # "Decision accurate"
  dec_err_lbl = txt_lbl_def$dec_err_lbl,        # "Decision inaccurate"
  # 4. 4 SDT cases (combinations):
  sdt_lbl = txt_lbl_def$sdt_lbl,  # case/category/cell/SDT
  hi_lbl = txt_lbl_def$hi_lbl,    # hits               = "True positive"
  mi_lbl = txt_lbl_def$mi_lbl,    # misses             = "False negative"
  fa_lbl = txt_lbl_def$fa_lbl,    # false alarms       = "False positive"
  cr_lbl = txt_lbl_def$cr_lbl     # correct rejections = "True negative"
) {

  ## 1. Initialize txt:
  txt <- NULL

  ## 2. Pass arguments to list:
  txt <- list(

    # 1. Scenario:
    scen_lbl = scen_lbl,
    scen_txt = scen_txt,
    scen_src = scen_src,
    scen_apa = scen_apa,
    scen_lng = scen_lng,

    # 2. Population:
    popu_lbl = popu_lbl,
    N_lbl = N_lbl,

    # 3a. Condition:
    cond_lbl = cond_lbl,
    cond_true_lbl  = cond_true_lbl,
    cond_false_lbl = cond_false_lbl,

    # 3b. Decision/prediction/predicted condition:
    dec_lbl = dec_lbl,
    dec_pos_lbl = dec_pos_lbl,
    dec_neg_lbl = dec_neg_lbl,

    # 3c. Accuracy:
    acc_lbl = acc_lbl,
    dec_cor_lbl = dec_cor_lbl,
    dec_err_lbl = dec_err_lbl,

    # 4. Labels for the 4 SDT cases/cells (combinations):
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



## (C) Initialize txt lists to contain current text elements: ---------

#' Basic text elements.
#'
#' \code{txt} is initialized to a list of named elements
#' to define basic scenario titles and labels.
#'
#' All textual elements that specify generic labels and titles of \code{riskyr} scenarios
#' are stored as named elements (of type character) in a list \code{txt}.
#' To change an element, assign a new character object to an existing name.
#'
#' The list \code{txt} is used throughout the \code{riskyr} package
#' unless a scenario defines scenario-specific text labels
#' (when using the \code{\link{riskyr}} function).
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
#' \code{txt} currently contains the following text labels:
#'
#' \enumerate{
#'
#' Scenario information:
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
#' Options: \code{"en"}: English, \code{"de"}: German.
#'
#' Population:
#'
#' \item \code{popu_lbl} A general name describing the current \emph{population}.
#'
#' \item \code{N_lbl} A short label for the current population \code{\link{popu}} or sample.
#'
#' Condition:
#'
#' \item \code{cond_lbl} A general name for the \emph{condition} dimension,
#' or the feature (e.g., some disease) currently considered.
#'
#' \item \code{cond_true_lbl} A short label for the \emph{presence} of the current condition
#' or \code{\link{cond_true}} cases (the condition's true state of being TRUE).
#'
#' \item \code{cond_false_lbl} A short label for the \emph{absence} of the current condition
#' or \code{\link{cond_false}} cases (the condition's true state of being FALSE).
#'
#' Decision:
#'
#' \item \code{dec_lbl} A general name for the \emph{decision} dimension,
#' or the judgment (e.g., some diagnostic test) currently made.
#'
#' \item \code{dec_pos_lbl} A short label for \emph{positive} decisions
#' or \code{\link{dec_pos}} cases (e.g., predicting the presence of the condition).
#'
#' \item \code{dec_neg_lbl} A short label for \emph{negative} decisions
#' or \code{\link{dec_neg}} cases (e.g., predicting the absence of the condition).
#'
#' Accuracy:
#'
#' \item \code{acc_lbl} A general name for the \emph{accuracy} dimension,
#' or the correspondence between the condition currently considered
#' and the decision judgment currently made.
#'
#' \item \code{dec_cor_lbl} A short label for \emph{correct} and \emph{accurate} decisions
#' or \code{\link{dec_cor}} cases (accurate predictions).
#'
#' \item \code{dec_err_lbl} A short label for \emph{incorrect} decisions
#' or \code{\link{dec_err}} cases (erroneous predictions).
#'
#' Cases:
#'
#' \item \code{sdt_lbl} A general name for all 4 \emph{cases/categories/cells}
#' of the 2x2 contingency table (e.g., condition x decision, using SDT).
#'
#' \item \code{hi_lbl} A short label for \emph{hits} or \emph{true positives} \code{\link{hi}}/TP cases
#' (i.e., correct decisions of the presence of the condition, when the condition is actually present).
#'
#' \item \code{mi_lbl} A short label for \emph{misses} or \emph{false negatives} \code{\link{mi}}/FN cases
#' (i.e., incorrect decisions of the absence of the condition when the condition is actually present).
#'
#' \item \code{fa_lbl} A short label for \emph{false alarms} or \emph{false positives} \code{\link{fa}}/FP cases
#' (i.e., incorrect decisions of the presence of the condition when the condition is actually absent).
#'
#' \item \code{cr_lbl} A short label for \emph{correct rejections} or \emph{true negatives} \code{\link{cr}}/TN cases
#' (i.e., a correct decision of the absence of the condition, when the condition is actually absent).
#'
#' }
#'
#' @examples
#' txt           # Show  all current names and elements
#' txt$scen_lbl  # Show the current scenario label (e.g., used in plot titles)
#' txt$scen_lbl <- "My example"  # Set a new scenario title
#'
#' @family lists containing current scenario information
#'
#' @seealso
#' \code{\link{init_txt}} initializes text information;
#' \code{\link{riskyr}} initializes a \code{riskyr} scenario;
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

## txt_org: A copy of original text labels: --------

#' List of original values of text elements.
#'
#' \code{txt_org} is a copy of the initial list of text elements
#' to define all scenario titles and labels.
#'
#' See \code{\link{txt}} for details and default text information.
#'
#' Assign \code{txt <- txt_org} to re-set default text labels.
#'
#' @examples
#' txt_org        # shows original text labels
#' txt_org["hi"]  # shows the original label for hits ("hi")
#' txt_org["hi"] <- "TP" # defines a new label for hits (true positives, TP)
#'
#' @family lists containing current scenario information
#'
#' @seealso
#' \code{\link{txt}} contains current text information;
#' \code{\link{init_txt}} initializes text information;
#' \code{\link{pal}} contains current color information;
#' \code{\link{init_pal}} initializes color information.
#'
#' @export

txt_org <- txt  # copy txt


## txt_TF:  An alternative set of text labels: --------

#' Alternative text labels (TP, FN, FP, TN).
#'
#' \code{txt_TF} is initialized to alternative text labels
#' to define a frequency naming scheme in which
#' (hi, mi, fa, cr) are called (TP, FN, FP, TN).
#'
#' See \code{\link{txt}} for details and default text information.
#'
#' Assign \code{txt <- txt_TF} to use as default text labels.
#'
#' @examples
#' txt_TF        # shows text labels of txt_TF
#' txt_TF["hi"]  # shows the current label for hits ("TP")
#' txt_TF["hi"] <- "hit" # defines a new label for hits (true positives, TP)
#'
#' @family lists containing current scenario information
#'
#' @seealso
#' \code{\link{txt}} contains current text information;
#' \code{\link{init_txt}} initializes text information;
#' \code{\link{pal}} contains current color information;
#' \code{\link{init_pal}} initializes color information.
#'
#' @export

txt_TF <- init_txt(# scen_lbl = "",  # no scenario title
  cond_lbl = "Truth",   cond_true_lbl = "True",   cond_false_lbl = "False",
  dec_lbl = "Test",     dec_pos_lbl = "Positive", dec_neg_lbl = "Negative",
  acc_lbl = "Accuracy", dec_cor_lbl = "Correct",  dec_err_lbl = "Incorrect",
  hi_lbl = "TP", mi_lbl = "FN", fa_lbl = "FP", cr_lbl = "TN")

## Check:
# length(txt_TF) # 21
# txt_TF$sdt_lbl # unchanged from txt_lbl_def
# txt_TF$hi      # "TP", rather than "hi"

## Default text information: --------

## Use txt_TF by default:
txt <- txt_TF

## (*) Done: -------------------------------------

## - name_prob: Add function to look up the prob that corresponds
##   to 2 freq    [2018 08 20].
## - label_freq + label_prob: Added functions to obtain labels
##   of freq and prob (from fname or pname).
## - Rename acc.cor and acc.err to dec_cor and dec_err
##   (to remain consistent with freq names).
## - Add text labels for accuracy/correspondence of decision to condition
##   (acc_lbl, dec_cor_lbl, dec_err_lbl) and for sdt_lbl.
## - Clean up code [2018 08 20].

## (+) ToDo: -------------------------------------

## - Integrate prob_lbl_def into txt (to also include prob labels).
## - Add shorter, abbreviated versions of text elements (to use as labels)?
##   (e.g., hi/TP, mi/FN, fa/FN, cr/TN)
## - Add pre-defined naming schemes (like txt_TF, to accommodate
##   different scenario types).

## eof. ------------------------------------------
