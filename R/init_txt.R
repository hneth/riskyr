## init_txt.R | riskyR
## 2018 01 20
## -----------------------------------------------
## Define and initialize current set of
## custom text elements (txt):

## Initially, txt contains defaults for user inputs.
## -----------------------------------------------

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
#' @param scen.lbl The current scenario title (sometimes in Title Caps).
#' @param scen.txt A longer text description of the current scenario
#' (which may extend over several lines).
#' @param scen.src The source information for the current scenario.
#'
#' @param popu.lbl A brief description of the current target population or sample.
#'
#' @param cond.lbl A name for the \emph{condition} or feature (e.g., some disease) currently considered.
#' @param cond.true.lbl A label for the \emph{presence} of the current condition (the condition's true state of TRUE).
#' @param cond.false.lbl A label for the \emph{absence} of the current condition (the condition's true state of FALSE).
#'
#' @param dec.lbl A name for the \emph{decision} or judgment (e.g., some diagnostic test) currently made.
#' @param dec.true.lbl A label for a \emph{positive} decision (e.g., predicting the presence of the condition).
#' @param dec.false.lbl A label for a \emph{negative} decision (e.g., predicting the absence of the condition).
#'
#' @param sdt.hi.lbl A label for \emph{hits} or \emph{true positives} (i.e., a correct decision
#' of the presence of the condition, when the condition is actually present).
#' @param sdt.mi.lbl A label for \emph{misses} or \emph{false negatives} (i.e., an incorrect decision
#' of the absence of the condition when the condition is actually present).
#' @param sdt.fa.lbl A label for \emph{false alarms} or \emph{false positives} (i.e., an incorrect decision
#' of the presence of the condition when the condition is actually absent).
#' @param sdt.cr.lbl A label for \emph{correct rejections} or \emph{true negatives} (i.e., a correct decision
#' of the absence of the condition, when the condition is actually absent).
#'
#' @examples
#' txt          # displays all current names and elements
#' txt$scen.lbl # displays the current scenario label (e.g., used in plot titles)
#' txt$scen.lbl <- "My favorite example" # defines a new scenario label
#'
#' @family lists containing basic scenario settings
#' @seealso \code{\link{num}} for numeric parameters; \code{\link{pal}} for color settings

## -----------------------------------------------
## Defaults for all titles and labels (txt):

txt <- list(
  scen.lbl = "Scenario Title", # in Title Caps (to print in plot titles)
  scen.txt = "Describe the current scenario in a short paragraph of text here.\nThis description may include several sentences.",
  scen.src = "Describe the source information for this scenario here",

  popu.lbl = "Describe the target population in a few words here",

  ## (a) Condition:
  cond.lbl = "Condition",             # "Condition X", "disease", "Breast cancer", "HIV", "Sepsis" etc.
  cond.true.lbl  = "Condition true",  # "has condition", "is affected"
  cond.false.lbl = "Condition false", # "does not have condition", "is unaffected"

  ## (b) Decision/prediction/predicted condition:
  dec.lbl = "Decision",                # "Prediction", "Diagnostic decision", "Mammography", "HIV test"
  dec.true.lbl  = "Decision positive", # "Prediction positive", "called", "is judged to have condition", "is classified as affected"
  dec.false.lbl = "Decision negative", # "Prediction negative", "not called", "is judged to not have condition", "is classified as unaffected"

  ## (c) Labels for the 4 SDT cases (combinations):
  sdt.hi.lbl = "True positive",  # "hit", "true positive", "has condition and is detected/predicted as such", "set(dec POS & cond TRUE)"
  sdt.mi.lbl = "False negative", # "miss", "false negative", "omission", "has condition and is NOT detected/predicted as such", "set(dec NEG & cond TRUE)"
  sdt.fa.lbl = "False positive", # "false alarm",       "false positive", "set(dec POS & cond FALSE)"
  sdt.cr.lbl = "True negative"   # "correct rejection", "true negative",  "set(dec NEG & cond FALSE)"
  )

## Check:
# txt
# length(txt)

## -----------------------------------------------
## (+) ToDo:

## - add txt to a cus object?
## - make txt user-customizable!

## -----------------------------------------------
## eof.
