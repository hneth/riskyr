## init_txt.R | riskyR
## 2018 01 20
## -----------------------------------------------
## Define defaults and initialize the
## current set of all text elements (txt):

## -----------------------------------------------
## (A) Defaults for all titles and labels (txt):

txt.def <- list(
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

# length(txt.def) # => 14 text elements

## -----------------------------------------------
## (B) Initialization function for all text
##     elements (all titles and labels):

#' Initialize basic text elements (titles and labels).
#'
#' \code{init_txt} initializes basic text elements
#' (i.e., all titles and labels corresponding to the current scenario
#' and used throughout the \code{riskyr} package).
#'
#' All textual elements that specify titles and details of the current scenario
#' are stored as named elements (of type character) in a list \code{txt}.
#' \code{init_txt} allows changing elements by assigning new character
#' objects to existing names.
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
#' init_txt()          # => returns list of default text elements
#' length(init_txt())  # => 14
#' txt <- init_txt(scen.lbl = "My favorite example", scen.src = "Some stable genius") # => customizes elements of txt
#'
#' @family functions to initialize scenario settings
#'
#' @seealso \code{\link{txt}} for current text values;
#' \code{\link{pal}} for current color settings;
#' \code{\link{num}} for current numeric parameters

init_txt <- function(scen.lbl = txt.def$scen.lbl,  # Scenario title
                     scen.txt = txt.def$scen.txt,  # text with scenario description
                     scen.src = txt.def$scen.src,  # scenario source information
                     popu.lbl = txt.def$popu.lbl,  # target population
                     ## (a) Condition:
                     cond.lbl = txt.def$cond.lbl,              # Condition
                     cond.true.lbl  = txt.def$cond.true.lbl,   # Condition true
                     cond.false.lbl = txt.def$cond.false.lbl,  # Condition false
                     ## (b) Decision/prediction/predicted condition:
                     dec.lbl = txt.def$dec.lbl,              # Decision
                     dec.true.lbl  = txt.def$dec.true.lbl,   # "Decision positive"
                     dec.false.lbl = txt.def$dec.false.lbl,  # "Decision negative"
                     ## (c) Labels for the 4 SDT cases (combinations):
                     sdt.hi.lbl = txt.def$sdt.hi.lbl,  # hits               = "True positive"
                     sdt.mi.lbl = txt.def$sdt.mi.lbl,  # misses             = "False negative"
                     sdt.fa.lbl = txt.def$sdt.fa.lbl,  # false alarms       = "False positive"
                     sdt.cr.lbl = txt.def$sdt.cr.lbl   # correct rejections = "True negative"
                     ) {


  ## 1. Initialize txt:
  txt <- NULL

  ## 2. Pass arguments to list:
  txt <- list(
    scen.lbl = scen.lbl,
    scen.txt = scen.txt,
    scen.src = scen.src,
    popu.lbl = popu.lbl,
    ## (a) Condition:
    cond.lbl = cond.lbl,
    cond.true.lbl  = cond.true.lbl,
    cond.false.lbl = cond.false.lbl,
    ## (b) Decision/prediction/predicted condition:
    dec.lbl = dec.lbl,
    dec.true.lbl  = dec.true.lbl,
    dec.false.lbl = dec.false.lbl,
    ## (c) Labels for the 4 SDT cases (combinations):
    sdt.hi.lbl = sdt.hi.lbl,
    sdt.mi.lbl = sdt.mi.lbl,
    sdt.fa.lbl = sdt.fa.lbl,
    sdt.cr.lbl = sdt.cr.lbl
  )

  ## 3. Return entire list txt:
  return(txt)

}

## Check:
init_txt()          # => returns list of default text elements
length(init_txt())  # => 14
init_txt(scen.lbl = "My favorite example", scen.src = "Some stable genius") # => customizes current text elements

## -----------------------------------------------
## (C) Initialize a list txt to contain
##     all current text elements:

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

txt <- init_txt()

## -----------------------------------------------
## (+) ToDo:

## - add txt to a cus object?

## -----------------------------------------------
## eof.
