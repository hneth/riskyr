## init_txt.R | riskyr
## 2018 08 15
## -----------------------------------------------
## Define defaults and initialize the
## current set of all text elements (txt):

## -----------------------------------------------
## (A) Defaults for all titles and labels (txt):

txt.def <- list(
  scen.lbl = "Scenario", # provide scenario name/label in Title Caps (if desired in plot titles)
  scen.txt = "Description of the current scenario in a short paragraph of text. This description may include several sentences.",
  scen.src = "Source information",
  scen.apa = "Source information in APA format",
  scen.lng = "en/de", # language

  popu.lbl = "Current population",

  ## (a) Condition:
  cond.lbl = "Condition",  # Dimension label: "Condition X", "disease", "Breast cancer", "HIV", "Sepsis" etc.
  cond.true.lbl  = "Condition true",   # "has condition", "is affected"
  cond.false.lbl = "Condition false",  # "does not have condition", "is unaffected"

  ## (b) Decision/prediction/predicted condition:
  dec.lbl = "Decision",  # Dimension label: "Prediction", "Diagnostic decision", "Mammography", "HIV test"
  dec.pos.lbl  = "Decision positive",  # "Prediction positive", "called", "is judged to have condition", "is classified as affected"
  dec.neg.lbl = "Decision negative",   # "Prediction negative", "not called", "is judged to not have condition", "is classified as unaffected"

  ## (c) Accuracy/correspondence:
  acc.lbl = "Accuracy",  # accuracy  # Dimension label: accuracy/correspondence
  dec.cor.lbl = "accurate",    # cor  # accurate decision
  dec.err.lbl = "inaccurate",  # err  # inaccurate decision

  ## (d) Labels for the 4 SDT cases/cells in 2x2 contingency table (combinations):
  sdt.lbl = "Case",  # Dimension label: cell/case/SDT 2x2 table
  hi.lbl = "True positive",  # "hit", "true positive", "has condition and is detected/predicted as such", "set(dec POS & cond TRUE)"
  mi.lbl = "False negative", # "miss", "false negative", "omission", "has condition and is NOT detected/predicted as such", "set(dec NEG & cond TRUE)"
  fa.lbl = "False positive", # "false alarm",       "false positive", "set(dec POS & cond FALSE)"
  cr.lbl = "True negative"   # "correct rejection", "true negative",  "set(dec NEG & cond FALSE)"
)

# length(txt.def)  # => 20 text elements

## -----------------------------------------------
## (B) Initialization function for all text
##     elements (all titles and labels):

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
#' Options: \code{"en"}...English, \code{"de"}... German.
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
#' @examples
#' init_txt()          # => defines a list of (default) text elements
#' length(init_txt())  # => 16
#'
#' # Customizing current text elements:
#' txt <- init_txt(scen.lbl = "US or Them",
#'                 scen.src = "Some stable genius",
#'                 popu.lbl = "We, the people")
#'
#' @family functions initializing scenario information
#'
#' @seealso
#' \code{\link{txt}} for current text settings;
#' \code{\link{pal}} for current color settings;
#' \code{\link{num}} for basic numeric parameters
#'
#' @export
#'

init_txt <- function(scen.lbl = txt.def$scen.lbl,  # Scenario title
                     scen.txt = txt.def$scen.txt,  # text with scenario description
                     scen.src = txt.def$scen.src,  # scenario source
                     scen.apa = txt.def$scen.apa,  # scenario source in APA format
                     scen.lng = txt.def$scen.lng,  # language
                     popu.lbl = txt.def$popu.lbl,  # target population
                     ## (a) Condition:
                     cond.lbl = txt.def$cond.lbl,              # Condition
                     cond.true.lbl  = txt.def$cond.true.lbl,   # Condition true
                     cond.false.lbl = txt.def$cond.false.lbl,  # Condition false
                     ## (b) Decision/prediction/predicted condition:
                     dec.lbl = txt.def$dec.lbl,                # Decision
                     dec.pos.lbl  = txt.def$dec.pos.lbl,       # "Decision positive"
                     dec.neg.lbl = txt.def$dec.neg.lbl,        # "Decision negative"
                     ## (c) Labels for the 4 SDT cases (combinations):
                     hi.lbl = txt.def$hi.lbl,  # hits               = "True positive"
                     mi.lbl = txt.def$mi.lbl,  # misses             = "False negative"
                     fa.lbl = txt.def$fa.lbl,  # false alarms       = "False positive"
                     cr.lbl = txt.def$cr.lbl   # correct rejections = "True negative"
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
    ## Population:
    popu.lbl = popu.lbl,
    ## (a) Condition:
    cond.lbl = cond.lbl,
    cond.true.lbl  = cond.true.lbl,
    cond.false.lbl = cond.false.lbl,
    ## (b) Decision/prediction/predicted condition:
    dec.lbl = dec.lbl,
    dec.pos.lbl = dec.pos.lbl,
    dec.neg.lbl = dec.neg.lbl,
    ## (c) Labels for the 4 SDT cases (combinations):
    hi.lbl = hi.lbl,
    mi.lbl = mi.lbl,
    fa.lbl = fa.lbl,
    cr.lbl = cr.lbl
  )

  ## 3. Return entire list txt:
  return(txt)

}

## Check:
{
  # init_txt()          # => defines a list of (default) text elements
  # length(init_txt())  # => 16
  #
  # # Customizing current text elements:
  # txt <- init_txt(scen.lbl = "US or Them",
  #                 scen.src = "Some stable genius",
  #                 popu.lbl = "We, the people")
}

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
#' txt           # => show  all current names and elements
#' txt$scen.lbl  # => show the current scenario label (e.g., used in plot titles)
#' txt$scen.lbl <- "My favorite example"  # => set a new scenario title
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
#'

## Apply:
txt <- init_txt()

## Check:
{
  # txt          # displays all current names and elements
  # txt$scen.lbl # displays the current scenario label (e.g., used in plot titles)
  # txt$scen.lbl <- "My favorite example" # sets a new scenario title
}

## -----------------------------------------------
## (+) ToDo:

## - add labels for accuracy/correspondence dimension and subgroups!
## - add txt to a cus object?

## -----------------------------------------------
## eof.
