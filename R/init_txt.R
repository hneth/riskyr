## init_txt.R | riskyr
## 2018 08 20
## Define defaults and initialize the
## current set of all text elements (txt):
## -----------------------------------------------

## (A) Defaults for all titles and labels (txt): ----------

txt.def <- list(

  # (a) Scenario:
  scen.lbl = "Scenario", # provide scenario name/label in Title Caps (if desired in plot titles)
  scen.txt = "Description of the current scenario in a short paragraph of text. This description may include several sentences.",
  scen.src = "Source information",
  scen.apa = "Source information in APA format",
  scen.lng = "en/de", # language

  # (b) Population:
  popu.lbl = "Population",  # current population label

  # (c) Condition:
  cond.lbl = "Condition",  # Dimension label: "Condition X", "disease", "Breast cancer", "HIV", "Sepsis" etc.
  cond.true.lbl  = "Condition true",   # "has condition", "is affected"
  cond.false.lbl = "Condition false",  # "does not have condition", "is unaffected"

  # (d) Decision/prediction/predicted condition:
  dec.lbl = "Decision",  # Dimension label: "Prediction", "Diagnostic decision", "Mammography", "HIV test"
  dec.pos.lbl  = "Decision positive",  # "Prediction positive", "called", "is judged to have condition", "is classified as affected"
  dec.neg.lbl = "Decision negative",   # "Prediction negative", "not called", "is judged to not have condition", "is classified as unaffected"

  # (e) Accuracy/correspondence of decision to condition:
  acc.lbl = "Accuracy",   # Dimension label: accuracy/correspondence
  dec.cor.lbl = "Accurate decision",    # cor  # accurate decision
  dec.err.lbl = "Inaccurate decision",  # err  # inaccurate decision

  # (f) Labels for the 4 SDT cases/cells in 2x2 contingency table (combinations):
  sdt.lbl = "Case",  # Dimension label: cell/case/SDT 2x2 table
  hi.lbl = "True positive",  # "hit", "true positive", "has condition and is detected/predicted as such", "set(dec POS & cond TRUE)"
  mi.lbl = "False negative", # "miss", "false negative", "omission", "has condition and is NOT detected/predicted as such", "set(dec NEG & cond TRUE)"
  fa.lbl = "False positive", # "false alarm",       "false positive", "set(dec POS & cond FALSE)"
  cr.lbl = "True negative"   # "correct rejection", "true negative",  "set(dec NEG & cond FALSE)"

)

## Check:
# length(txt.def)  # => 20 text elements


## (B) Initialization of all text elements (titles and labels): ----------

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
#' @param acc.lbl A name for judgment \emph{accuracy} (e.g., correspondence of decision to condition).
#' @param dec.cor.lbl A label for \emph{correct} decisions
#' or \code{\link{dec.cor}} cases (e.g., accurately predicting the condition).
#' @param dec.err.lbl A label for \emph{erroneous} decisions
#' or \code{\link{dec.err}} cases (e.g., inaccurately predicting the condition).
#'
#' @param sdt.lbl A name for the case/category/cell in the 2x2 contingency table (SDT: condition x decision).
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
                     ## (c) Accuracy/correspondence of decision to condition:
                     acc.lbl = txt.def$acc.lbl,                # Accuracy
                     dec.cor.lbl = txt.def$dec.cor.lbl,        # "Decision accurate"
                     dec.err.lbl = txt.def$dec.err.lbl,        # "Decision inaccurate"
                     ## (d) Labels for the 4 SDT cases (combinations):
                     sdt.lbl = txt.def$sdt.lbl,  # case/category/cell/SDT
                     hi.lbl = txt.def$hi.lbl,    # hits               = "True positive"
                     mi.lbl = txt.def$mi.lbl,    # misses             = "False negative"
                     fa.lbl = txt.def$fa.lbl,    # false alarms       = "False positive"
                     cr.lbl = txt.def$cr.lbl     # correct rejections = "True negative"
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
{
  init_txt()          # => defines a list of (default) text elements
  length(init_txt())  # => 20
  #
  # # Customizing current text elements:
  # txt <- init_txt(scen.lbl = "US or Them",
  #                 scen.src = "Some stable genius",
  #                 popu.lbl = "We, the people")
}


## (C) Initialize a list txt to contain all current text elements: ---------

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

## Apply:
txt <- init_txt()

## Check:
{
  # txt          # displays all current names and elements
  # txt$scen.lbl # displays the current scenario label (e.g., used in plot titles)
  # txt$scen.lbl <- "My favorite example" # sets a new scenario title
}

## label_freq: Helper function to return label of a known freq ----------

label_freq <- function(fname, ltype = "default"
                       #, freq = freq, txt = txt  # use current lists
                       ) {

  f_lbl <- fname  # initialize to fname (in case of unknown freq)

  if (ltype == "nam") {  # (1) Name of freq:

    # if (fname == "N") { f_lbl <- "N" }           # use "N" as f_lbl
    if (fname == "N") { f_lbl <- txt$popu.lbl }  # use population label as f_lbl

    if (fname == "hi") { f_lbl <- txt$hi.lbl }
    if (fname == "mi") { f_lbl <- txt$mi.lbl }
    if (fname == "fa") { f_lbl <- txt$fa.lbl }
    if (fname == "cr") { f_lbl <- txt$cr.lbl }

    if (fname == "cond.true")  { f_lbl <- txt$cond.true.lbl }
    if (fname == "cond.false") { f_lbl <- txt$cond.false.lbl }

    if (fname == "dec.pos") { f_lbl <- txt$dec.pos.lbl }
    if (fname == "dec.neg") { f_lbl <- txt$dec.neg.lbl }

    if (fname == "dec.cor") { f_lbl <- txt$dec.cor.lbl }
    if (fname == "dec.err") { f_lbl <- txt$dec.err.lbl }


  } else {  # (9) Any other ltype: Basic names as default:

      if (fname == "N") { f_lbl <- "N" }

      if (fname == "hi") { f_lbl <- "hi" }
      if (fname == "mi") { f_lbl <- "mi" }
      if (fname == "fa") { f_lbl <- "fa" }
      if (fname == "cr") { f_lbl <- "cr" }

      if (fname == "cond.true")  { f_lbl <- "cond.true" }
      if (fname == "cond.false") { f_lbl <- "cond.false" }

      if (fname == "dec.pos") { f_lbl <- "dec.pos" }
      if (fname == "dec.neg") { f_lbl <- "dec.neg" }

      if (fname == "dec.cor") { f_lbl <- "dec.cor" }
      if (fname == "dec.err") { f_lbl <- "dec.err" }

  }

  return(f_lbl)  # return label

}

## Check:
# label_freq("cr", ltype = "nam")
# label_freq("cr", ltype = "nix")
# label_freq("dec.err", ltype = "nam")
#
# label_freq("dec.err")        # no ltype specified: use default
# label_freq("unknown fname")  # unknown freq: return fname


## +++ here now +++

## - Add functions to obtain labels of freq and prob (given freq).
## - Determine the prob that corresponds to 2 freq.

## (*) Done: -------------------------------------

## - Rename acc.cor and acc.err to dec.cor and dec.err
##   (to remain consistent with freq names).
## - Add text labels for accuracy/correspondence of decision to condition
##   (acc.lbl, dec.cor.lbl, dec.err.lbl) and for sdt.lbl.
## - Clean up code [2018 08 20].

## (+) ToDo: -------------------------------------

## - Add functions to obtain labels of freq and prob (given freq).
## - Determine the prob that corresponds to 2 freq.
## - Add txt to a cus object?

## eof. ------------------------------------------
