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
  dec.cor.lbl = "Decision correct",  # cor  # accurate decision
  dec.err.lbl = "Decision error",    # err  # inaccurate decision

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
# txt          # displays all current names and elements
# txt$scen.lbl # displays the current scenario label (e.g., used in plot titles)
# txt$scen.lbl <- "My favorite example" # sets a new scenario title


## label_freq: Helper function to get label of a known frequency in freq ----------
label_freq <- function(fname,
                       ltype = "default"  # label type: "nam", "num", "namnum", "default".
                       #, freq = freq, txt = txt  # use current lists
) {

  ## Initialize:
  f_lbl <- fname # initialize (in case of unknown freq)
  f_val  <- NA
  f_type <- NA

  ## (1) Determine the frequency value of freq corresponding to fname:

  if (ltype != "nam") {

    if (tolower(fname) %in% tolower(names(freq))) { # if fname corresponds to named frequency in freq:

      # f_lbl <- fname  # initialize to fname

      # Derive current value corresponding to freq:
      ix <- which(tolower(names(freq)) == tolower(fname))  # index in freq

      # Value of frequency in freq:
      f_val <- freq[ix]

      # Type of frequency:
      # f_type <- comp_freq_type(fname)  # see helper function (defined in init_freq_num.R)

    }
  }

  ## (2) Compose f_lbl based on ltype:

  if (ltype == "num" || ltype == "val" ){

    # (a) Value:
    f_lbl <- as.character(f_val)

  } else if ( ltype == "namnum" || ltype == "namval" ||
              ltype == "full" || ltype == "all" ){

    ## (b) Name AND value of freq:

    # if (tolower(fname) == "n") { f_lbl <- "N" }         # use "N" as f_lbl
    if (tolower(fname) == "n") { f_lbl <- txt$popu.lbl }  # use population label as f_lbl

    if (tolower(fname) == "hi") { f_lbl <- txt$hi.lbl }
    if (tolower(fname) == "mi") { f_lbl <- txt$mi.lbl }
    if (tolower(fname) == "fa") { f_lbl <- txt$fa.lbl }
    if (tolower(fname) == "cr") { f_lbl <- txt$cr.lbl }

    if (tolower(fname) == "cond.true")  { f_lbl <- txt$cond.true.lbl }
    if (tolower(fname) == "cond.false") { f_lbl <- txt$cond.false.lbl }

    if (tolower(fname) == "dec.pos") { f_lbl <- txt$dec.pos.lbl }
    if (tolower(fname) == "dec.neg") { f_lbl <- txt$dec.neg.lbl }

    if (tolower(fname) == "dec.cor") { f_lbl <- txt$dec.cor.lbl }
    if (tolower(fname) == "dec.err") { f_lbl <- txt$dec.err.lbl }

    # Combine f_lbl with f_val (from above):
    f_lbl <- paste0(f_lbl, " = ", as.character(f_val))

  } else if (ltype == "nam") {

    ## (c) ONLY the name of freq:

    # if (tolower(fname) == "n") { f_lbl <- "N" }         # use "N" as f_lbl
    if (tolower(fname) == "n") { f_lbl <- txt$popu.lbl }  # use population label as f_lbl

    if (tolower(fname) == "hi") { f_lbl <- txt$hi.lbl }
    if (tolower(fname) == "mi") { f_lbl <- txt$mi.lbl }
    if (tolower(fname) == "fa") { f_lbl <- txt$fa.lbl }
    if (tolower(fname) == "cr") { f_lbl <- txt$cr.lbl }

    if (tolower(fname) == "cond.true")  { f_lbl <- txt$cond.true.lbl }
    if (tolower(fname) == "cond.false") { f_lbl <- txt$cond.false.lbl }

    if (tolower(fname) == "dec.pos") { f_lbl <- txt$dec.pos.lbl }
    if (tolower(fname) == "dec.neg") { f_lbl <- txt$dec.neg.lbl }

    if (tolower(fname) == "dec.cor") { f_lbl <- txt$dec.cor.lbl }
    if (tolower(fname) == "dec.err") { f_lbl <- txt$dec.err.lbl }

  } else {  ## "default":

    ## (d) Any other ltype: Use basic fname + f_val as default:

    f_lbl <- paste0(fname, " = ", as.character(f_val))

  }

  ## (3) Split/re-format long f_lbl into 2 lines of text:
  nchar_max <- 99  # criterium for f_lbl being too long (currently fixed)

  # if f_lbl is too long and it contains a " = ":
  if ((nchar(f_lbl) > nchar_max) && any(grep(pattern = " = ", x = f_lbl))) {

    # Split into 2 parts:
    lbl_parts <- unlist(strsplit(f_lbl, split = " = "))
    lbl_part1 <- lbl_parts[1]  # 1st part of f_lbl
    lbl_part2 <- lbl_parts[2]  # 2nd part of f_lbl

    f_lbl <- paste0(lbl_part1, ":\n", lbl_part2)  # Put into 2 lines.
  }

  ## (4) Return f_lbl:
  return(f_lbl)

}

# ## Check:
# label_freq("cr", ltype = "num")
# label_freq("cr", ltype = "nam")
# label_freq("cr", ltype = "namnum")
# label_freq("cr", ltype = "nix")  # default ltype
# label_freq("dec.err", ltype = "all")
# label_freq("dec.err")        # no ltype specified: use default
# label_freq("unknown fname")  # unknown freq: return fname


## label_prob: Create a label for a known probability in prob ----------
label_prob <- function(pname,
                       ltype = "default"  # label type: "nam", "num", "namnum", "default".
                       #, prob = prob, accu = accu, txt = txt  # use current lists
) {

  ## Initialize:
  p_lbl  <- pname # initialize (in case of unknown prob)
  p_val  <- NA
  p_type <- NA

  ## Currently fixed parameters:
  n_dec <- 1  # number of decimals to round percentage to.

  ## (1) Determine the probability value p_val of prob corresponding to pname:
  if (ltype != "nam") {

    if (tolower(pname) %in% tolower(names(prob))) { # if pname corresponds to named prob in prob:

      # p_lbl <- pname  # initialize to pname

      # Derive current value corresponding to prob:
      ix <- which(tolower(names(prob)) == tolower(pname))  # index in prob

      # Value of probability in prob:
      p_val <- prob[ix]

      # Type of probability:
      # p_type <- comp_prob_type(pname)  # toDo: helper function (to be defined in init_prob_num.R)

    }

    # Special cases:
    if (tolower(pname) == "cprev") {  # if complement of prevalence:
      p_val <- (1 - prob$prev)
    }

    if (tolower(pname) == "cppod" || tolower(pname) == "pned") {  # if complement of ppod:
      p_val <- (1 - ppod)
    }

    # Accuracy (as probability):
    if (tolower(pname) == "acc") { p_val <- accu$acc }
    if (tolower(pname) == "cor") { p_val <- accu$acc }
    if (tolower(pname) == "err") { p_val <- (1 - accu$acc) }

    # Ensure that p_val is numeric:
    p_val <- as.numeric(p_val)

  }

  ## (2) Compose p_lbl based on ltype:

  if (ltype == "num" || ltype == "val" ){

    # (a) Value only:
    if (is_prob(p_val)) {
      # Label p_val (ONLY) as a percentage:
      p_lbl <- paste0(as.character(as_pc(p_val, n.digits = n_dec)), "%")
    } else {
      p_lbl <- paste0(as.character(p_val))  # Label p_val as is (as number)
    }

  } else if ( ltype == "namnum" || ltype == "namval" ||
              ltype == "full" || ltype == "all" ){

    ## (b) Name AND value of prob:

    if (tolower(pname) == "prev") { p_lbl <- "Prevalence" }
    if (tolower(pname) == "cprev") { p_lbl <- "1 - prevalence" }

    if (tolower(pname) == "sens") { p_lbl <- "Sensitivity" }
    if (tolower(pname) == "spec") { p_lbl <- "Specificity" }

    if (tolower(pname) == "mirt") { p_lbl <- "Miss rate" }
    if (tolower(pname) == "fart") { p_lbl <- "False alarm rate" }

    if (tolower(pname) == "ppod") { p_lbl <- "Proportion positive" }
    if (tolower(pname) == "cppod"){ p_lbl <- "Proportion negative" }
    if (tolower(pname) == "pned") { p_lbl <- "Proportion negative" }

    if (tolower(pname) == "ppv") { p_lbl <- "Positive predictive value" }
    if (tolower(pname) == "npv") { p_lbl <- "Negative predictive value" }

    if (tolower(pname) == "fdr") { p_lbl <- "False detection rate" }
    if (tolower(pname) == "for") { p_lbl <- "False omission rate" }

    # Accuracy (as probability):
    if (tolower(pname) == "acc") { p_lbl <- txt$dec.cor.lbl }
    if (tolower(pname) == "cor") { p_lbl <- "Rate correct" }
    if (tolower(pname) == "err") { p_lbl <- "Rate incorrect" }

    # Combine p_lbl (NOT pname) with p_val (from above):
    if (is_prob(p_val)) {
      # Label p_val as a percentage:
      p_lbl <- paste0(p_lbl, " = ", as.character(as_pc(p_val, n.digits = n_dec)), "%")
    } else {
      p_lbl <- paste0(p_lbl, " = ", as.character(p_val))  # Label p_val as is (as number)
    }

  } else if (ltype == "nam") {

    ## (c) ONLY the name of prob:

    if (tolower(pname) == "prev") { p_lbl <- "Prevalence" }
    if (tolower(pname) == "cprev") { p_lbl <- "1 - prevalence" }

    if (tolower(pname) == "sens") { p_lbl <- "Sensitivity" }
    if (tolower(pname) == "spec") { p_lbl <- "Specificity" }

    if (tolower(pname) == "mirt") { p_lbl <- "Miss rate" }
    if (tolower(pname) == "fart") { p_lbl <- "False alarm rate" }

    if (tolower(pname) == "ppod") { p_lbl <- "Proportion positive" }
    if (tolower(pname) == "cppod"){ p_lbl <- "Proportion negative" }
    if (tolower(pname) == "pned") { p_lbl <- "Proportion negative" }

    if (tolower(pname) == "ppv") { p_lbl <- "Positive predictive value" }
    if (tolower(pname) == "npv") { p_lbl <- "Negative predictive value" }

    if (tolower(pname) == "fdr") { p_lbl <- "False detection rate" }
    if (tolower(pname) == "for") { p_lbl <- "False omission rate" }

    # Accuracy (as probability):
    if (tolower(pname) == "acc") { p_lbl <- txt$dec.cor.lbl }
    if (tolower(pname) == "cor") { p_lbl <- "Rate correct" }
    if (tolower(pname) == "err") { p_lbl <- "Rate incorrect" }

  } else {  ## "default":

    ## (d) Any other ltype: Use basic pname + p_val as default:

    # Special cases: Set pname to some default value:

    if (tolower(pname) == "cprev") {  # if complement of prevalence:
      pname <- "(1 - prev)"           # custom basic name
    }
    if (tolower(pname) == "cppod" || tolower(pname) == "pned") {  # if complement of ppod:
      pname <- "(1 - ppod)"           # custom basic name
    }

    # Accuracy (as probability):
    if (tolower(pname) == "acc") { pname <- "acc" }
    if (tolower(pname) == "cor") { pname <- "cor" }
    if (tolower(pname) == "err") { pname <- "err" }

    # print(p_val)
    # is.numeric(p_val)

    # Combine pname (NOT p_lbl) with p_val (from above):
    if (is_prob(p_val)) {
      # Label p_val as a percentage:
      p_lbl <- paste0(pname, " = ", as.character(as_pc(p_val, n.digits = n_dec)), "%")
    } else {
      p_lbl <- paste0(pname, " = ", as.character(p_val))  # Label p_val as is (as number)
    }

  }

  ## (3) Split/re-format long p_lbl into 2 lines of text:
  nchar_max <- 99  # criterium for p_lbl being too long (currently fixed)

  # if p_lbl is too long and it contains a " = ":
  if ((nchar(p_lbl) > nchar_max) && any(grep(pattern = " = ", x = p_lbl))) {

    # Split into 2 parts:
    lbl_parts <- unlist(strsplit(p_lbl, split = " = "))
    lbl_part1 <- lbl_parts[1]  # 1st part of p_lbl
    lbl_part2 <- lbl_parts[2]  # 2nd part of p_lbl

    p_lbl <- paste0(lbl_part1, ":\n", lbl_part2)  # Put into 2 lines.
  }

  ## (4) Return p_lbl:
  return(p_lbl)

}

## Check:
# label_prob("prev", ltype = "default")
# label_prob("sens", ltype = "nam")
# label_prob("spec", ltype = "num")
# label_prob("fart", ltype = "namnum")
# label_prob("PPV", ltype = "namnum")
# label_prob("NPV", ltype = "namnum")
## Special cases:
# label_prob("cprev", ltype = "default")  # complement to prev
# label_prob("cprev", ltype = "nam")      # complement to prev
# label_prob("cprev", ltype = "namnum")   # complement to prev
# label_prob("cppod", ltype = "default")
# label_prob("pned", ltype = "namnum")
# label_prob("acc", ltype = "default")
# label_prob("cor", ltype = "nam")
# label_prob("err", ltype = "namnum")
# label_prob("unknown pname")  # unknown prob: return pname


## name_prob: Determine the (name of the) prob that links 2 freq: ---------

name_prob <- function(freq1, freq2) {

  # (0) Prepare:
  pname <- NA  # initialize

  freq1 <- tolower(freq1)  # all lowercase
  freq2 <- tolower(freq2)

  # (1) by condition (bc):

  if ( (freq1 == "n" & freq2 == "cond.true") ||
       (freq2 == "n" & freq1 == "cond.true") ) { pname <- "prev" }
  if ( (freq1 == "n" & freq2 == "cond.false") ||
       (freq2 == "n" & freq1 == "cond.false") ) { pname <- "cprev" }

  if ( (freq1 == "hi" & freq2 == "cond.true") ||
       (freq2 == "hi" & freq1 == "cond.true") ) { pname <- "sens" }
  if ( (freq1 == "mi" & freq2 == "cond.true") ||
       (freq2 == "mi" & freq1 == "cond.true") ) { pname <- "mirt" }

  if ( (freq1 == "cr" & freq2 == "cond.false") ||
       (freq2 == "cr" & freq1 == "cond.false") ) { pname <- "spec" }
  if ( (freq1 == "fa" & freq2 == "cond.false") ||
       (freq2 == "fa" & freq1 == "cond.false") ) { pname <- "fart" }

  # (2) by decision (dc):

  if ( (freq1 == "n" & freq2 == "dec.pos") ||
       (freq2 == "n" & freq1 == "dec.pos") ) { pname <- "ppod" }
  if ( (freq1 == "n" & freq2 == "dec.neg") ||
       (freq2 == "n" & freq1 == "dec.neg") ) { pname <- "cppod" } # aka. "pned"

  if ( (freq1 == "hi" & freq2 == "dec.pos") ||
       (freq2 == "hi" & freq1 == "dec.pos") ) { pname <- "PPV" }
  if ( (freq1 == "fa" & freq2 == "dec.pos") ||
       (freq2 == "fa" & freq1 == "dec.pos") ) { pname <- "FDR" }

  if ( (freq1 == "cr" & freq2 == "dec.neg") ||
       (freq2 == "cr" & freq1 == "dec.neg") ) { pname <- "NPV" }
  if ( (freq1 == "mi" & freq2 == "dec.neg") ||
       (freq2 == "mi" & freq1 == "dec.neg") ) { pname <- "FOR" }

  # (3) by accuracy/correspondence (ac):

  if ( (freq1 == "n" & freq2 == "dec.cor") ||
       (freq2 == "n" & freq1 == "dec.cor") ) { pname <- "acc" } # aka. "cor"
  if ( (freq1 == "n" & freq2 == "dec.err") ||
       (freq2 == "n" & freq1 == "dec.err") ) { pname <- "err" } # error rate

  # Note: No prob for links between dec.cor OR dec.err and
  #       4 SDT cases (hi, mi, fa, cr).

  # (4) Return:
  return(pname)

}

## Check:
# name_prob("no", "nix")       # => NA
# name_prob("N", "cond.true")  # => "prev"
# name_prob("cond.false", "N") # => "cprev"
#
# name_prob("N", "dec.neg")
# name_prob("dec.pos", "hi")
# name_prob("dec.neg", "cr")
# name_prob("dec.neg", "mi")
#
# name_prob("dec.cor", "N")
# name_prob("dec.err", "N")
#
# label_prob(pname = name_prob("fa", "cond.false"), ltype = "default")
# label_prob(pname = name_prob("hi", "dec.pos"), ltype = "namnum")
# label_prob(pname = name_prob("N", "dec.err"), ltype = "namnum")


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

## - Add txt to a cus object?

## eof. ------------------------------------------
