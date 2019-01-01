## comp_popu.R | riskyr
## 2018 12 20
## Compute a population (popu) as 3 x N data frame
## based on only the 4 essential frequencies:
##  [a. the current N from num (not needed)]
##   b. the 4 essential frequencies of freq (hi mi fa cr)
##   c. the current text labels of txt
## -----------------------------------------------

## (1) Compute current population (popu): --------

## comp_popu Documentation: --------

#' Compute a population table from frequencies.
#'
#' \code{comp_popu} is a function that computes
#' a table \code{\link{popu}} (as an R data frame)
#' from the current frequency information
#' (contained in \code{\link{freq}}).
#'
#' \code{comp_popu} also uses the current text settings
#' contained in \code{\link{txt}}.
#'
#' A visualization of the current population
#' contained in \code{\link{popu}}
#' is provided by \code{plot_icon}.
#'
#' @format An object of class \code{data.frame}
#' with \code{\link{N}} rows
#' and 3 columns (\code{"Truth", "Decision", "SDT"}).
#'
#' @return A data frame \code{popu}
#' containing \code{\link{N}} rows (individual cases)
#' and 3 columns (\code{"Truth", "Decision", "SDT"})
#' encoded as ordered factors
#' (with 2, 2, and 4 levels, respectively).
#'
#' @param hi The number of hits \code{\link{hi}} (or true positives).
#' @param mi The number of misses \code{\link{mi}} (or false negatives).
#' @param fa The number of false alarms \code{\link{fa}} (or false positives).
#' @param cr The number of correct rejections \code{\link{cr}} (or true negatives).
#'
#' @param cond_lbl Text label for condition dimension ("by cd" perspective).
#' @param cond_true_lbl Text label for \code{\link{cond_true}} cases.
#' @param cond_false_lbl Text label for \code{\link{cond_false}} cases.
#'
#' @param dec_lbl Text label for decision dimension ("by dc" perspective).
#' @param dec_pos_lbl Text label for \code{\link{dec_pos}} cases.
#' @param dec_neg_lbl Text label for \code{\link{dec_neg}} cases.
#'
#' @param sdt_lbl Text label for 4 cases/combinations (SDT classifications).
#' @param hi_lbl Text label for \code{\link{hi}} cases.
#' @param mi_lbl Text label for \code{\link{mi}} cases.
#' @param fa_lbl Text label for \code{\link{fa}} cases.
#' @param cr_lbl Text label for \code{\link{cr}} cases.
#'
#' @examples
#' popu <- comp_popu()  # => initializes popu (with current values of freq and txt)
#' dim(popu)            # => N x 3
#' head(popu)
#'
#' # (A) Diagnostic/screening scenario (using default labels):
#' comp_popu(hi = 4, mi = 1, fa = 2, cr = 3)  # => computes a table of N = 10 cases.
#'
#' # (B) Intervention/treatment scenario:
#' comp_popu(hi = 3, mi = 2, fa = 1, cr = 4,
#'           cond_lbl = "Treatment", cond_true_lbl = "pill", cond_false_lbl = "placebo",
#'           dec_lbl = "Health status", dec_pos_lbl = "healthy", dec_neg_lbl = "sick")
#'
#' # (C) Prevention scenario (e.g., vaccination):
#' comp_popu(hi = 3, mi = 2, fa = 1, cr = 4,
#'           cond_lbl = "Vaccination", cond_true_lbl = "yes", cond_false_lbl = "no",
#'           dec_lbl = "Disease", dec_pos_lbl = "no flu", dec_neg_lbl = "flu")
#'
#' @family functions computing frequencies
#'
#' @seealso
#' the corresponding data frame \code{\link{popu}};
#' \code{\link{read_popu}} interprets a data frame as a riskyr scenario;
#' \code{\link{num}} for basic numeric parameters;
#' \code{\link{freq}} for current frequency information;
#' \code{\link{txt}} for current text settings;
#' \code{\link{pal}} for current color settings.
#'
#' @export

## comp_popu Definition: --------

comp_popu <- function(hi = freq$hi,  # 4 essential frequencies
                      mi = freq$mi,
                      fa = freq$fa,
                      cr = freq$cr,
                      ## text labels (from txt):
                      cond_lbl = txt$cond_lbl, cond_true_lbl = txt$cond_true_lbl, cond_false_lbl = txt$cond_false_lbl,
                      dec_lbl = txt$dec_lbl, dec_pos_lbl = txt$dec_pos_lbl, dec_neg_lbl = txt$dec_neg_lbl,
                      sdt_lbl = txt$sdt_lbl, hi_lbl = txt$hi_lbl, mi_lbl = txt$mi_lbl, fa_lbl = txt$fa_lbl, cr_lbl = txt$cr_lbl) {

  ## (1) Compute combined frequencies from 4 essential frequencies:
  # cond_true  <- (hi + fa)
  # cond_false <- (mi + cr)

  # dec_pos <- (hi + mi)
  # dec_neg <- (mi + cr)

  ## (2) Define and initialize 3 vectors of length N:
  ## (a) Truth (= true condition or state):
  # truth <- c(rep(TRUE, cond_true), rep(FALSE, cond_false))  # a. using combined freq
  truth <- c(rep(TRUE, hi), rep(TRUE, mi),   # = cond true  # b. using 4 essential freq
             rep(FALSE, fa), rep(FALSE, cr)) # = cond_false

  ## (b) Decision (ordered by ACTUAL truth values of condition):
  decision <- c(rep(TRUE, hi), rep(FALSE, mi),
                rep(TRUE, fa), rep(FALSE, cr))

  ## (c) SDT (status decision/truth):
  sdt <- c(rep("hi", hi), rep("mi", mi),
           rep("fa", fa), rep("cr", cr))

  ## (2) Coerce 3 vectors into ordered factors:
  ## (a) Condition (truth):
  truth <- factor(truth,
                  levels = c(TRUE, FALSE),                   # as Booleans
                  labels = c(cond_true_lbl, cond_false_lbl), # explicit labels: "true" vs. "false"
                  ordered = TRUE)

  ## (b) Decision (ordered by ACTUAL truth values of condition):
  decision <- factor(decision,
                     levels = c(TRUE, FALSE),              # also as Booleans, NOT: (-1, +1) or (0, 1)
                     labels = c(dec_pos_lbl, dec_neg_lbl), # explicit labels: "pos" vs. "neg"
                     ordered = TRUE)

  ## (c) SDT (status decision/truth):
  sdt <- factor(sdt,
                levels = c("hi", "mi", "fa", "cr"),         # as character: 4 cases
                labels = c(hi_lbl, mi_lbl, fa_lbl, cr_lbl), # explicit labels (e.g., "TP", "FN", "FP", "TN")
                # labels = c("hi", "mi", "fa", "cr"),       # implicit labels
                ordered = TRUE)

  ## (3) Combine 3 vectors in a population data frame pop:
  popu <- data.frame(truth = truth,
                     decision = decision,
                     sdt = sdt)

  ## (4) Name variables (by labels of dimensions):
  # names(popu) <- c("Truth", "Decision", "STD")
  names(popu) <- c(cond_lbl, dec_lbl, sdt_lbl)

  ## (4) Return the entire data frame pop:
  return(popu)

}

## Check: --------
# popu <- comp_popu()  # => initializes popu (with current values of freq and txt)
# dim(popu)            # => N x 3
# head(popu)
#
# # (A) Diagnostic/screening scenario (using default labels):
# comp_popu(hi = 4, mi = 1, fa = 2, cr = 3)  # => computes a table of N = 10 cases.
#
# # (B) Intervention/treatment scenario:
# comp_popu(hi = 3, mi = 2, fa = 1, cr = 4,
#           cond_lbl = "Treatment", cond_true_lbl = "pill", cond_false_lbl = "placebo",
#           dec_lbl = "Health status", dec_pos_lbl = "healthy", dec_neg_lbl = "sick")
#
# # (C) Prevention scenario (e.g., vaccination):
# comp_popu(hi = 3, mi = 2, fa = 1, cr = 4,
#           cond_lbl = "Vaccination", cond_true_lbl = "yes", cond_false_lbl = "no",
#           dec_lbl = "Disease", dec_pos_lbl = "no flu", dec_neg_lbl = "flu")


## (2) Apply to initialize popu (as data frame): ----------

## popu Documentation: --------

#' A population table based on current frequencies.
#'
#' \code{popu} is an R data frame that is computed
#' by \code{\link{comp_popu}} from the current
#' frequency information (contained in \code{\link{freq}}).
#' Each individual is represented as a row;
#' columns represent the individual's
#' condition (\code{TRUE} or \code{FALSE}),
#' a corresponding decision
#' (also encoded as \code{TRUE} = positive or \code{FALSE} = negative),
#' and its classification (in SDT terms) as either
#' true positive (an individual hit \code{\link{hi}}),
#' false negative (an individual miss \code{\link{mi}}),
#' false positive (an individual false alarm \code{\link{fa}}), or
#' true negative (an individual correct rejection \code{\link{cr}}).
#'
#' #' \code{popu} is initialized to \code{NULL}
#' and needs to be computed by calling \code{\link{comp_popu}}
#' with current parameter settings.
#'
#' \code{comp_popu} uses the current text information
#' contained in \code{\link{txt}} to define
#' the labels of conditions, decisions, and
#' SDT classifications.
#'
#' A visualization of the current population
#' \code{popu} is provided by \code{\link{plot_icons}}.
#'
#' @return A data frame \code{popu}
#' containing \code{\link{N}} rows (individual cases)
#' and 3 columns (\code{"Truth", "Decision", "SDT"})
#' encoded as ordered factors
#' (with 2, 2, and 4 levels, respectively).
#'
#' @examples
#' popu <- comp_popu()  # => initializes popu with current values of freq and txt
#' dim(popu)            # => N x 3
#' head(popu)           # => shows head of data frame
#'
#' @seealso
#' the corresponding generating function \code{\link{comp_popu}};
#' \code{\link{read_popu}} interprets a data frame as a riskyr scenario;
#' \code{\link{num}} for basic numeric parameters;
#' \code{\link{freq}} for current frequency information;
#' \code{\link{txt}} for current text settings.
#'
#' @export

## popu Definition: --------

## NOT RUN for random num (as it would always change dimensions):
# popu <- comp_popu() # set to current global parameters

## INSTEAD:
popu <- NULL          # initialize


## Check: --------
# popu
# dim(popu)
# head(popu)
# tail(popu)


## (*) Done: -----------

## - changed on 2018 01 25 to use only
##   the 4 essential frequencies of freq (hi mi fa cr)
## - Called "popu" rather than "pop" as it is an output,
##   rather than an input!
## - Clean up code         [2018 09 02].
## - Update variable names [2018 11 21].

## (+) ToDo: ----------

## - ...

## eof. ------------------------------------------
