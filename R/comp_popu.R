## comp_popu.R | riskyR
## 2018 01 21
## -----------------------------------------------
## Compute a population (popu) as 3 x N data frame
## based on only the necessary parameters of:
##  [a. the current N from num (not needed)]
##   b. the current frequencies of freq
##   c. the current text labels of txt

## Notes:
## - Called "popu" rather than "pop" as it is an output,
##   rather than an input!
## - Always use num (essential) rather than env (NON-essential)!

## -----------------------------------------------
## (1) Compute current population (popu):

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
#' is provided by \code{\link{plot_iconarray}}.
#'
#' @return A data frame \code{popu}
#' containing \code{\link{N}} rows (individual cases)
#' and 3 columns (\code{"Truth", "Decision", "SDT"}).
#'
#' @examples
#' popu <- comp_popu()  # => initializes popu with current values of freq and txt
#' dim(popu)            # => N x 3
#' head(popu)           # => shows head of data frame
#'
#' @seealso
#' The corresponding data frame \code{\link{popu}};
#' \code{\link{num}} for basic numeric parameters;
#' \code{\link{txt}} for current text settings;
#' \code{\link{pal}} for current color settings

comp_popu <- function(## N = num$N, ## (not needed)
  ## frequencies from freq:
  n.true = freq$cond.true, n.false = freq$cond.false,
  n.hi = freq$hi, n.mi = freq$mi, n.fa = freq$fa, n.cr = freq$cr,
  ## text labels from txt:
  cond.true.lbl = txt$cond.true.lbl, cond.false.lbl = txt$cond.false.lbl,
  dec.true.lbl = txt$dec.true.lbl, dec.false.lbl = txt$dec.false.lbl,
  sdt.hi.lbl = txt$sdt.hi.lbl, sdt.mi.lbl = txt$sdt.mi.lbl,
  sdt.fa.lbl = txt$sdt.fa.lbl, sdt.cr.lbl = txt$sdt.cr.lbl) {

  ## (1) Define and initialize 3 vectors of length N:
  ## (a) Truth:
  truth <- c(rep(TRUE, n.true), rep(FALSE, n.false))

  ## (b) Decisions (ordered by ACTUAL truth values of condition):
  decision <- c(rep(TRUE, n.hi), rep(FALSE, n.mi),
                rep(TRUE, n.fa), rep(FALSE, n.cr))

  ## (c) SDT (status decision/truth):
  sdt <- c(rep("hi", n.hi), rep("mi", n.mi),
           rep("fa", n.fa), rep("cr", n.cr))

  ## (2) Coerce 3 vectors into ordered factors:
  ## (a) Truth:
  truth <- factor(truth,
                  levels = c(TRUE, FALSE),
                  labels = c(cond.true.lbl, cond.false.lbl), # explicit labels
                  ordered = TRUE)

  ## (b) Decisions (ordered by ACTUAL truth values of condition):
  decision <- factor(decision,
                     levels = c(TRUE, FALSE),
                     labels = c(dec.true.lbl, dec.false.lbl), # explicit labels
                     ordered = TRUE)

  ## (c) SDT (status decision/truth):
  sdt <- factor(sdt,
                levels = c("hi", "mi", "fa", "cr"),
                labels = c(sdt.hi.lbl, sdt.mi.lbl, sdt.fa.lbl, sdt.cr.lbl), # explicit labels
                # labels = c("hi", "mi", "fa", "cr"), # implicit labels
                ordered = TRUE)

  ## (3) Combine 3 vectors in a population data frame pop:
  popu <- data.frame(tru = truth,
                     dec = decision,
                     sdt = sdt)
  names(popu) <- c("Truth", "Decision", "STD")

  ## (4) Return the entire data frame pop:
  return(popu)

}

## -----------------------------------------------
## (2) Apply to initialize popu (as data frame):

#' A population table based on current parameters.
#'
#' \code{popu} is an R data frame that is computed
#' by \code{\link{comp_popu}} from the current
#' frequency information (contained in \code{\link{freq}}).
#' Each individual is represented as a row;
#' columns represent the individual's
#' condition (\code{TRUE} or \code{FALSE}),
#' a corresponding decision (positive or negative),
#' and its classification (in SDT terms) as either
#' true positive (hit), false negative (miss),
#' false positive (false alarm), or true negative
#' (correct rejection).
#'
#' \code{comp_popu} uses the current text settings
#' contained in \code{\link{txt}} to define
#' the labels of conditions, decisions, and
#' SDT classifications.
#'
#' A visualization of the current population
#' \code{popu} is provided by \code{\link{plot_iconarray}}.
#'
#' @return A data frame \code{popu}
#' containing \code{\link{N}} rows (individual cases)
#' and 3 columns (\code{"Truth", "Decision", "SDT"}).
#'
#' @examples
#' popu <- comp_popu()  # => initializes popu with current values of freq and txt
#' dim(popu)            # => N x 3
#' head(popu)           # => shows head of data frame
#'
#' @seealso
#' The corresponding generating function \code{\link{comp_popu}};
#' \code{\link{num}} for basic numeric parameters;
#' \code{\link{txt}} for current text settings

popu <- comp_popu()
# popu

## Check:
# dim(popu)
# head(popu)
# tail(popu)

## -----------------------------------------------
## (+) ToDo:

## - ...

## -----------------------------------------------
## eof.
