## comp_prob_freq.R | riskyr
## 2018 12 20
## Compute probabilities (prob) from frequencies (freq):
## -----------------------------------------------

## (A) Compute ALL probabilities from (4 essential) frequencies: --------

## comp_prob_freq: Documentation ------

#' Compute probabilities from (4 essential) frequencies.
#'
#' \code{comp_prob_freq} computes current probability information
#' from 4 essential frequencies
#' (\code{\link{hi}}, \code{\link{mi}}, \code{\link{fa}}, \code{\link{cr}}).
#' It returns a list of 11 frequencies \code{\link{freq}}
#' for a population of \code{\link{N}} individuals
#' as its output.
#'
#'
#' Key relationships between frequencies and probabilities
#' (see documentation of \code{\link{comp_freq}} or \code{\link{comp_prob}} for details):
#'
#' \itemize{
#'
#'   \item Three perspectives on a population:
#'
#'   by condition / by decision / by accuracy.
#'
#'   \item Defining probabilities in terms of frequencies:
#'
#'   Probabilities can be computed as ratios between frequencies, but beware of rounding issues.
#'
#' }
#'
#'
#' Functions translating between representational formats:
#' \code{\link{comp_prob_prob}}, \code{comp_prob_freq},
#' \code{\link{comp_freq_prob}}, \code{\link{comp_freq_freq}}
#' (see documentation of \code{\link{comp_prob_prob}} for details).
#'
#'
#' @param hi  The number of hits \code{\link{hi}} (or true positives).
#' @param mi  The number of misses \code{\link{mi}} (or false negatives).
#' @param fa  The number of false alarms \code{\link{fa}} (or false positives).
#' @param cr  The number of correct rejections \code{\link{cr}} (or true negatives).
#'
#'
#' @examples
#' ## Basics:
#' comp_prob_freq()  # => computes prob from current freq
#'
#' ## Beware of rounding:
#' all.equal(prob, comp_prob_freq())  # => would be TRUE (IF freq were NOT rounded)!
#' fe <- comp_freq(round = FALSE)     # compute exact freq (not rounded)
#' all.equal(prob, comp_prob_freq(fe$hi, fe$mi, fe$fa, fe$cr))  # is TRUE (qed).
#'
#' ## Explain by circular chain (compute prob 1. from num and 2. from freq)
#' # 0. Inspect current numeric parameters:
#' num
#'
#' # 1. Compute currently 11 probabilities in prob (from essential probabilities):
#' prob <- comp_prob()
#' prob
#'
#' # 2. Compute currently 11 frequencies in freq (from essential probabilities):
#' freq <- comp_freq(round = FALSE)   # no rounding (to obtain same probabilities later)
#' freq
#'
#' # 3. Compute currently 11 probabilities again (but now from frequencies):
#' prob_freq <- comp_prob_freq()
#' prob_freq
#'
#' # 4. Check equality of probabilities (in steps 1. and 3.):
#' all.equal(prob, prob_freq)  # => should be TRUE!
#'
#' @family functions computing probabilities
#' @family format conversion functions
#'
#' @seealso
#' \code{\link{comp_freq_freq}} computes current frequency information from (4 essential) frequencies;
#' \code{\link{comp_freq_prob}} computes current frequency information from (3 essential) probabilities;
#' \code{\link{comp_prob_prob}} computes current probability information from (3 essential) probabilities;
#' \code{\link{num}} contains basic numeric parameters;
#' \code{\link{init_num}} initializes basic numeric parameters;
#' \code{\link{prob}} contains current probability information;
#' \code{\link{comp_prob}} computes current probability information;
#' \code{\link{freq}} contains current frequency information;
#' \code{\link{comp_freq}} computes current frequency information;
#' \code{\link{is_prob}} verifies probability inputs;
#' \code{\link{is_freq}} verifies frequency inputs.
#'
#' @export

## comp_prob_freq: Definition ------

comp_prob_freq <- function(hi = freq$hi,  # 4 essential frequencies from freq (which may be rounded OR not rounded)
                           mi = freq$mi,
                           fa = freq$fa,
                           cr = freq$cr
                           # N.new,       # to verify sum OR re-scale to new population size if different from freq$N?
) {

  ## Initialize compound freq:
  N <- NA
  cond.true  <- NA
  cond.false <- NA
  dec.pos    <- NA
  dec.neg    <- NA
  dec.cor    <- NA
  dec.err    <- NA

  prob <- init_prob()  # initialize prob (containing only NA values)


  ## Compute compound freq from 4 essential freq:

  ## (a) Using sum:
  N  <- sum(c(hi, mi, fa, cr), na.rm = TRUE)   # N

  cond.true  <- sum(c(hi, mi), na.rm = TRUE)   # freq of cond.true cases
  cond.false <- sum(c(fa, cr), na.rm = TRUE)   # freq of cond.false cases

  dec.pos  <-   sum(c(hi, fa), na.rm = TRUE)   # freq of dec.pos cases
  dec.neg  <-   sum(c(mi, cr), na.rm = TRUE)   # freq of dec.neg cases

  dec.cor  <-   sum(c(hi, cr), na.rm = TRUE)   # freq of dec.cor cases
  dec.err  <-   sum(c(mi, fa), na.rm = TRUE)   # freq of dec.err cases

  # ## (b) Using +:
  # N <- (hi + mi + fa + cr) # N
  #
  # cond.true  <- (hi + mi)  # freq of cond.true cases
  # cond.false <- (fa + cr)  # freq of cond.false cases
  #
  # dec.pos  <-   (hi + fa)  # freq of dec.pos cases
  # dec.neg  <-   (mi + cr)  # freq of dec.neg cases
  #
  # dec.cor  <-   (hi + cr)  # freq of dec.cor cases
  # dec.err  <-   (mi + fa)  # freq of dec.err cases

  ## Check for existence:
  if (is.na(N)) {

    warning("N is NA. At least one essential frequency is required.")

  } else {

    ## Check current frequencies for consistency:
    tol <- .0001  # tolerance threshold for mismatch of sums

    if (#isTRUE(all.equal(N, (hi + mi + fa + cr), tolerance = tol)) &&
      (abs(N - (hi + mi + fa + cr)) > tol) ||
      (abs(cond.true - (hi + mi)) > tol)             ||
      (abs(cond.false - (fa + cr)) > tol)            ||
      (abs(dec.pos - (hi + fa)) > tol)               ||
      (abs(dec.neg - (mi + cr)) > tol)               ||
      (abs(dec.cor - (hi + cr)) > tol)               ||
      (abs(dec.err - (mi + fa)) > tol)               ||
      (abs(N - (cond.true + cond.false)) > tol)      ||
      (abs(N - (dec.pos + dec.neg)) > tol)           ||
      (abs(N - (dec.cor + dec.err)) > tol)
    ) {

      warning("Current frequencies do NOT add up to N.")
    }

  }

  ## Compute all (currently 13) probabilities in prob from frequencies:
  prob$prev <- cond.true/N
  prob$sens <- hi/cond.true
  prob$mirt <- mi/cond.true
  prob$spec <- cr/cond.false
  prob$fart <- fa/cond.false

  prob$ppod <- dec.pos/N
  prob$PPV  <- hi/dec.pos
  prob$NPV  <- cr/dec.neg
  prob$FDR  <- fa/dec.pos
  prob$FOR  <- mi/dec.neg

  prob$acc  <- dec.cor/N
  ## NOW also computed/stored in prob [2018 09 30]:
  prob$p_acc_hi <- hi/dec.cor  # p(hi | acc)
  prob$p_err_fa <- fa/dec.err  # p(fa | err)

  ## Return entire list prob:
  return(prob)

}

## Check: ------

# ## Basics:
# comp_prob_freq()  # => computes prob from current freq
#
# ## Beware of rounding:
# all.equal(prob, comp_prob_freq())  # => would be TRUE (IF freq were NOT rounded)!
# fe <- comp_freq(round = FALSE)     # compute exact freq (not rounded)
# all.equal(prob, comp_prob_freq(fe$hi, fe$mi, fe$fa, fe$cr))  # is TRUE (qed).
#
# ## Explain by circular chain (compute prob 1. from num and 2. from freq)
#
# # 0. Inspect current numeric parameters:
# # num
#
# # 1. Compute currently 11 probabilities in prob (from essential probabilities):
# prob <- comp_prob()
# prob
#
# # 2. Compute currently 11 frequencies in freq (from essential probabilities):
# freq <- comp_freq(round = FALSE)   # no rounding (to obtain same probabilities later)
# freq
#
# # 3. Compute currently 11 probabilities again (but now from frequencies):
# prob_freq <- comp_prob_freq()
# prob_freq
#
# # 4. Check equality of probabilities (in steps 1. and 3.):
# all.equal(prob, prob_freq)  # => should be TRUE!
#
#
## Computing individual probabilities from freq: --------

## (obsolete, when ALL can be obtained above?)


## (B) Compute 3 essential probabilities (prev, sens, spec) from existing frequencies ------

## comp_prev: Documentation -----

#' Compute the condition's prevalence (baseline probability) from frequencies.
#'
#' \code{comp_prev} computes a condition's prevalence value \code{\link{prev}}
#' (or baseline probability) from 4 essential frequencies
#' (\code{\link{hi}}, \code{\link{mi}}, \code{\link{fa}}, \code{\link{cr}}).
#'
#' A condition's prevalence value \code{\link{prev}} is
#' the probability of the condition being \code{TRUE}.
#'
#' The probability \code{\link{prev}} can be computed from frequencies
#' as the the ratio of
#' \code{\link{cond.true}} (i.e., \code{\link{hi} + \link{mi}})
#' divided by
#' \code{\link{N}} (i.e., \code{\link{hi} + \link{mi} + \link{fa} + \link{cr}}):
#'
#' \code{prev = cond.true/N = (hi + mi)/(hi + mi + fa + cr)}
#'
#' @param hi  The number of hits \code{\link{hi}} (or true positives).
#' @param mi  The number of misses \code{\link{mi}} (or false negatives).
#' @param fa  The number of false alarms \code{\link{fa}} (or false positives).
#' @param cr  The number of correct rejections \code{\link{cr}} (or true negatives).
#'
#' @family functions computing probabilities from frequencies
#'
#' @seealso
#' \code{\link{num}} contains basic numeric parameters;
#' \code{\link{init_num}} initializes basic numeric parameters;
#' \code{\link{prob}} contains current probability information;
#' \code{\link{comp_prob}} computes current probability information;
#' \code{\link{freq}} contains current frequency information;
#' \code{\link{comp_freq}} computes current frequency information;
#' \code{\link{is_prob}} verifies probability inputs;
#' \code{\link{is_freq}} verifies frequency inputs.

## comp_prev: Definition -----

comp_prev <- function(hi = freq$hi, mi = freq$mi, fa = freq$fa, cr = freq$cr   # 4 essential frequencies from freq
                      ## N.new,       # to verify sum OR re-scale to new population size if different from freq$N?
) {

  ## Initialize prob:
  prob <- init_prob()  # initialize prob (containing only NA values)

  ## Compute 5 other freq from 4 essential freq:
  cond.true  <- sum(hi, mi, na.rm = TRUE)   # freq of cond.true cases
  cond.false <- sum(fa, cr, na.rm = TRUE)   # freq of cond.false cases
  dec.pos <- sum(hi, fa, na.rm = TRUE)      # freq of dec.pos cases
  dec.neg <- sum(mi, cr, na.rm = TRUE)      # freq of dec.neg cases
  N <- sum(cond.true, cond.false)           # N

  ## Check for consistency:
  if ( (N != hi + mi + fa + cr)      ||
       (N != cond.true + cond.false) ||
       (N != dec.pos + dec.neg)      ||
       (cond.true != hi + mi)        ||
       (cond.false != fa + cr)       ||
       (dec.pos != hi + fa)          ||
       (dec.neg != mi + cr)           )
  {

    warning("Current frequencies do NOT add up.")
  }

  ## Compute all probabilities in prob:
  prob$prev <- cond.true/N

  ## Return:
  return(prob$prev)

}

## Check: ------

# num
# freq <- comp_freq(round = FALSE)
# freq

# comp_prev()



## (C) Compute other probabilities (mirt, fart) from existing frequencies ------

## (D) Compute non-essential probabilities (mirt, fart) and derived probabilities (ppod, PPV, NPV, ...) from frequencies ------

## ALL probabilities can be computed from 4 frequencies
## of the confusion table!

## See https://en.wikipedia.org/wiki/Confusion_matrix
## for a collection of metrics.


## (E) Compute predictive values (PVs: PPV and NPV, FDR and FOR) from various frequencies (alternative versions) ------

## 1. comp_PPV_freq: Positive predictive value (PPV) from frequencies ------

comp_PPV_freq <- function(n.hi = freq$hi, n.fa = freq$fa) {

  PPV <- NA # initialize

  ## PPV = hits / positive decisions
  ##     = hits / (hits + false alarms)

  if ((n.hi + n.fa) == 0) {
    stop( "Stop: Division by zero: n.hi + n.fa = 0 [comp_PPV_freq()]." )
  }

  PPV <- n.hi / (n.hi + n.fa)

  return(PPV)
}


## 2. comp_NPV_freq: Negative predictive value (NPV) from frequencies ------

comp_NPV_freq <- function(n.cr = freq$cr, n.mi = freq$mi) {

  NPV <- NA # initialize

  ## NPV = correct rejections / negative decisions
  ##     = correct rejections / (correct rejections + misses)

  if ((n.cr + n.mi) == 0) {
    stop( "Stop: Division by zero: n.cr + n.mi = 0 [comp_NPV_freq()]." )
  }

  NPV <- n.cr / (n.cr + n.mi)

  return(NPV)
}


## (F): Comparing the alternative PV calculations: ------

# ## A: Using default settings:
# num$N
# freq <- comp_freq(round = TRUE)
# freq
#
# comp_PPV()
# comp_PPV_freq() # Note: Deviation due to rounding of freq to nearest integers!
#
# comp_NPV()
# comp_NPV_freq() # Note: Deviation due to rounding of freq to nearest integers!

# ## B: But when using exact frequencies for freq:
# freq <- comp_freq(round = FALSE) # do NOT round to nearest integers:
# all.equal(comp_PPV_freq(), comp_PPV()) # => TRUE
# all.equal(comp_NPV_freq(), comp_NPV()) # => TRUE

## (*) Done: ----------

## - Clean up code.  [2018 08 28]


## (+) ToDo: ----------

## - Compute the set of ALL current probabilities from frequencies (freq): ------

## - Add alternative ways to compute probabilities from frequencies
##   (based on different elements of freq)!
##
## - Compute basic parameters (probabilities and frequencies)
##   from MIX of existing probabilities and frequencies!
##
## - Compute alternative prob from freq with
##   a. N of dec.pos (rather than N of fa) and
##   b. N of dec.neg (rather than N of mi) provided.

## eof. ------------------------------------------
