## comp_prob_freq.R | riskyr
## 2018 01 26
## -----------------------------------------------
## Compute probabilities (prob) from frequencies (freq):

## Note: Always use num (essential) and freq (derived)
##       rather than env (NON-essential)!

## -----------------------------------------------
## Table of current terminology:

# Probabilities (10):               Frequencies (9):
# -------------------               ------------------
# (A) by condition:

# non-conditional:                          N
# prev*                           cond.true | cond.false

# conditional:
# sens* = hit rate = TPR                hi* = TP
# mirt  = miss rate = FNR               mi* = FN
# fart  = false alarm rate = FPR        fa* = FP
# spec* = true negative rate = TNR      cr* = TN

# [Note: *...is essential]


# (B) by decision:                 Combined frequencies:

# non-conditional:
# ppod = proportion of dec.pos     dec.pos | dec.neg

# conditional:
# PPV = precision
# FDR = false detection rate
# FOR = false omission rate
# NPV = neg. pred. value


## -----------------------------------------------
## Data flow: Two basic directions:

## (1) Probabilities ==> frequencies:
##     Bayesian: based on 3 essential probabilities:
##   - given:   prev;  sens, spec
##   - derived: all other values

## (2) Frequencies ==> probabilities:
##     Frequentist: based on 4 essential natural frequencies:
##   - given:   N = hi, mi, fa, cr
##   - derived: all other values

## -----------------------------------------------
## ad (2) Frequencies ==> probabilities:
## -----------------------------------------------


## -----------------------------------------------
## (A) Compute ALL probabilities from 4 essential frequencies:

#' Compute probabilities from frequencies.
#'
#' \code{comp_prob_freq} computes all probabilities
#' contained in \code{\link{prob}} from 4 essential frequencies
#' (\code{\link{hi}}, \code{\link{mi}}, \code{\link{fa}}, \code{\link{cr}}).
#'
#' \code{comp_prob_freq} is an analog to \code{\link{comp_freq_freq}}
#' that computes all \emph{frequencies} contained in \code{\link{freq}}
#' from the same 4 essential frequencies.
#'
#' The following relationships hold (and are used in computations):
#'
#' \enumerate{
#'
#'   \item prevalence \code{\link{prev}}:
#'
#'   \code{\link{prev} = \link{cond.true}/\link{N}  =  (\link{hi} + \link{mi}) / (\link{hi} + \link{mi} + \link{fa} + \link{cr})}
#'
#'
#'   \item sensitivity \code{\link{sens}}:
#'
#'   \code{\link{sens} = \link{hi}/\link{cond.true}  =  \link{hi} / (\link{hi} + \link{mi})  =  (1 - \link{mirt})}
#'
#'
#'   \item miss rate \code{\link{mirt}}:
#'
#'   \code{\link{mirt} = \link{mi}/\link{cond.true}  =  \link{mi} / (\link{hi} + \link{mi})  =  (1 - \link{sens})}
#'
#'
#'   \item specificity \code{\link{spec}}:
#'
#'   \code{\link{spec} = \link{cr}/\link{cond.false}  =  \link{cr} / (\link{fa} + \link{cr})  =  (1 - \link{fart})}
#'
#'
#'   \item false alarm rate \code{\link{fart}}:
#'
#'   \code{\link{fart} = \link{fa}/\link{cond.false}  =  \link{fa} / (\link{fa} + \link{cr})  =  (1 - \link{spec})}
#'
#'
#'
#'   \item proportion of positive decisions \code{\link{ppod}}:
#'
#'   \code{\link{ppod} = \link{dec.pos}/\link{N}  =  (\link{hi} + \link{fa}) / (\link{hi} + \link{mi} + \link{fa} + \link{cr})}
#'
#'
#'   \item positive predictive value \code{\link{PPV}}:
#'
#'   \code{\link{PPV} = \link{hi}/\link{dec.pos}  =  \link{hi} / (\link{hi} + \link{fa})  =  (1 - \link{FDR})}
#'
#'
#'   \item negative predictive value \code{\link{NPV}}:
#'
#'   \code{\link{NPV} = \link{cr}/\link{dec.neg}  =  \link{cr} / (\link{mi} + \link{cr})  =  (1 - \link{FOR})}
#'
#'
#'   \item false detection rate \code{\link{FDR}}:
#'
#'   \code{\link{FDR} = \link{fa}/\link{dec.pos}  =  \link{fa} / (\link{hi} + \link{fa})  =  (1 - \link{PPV})}
#'
#'
#'   \item false omission rate \code{\link{FOR}}:
#'
#'   \code{\link{FOR} = \link{mi}/\link{dec.neg}  =  \link{mi} / (\link{mi} + \link{cr})  =  (1 - \link{NPV})}
#'
#' }
#'
#'
#'
#' @param hi  The number true positives, or hits \code{\link{hi}}
#' @param mi  The number false negatives, or misses \code{\link{mi}}
#' @param fa  The number false positives, or false alarms \code{\link{fa}}
#' @param cr  The number true negatives, or correct rejections \code{\link{cr}}
#'
#' @param N  The population size \code{\link{N}} (not used yet, use for scaling to new population sizes)
#'
#'
#' @examples
#' ## Basics:
#' comp_prob_freq()
#' all.equal(prob, comp_prob_freq())  # => should be TRUE!
#'
#'
#' ## Circular chain:
#' # 1. Current numeric parameters:
#' num
#'
#' # 2. Compute all 10 probabilities in prob (from essential probabilities):
#' prob <- comp_prob()
#'
#' # 3. Compute 9 frequencies in freq from probabilities:
#' freq <- comp_freq(round = FALSE)   # prevent rounding (to obtain same probabilities later)
#' freq
#'
#' # 4. Compute all 10 probabilities again (but now from frequencies):
#' prob_freq <- comp_prob_freq()
#' prob_freq
#'
#' # 5. Check equality of results (steps 2. and 4.):
#' all.equal(prob, prob_freq)  # => should be TRUE
#'
#'
#' @family functions computing probabilities
#'
#'
#' @seealso
#' \code{\link{comp_freq_freq}} computes full frequency information from essential frequencies;
#' \code{\link{num}} contains basic numeric parameters;
#' \code{\link{init_num}} initializes basic numeric parameters;
#' \code{\link{prob}} contains current probability information;
#' \code{\link{comp_prob}} computes current probability information;
#' \code{\link{freq}} contains current frequency information;
#' \code{\link{comp_freq}} computes current frequency information;
#' \code{\link{is_prob}} verifies probability inputs;
#' \code{\link{is_freq}} verifies frequency inputs.

comp_prob_freq <- function(hi = freq$hi,  # 4 essential frequencies from freq
                           mi = freq$mi,
                           fa = freq$fa,
                           cr = freq$cr
                           # N.new,       # to verify sum OR re-scale to new population size if different from freq$N?
                           ) {

  ## Initialize:
  N <- NA
  cond.true  <- NA
  cond.false <- NA
  dec.pos    <- NA
  dec.neg    <- NA

  prob <- init_prob()  # initialize prob (containing only NA values)


  ## Compute 5 other freq from 4 essential freq:
  N  <- sum(c(hi, mi, fa, cr), na.rm = TRUE)   # N
  cond.true  <- sum(c(hi, mi), na.rm = TRUE)   # freq of cond.true cases
  cond.false <- sum(c(fa, cr), na.rm = TRUE)   # freq of cond.false cases
  dec.pos  <-   sum(c(hi, fa), na.rm = TRUE)   # freq of dec.pos cases
  dec.neg  <-   sum(c(mi, cr), na.rm = TRUE)   # freq of dec.neg cases


  ## Check for existence:
  if (is.na(N)) {

    warning("N is NA. At least one essential frequency is required.")

  } else {

    ## Check for consistency:
    if ( (N != (hi + mi + fa + cr))      ||
         (N != (cond.true + cond.false)) ||
         (N != (dec.pos + dec.neg))      ||
         (cond.true != (hi + mi))        ||
         (cond.false != (fa + cr))       ||
         (dec.pos != (hi + fa))          ||
         (dec.neg != (mi + cr))   ){

      warning("Current frequencies fail to add up.")

    }
  }

  ## Compute all 10 probabilities in prob from frequencies:
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

  ## Return entire list prob:
  return(prob)

}

## Check:

{
  ## Basics:
  # comp_prob_freq()

  ## Circular chain:
  #
  # # 1. Current numeric parameters:
  # num
  #
  # # 2. Compute all 10 probabilities in prob (from essential probabilities):
  # prob <- comp_prob()
  #
  # # 3. Compute 9 frequencies in freq from probabilities:
  # freq <- comp_freq(round = FALSE)   # no rounding (to obtain same probabilities later)
  # freq
  #
  # # 4. Compute all 10 probabilities again (but now from frequencies):
  # prob_freq <- comp_prob_freq()
  # prob_freq
  #
  # ## 5. Check equality of results (steps 2. and 4.):
  # all.equal(prob, prob_freq)  # => should be TRUE!
}

## -----------------------------------------------


# +++ here now +++

## ToDo:

## Analog function comp_freq_freq
## that computes all 9 frequencies from 4 essential ones:
##
## \code{\link{comp_freq_freq}} computes full frequency information from essential frequencies

## -----------------------------------------------
##  (B) Compute 3 essential probabilities (prev, sens, spec)
##     from existing frequencies

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
#'
#' @param N  The population size \code{\link{N}}
#'
#' @param hi  The number true positives, or hits \code{\link{hi}}
#' @param mi  The number false negatives, or misses \code{\link{mi}}
#' @param fa  The number false positives, or false alarms \code{\link{fa}}
#' @param cr  The number true negatives, or correct rejections \code{\link{cr}}
#'
#'
#' @family functions computing probabilities from frequencies
#'
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

## Check:
# num
# freq <- comp_freq(round = FALSE)
# freq

# comp_prev()



## -----------------------------------------------
##  (C) Compute other probabilities (mirt, fart)
##     from existing frequencies
## -----------------------------------------------
##  (D) Compute non-essential probabilities (mirt, fart)
##      and derived probabilities (ppod, PPV, NPV, ...)
##      from existing frequencies.





# ALL probabilities can be computed from 4 frequencies
# of the confusion table!

# See https://en.wikipedia.org/wiki/Confusion_matrix
# for a collection of metrics.


## (...)


## -----------------------------------------------
## (X) Compute predictive values (PVs:
##     PPV and NPV, FDR and FOR)
##     from various frequencies (alternative versions):

## -----------------------------------------------
## 1. Positive predictive value (PPV) from frequencies:

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

## -----------------------------------------------
## 2. Negative predictive value (NPV) from frequencies:

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

## -----------------------------------------------
## ToDo: Add alternative ways to compute probabilities
## from frequencies (based on different elements of freq)!
## (...)

## -----------------------------------------------
## (C): Comparing the alternative PV calculations:

{
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
  #
  # ## B: But when using exact frequencies for freq:
  # freq <- comp_freq(round = FALSE) # do NOT round to nearest integers:
  # all.equal(comp_PPV_freq(), comp_PPV()) # => TRUE
  # all.equal(comp_NPV_freq(), comp_NPV()) # => TRUE

}

## -----------------------------------------------
## (D) Compute the set of ALL current probabilities:
##     from frequencies (freq):

## (...)

## -----------------------------------------------
## (+) ToDo:

## - Add alternative ways to compute probabilities
##   from frequencies (based on various elements of freq)!
##
## - Compute basic parameters (probabilities and frequencies)
##   from MIX of existing probabilities and frequencies!
##
## - Compute alternative prob from freq with
##   a. N of dec.pos (rather than N of fa) and
##   b. N of dec.neg (rather than N of mi) provided.

## -----------------------------------------------
## eof.
