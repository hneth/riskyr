## comp_freq_freq.R | riskyr
## 2018 01 27
## -----------------------------------------------
## Compute ALL current frequencies (freq)
## from 4 essential frequencies (contained in freq):

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
## 2 functions convert between formats:

## a. comp_freq_prob: Computes freq from prob
## b. comp_prob_freq: Computes prob from freq

## -----------------------------------------------
## (1) Initialize freq as a list (of NA values)
##     of 9 frequencies (4 essential ones):
## -----------------------------------------------


## -----------------------------------------------
## (A) Compute ALL frequencies from (4 essential) frequencies:

#' Compute frequencies from (4 essential) frequencies.
#'
#' \code{comp_freq_freq} computes current frequency information
#' from 4 essential frequencies
#' (\code{\link{hi}}, \code{\link{mi}}, \code{\link{fa}}, \code{\link{cr}}).
#' It returns a list of 9 frequencies \code{\link{freq}}
#' for a population of \code{\link{N}} individuals
#' as its output.
#'
#' Key relationships:
#'
#' \itemize{
#'
#' \item Other functions translating between representational formats:
#'
#'    \enumerate{
#'
#'    \item \code{comp_freq_freq} (defined here) is
#'    an analog to 3 other format conversion functions:
#'
#'    \item \code{\link{comp_freq_prob}} computes
#'    current \emph{frequency} information contained in \code{\link{freq}}
#'    from 3 essential probabilities
#'    (\code{\link{prev}}, \code{\link{sens}}, \code{\link{spec}}).
#'
#'    \item \code{\link{comp_prob_freq}} computes
#'    current \emph{probability} information contained in \code{\link{prob}}
#'    from 4 essential frequencies
#'    (\code{\link{hi}}, \code{\link{mi}}, \code{\link{fa}}, \code{\link{cr}}).
#'
#'    \item \code{\link{comp_prob_prob}} computes
#'    current \emph{probability} information contained in \code{\link{prob}}
#'    from 3 essential probabilities
#'    (\code{\link{prev}}, \code{\link{sens}}, \code{\link{spec}}).
#'
#'    }
#'
#'
#' \item Two perspectives:
#'
#' A population of \code{\link{N}} individuals can be split into 2 subsets
#' in 2 different ways:
#'
#'    \enumerate{
#'
#'    \item by condition:
#'
#'    The frequency \code{\link{cond.true}} depends on the prevalence \code{\link{prev}}
#'    and
#'    the frequency \code{\link{cond.false}} depends on the prevalence's complement \code{1 - \link{prev}}.
#'
#'    \item by decision:
#'
#'    The frequency \code{\link{dec.pos}} depends on the proportion of positive decisions \code{\link{ppod}}
#'    and
#'    the frequency \code{\link{dec.neg}} depends on the proportion of negative decisions \code{1 - \link{ppod}}.
#'
#'    }
#'
#' The population size \code{\link{N}} is a free parameter (independent of the
#' essential probabilities \code{\link{prev}}, \code{\link{sens}}, and \code{\link{spec}}).
#'
#' If \code{\link{N}} is unknown (\code{NA}), a suitable minimum value can be computed by \code{\link{comp_min_N}}.
#'
#'
#' \item Combinations of frequencies:
#'
#'    In a population of size \code{\link{N}} the following relationships hold:
#'
#'    \enumerate{
#'
#'     \item \code{\link{N} = \link{cond.true} + \link{cond.false} = (\link{hi} + \link{mi}) + (\link{fa} + \link{cr})} (by condition)
#'
#'     \item \code{\link{N} = \link{dec.pos} + \link{dec.neg} = (\link{hi} + \link{fa}) + (\link{mi} + \link{cr})} (by decision)
#'
#'     \item \code{\link{N} = \link{hi} + \link{mi} + \link{fa} + \link{cr}} (by condition x decision)
#'
#'    }
#'
#'   The two perspectives (by condition vs. by decision) combine the 4 essential frequencies
#'   (i.e., \code{\link{hi}}, \code{\link{mi}}, \code{\link{fa}}, \code{\link{cr}})
#'   in 2 different ways.
#'
#'
#'
#' \item Defining probabilities in terms of frequencies:
#'
#' Probabilities \emph{are} -- determine, describe, or are defined as -- the relationships between frequencies.
#' Thus, they can be computed as ratios between frequencies.
#'
#' The following relationships hold (and are used in computations):
#'
#'    \enumerate{
#'
#'    \item prevalence \code{\link{prev}}:
#'
#'    \code{\link{prev} = \link{cond.true}/\link{N}  =  (\link{hi} + \link{mi}) / (\link{hi} + \link{mi} + \link{fa} + \link{cr})}
#'
#'
#'    \item sensitivity \code{\link{sens}}:
#'
#'    \code{\link{sens} = \link{hi}/\link{cond.true}  =  \link{hi} / (\link{hi} + \link{mi})  =  (1 - \link{mirt})}
#'
#'
#'    \item miss rate \code{\link{mirt}}:
#'
#'    \code{\link{mirt} = \link{mi}/\link{cond.true}  =  \link{mi} / (\link{hi} + \link{mi})  =  (1 - \link{sens})}
#'
#'
#'    \item specificity \code{\link{spec}}:
#'
#'    \code{\link{spec} = \link{cr}/\link{cond.false}  =  \link{cr} / (\link{fa} + \link{cr})  =  (1 - \link{fart})}
#'
#'
#'    \item false alarm rate \code{\link{fart}}:
#'
#'    \code{\link{fart} = \link{fa}/\link{cond.false}  =  \link{fa} / (\link{fa} + \link{cr})  =  (1 - \link{spec})}
#'
#'
#'
#'    \item proportion of positive decisions \code{\link{ppod}}:
#'
#'    \code{\link{ppod} = \link{dec.pos}/\link{N}  =  (\link{hi} + \link{fa}) / (\link{hi} + \link{mi} + \link{fa} + \link{cr})}
#'
#'
#'    \item positive predictive value \code{\link{PPV}}:
#'
#'    \code{\link{PPV} = \link{hi}/\link{dec.pos}  =  \link{hi} / (\link{hi} + \link{fa})  =  (1 - \link{FDR})}
#'
#'
#'    \item negative predictive value \code{\link{NPV}}:
#'
#'    \code{\link{NPV} = \link{cr}/\link{dec.neg}  =  \link{cr} / (\link{mi} + \link{cr})  =  (1 - \link{FOR})}
#'
#'
#'   \item false detection rate \code{\link{FDR}}:
#'
#'    \code{\link{FDR} = \link{fa}/\link{dec.pos}  =  \link{fa} / (\link{hi} + \link{fa})  =  (1 - \link{PPV})}
#'
#'
#'    \item false omission rate \code{\link{FOR}}:
#'
#'    \code{\link{FOR} = \link{mi}/\link{dec.neg}  =  \link{mi} / (\link{mi} + \link{cr})  =  (1 - \link{NPV})}
#'
#'    }
#' }
#'
#'
#' @param hi  The number true positives, or hits \code{\link{hi}}
#' @param mi  The number false negatives, or misses \code{\link{mi}}
#' @param fa  The number false positives, or false alarms \code{\link{fa}}
#' @param cr  The number true negatives, or correct rejections \code{\link{cr}}
#'
#' @param N.new  A new population size \code{\link{N}}
#' (not used yet, but could be used for scaling to new population sizes)
#'
#'
#' @examples
#' ## Basics:
#' comp_freq_freq()
#' all.equal(freq, comp_freq_freq())  # => should be TRUE
#'
#'
#' ## Circular chain:
#' # 1. Current numeric parameters:
#' num
#'
#' # 2. Compute all 10 probabilities in prob (from essential probabilities):
#' prob <- comp_prob()
#' prob
#'
#' # 3. Compute 9 frequencies in freq from probabilities:
#' freq <- comp_freq(round = FALSE)   # no rounding (to obtain same probabilities later)
#' freq
#'
#' # 4. Compute 9 frequencies AGAIN (but now from frequencies):
#' freq_freq <- comp_freq_freq()
#'
#' # 5. Check equality of results (steps 2. and 4.):
#' all.equal(freq, freq_freq)  # => should be TRUE!
#'
#'
#' @family functions computing frequencies
#' @family format conversion functions
#'
#'
#' @seealso
#' \code{\link{comp_freq_prob}} computes current frequency information from (3 essential) probabilities;
#' \code{\link{comp_prob_freq}} computes current probability information from (4 essential) frequencies;
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
#'

comp_freq_freq <- function(hi = freq$hi,  # 4 essential frequencies from freq
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

  new.freq <- init_freq()  # initialize new.freq (containing only NA values). BEWARE: Must NOT be "freq"!!!


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
         (cond.true != (hi + mi) )       ||
         (cond.false != (fa + cr) )      ||
         (dec.pos != (hi + fa) )         ||
         (dec.neg != (mi + cr) )   ){

      warning("Current frequencies fail to add up.")

    }
  }

  ## Insert 9 new frequencies into empty slots of new.freq:
  new.freq$N <- N
  new.freq$cond.true  <- cond.true
  new.freq$cond.false <- cond.false
  new.freq$dec.pos <- dec.pos
  new.freq$dec.neg <- dec.neg
  new.freq$hi <- hi
  new.freq$mi <- mi
  new.freq$fa <- fa
  new.freq$cr <- cr


  ## Return entire list new.freq:
  return(new.freq)

  # ## ALTERNATIVE or ADDITIVE (to compute freq and prob at once):
  #
  # ## Initialize prob:
  # prob <- init_prob()  # initialize prob (containing only NA values)
  #
  # ## Compute all 10 probabilities in prob from frequencies:
  # prob$prev <- cond.true/N
  # prob$sens <- hi/cond.true
  # prob$mirt <- mi/cond.true
  # prob$spec <- cr/cond.false
  # prob$fart <- fa/cond.false
  #
  # prob$ppod <- dec.pos/N
  # prob$PPV  <- hi/dec.pos
  # prob$NPV  <- cr/dec.neg
  # prob$FDR  <- fa/dec.pos
  # prob$FOR  <- mi/dec.neg
  #
  # ## Return entire list prob:
  # return(prob)

}


## Check:
{
  # ## Basics:
  # comp_freq_freq()
  # all.equal(freq, comp_freq_freq())  # => should be TRUE
  #
  # ## Circular chain:
  # #
  # # # 1. Current numeric parameters:
  # num
  # #
  # # # 2. Compute all 10 probabilities in prob (from essential probabilities):
  # prob <- comp_prob()
  # prob
  # #
  # # # 3. Compute 9 frequencies in freq from probabilities:
  # freq <- comp_freq(round = FALSE)   # no rounding (to obtain same probabilities later)
  # freq
  # #
  # # # 4. Compute 9 frequencies AGAIN (but now from frequencies):
  # freq_freq <- comp_freq_freq()
  # #
  # # # 5. Check equality of results (steps 2. and 4.):
  # all.equal(freq, freq_freq)  # => should be TRUE!
}

## -----------------------------------------------

## -----------------------------------------------
## (+) ToDo:

## ...

## -----------------------------------------------
## eof.
