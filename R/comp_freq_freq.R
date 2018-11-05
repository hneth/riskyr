## comp_freq_freq.R | riskyr
## 2018 02 09
## Compute ALL current frequencies (freq)
## from 4 essential frequencies:
## -----------------------------------------------

## (A) Compute ALL frequencies from (4 essential) frequencies: --------

## comp_freq_freq: Documentation ----------

#' Compute frequencies from (4 essential) frequencies.
#'
#' \code{comp_freq_freq} computes current frequency information
#' from 4 essential frequencies
#' (\code{\link{hi}}, \code{\link{mi}}, \code{\link{fa}}, \code{\link{cr}}).
#' It returns a list of 11 frequencies \code{\link{freq}}
#' for a population of \code{\link{N}} individuals
#' as its output.
#'
#' Key relationships between frequencies and probabilities:
#' See documentation of \code{\link{comp_freq_prob}}.
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
#' comp_freq_freq()
#' all.equal(freq, comp_freq_freq())  # => should be TRUE
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
#' @family functions computing frequencies
#' @family format conversion functions
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

## comp_freq_freq: Definition ----------

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


## Check: --------

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

## (+) ToDo: ----------

## ...

## eof. ------------------------------------------
