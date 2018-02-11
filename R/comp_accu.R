## comp_accu.R | riskyr
## 2018 02 11
## -----------------------------------------------
## Compute accuracy metrics
## based on only the 4 essential frequencies
## of freq (hi mi fa cr).
## or 3 essential probabilities (prev, sens, spec)
## -----------------------------------------------
## Notes:
## Assumes that freq has been computed before.
## -----------------------------------------------

## (1) Compute all accuracy metrics (from freq)
## (2) Accuracy metrics from probabilities

## -----------------------------------------------
## (1) Compute accuracy metrics for current
##     classification result.

## (A) Compute all accuracy metrics from freq:

#' Compute acccuracy of current classification results.
#'
#' \code{comp_accu} computes current accuracy metrics
#' from the 4 essential frequencies (\code{\link{hi}},
#' \code{\link{mi}}, \code{\link{fa}}, \code{\link{cr}})
#' that constitute the current confusion matrix and
#' are contained in \code{\link{freq}}.
#'
#' Currently computed metrics include:
#'
#' \enumerate{
#'
#'    \item \code{acc}: Overall accuracy as the proportion (or probability)
#'    of correctly classifying cases or of \code{\link{dec.cor}} cases:
#'
#'    \code{acc = dec.cor/N = (hi + cr)/(hi + mi + fa + cr)}
#'
#'    Values range from 0 (no correct prediction) to 1 (perfect prediction).
#'
#'
#'    \item \code{wacc}: Weighted accuracy, as a weighted average of the
#'    sensitivity \code{\link{sens}} (aka. hit rate \code{\link{HR}}, \code{\link{TPR}},
#'    \code{\link{power}} or \code{\link{recall}})
#'    and the the specificity \code{\link{spec}} (aka. \code{\link{TNR}})
#'    in which \code{\link{sens}} is multiplied by a weighting parameter \code{w}
#'    (ranging from 0 to 1) and \code{\link{spec}} is multiplied by
#'    \code{w}'s complement \code{(1 - w)}:
#'
#'    \code{wacc = (w * sens) + ((1 - w) * spec)}
#'
#'    If \code{w = .50}, \code{wacc} becomes \emph{balanced} accuracy \code{bacc}.
#'
#'
#'    \item \code{mcc}: The Matthews correlation coefficient (with values ranging from -1 to +1):
#'
#'    \code{mcc = ((hi * cr) - (fa * mi)) / sqrt((hi + fa) * (hi + mi) * (cr + fa) * (cr + mi))}
#'
#'    A value of \code{mcc = 0} implies random performance; \code{mcc = 1} implies perfect performance.
#'
#'    See \href{https://en.wikipedia.org/wiki/Matthews_correlation_coefficient}{Wikipedia: Matthews correlation coefficient}
#'    for additional information.
#'
#'    \item \code{f1s}: The harmonic mean of the positive predictive value \code{\link{PPV}}
#'    (aka. \code{\link{precision}})
#'    and the sensitivity \code{\link{sens}} (aka. hit rate \code{\link{HR}},
#'    \code{\link{TPR}}, \code{\link{power}} or \code{\link{recall}}):
#'
#'    \code{f1s =  2 * (PPV * sens) / (PPV + sens)}
#'
#'    See \href{https://en.wikipedia.org/wiki/F1_score}{Wikipedia: F1 score} for additional information.
#'
#' }
#'
#' Note that some accuracy metrics can be interpreted
#' as probabilities (e.g., \code{acc}) or correlations (e.g., \code{mcc}).
#'
#' @return A list \code{\link{accu}} containing current accuracy metrics.
#'
#' @param hi  The number of hits \code{\link{hi}} (or true positives).
#' @param mi  The number of misses \code{\link{mi}} (or false negatives).
#' @param fa  The number of false alarms \code{\link{fa}} (or false positives).
#' @param cr  The number of correct rejections \code{\link{cr}} (or true negatives).
#'
#' @param w   The weighting parameter \code{w} (from 0 to 1)
#' for computing weighted accuracy \code{wacc}.
#' Default: \code{w = .50} (i.e., yielding balanced accuracy \code{bacc}).
#'
#' @examples
#' comp_accu()  # => computes accuracy metrics for current default scenario
#' comp_accu(hi = 1, mi = 2, fa = 3, cr = 4)  # medium accuracy, but cr > hi
#'
#' # Extreme cases:
#' comp_accu(hi = 1, mi = 1, fa = 1, cr = 1)  # random performance
#' comp_accu(hi = 1, mi = 0, fa = 0, cr = 1)  # perfect accuracy/optimal performance
#' comp_accu(hi = 0, mi = 1, fa = 1, cr = 0)  # zero accuracy/worst performance, but see f1s
#' comp_accu(hi = 1, mi = 0, fa = 0, cr = 0)  # perfect accuracy, but see wacc and mcc
#'
#' # Effects of w:
#' comp_accu(hi = 3, mi = 2, fa = 1, cr = 4, w = 1/2)  # equal weights to sens and spec
#' comp_accu(hi = 3, mi = 2, fa = 1, cr = 4, w = 2/3)  # more weight to sens
#' comp_accu(hi = 3, mi = 2, fa = 1, cr = 4, w = 1/3)  # more weight to spec
#'
#'
#' @references
#' Consult \href{https://en.wikipedia.org/wiki/Confusion_matrix}{Wikipedia: Confusion matrix}
#' for additional information.
#'
#' @family metrics
#' @family functions computing probabilities
#'
#' @seealso
#' The corresponding data frame ;
#' \code{\link{num}} for basic numeric parameters;
#' \code{\link{freq}} for current frequency information;
#' \code{\link{txt}} for current text settings;
#' \code{\link{pal}} for current color settings;
#' \code{\link{popu}} for a table of the current population.
#'
#' @export

comp_accu <- function(hi = freq$hi, mi = freq$mi,  # 4 essential frequencies
                      fa = freq$fa, cr = freq$cr,
                      w = .50  # weight for wacc (from 0 to 1). Default: w = .50 (aka. bacc).
) {

  ## Verify w:
  if (!is_prob(w)) {
    warning("The weighting parameter w (for wacc) must range from 0 to 1.")
  }

  ## (1) Initialize accu:    # Metric:
  accu <- list("acc"  = NA,  # 1. overall accuracy
               "w"    = w,   #    weighting parameter w
               "wacc" = NA,  # 2. weighted/balanced accuracy
               "mcc"  = NA,  # 3. MCC
               "f1s"  = NA   # 4. F1 score
  )

  ## (2) Compute combined frequencies from 4 essential frequencies:
  ## (a) by condition (columns of confusion matrix):
  cond.true  <- (hi + mi)
  cond.false <- (fa + cr)
  N.cond     <- (cond.true + cond.false)

  ## (b) by decision (rows of confusion matrix):
  dec.pos <- (hi + fa)
  dec.neg <- (mi + cr)
  N.dec   <- (dec.pos + dec.neg)

  ## (c) by truth/correctness of decision (diagonals of confusion matrix):
  dec.cor <- (hi + cr)  # correct decisions
  dec.err <- (mi + fa)  # erroneous decisions
  N.truth <- (dec.cor + dec.err)

  ## Check:
  if ((N.cond != N.dec) || (N.cond != N.truth))  {
    warning("Something strange occurred: 4 basic frequencies do not add up to N.")
  } else {
    N <- N.cond
  }

  ## (3) Compute conditional probabilities:
  sens <- hi/cond.true
  spec <- cr/cond.false

  PPV <- hi/dec.pos
  NPV <- cr/dec.neg

  ## (4) Compute accuracy measures:
  accu$acc  <- dec.cor/N                      # 1. acc
  accu$wacc <- (w * sens) + ((1 - w) * spec)  # 2. wacc/bacc

  mcc_num  <- ((hi * cr) - (fa * mi))
  mcc_den  <- sqrt((hi + fa) * (hi + mi) * (cr + fa) * (cr + mi))
  if (mcc_den == 0) {
    mcc_den <- 1  # correction
    warning("accu$mcc: A denominator of 0 was corrected to 1, resulting in mcc = 0.")
  }
  accu$mcc  <- mcc_num/mcc_den                # 3. mcc

  accu$f1s  <- 2 * (PPV * sens)/(PPV + sens)  # 4. f1s

  ## (5) Return the entire list accu:
  return(accu)

}

## Check:
{
  # comp_accu(hi = 1, mi = 2, fa = 3, cr = 4)  # medium accuracy, but cr > hi.
  #
  # # Extreme cases:
  # comp_accu(hi = 1, mi = 1, fa = 1, cr = 1)  # random performance
  # comp_accu(hi = 1, mi = 0, fa = 0, cr = 1)  # perfect accuracy/optimal prediction performance
  # comp_accu(hi = 0, mi = 1, fa = 1, cr = 0)  # zero accuracy/worst prediction performance, but see f1s
  # comp_accu(hi = 1, mi = 0, fa = 0, cr = 0)  # perfect accuracy, but see wacc and mcc (corrected)
  #
  # # Effects of w:
  # comp_accu(hi = 3, mi = 2, fa = 1, cr = 4, w = 1/2)  # equal weights to sens and spec
  # comp_accu(hi = 3, mi = 2, fa = 1, cr = 4, w = 2/3)  # more weight to sens
  # comp_accu(hi = 3, mi = 2, fa = 1, cr = 4, w = 1/3)  # more weight to spec
}

## -----------------------------------------------
## (B) Apply to initialize accu (as a list):

#' A list containing current accuracy information.
#'
#' \code{accu} contains current accuracy information
#' returned by the corresponding generating function
#' \code{\link{comp_accu}}.
#'
#' Current metrics include:
#'
#' \enumerate{
#'
#'    \item \code{acc}: Overall accuracy as the proportion (or probability)
#'    of correctly classifying cases or of \code{\link{dec.cor}} cases:
#'
#'    \code{acc = dec.cor/N = (hi + cr)/(hi + mi + fa + cr)}
#'
#'    Values range from 0 (no correct prediction) to 1 (perfect prediction).
#'
#'
#'    \item \code{wacc}: Weighted accuracy, as a weighted average of the
#'    sensitivity \code{\link{sens}} (aka. hit rate \code{\link{HR}}, \code{\link{TPR}},
#'    \code{\link{power}} or \code{\link{recall}})
#'    and the the specificity \code{\link{spec}} (aka. \code{\link{TNR}})
#'    in which \code{\link{sens}} is multiplied by a weighting parameter \code{w}
#'    (ranging from 0 to 1) and \code{\link{spec}} is multiplied by
#'    \code{w}'s complement \code{(1 - w)}:
#'
#'    \code{wacc = (w * sens) + ((1 - w) * spec)}
#'
#'    If \code{w = .50}, \code{wacc} becomes \emph{balanced} accuracy \code{bacc}.
#'
#'
#'    \item \code{mcc}: The Matthews correlation coefficient (with values ranging from -1 to +1):
#'
#'    \code{mcc = ((hi * cr) - (fa * mi)) / sqrt((hi + fa) * (hi + mi) * (cr + fa) * (cr + mi))}
#'
#'    A value of \code{mcc = 0} implies random performance; \code{mcc = 1} implies perfect performance.
#'
#'    See \href{https://en.wikipedia.org/wiki/Matthews_correlation_coefficient}{Wikipedia: Matthews correlation coefficient}
#'    for additional information.
#'
#'    \item \code{f1s}: The harmonic mean of the positive predictive value \code{\link{PPV}}
#'    (aka. \code{\link{precision}})
#'    and the sensitivity \code{\link{sens}} (aka. hit rate \code{\link{HR}},
#'    \code{\link{TPR}}, \code{\link{power}} or \code{\link{recall}}):
#'
#'    \code{f1s =  2 * (PPV * sens) / (PPV + sens)}
#'
#'    See \href{https://en.wikipedia.org/wiki/F1_score}{Wikipedia: F1 score} for additional information.
#'
#' }
#'
#' Note that some accuracy metrics can be interpreted
#' as probabilities (e.g., \code{acc}) or correlations (e.g., \code{mcc}).
#'
#'
#' @examples
#' accu <- comp_accu()  # => computes current accuracy information and saves results in accu
#' accu                 # => shows current accuracy information
#'
#' @family lists containing current scenario information
#' @family metrics
#'
#' @seealso
#' The corresponding generating function \code{\link{comp_accu}};
#' \code{\link{num}} for basic numeric parameters;
#' \code{\link{freq}} for current frequency information;
#' \code{\link{prob}} for current probability information;
#' \code{\link{txt}} for current text settings.
#'
#' @export

accu <- comp_accu()
# accu

## Check:

## -----------------------------------------------
## (2) Accuracy metrics from probabilities
## -----------------------------------------------
## (A) acc = overall accuracy

#' Compute overall accuracy (acc) from probabilities.
#'
#' \code{comp_acc} computes overall accuracy \code{\link{acc}}
#' from 3 essential probabilities
#' \code{\link{prev}}, \code{\link{sens}}, and \code{\link{spec}}.
#'
#' \code{comp_acc} uses probabilities (not frequencies) as
#' inputs and returns a proportion (probability)
#' without rounding.
#'
#' Definition: \code{acc} is the overall accuracy
#' as the proportion (or probability)
#' of correctly classifying cases or of \code{\link{dec.cor}} cases:
#'
#' \code{acc = dec.cor/N = (hi + cr)/(hi + mi + fa + cr)}
#'
#' Values range from 0 (no correct prediction) to 1 (perfect prediction).
#'
#' Importantly, correct decisions \code{\link{dec.cor}}
#' are not necessariliy positive decisions \code{\link{dec.pos}}.
#'
#'
#' @param prev The condition's prevalence \code{\link{prev}}
#' (i.e., the probability of condition being \code{TRUE}).
#'
#' @param sens The decision's sensitivity \code{\link{sens}}
#' (i.e., the conditional probability of a positive decision
#' provided that the condition is \code{TRUE}).
#'
#' @param spec The decision's specificity value \code{\link{spec}}
#' (i.e., the conditional probability
#' of a negative decision provided that the condition is \code{FALSE}).
#'
#'
#' @return Overall accuracy \code{\link{acc}} as a proportion (probability).
#' A warning is provided for NaN values.
#'
#' See \code{\link{comp_accu}} and \code{\link{accu}} for
#' accuracy metrics based on frequencies.
#'
#' @examples
#' # ways to work:
#' comp_acc(.10, .200, .300)  # => acc = 0.29
#' comp_acc(.50, .333, .666)  # => acc = 0.4995
#'
#' # watch out for vectors:
#' prev.range <- seq(0, 1, by = .1)
#' comp_acc(prev.range, .5, .5)  # => 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5
#'
#' # watch out for extreme values:
#' comp_acc(1, 1, 1)  #  => 1
#' comp_acc(1, 1, 0)  #  => 1
#'
#' comp_acc(1, 0, 1)  #  => 0
#' comp_acc(1, 0, 0)  #  => 0
#'
#' comp_acc(0, 1, 1)  #  => 1
#' comp_acc(0, 1, 0)  #  => 0
#'
#' comp_acc(0, 0, 1)  #  => 1
#' comp_acc(0, 0, 0)  #  => 0
#'
#'
#' @family functions computing probabilities
#' @family metrics
#'
#' @seealso
#' \code{\link{comp_sens}} and \code{\link{comp_PPV}} compute related probabilities;
#' \code{\link{is_extreme_prob_set}} verifies extreme cases;
#' \code{\link{comp_complement}} computes a probability's complement;
#' \code{\link{is_complement}} verifies probability complements;
#' \code{\link{comp_prob}} computes current probability information;
#' \code{\link{prob}} contains current probability information;
#' \code{\link{is_prob}} verifies probabilities.
#'
#' @export

comp_acc <- function(prev, sens, spec) {

  acc <- NA # initialize

  ## ToDo: Add condition
  ## if (is_valid_prob_set(prev, sens, mirt, spec, fart)) { ... }

  ## Definition: acc = dec.cor / N  =  (hi + cr) / (hi + mi + fa + cr)

  ## Computation:
  hi <- prev * sens
  mi <- prev * (1 - sens)
  cr <- (1 - prev) * spec
  fa <- (1 - prev) * (1 - spec)

  acc <- (hi + cr) / (hi + mi + fa + cr)

  ## Print a warning if NaN:
  if (any(is.nan(acc))) {
    warning("acc is NaN.")
  }

  return(acc)
}

## Check:
{
  # # Basics:
  # comp_acc(1, 1, 1)  #  => 1
  # comp_acc(1, 1, 0)  #  => 1
  #
  # comp_acc(1, 0, 1)  #  => 0
  # comp_acc(1, 0, 0)  #  => 0
  #
  # comp_acc(0, 1, 1)  #  => 1
  # comp_acc(0, 1, 0)  #  => 0
  #
  # comp_acc(0, 0, 1)  #  => 1
  # comp_acc(0, 0, 0)  #  => 0
  #
  # # Vectors:
  # prev.range <- seq(0, 1, by = .1)
  # comp_acc(prev.range, .5, .5)
}

## for extreme values:
## \code{\link{is_extreme_prob_set}} verifies extreme cases;

## -----------------------------------------------
## (+) ToDo:

## - Provide separate functions for other
##   common metrics (like wacc, mcc)
##   (for plotting curves and planes...).

## -----------------------------------------------
## eof.
