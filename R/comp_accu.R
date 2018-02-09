## comp_accu.R | riskyr
## 2018 02 09
## -----------------------------------------------
## Compute accuracy metrics
## based on only the 4 essential frequencies
## of freq (hi mi fa cr).
## -----------------------------------------------
## Notes:

## Assumes that freq has been computed before.

## -----------------------------------------------
## (1) Compute accuracy metrics for current
## classification result.

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
#'    of correctly classifying cases:
#'
#'    \code{acc = n.correct/N = (hi + cr)/(hi + mi + fa + cr)}
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
#'
#' @examples
#' comp_accu()  # => computes accuracy metrics for current default scenario
#' comp_accu(hi = 1, mi = 2, fa = 3, cr = 4)  # medium accuracy, but cr > hi
#'
#' # Extreme cases:
#' comp_accu(hi = 1, mi = 1, fa = 1, cr = 1)  # random performance
#' comp_accu(hi = 1, mi = 0, fa = 0, cr = 1)  # perfect accuracy/optimal prediction performance
#' comp_accu(hi = 0, mi = 1, fa = 1, cr = 0)  # zero accuracy/worst prediction performance, but see f1s
#' comp_accu(hi = 1, mi = 0, fa = 0, cr = 0)  # perfect accuracy, but see wacc and mcc (corrected)
#'
#' # Effects of w:
#' comp_accu(hi = 3, mi = 2, fa = 1, cr = 4, w = 1/2)  # equal weights to sens and spec
#' comp_accu(hi = 3, mi = 2, fa = 1, cr = 4, w = 2/3)  # more weight to sens
#' comp_accu(hi = 3, mi = 2, fa = 1, cr = 4, w = 1/3)  # more weight to spec
#'
#'
#' @references
#' Consult \href{https://en.wikipedia.org/wiki/Confusion_matrix}{Wikipedia: Confusion matrix} for additional information.
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
## (2) Apply to initialize accu (as a list):

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
#'    of correctly classifying cases:
#'
#'    \code{acc = n.correct/N = (hi + cr)/(hi + mi + fa + cr)}
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
## (+) ToDo:

## - Provide separate functions for most
##   common metrics (like acc, wacc, mcc)
##   (to use for matrices of 3d planes...).

## -----------------------------------------------
## eof.
