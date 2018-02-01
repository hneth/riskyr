## comp_accu.R | riskyr
## 2018 02 01
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
#' Current metrics include:
#'
#' \enumerate{
#'
#'    \item \code{acc}: Accuracy as the proportion or probability of cases correctly classified:
#'
#'    \code{acc = n.correct/N = (hi + cr)/(hi + mi + fa + cr)}.
#'
#'    \item \code{wacc}: Weighted accuracy, as a weighted average of the
#'    sensitivity \code{\link{sens}} (aka. hit rate \code{\link{HR}}, \code{\link{TPR}},
#'    \code{\link{power}} or \code{\link{recall}})
#'    and the the specificity \code{\link{spec}} (aka. \code{\link{TNR}})
#'    in which \code{\link{sens}} is multiplied by a weighting parameter \code{w}
#'    (ranging from 0 to 1) and  \code{\link{spec}} is multiplied by the complement
#'    \code{(1 - w)}:
#'
#'    \code{wacc = (w * sens) + ((1 - w) * spec)}.
#'
#'    \item \code{f1s}: The harmonic mean of the positive predictive value \code{\link{PPV}}
#'    (aka. \code{\link{precision}})
#'    and the sensitivity \code{\link{sens}} (aka. hit rate \code{\link{HR}},
#'    \code{\link{TPR}}, \code{\link{power}} or \code{\link{recall}}):
#'
#'    \code{f1s =  2 * (PPV * sens) / (PPV + sens)}.
#'
#'    Consult \href{https://en.wikipedia.org/wiki/F1_score}{Wikipedia: F1 score}
#' for additional information.
#'
#'    \item \code{mcc}: The Matthews correlation coefficient (with values ranging from -1 to +1):
#'
#'    \code{mcc = ((hi * cr) - (fa * mi)) / sqrt((hi + fa) * (hi + mi) * (cr + fa) * (cr + mi))}.
#'
#'    Consult \href{https://en.wikipedia.org/wiki/Matthews_correlation_coefficient}{Wikipedia: Matthews correlation coefficient}
#' for additional information.
#' }
#'
#'
#' @return A list \code{\link{accu}} containing all current accuracy metrics.
#'
#'
#' @examples
#' comp_accu(hi = 1, mi = 2, fa = 3, cr = 4)  # medium accuracy, but cr > hi.
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
  accu <- list("acc"  = NA,  # accuracy
               "w"    = w,   # weighting parameter w
               "wacc" = NA,  # weighted accuracy
               "f1s"  = NA,  # F1 score
               "mcc"  = NA   # MCC
  )

  ## (2) Compute combined frequencies from 4 essential frequencies:
  cond.true  <- (hi + mi)
  cond.false <- (fa + cr)
  N.cond     <- (cond.true + cond.false)

  dec.pos <- (hi + fa)
  dec.neg <- (mi + cr)
  N.dec   <- (dec.pos + dec.neg)

  n.correct   <- (hi + cr)
  n.incorrect <- (mi + fa)
  N.truth     <- (n.correct + n.incorrect)


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
  accu$acc  <- n.correct/N
  accu$wacc <- (w * sens) + ((1 - w) * spec)
  accu$f1s  <- 2 * (PPV * sens)/(PPV + sens)

  mcc_num  <- ((hi * cr) - (fa * mi))
  mcc_den  <- sqrt((hi + fa) * (hi + mi) * (cr + fa) * (cr + mi))
  if (mcc_den == 0) {
    mcc_den <- 1  # correction
    warning("accu$mcc: A denominator of 0 was corrected to 1, resulting in mcc = 0.")
  }
  accu$mcc  <- mcc_num/mcc_den


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
#' @examples
#' accu <- comp_accu()  # => computes current accuracy information and stores result in accu
#'
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

# +++ here now +++

## -----------------------------------------------
## (+) ToDo:

## - ...

## -----------------------------------------------
## eof.
