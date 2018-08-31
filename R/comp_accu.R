## comp_accu.R | riskyr
## 2018 08 31
## Compute accuracy metrics based on only
## - 4 essential frequencies of freq (hi mi fa cr), or
## - 3 essential probabilities of prob (prev, sens, spec)
## -----------------------------------------------

## Two ways:
## (1) Compute all accuracy metrics (from freq)
## (2) Accuracy metrics from probabilities

## (1) assumes that freq has been computed before.



## (A) Accuracy metrics for a classification result (i.e., based on freq) ------

## 1. ALL current accuracy metrics from 4 freq: ------

## Note: comp_accu_freq uses 4 freq as inputs and returns corresponding accuracy metrics.
##       If input freq were rounded, the values of accu may be imprecise.
##       Use comp_accu_prob (below) to get exact values from probability inputs.

## comp_accu_freq: Documentation --------

#' Compute accuracy metrics of current classification results.
#'
#' \code{comp_accu_freq} computes a list of current accuracy metrics
#' from the 4 essential frequencies (\code{\link{hi}},
#' \code{\link{mi}}, \code{\link{fa}}, \code{\link{cr}})
#' that constitute the current confusion matrix and
#' are contained in \code{\link{freq}}.
#'
#' Currently computed accuracy metrics include:
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
#' Notes:
#'
#' \itemize{
#'
#'    \item Accuracy metrics describe the \emph{correspondence} of decisions (or predictions) to actual conditions (or truth).
#'
#'    There are several possible interpretations of accuracy:
#'
#'    \enumerate{
#'
#'      \item as \emph{frequencies} (e.g., as classifying a population of \code{\link{N}}
#'      individuals into cases of \code{\link{dec.cor}} vs. \code{\link{dec.err}}),
#'
#'      \item as \emph{probabilities} (i.e., \code{acc} being the proportion of correct classifications or the ratio \code{\link{dec.cor}}/\code{\link{N}}),
#'
#'      \item as \emph{correlations} (e.g., see \code{mcc} in \code{\link{accu}}).
#'
#'    }
#'
#'    \item Computing exact accuracy values based on probabilities (by \code{\link{comp_accu_prob}}) may differ from
#'    accuracy values computed from (possibly rounded) frequencies (by \code{\link{comp_accu_freq}}).
#'
#'    When frequencies are rounded to integers (see the default of \code{round = TRUE}
#'    in \code{\link{comp_freq}} and \code{\link{comp_freq_prob}}) the accuracy metrics computed by
#'    \code{comp_accu_freq} correspond to these rounded values.
#'    Use \code{\link{comp_accu_prob}} to obtain exact accuracy metrics from probabilities.
#'
#'    }
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
#'
#' @examples
#' comp_accu_freq()  # => accuracy metrics for freq of current scenario
#' comp_accu_freq(hi = 1, mi = 2, fa = 3, cr = 4)  # medium accuracy, but cr > hi
#'
#' # Extreme cases:
#' comp_accu_freq(hi = 1, mi = 1, fa = 1, cr = 1)  # random performance
#' comp_accu_freq(hi = 1, mi = 0, fa = 0, cr = 1)  # perfect accuracy/optimal performance
#' comp_accu_freq(hi = 0, mi = 1, fa = 1, cr = 0)  # zero accuracy/worst performance, but see f1s
#' comp_accu_freq(hi = 1, mi = 0, fa = 0, cr = 0)  # perfect accuracy, but see wacc and mcc
#'
#' # Effects of w:
#' comp_accu_freq(hi = 3, mi = 2, fa = 1, cr = 4, w = 1/2)  # equal weights to sens and spec
#' comp_accu_freq(hi = 3, mi = 2, fa = 1, cr = 4, w = 2/3)  # more weight to sens
#' comp_accu_freq(hi = 3, mi = 2, fa = 1, cr = 4, w = 1/3)  # more weight to spec
#'
#' ## Contrasting comp_accu_freq and comp_accu_prob:
#' # (a) comp_accu_freq (based on rounded frequencies):
#' freq1 <- comp_freq(N = 10, prev = 1/3, sens = 2/3, spec = 3/4)   # => hi = 2, mi = 1, fa = 2, cr = 5
#' accu1 <- comp_accu_freq(freq1$hi, freq1$mi, freq1$fa, freq1$cr)  # => accu1 (based on rounded freq).
#' # accu1
#' #
#' # (b) comp_accu_prob (based on probabilities):
#' accu2 <- comp_accu_prob(prev = 1/3, sens = 2/3, spec = 3/4)      # => exact accu (based on prob).
#' # accu2
#' all.equal(accu1, accu2)  # => 4 differences!
#' #
#' # (c) comp_accu_freq (exact values, i.e., without rounding):
#' freq3 <- comp_freq(N = 10, prev = 1/3, sens = 2/3, spec = 3/4, round = FALSE)
#' accu3 <- comp_accu_freq(freq3$hi, freq3$mi, freq3$fa, freq3$cr)  # => accu3 (based on EXACT freq).
#' # accu3
#' all.equal(accu2, accu3)  # => TRUE (qed).
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
#' \code{\link{accu}} for all accuracy metrics;
#' \code{\link{comp_accu_prob}} computes exact accuracy metrics from probabilities;
#' \code{\link{num}} for basic numeric parameters;
#' \code{\link{freq}} for current frequency information;
#' \code{\link{txt}} for current text settings;
#' \code{\link{pal}} for current color settings;
#' \code{\link{popu}} for a table of the current population.
#'
#' @export

## comp_accu_freq: Definition --------

comp_accu_freq <- function(hi = freq$hi, mi = freq$mi,  # 4 essential frequencies
                           fa = freq$fa, cr = freq$cr,
                           w = .50  # weight for wacc (from 0 to 1). Default: w = .50 (aka. bacc).
) {

  ## (1) Verify w:
  if (!is_prob(w)) {
    warning("The weighting parameter w (for wacc) must range from 0 to 1.")
  }

  ## (2) Initialize accu list: # Metric:
  accu <- list("acc"  = NA,    # 1. overall accuracy
               "w"    = w,     #    weighting parameter w
               "wacc" = NA,    # 2. weighted/balanced accuracy
               "mcc"  = NA,    # 3. MCC
               "f1s"  = NA     # 4. F1 score
  )

  ## (3) Compute combined frequencies from 4 essential frequencies:

  ## (a) by condition (columns of confusion matrix):
  cond.true  <- (hi + mi)
  cond.false <- (fa + cr)
  N.cond     <- (cond.true + cond.false)

  ## (b) by decision (rows of confusion matrix):
  dec.pos <- (hi + fa)  # positive decisions
  dec.neg <- (mi + cr)
  N.dec   <- (dec.pos + dec.neg)

  ## (c) by truth/correctness of decision (diagonals of confusion matrix):
  dec.cor <- (hi + cr)  # correct decisions
  dec.err <- (mi + fa)  # erroneous decisions
  N.truth <- (dec.cor + dec.err)

  ## Check:
  if ((N.cond != N.dec) || (N.cond != N.truth))  {
    warning("A violation of commutativity occurred: 4 basic frequencies do not add up to N.")
  } else {
    N <- N.cond
  }

  ## (4) Compute auxiliary values (used below):

  sens <- hi/cond.true  # conditional probabilities 1
  spec <- cr/cond.false

  PPV <- hi/dec.pos    # conditional probabilities 2
  # NPV <- cr/dec.neg  # (not needed here)


  ## (5) Compute current accuracy metrics:

  # 1. Overall accuracy (acc) / proportion correct:
  accu$acc  <- dec.cor/N

  # 2. Weighted/balanced accuracy (wacc/bacc):
  accu$wacc <- (w * sens) + ((1 - w) * spec)

  # 3. Matthews correlation coefficient (mcc):
  mcc_num  <- ((hi * cr) - (fa * mi))
  mcc_den  <- sqrt((hi + fa) * (hi + mi) * (cr + fa) * (cr + mi))
  if (mcc_den == 0) {
    mcc_den <- 1  # correction
    warning("accu$mcc: A denominator of 0 was corrected to 1, resulting in mcc = 0.")
  }
  accu$mcc  <- mcc_num/mcc_den

  # 4. F1Score (f1s): Harmonic mean of PPV (precision) and sens (recall):
  accu$f1s  <- 2 * (PPV * sens)/(PPV + sens)


  ## (5) Return the entire list accu:
  return(accu)

}

## Check: ------
# comp_accu_freq(hi = 1, mi = 2, fa = 3, cr = 4)  # medium accuracy, but cr > hi.
#
# # Extreme cases:
# comp_accu_freq(hi = 1, mi = 1, fa = 1, cr = 1)  # random performance
# comp_accu_freq(hi = 1, mi = 0, fa = 0, cr = 1)  # perfect accuracy/optimal prediction performance
# comp_accu_freq(hi = 0, mi = 1, fa = 1, cr = 0)  # zero accuracy/worst prediction performance, but see f1s
# comp_accu_freq(hi = 1, mi = 0, fa = 0, cr = 0)  # perfect accuracy, but see wacc (NaN) and mcc (corrected)
#
# # Effects of w:
# comp_accu_freq(hi = 3, mi = 2, fa = 1, cr = 4, w = 1/2)  # equal weights to sens and spec
# comp_accu_freq(hi = 3, mi = 2, fa = 1, cr = 4, w = 2/3)  # more weight to sens
# comp_accu_freq(hi = 3, mi = 2, fa = 1, cr = 4, w = 1/3)  # more weight to spec



## 2. comp_accu: Wrapper function for comp_accu_freq (above) -------

comp_accu <- function(hi = freq$hi, mi = freq$mi,  # 4 essential frequencies
                      fa = freq$fa, cr = freq$cr,
                      w = .50  # weight for wacc (from 0 to 1). Default: w = .50 (aka. bacc).
) {

  ## Pass parameters to comp_accu_freq (above):
  comp_accu_freq(hi, mi, fa, cr, w)

}

## Check: ------
# comp_accu(hi = 1, mi = 2, fa = 3, cr = 4)  # medium accuracy, but cr > hi.



## 3. Individual accuracy metrics from 4 freq: --------

## yet ToDo (but included in comp_accu above).


## (B) Accuracy metrics based on probabilities (without rounding) : ----------


## 1. ALL accuracy metrics from 3 prob: --------

## comp_accu_prob: Documentation --------

#' Compute exact accuracy metrics based on probabilities.
#'
#' \code{comp_accu_prob} computes a list of exact accuracy metrics
#' from a sufficient and valid set of 3 essential probabilities
#' (\code{\link{prev}}, and
#' \code{\link{sens}} or its complement \code{\link{mirt}}, and
#' \code{\link{spec}} or its complement \code{\link{fart}}).
#'
#' Currently computed accuracy metrics include:
#'
#' \enumerate{
#'
#'    \item \code{acc}: Overall accuracy as the proportion (or probability)
#'    of correctly classifying cases or of \code{\link{dec.cor}} cases:
#'
#'    (a) from \code{freq}: \code{acc = dec.cor/N = (hi + cr)/(hi + mi + fa + cr)}
#'
#'    (b) from \code{prob}: \code{acc = (prev x sens) + [(1 - prev) x spec]}
#'
#'    Values range from 0 (no correct prediction) to 1 (perfect prediction).
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
#' as probabilities (e.g., \code{acc}) and some as correlations (e.g., \code{mcc}).
#'
#' Also, accuracy can be viewd as a probability (e.g., the ratio of or link between
#' \code{\link{dec.cor}} and \code{\link{N}}) or as a frequency type
#' (containing \code{\link{dec.cor}} and \code{\link{dec.err}}).
#'
#' \code{comp_accu_prob} computes exact accuracy metrics from probabilities.
#' When input frequencies were rounded (see the default of \code{round = TRUE}
#' in \code{\link{comp_freq}} and \code{\link{comp_freq_prob}}) the accuracy
#' metrics computed by \code{comp_accu} correspond these rounded values.
#'
#' @return A list \code{\link{accu}} containing current accuracy metrics.
#'
#' @param prev The condition's prevalence \code{\link{prev}}
#' (i.e., the probability of condition being \code{TRUE}).
#'
#' @param sens The decision's sensitivity \code{\link{sens}}
#' (i.e., the conditional probability of a positive decision
#' provided that the condition is \code{TRUE}).
#' \code{sens} is optional when its complement \code{mirt} is provided.
#'
#' @param mirt The decision's miss rate \code{\link{mirt}}
#' (i.e., the conditional probability of a negative decision
#' provided that the condition is \code{TRUE}).
#' \code{mirt} is optional when its complement \code{sens} is provided.
#'
#' @param spec The decision's specificity value \code{\link{spec}}
#' (i.e., the conditional probability
#' of a negative decision provided that the condition is \code{FALSE}).
#' \code{spec} is optional when its complement \code{fart} is provided.
#'
#' @param fart The decision's false alarm rate \code{\link{fart}}
#' (i.e., the conditional probability
#' of a positive decision provided that the condition is \code{FALSE}).
#' \code{fart} is optional when its complement \code{spec} is provided.
#'
#' @param tol A numeric tolerance value for \code{\link{is_complement}}.
#' Default: \code{tol = .01}.
#'
#' @param w   The weighting parameter \code{w} (from 0 to 1)
#' for computing weighted accuracy \code{wacc}.
#' Default: \code{w = .50} (i.e., yielding balanced accuracy \code{bacc}).
#'
#' Notes:
#'
#' \itemize{
#'
#'    \item Accuracy metrics describe the \emph{correspondence} of decisions (or predictions) to actual conditions (or truth).
#'
#'    There are several possible interpretations of accuracy:
#'
#'    \enumerate{
#'
#'      \item as \emph{frequencies} (e.g., as classifying a population of \code{\link{N}}
#'      individuals into cases of \code{\link{dec.cor}} vs. \code{\link{dec.err}}),
#'
#'      \item as \emph{probabilities} (i.e., \code{acc} being the proportion of correct classifications or the ratio \code{\link{dec.cor}}/\code{\link{N}}),
#'
#'      \item as \emph{correlations} (e.g., see \code{mcc} in \code{\link{accu}}).
#'
#'    }
#'
#'    \item Computing exact accuracy values based on probabilities (by \code{\link{comp_accu_prob}}) may differ from
#'    accuracy values computed from (possibly rounded) frequencies (by \code{\link{comp_accu_freq}}).
#'
#'    When frequencies are rounded to integers (see the default of \code{round = TRUE}
#'    in \code{\link{comp_freq}} and \code{\link{comp_freq_prob}}) the accuracy metrics computed by
#'    \code{comp_accu_freq} correspond to these rounded values.
#'    Use \code{\link{comp_accu_prob}} to obtain exact accuracy metrics from probabilities.
#'
#'    }
#'
#' @examples
#' comp_accu_prob()  # => accuracy metrics for prob of current scenario
#' comp_accu_prob(prev = .2, sens = .5, spec = .5)  # medium accuracy, but cr > hi.
#'
#' ## Extreme cases:
#' comp_accu_prob(prev = .5, sens = .5, spec = .5)  # random performance
#' comp_accu_prob(prev = .5, sens = 1,  spec = 1)   # perfect accuracy
#' comp_accu_prob(prev = .5, sens = 0,  spec = 0)   # zero accuracy, see f1s
#' comp_accu_prob(prev = 1,  sens = 1,  spec = 0)   # perfect, but see wacc (0.5) and mcc (0)
#'
#' ## Effects of w:
#' comp_accu_prob(prev = .5, sens = .6, spec = .4, w = 1/2)  # equal weights to sens and spec
#' comp_accu_prob(prev = .5, sens = .6, spec = .4, w = 2/3)  # more weight to sens
#' comp_accu_prob(prev = .5, sens = .6, spec = .4, w = 1/3)  # more weight to spec
#'
#' ## Contrasting comp_accu_freq and comp_accu_prob:
#' # (a) comp_accu_freq (based on rounded frequencies):
#' freq1 <- comp_freq(N = 10, prev = 1/3, sens = 2/3, spec = 3/4)   # => rounded frequencies!
#' accu1 <- comp_accu_freq(freq1$hi, freq1$mi, freq1$fa, freq1$cr)  # => accu1 (based on rounded freq).
#' # accu1
#' #
#' # (b) comp_accu_prob (based on probabilities):
#' accu2 <- comp_accu_prob(prev = 1/3, sens = 2/3, spec = 3/4)      # => exact accu (based on prob).
#' # accu2
#' all.equal(accu1, accu2)  # => 4 differences!
#' #
#' # (c) comp_accu_freq (exact values, i.e., without rounding):
#' freq3 <- comp_freq(N = 10, prev = 1/3, sens = 2/3, spec = 3/4, round = FALSE)
#' accu3 <- comp_accu_freq(freq3$hi, freq3$mi, freq3$fa, freq3$cr)  # => accu3 (based on EXACT freq).
#' # accu3
#' all.equal(accu2, accu3)  # => TRUE (qed).
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
#' \code{\link{accu}} for all accuracy metrics;
#' \code{\link{comp_accu_freq}} computes accuracy metrics from frequencies;
#' \code{\link{num}} for basic numeric parameters;
#' \code{\link{freq}} for current frequency information;
#' \code{\link{txt}} for current text settings;
#' \code{\link{pal}} for current color settings;
#' \code{\link{popu}} for a table of the current population.
#'
#' @export

## comp_accu_prob: Definition --------

comp_accu_prob <- function(prev = prob$prev,  # 3 essential probabilities (removed: mirt & fart):
                           sens = prob$sens, mirt = NA, # using current probability info contained in prob!
                           spec = prob$spec, fart = NA,
                           tol = .01,         # tolerance for is_complement and N = 1 (below).
                           w = .50  # weight for wacc (from 0 to 1). Default: w = .50 (aka. bacc).
) {

  ## (A) If a valid set of probabilities was provided:
  if (is_valid_prob_set(prev = prev, sens = sens, mirt = mirt, spec = spec, fart = fart, tol = tol)) {

    ## (0) Compute the complete quintet of probabilities:
    prob_quintet <- comp_complete_prob_set(prev, sens, mirt, spec, fart)
    sens <- prob_quintet[2]  # gets sens (if not provided)
    mirt <- prob_quintet[3]  # gets mirt (if not provided)
    spec <- prob_quintet[4]  # gets spec (if not provided)
    fart <- prob_quintet[5]  # gets fart (if not provided)

    ## (1) Verify w:
    if (!is_prob(w)) {
      warning("The weighting parameter w (for wacc) must range from 0 to 1.")
    }

    ## (2) Initialize accu list: # Metric:
    accu <- list("acc"  = NA,    # 1. overall accuracy
                 "w"    = w,     #    weighting parameter w
                 "wacc" = NA,    # 2. weighted/balanced accuracy
                 "mcc"  = NA,    # 3. MCC
                 "f1s"  = NA     # 4. F1 score
    )

    ## (3) Computation of 4 freq (as probabilities without rounding):
    hi <- prev * sens
    mi <- prev * (1 - sens)
    cr <- (1 - prev) * spec
    fa <- (1 - prev) * (1 - spec)

    N <- (hi + mi + cr + fa)  ## should be 1
    if (abs(N - 1) > tol) {   ## Check integrity of sum:
      warning("The 4 freq (as probabilities) should add up to 1.")
    }

    ## (4) Compute some auxiliary values (used below):

    dec.cor <- (hi + cr)  # correct decisions
    dec.pos <- (hi + fa)  # positive decisions
    PPV <- hi/dec.pos     # conditional probabilities 2

    ## (5) Compute current accuracy metrics:

    # 1. Overall accuracy (acc) / proportion correct:
    accu$acc  <- dec.cor/N

    # 2. Weighted/balanced accuracy (wacc/bacc):
    accu$wacc <- (w * sens) + ((1 - w) * spec)

    # 3. Matthews correlation coefficient (mcc):
    mcc_num  <- ((hi * cr) - (fa * mi))
    mcc_den  <- sqrt((hi + fa) * (hi + mi) * (cr + fa) * (cr + mi))
    if (mcc_den == 0) {
      mcc_den <- 1  # correction
      warning("accu$mcc: A denominator of 0 was corrected to 1, resulting in mcc = 0.")
    }
    accu$mcc  <- mcc_num/mcc_den

    # 4. F1Score (f1s): Harmonic mean of PPV (precision) and sens (recall):
    accu$f1s  <- 2 * (PPV * sens)/(PPV + sens)


    ## (5) Return the entire list accu:
    return(accu)

  }

  else { # (B) NO valid set of probabilities was provided:

    warning("Please enter a valid set of essential probabilities.")

  }

}

## Check: ------
# comp_accu_prob()  # => accuracy metrics for prob of current scenario
# comp_accu_prob(prev = .2, sens = .5, spec = .5)  # medium accuracy, but cr > hi.
#
# ## Extreme cases:
# comp_accu_prob(prev = .5, sens = .5, spec = .5)  # random performance
# comp_accu_prob(prev = .5, sens = 1,  spec = 1)   # perfect accuracy
# comp_accu_prob(prev = .5, sens = 0,  spec = 0)   # zero accuracy, see f1s
# comp_accu_prob(prev = 1,  sens = 1,  spec = 0)   # perfect, but see wacc (0.5) and mcc (0)
#
# ## Effects of w:
# comp_accu_prob(prev = .5, sens = .6, spec = .4, w = 1/2)  # equal weights to sens and spec
# comp_accu_prob(prev = .5, sens = .6, spec = .4, w = 2/3)  # more weight to sens
# comp_accu_prob(prev = .5, sens = .6, spec = .4, w = 1/3)  # more weight to spec
#
# ## Comparing comp_accu_prob with comp_accu (based on freq):
# # (a) comp_accu_freq (based on rounded frequencies):
# freq1 <- comp_freq(N = 10, prev = 1/3, sens = 2/3, spec = 3/4)   # => rounded frequencies!
# accu1 <- comp_accu_freq(freq1$hi, freq1$mi, freq1$fa, freq1$cr)  # => accu1 (based on rounded freq).
# # accu1
# #
# # (b) comp_accu_prob (based on probabilities):
# accu2 <- comp_accu_prob(prev = 1/3, sens = 2/3, spec = 3/4)      # => exact accu (based on prob).
# # accu2
# all.equal(accu1, accu2)  # => 4 differences!
# #
# # (c) comp_accu_freq (exact values, i.e., without rounding):
# freq3 <- comp_freq(N = 10, prev = 1/3, sens = 2/3, spec = 3/4, round = FALSE)
# accu3 <- comp_accu_freq(freq3$hi, freq3$mi, freq3$fa, freq3$cr)  # => accu3 (based on EXACT freq).
# # accu3
# all.equal(accu2, accu3)  # => TRUE (qed).


## 2. Individual accuracy metrics from 3 prob: --------

## yet ToDo (but included in comp_accu_prob above).

## Note: The following functions compute individual metrics (i.e., 1 value)
##       and use probabilities (prob) as inputs to return values to avoid rounding!

## a. Overall accuracy (acc) from 3 prob: -------

## comp_acc: Documentation --------

#' Compute overall accuracy (acc) from probabilities.
#'
#' \code{comp_acc} computes overall accuracy \code{acc}
#' from 3 essential probabilities
#' \code{\link{prev}}, \code{\link{sens}}, and \code{\link{spec}}.
#'
#' \code{comp_acc} uses probabilities (not frequencies) as
#' inputs and returns an exact proportion (probability)
#' without rounding.
#'
#' Definition: \code{acc} is the overall accuracy
#' as the proportion (or probability)
#' of correctly classifying cases or of \code{\link{dec.cor}} cases:
#'
#'  (a) from \code{freq}: \code{acc = dec.cor/N = (hi + cr)/(hi + mi + fa + cr)}
#'
#'  (b) from \code{prob}: \code{acc = (prev x sens) + [(1 - prev) x spec]}
#'
#' Values range from 0 (no correct prediction) to 1 (perfect prediction).
#'
#' Importantly, correct decisions \code{\link{dec.cor}}
#' are not necessarily positive decisions \code{\link{dec.pos}}.
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
#' @return Overall accuracy \code{acc} as a proportion (probability).
#' A warning is provided for NaN values.
#'
#' See \code{\link{accu}} for other accuracy metrics.
#' \code{\link{comp_accu_freq}} and \code{\link{comp_accu_prob}}
#' compute accuracy metrics from frequencies and probabilities.
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
#' @family functions computing probabilities
#' @family metrics
#'
#' @seealso
#' \code{\link{accu}} for all accuracy metrics;
#' \code{\link{comp_accu_freq}} computes accuracy metrics from frequencies;
#' \code{\link{comp_accu_prob}} computes exact accuracy metrics from probabilities;
#' \code{\link{comp_sens}} and \code{\link{comp_PPV}} compute related probabilities;
#' \code{\link{is_extreme_prob_set}} verifies extreme cases;
#' \code{\link{comp_complement}} computes a probability's complement;
#' \code{\link{is_complement}} verifies probability complements;
#' \code{\link{comp_prob}} computes current probability information;
#' \code{\link{prob}} contains current probability information;
#' \code{\link{is_prob}} verifies probabilities.
#'
#' @export

## comp_acc: Definition --------

comp_acc <- function(prev, sens, spec) {

  acc <- NA  # initialize

  ## ToDo: Add condition
  ## if (is_valid_prob_set(prev, sens, mirt, spec, fart)) { ... }

  ## Definition: acc = dec.cor / N  =  (hi + cr) / (hi + mi + fa + cr)

  ## Computation of 4 freq (without rounding):
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

## Check: ----

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

## for extreme values:
## \code{\link{is_extreme_prob_set}} verifies extreme cases;






## (C). Apply comp_accu_prob to initialize accu (as a list): ------

## accu: List containing current accuracy information ------

#' A list containing current accuracy information.
#'
#' \code{accu} contains current accuracy information
#' returned by the corresponding generating function
#' \code{\link{comp_accu_prob}}.
#'
#' Current metrics include:
#'
#' \enumerate{
#'
#'    \item \code{acc}: Overall accuracy as the proportion (or probability)
#'    of correctly classifying cases or of \code{\link{dec.cor}} cases:
#'
#'    (a) from \code{freq}: \code{acc = dec.cor/N = (hi + cr)/(hi + mi + fa + cr)}
#'
#'    (b) from \code{prob}: \code{acc = (prev x sens) + [(1 - prev) x spec]}
#'
#'    Values range from 0 (no correct prediction) to 1 (perfect prediction).
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
#' Notes:
#'
#' \itemize{
#'
#'    \item Accuracy metrics describe the \emph{correspondence} of decisions (or predictions) to actual conditions (or truth).
#'
#'    There are several possible interpretations of accuracy:
#'
#'    \enumerate{
#'
#'      \item as \emph{frequencies} (e.g., as classifying a population of \code{\link{N}}
#'      individuals into cases of \code{\link{dec.cor}} vs. \code{\link{dec.err}}),
#'
#'      \item as \emph{probabilities} (i.e., \code{acc} being the proportion of correct classifications,
#'      or the ratio \code{\link{dec.cor}}/\code{\link{N}}),
#'
#'      \item as \emph{correlations} (e.g., see \code{mcc} in \code{\link{accu}}).
#'
#'    }
#'
#'    \item Computing exact accuracy values based on probabilities (by \code{\link{comp_accu_prob}}) may differ from
#'    accuracy values computed from (possibly rounded) frequencies (by \code{\link{comp_accu_freq}}).
#'
#'    When frequencies are rounded to integers (see the default of \code{round = TRUE}
#'    in \code{\link{comp_freq}} and \code{\link{comp_freq_prob}}) the accuracy metrics computed by
#'    \code{comp_accu_freq} correspond to these rounded values.
#'    Use \code{\link{comp_accu_prob}} to obtain exact accuracy metrics from probabilities.
#'
#'    }
#'
#' @examples
#' accu <- comp_accu_prob()  # => compute exact accuracy metrics (from probabilities)
#' accu                      # => current accuracy information
#'
#' ## Contrasting comp_accu_freq and comp_accu_prob:
#' # (a) comp_accu_freq (based on rounded frequencies):
#' freq1 <- comp_freq(N = 10, prev = 1/3, sens = 2/3, spec = 3/4)   # => rounded frequencies!
#' accu1 <- comp_accu_freq(freq1$hi, freq1$mi, freq1$fa, freq1$cr)  # => accu1 (based on rounded freq).
#' # accu1
#' #
#' # (b) comp_accu_prob (based on probabilities):
#' accu2 <- comp_accu_prob(prev = 1/3, sens = 2/3, spec = 3/4)      # => exact accu (based on prob).
#' # accu2
#' all.equal(accu1, accu2)  # => 4 differences!
#' #
#' # (c) comp_accu_freq (exact values, i.e., without rounding):
#' freq3 <- comp_freq(N = 10, prev = 1/3, sens = 2/3, spec = 3/4, round = FALSE)
#' accu3 <- comp_accu_freq(freq3$hi, freq3$mi, freq3$fa, freq3$cr)  # => accu3 (based on EXACT freq).
#' # accu3
#' all.equal(accu2, accu3)  # => TRUE (qed).
#'
#'
#' @family lists containing current scenario information
#' @family metrics
#'
#' @seealso
#' The corresponding generating function \code{\link{comp_accu_prob}} computes exact accuracy metrics from probabilities;
#' \code{\link{accu}} for all accuracy metrics;
#' \code{\link{comp_accu_freq}} computes accuracy metrics from frequencies;
#' \code{\link{num}} for basic numeric parameters;
#' \code{\link{freq}} for current frequency information;
#' \code{\link{prob}} for current probability information;
#' \code{\link{txt}} for current text settings.
#'
#' @export

accu <- comp_accu_prob()

## Check: --------
# accu



## (*) Done: ----------

## - Added comp_accu_prob to compute exact accuracy from prob.
## - Clean up code.  [2018 08 30]

## (+) ToDo: ----------

## - Provide separate functions for other common metrics (like wacc, mcc)
##   (for plotting curves and planes...).

## eof. ------------------------------------------
