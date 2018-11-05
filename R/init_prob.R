## init_prob.R | riskyr
## 2018 10 27
## Define and initialize ALL probabilities
## -----------------------------------------------

## Table of current terminology: -----------------

# Probabilities (13+):              Frequencies (11):
# -------------------               ------------------
# (A) by condition:

# non-conditional:                          N
# prev*                           cond.true | cond.false (columns)

# conditional:
# sens* = hit rate = TPR                hi* = TP
# mirt  = miss rate = FNR               mi* = FN
# fart  = false alarm rate = FPR        fa* = FP
# spec* = true negative rate = TNR      cr* = TN

# [Note: *...is essential]


# (B) by decision:                 Combined frequencies:

# non-conditional:
# ppod = proportion of dec.pos     dec.pos | dec.neg (rows)
#                                  dec.cor | dec.err (diagonal)

# conditional:
# PPV = precision
# FDR = false detection rate
# FOR = false omission rate
# NPV = neg. pred. value

# (C) by accuracy/correspondence of decision to condition (see accu):

# acc  = overall accuracy (probability/proportion correct decision)
# p_acc_hi = p(hi|acc)  # aka. acc-hi  "p(hi | dec.cor)"
# p_err_fa = p(fa|err)  # aka. err-fa  "p(fa | dec.err)"

# Other measures of accuracy (in accu):
# wacc = weighted accuracy
# mcc  = Matthews correlation coefficient
# f1s  = harmonic mean of PPV and sens

# err = error rate = (1 - acc)



## Data flow: Two basic directions: --------------

## (1) Probabilities ==> frequencies:
##     Bayesian: based on 3 essential probabilities:
##   - given:   prev;  sens, spec
##   - derived: all other values

## (2) Frequencies ==> probabilities:
##     Frequentist: based on 4 essential natural frequencies:
##   - given:   N = hi, mi, fa, cr
##   - derived: all other values


## A: Define probabilities by condition: ---------

## ***: 3 essential probabilities: prev; sens, spec

## (0) prev*** = base rate of condition: ---------

#' The prevalence (baseline probability) of a condition.
#'
#' \code{prev} defines a condition's prevalence value
#' (or baseline probability):
#' The probability of the condition being \code{TRUE}.
#'
#' Understanding or obtaining the prevalence value \code{prev}:
#'
#' \itemize{
#'
#'   \item Definition:
#'   \code{prev} is the (non-conditional) probability:
#'
#'   \code{prev = p(condition = TRUE)}
#'
#'   or the base rate (or baseline probability)
#'   of the condition's occurrence.
#'
#'
#'   \item In terms of frequencies,
#'   \code{prev} is the ratio of
#'   \code{\link{cond.true}} (i.e., \code{\link{hi} + \link{mi}})
#'   divided by \code{\link{N}} (i.e.,
#'   \code{\link{hi} + \link{mi}} + \code{\link{fa} + \link{cr}}):
#'
#'   \code{prev = cond.true/N = (hi + mi)/(hi + mi + fa + cr)}
#'
#'
#'   \item Perspective:
#'   \code{prev} classifies a population of \code{\link{N}} individuals
#'   by condition (\code{prev = cond.true/N}).
#'
#'   \code{prev} is the "by condition" counterpart to \code{\link{ppod}}
#'   (which adopts a "by decision" perspective).
#'
#'
#'   \item Alternative names:
#'   base rate of condition,
#'   proportion affected,
#'   rate of condition \code{= TRUE} cases
#'
#'   \code{prev} is often distinguished from the \emph{incidence rate}
#'   (i.e., the rate of new cases within a certain time period).
#'
#'
#'   \item Dependencies:
#'   \code{prev} is a feature of the population
#'   and condition, but independent of the decision process
#'   or diagnostic procedure.
#'
#'   The value of \code{prev} does \emph{not} depend
#'   on features of the decision process or diagnostic procedure.
#'   However, \code{prev} must be taken into account when
#'   computing the conditional probabilities
#'   \code{\link{sens}}, \code{\link{mirt}},
#'   \code{\link{spec}}, \code{\link{fart}},
#'   \code{\link{PPV}}, and \code{\link{NPV}}
#'   (as they partly depend on \code{prev}).
#'
#' }
#'
#' @aliases
#' baserate_cond.true
#'
#' @examples
#' prev <- .10     # => sets a prevalence value of 10%
#' prev <- 10/100  # => (condition = TRUE) for 10 out of 100 individuals
#' is_prob(prev)   # => TRUE (as prev is a probability)
#'
#' @family probabilities
#' @family essential parameters
#'
#' @seealso
#' \code{\link{num}} contains basic numeric variables;
#' \code{\link{init_num}} initializes basic numeric variables;
#' \code{\link{comp_prob}} computes derived probabilities;
#' \code{\link{comp_freq}} computes natural frequencies from probabilities;
#' \code{\link{is_prob}} verifies probability inputs.
#'
#' @references
#' Consult \href{https://en.wikipedia.org/wiki/Prevalence}{Wikipedia} for additional information.

prev <- 1/2  # default prevalence


## (1) sens*** = TPR: ----------------------------

#' The sensitivity (or hit rate) of a decision process or diagnostic procedure.
#'
#' \code{sens} defines a decision's sensitivity (or hit rate) value:
#' The conditional probability of the decision being positive
#' if the condition is \code{TRUE}.
#'
#' Understanding or obtaining the sensitivity \code{sens}
#' (or hit rate \code{HR}):
#'
#' \itemize{
#'
#'   \item Definition: \code{sens} is the conditional probability
#'   for a (correct) positive decision given that
#'   the condition is \code{TRUE}:
#'
#'   \code{sens = p(decision = positive | condition = TRUE)}
#'
#'   or the probability of correctly detecting true cases
#'   (\code{condition = TRUE}).
#'
#'
#'   \item Perspective:
#'   \code{sens} further classifies
#'   the subset of \code{\link{cond.true}} individuals
#'   by decision (\code{sens = hi/cond.true}).
#'
#'
#'   \item Alternative names:
#'   true positive rate (\code{TPR}),
#'   hit rate (\code{HR}),
#'   probability of detection,
#'   \code{power = 1 - beta},
#'   \code{recall}

#'
#'   \item Relationships:
#'
#'   a. \code{sens} is the complement of the miss rate
#'   \code{\link{mirt}} (aka. false negative rate \code{FNR} or the
#'   rate of Type-II errors):
#'
#'   \code{sens = (1 - miss rate) = (1 - FNR)}
#'
#'   b. \code{sens} is the opposite conditional probability
#'   -- but not the complement --
#'   of the positive predictive value \code{\link{PPV}}:
#'
#'   \code{PPV = p(condition = TRUE | decision = positive)}

#'
#'   \item In terms of frequencies,
#'   \code{sens} is the ratio of
#'   \code{\link{hi}} divided by
#'   \code{\link{cond.true}} (i.e., \code{\link{hi} + \link{mi}}):
#'
#'   \code{sens = hi/cond.true = hi/(hi + mi)}
#'
#'
#'   \item Dependencies:
#'   \code{sens} is a feature of a decision process
#'   or diagnostic procedure and a measure of
#'   correct decisions (true positives).
#'
#'   Due to being a conditional probability,
#'   the value of \code{sens} is not intrinsic to
#'   the decision process, but also depends on the
#'   condition's prevalence value \code{\link{prev}}.
#'
#' }
#'
#' @aliases
#' TPR HR power recall
#'
#' @references
#' Consult \href{https://en.wikipedia.org/wiki/Sensitivity_and_specificity}{Wikipedia} for additional information.
#'
#' @family probabilities
#' @family essential parameters
#'
#' @seealso
#' \code{\link{comp_sens}} computes \code{sens} as the complement of \code{\link{mirt}};
#' \code{\link{prob}} contains current probability information;
#' \code{\link{comp_prob}} computes current probability information;
#' \code{\link{num}} contains basic numeric parameters;
#' \code{\link{init_num}} initializes basic numeric parameters;
#' \code{\link{comp_freq}} computes current frequency information;
#' \code{\link{is_prob}} verifies probability inputs.
#'
#' @examples
#' sens <- .85     # => sets a sensitivity value of 85%
#' sens <- 85/100  # => (decision = positive) for 85 out of 100 people with (condition = TRUE)
#' is_prob(sens)   # => TRUE (as sens is a probability)

sens <- 1/2  # default sensitivity


## (2) mirt = FNR: -------------------------------

#' The miss rate of a decision process or diagnostic procedure.
#'
#' \code{mirt} defines a decision's miss rate value:
#' The conditional probability of the decision being negative
#' if the condition is \code{TRUE}.
#'
#' Understanding or obtaining the miss rate \code{mirt}:
#'
#' \itemize{
#'
#'   \item Definition: \code{sens} is the conditional probability
#'   for an incorrect negative decision given that
#'   the condition is \code{TRUE}:
#'
#'   \code{mirt = p(decision = negative | condition = TRUE)}
#'
#'   or the probability of failing to detect true cases
#'   (\code{condition = TRUE}).
#'

#'   \item Perspective:
#'   \code{mirt} further classifies
#'   the subset of \code{\link{cond.true}} individuals
#'   by decision (\code{mirt = mi/cond.true}).
#'
#'
#'   \item Alternative names:
#'   false negative rate (\code{FNR}),
#'   rate of type-II errors (\code{beta})
#'

#'   \item Relationships:
#'
#'   a. \code{mirt} is the complement of the
#'   sensitivity \code{\link{sens}} (aka. hit rate \code{HR}):
#'
#'   \code{mirt = (1 - sens) = (1 - HR)}
#'
#'   b. \code{mirt} is the _opposite_ conditional probability
#'   -- but not the complement --
#'   of the false omission rate \code{\link{FOR}}:
#'
#'   \code{FOR = p(condition = TRUE | decision = negative)}
#'

#'   \item In terms of frequencies,
#'   \code{mirt} is the ratio of
#'   \code{\link{mi}} divided by \code{\link{cond.true}}
#'   (i.e., \code{\link{hi} + \link{mi}}):
#'
#'   \code{mirt = mi/cond.true = mi/(hi + mi)}
#'
#'
#'   \item Dependencies:
#'   \code{mirt} is a feature of a decision process
#'   or diagnostic procedure and a measure of
#'   incorrect decisions (false negatives).
#'
#'   However, due to being a conditional probability,
#'   the value of \code{mirt} is not intrinsic to
#'   the decision process, but also depends on the
#'   condition's prevalence value \code{\link{prev}}.
#'
#' }
#'
#' @aliases
#' FNR beta
#'
#' @references
#' Consult \href{https://en.wikipedia.org/wiki/Sensitivity_and_specificity}{Wikipedia} for additional information.
#'
#' @family probabilities
#'
#' @seealso
#' \code{\link{comp_mirt}} computes \code{mirt} as the complement of \code{\link{sens}};
#' \code{\link{prob}} contains current probability information;
#' \code{\link{comp_prob}} computes current probability information;
#' \code{\link{num}} contains basic numeric parameters;
#' \code{\link{init_num}} initializes basic numeric parameters;
#' \code{\link{comp_freq}} computes current frequency information;
#' \code{\link{is_prob}} verifies probability inputs.
#'
#' @examples
#' mirt <- .15     # => sets a miss rate of 15%
#' mirt <- 15/100  # => (decision = negative) for 15 out of 100 people with (condition = TRUE)
#' is_prob(mirt)   # => TRUE (as mirt is a probability)

mirt <- 1 - sens  # default miss rate

## (3) spec*** = TNR: ----------------------------

#' The specificity of a decision process or diagnostic procedure.
#'
#' \code{spec} defines a decision's specificity value (or correct rejection rate):
#' The conditional probability of the decision being negative
#' if the condition is FALSE.
#'
#' Understanding or obtaining the specificity value \code{spec}:
#'
#' \itemize{
#'
#'   \item Definition:
#'   \code{spec} is the conditional probability
#'   for a (correct) negative decision given that
#'   the condition is \code{FALSE}:
#'
#'   \code{spec = p(decision = negative | condition = FALSE)}
#'
#'   or the probability of correctly detecting false cases
#'   (\code{condition = FALSE}).
#'
#'   \item Perspective:
#'   \code{spec} further classifies
#'   the subset of \code{\link{cond.false}} individuals
#'   by decision (\code{spec = cr/cond.false}).
#'
#'
#'   \item Alternative names:
#'   true negative rate (\code{TNR}),
#'   correct rejection rate,
#'   \code{1 - alpha}
#'
#'
#'   \item Relationships:
#'
#'   a. \code{spec} is the complement of the
#'   false alarm rate \code{\link{fart}}:
#'
#'   \code{spec = 1 - fart}
#'
#'   b. \code{spec} is the opposite conditional probability
#'   -- but not the complement --
#'   of the negative predictive value \code{\link{NPV}}:
#'
#'   \code{NPV = p(condition = FALSE | decision = negative)}
#'
#'
#'   \item In terms of frequencies,
#'   \code{spec} is the ratio of
#'   \code{\link{cr}} divided by \code{\link{cond.false}}
#'   (i.e., \code{\link{fa} + \link{cr}}):
#'
#'   \code{spec = cr/cond.false = cr/(fa + cr)}
#'
#'
#'   \item Dependencies:
#'   \code{spec} is a feature of a decision process
#'   or diagnostic procedure and a measure of
#'   correct decisions (true negatives).
#'
#'   However, due to being a conditional probability,
#'   the value of \code{spec} is not intrinsic to
#'   the decision process, but also depends on the
#'   condition's prevalence value \code{\link{prev}}.
#'
#' }
#'
#' @aliases
#' TNR
#'
#' @examples
#' spec <- .75     # => sets a specificity value of 75%
#' spec <- 75/100  # => (decision = negative) for 75 out of 100 people with (condition = FALSE)
#' is_prob(spec)   # => TRUE (as spec is a probability)
#'
#' @references
#' Consult \href{https://en.wikipedia.org/wiki/Sensitivity_and_specificity}{Wikipedia} for additional information.
#'
#' @family probabilities
#' @family essential parameters
#'
#' @seealso
#' \code{\link{comp_spec}} computes \code{spec} as the complement of \code{\link{fart}};
#' \code{\link{prob}} contains current probability information;
#' \code{\link{comp_prob}} computes current probability information;
#' \code{\link{num}} contains basic numeric parameters;
#' \code{\link{init_num}} initializes basic numeric parameters;
#' \code{\link{comp_freq}} computes current frequency information;
#' \code{\link{is_prob}} verifies probability inputs.
#'

spec <- 1/2 # default specificity


## (4) fart = FPR: -------------------------------

#' The false alarm rate (or false positive rate) of a decision process or diagnostic procedure.
#'
#' \code{fart} defines a decision's false alarm rate
#' (or the rate of false positives): The conditional probability
#' of the decision being positive if the condition is FALSE.
#'
#' Understanding or obtaining the false alarm rate \code{fart}:
#'
#' \itemize{
#'
#'   \item Definition:
#'   \code{fart} is the conditional probability
#'   for an incorrect positive decision given that
#'   the condition is \code{FALSE}:
#'
#'   \code{fart = p(decision = positive | condition = FALSE)}
#'
#'   or the probability of a false alarm.
#'
#'
#'   \item Perspective:
#'   \code{fart} further classifies
#'   the subset of \code{\link{cond.false}} individuals
#'   by decision (\code{fart = fa/cond.false}).
#'
#'
#'   \item Alternative names:
#'   false positive rate (\code{FPR}),
#'   rate of type-I errors (\code{alpha}),
#'   statistical significance level,
#'   \code{fallout}
#'
#'
#'   \item Relationships:
#'
#'   a. \code{fart} is the complement of the
#'   specificity \code{\link{spec}}:
#'
#'   \code{fart = 1 - spec}
#'
#'   b. \code{fart} is the opposite conditional probability
#'   -- but not the complement --
#'   of the false discovery rate
#'   or false detection rate \code{\link{FDR}}:
#'
#'   \code{FDR = p(condition = FALSE | decision = positive)}
#'
#'
#'   \item In terms of frequencies,
#'   \code{fart} is the ratio of
#'   \code{\link{fa}} divided by \code{\link{cond.false}}
#'   (i.e., \code{\link{fa} + \link{cr}}):
#'
#'   \code{fart = fa/cond.false = fa/(fa + cr)}
#'
#'
#'   \item Dependencies:
#'   \code{fart} is a feature of a decision process
#'   or diagnostic procedure and a measure of
#'   incorrect decisions (false positives).
#'
#'   However, due to being a conditional probability,
#'   the value of \code{fart} is not intrinsic to
#'   the decision process, but also depends on the
#'   condition's prevalence value \code{\link{prev}}.
#'
#' }
#'
#' @aliases
#' FPR
#' alpha
#' fallout
#'
#' @examples
#' fart <- .25     # => sets a false alarm rate of 25%
#' fart <- 25/100  # => (decision = positive) for 25 out of 100 people with (condition = FALSE)
#' is_prob(fart)   # => TRUE (as fart is a probability)
#'
#' @references
#' Consult \href{https://en.wikipedia.org/wiki/Sensitivity_and_specificity}{Wikipedia} for additional information.
#'
#' @family probabilities
#'
#' @seealso
#' \code{\link{comp_fart}} computes \code{fart} as the complement of \code{\link{spec}}
#' \code{\link{prob}} contains current probability information;
#' \code{\link{comp_prob}} computes current probability information;
#' \code{\link{num}} contains basic numeric parameters;
#' \code{\link{init_num}} initializes basic numeric parameters;
#' \code{\link{comp_freq}} computes current frequency information;
#' \code{\link{is_prob}} verifies probability inputs.
#'

fart <- 1 - spec   # default false alarm rate


## B: Define probabilities by decision: ------------------------

## (0) Proportion of positive decisions (ppod, PR): -----

## (0) ppod = proportion/base rate of decisions being positive (PR):

#' The proportion (or baseline) of a positive decision.
#'
#' \code{ppod} defines the proportion (baseline probability or rate) of
#' a decision being \code{positive} (but not necessarily accurate/correct).
#'
#' Understanding or obtaining the proportion of positive decisions \code{ppod}:
#'
#' \itemize{
#'
#'   \item Definition:
#'   \code{ppod} is the (non-conditional) probability:
#'
#'   \code{ppod = p(decision = positive)}
#'
#'   or the base rate (or baseline probability)
#'   of a decision being positive (but not necessarily accurate/correct).
#'
#'   \item Perspective:
#'   \code{ppod} classifies a population of \code{\link{N}} individuals
#'   by decision (\code{ppod = dec.pos/N}).
#'
#'   \code{ppod} is the "by decision" counterpart to \code{\link{prev}}
#'   (which adopts a "by condition" perspective).
#'
#'   \item Alternative names:
#'   base rate of positive decisions (\code{PR}),
#'   proportion predicted or diagnosed,
#'   rate of decision \code{= positive} cases
#'
#'   \item In terms of frequencies,
#'   \code{ppod} is the ratio of
#'   \code{\link{dec.pos}} (i.e., \code{\link{hi} + \link{fa}})
#'   divided by \code{\link{N}} (i.e.,
#'   \code{\link{hi} + \link{mi}} + \code{\link{fa} + \link{cr}}):
#'
#'   \code{ppod = dec.pos/N = (hi + fa)/(hi + mi + fa + cr)}
#'
#'   \item Dependencies:
#'   \code{ppod} is a feature of the decision process
#'   or diagnostic procedure.
#'
#'   However, the conditional probabilities
#'   \code{\link{sens}}, \code{\link{mirt}},
#'   \code{\link{spec}}, \code{\link{fart}},
#'   \code{\link{PPV}}, and \code{\link{NPV}}
#'   also depend on the condition's prevalence \code{\link{prev}}.
#'
#' }
#'
#' @aliases
#' baserate_dec.pos PR
#'
#' @examples
#' ppod <- .50     # => sets a rate of positive decisions of 50%
#' ppod <- 50/100  # => (decision = TRUE) for 50 out of 100 individuals
#' is_prob(ppod)   # => TRUE (as ppod is a probability)
#'
#' @family probabilities
#'
#' @seealso
#' \code{\link{prob}} contains current probability information;
#' \code{\link{comp_prob}} computes current probability information;
#' \code{\link{num}} contains basic numeric parameters;
#' \code{\link{init_num}} initializes basic numeric parameters;
#' \code{\link{freq}} contains current frequency information;
#' \code{\link{comp_freq}} computes current frequency information;
#' \code{\link{is_prob}} verifies probability inputs.
#'
#' @references
#' Consult \href{https://en.wikipedia.org/wiki/Positive_and_negative_predictive_values}{Wikipedia} for additional information.
#'

ppod <- 1/2  # default rate of positive decisions


## Predictive values (PVs): ----------------------

## (1) PPV: positive predictive value ------------

#' The positive predictive value of a decision process or diagnostic procedure.
#'
#' \code{PPV} defines some decision's positive predictive value (PPV):
#' The conditional probability of the condition being \code{TRUE}
#' provided that the decision is positive.
#'
#' Understanding or obtaining the positive predictive value \code{PPV}:
#'
#' \itemize{
#'
#'   \item Definition:
#'   \code{PPV} is the conditional probability
#'   for the condition being \code{TRUE}
#'   given a positive decision:
#'
#'   \code{PPV = p(condition = TRUE | decision = positive)}
#'
#'   or the probability of a positive decision being correct.
#'
#'
#'   \item Perspective:
#'   \code{PPV} further classifies
#'   the subset of \code{\link{dec.pos}} individuals
#'   by condition (\code{PPV = hi/dec.pos = hi/(hi + fa)}).
#'
#'
#'   \item Alternative names:
#'   \code{precision}
#'
#'
#'   \item Relationships:
#'
#'   a. \code{PPV} is the complement of the
#'   false discovery or false detection rate \code{\link{FDR}}:
#'
#'   \code{PPV = 1 - FDR}
#'
#'   b. \code{PPV} is the opposite conditional probability
#'   -- but not the complement --
#'   of the sensitivity \code{\link{sens}}:
#'
#'   \code{sens = p(decision = positive | condition = TRUE)}
#'
#'   \item In terms of frequencies,
#'   \code{PPV} is the ratio of
#'   \code{\link{hi}} divided by \code{\link{dec.pos}}
#'   (i.e., \code{\link{hi} + \link{fa}}):
#'
#'   \code{PPV = hi/dec.pos = hi/(hi + fa)}
#'
#'
#'   \item Dependencies:
#'   \code{PPV} is a feature of a decision process
#'   or diagnostic procedure and
#'   -- similar to the sensitivity \code{\link{sens}} --
#'   a measure of correct decisions (positive decisions
#'   that are actually TRUE).
#'
#'   However, due to being a conditional probability,
#'   the value of \code{PPV} is not intrinsic to
#'   the decision process, but also depends on the
#'   condition's prevalence value \code{\link{prev}}.
#'
#' }
#'
#' @aliases
#' precision
#'
#' @references
#' Consult \href{https://en.wikipedia.org/wiki/Positive_and_negative_predictive_values}{Wikipedia} for additional information.
#'
#' @examples
#' PPV <- .55     # => sets a positive predictive value of 55%
#' PPV <- 55/100  # => (condition = TRUE) for 55 out of 100 people with (decision = positive)
#' is_prob(PPV)   # => TRUE (as PPV is a probability)
#'
#' @family probabilities
#'
#' @seealso
#' \code{\link{comp_PPV}} computes \code{PPV};
#' \code{\link{prob}} contains current probability information;
#' \code{\link{comp_prob}} computes current probability information;
#' \code{\link{num}} contains basic numeric parameters;
#' \code{\link{init_num}} initializes basic numeric parameters;
#' \code{\link{comp_freq}} computes current frequency information;
#' \code{\link{is_prob}} verifies probability inputs.
#'

PPV <- 1/2  # default of positive predictive value (PPV)


## (2) FDR: false detection rate = 1 - PPV -------

#' The false detection rate of a decision process or diagnostic procedure.
#'
#' \code{FDR} defines a decision's false detection (or false discovery)
#' rate (\code{FDR}): The conditional probability of the condition
#' being \code{FALSE} provided that the decision is positive.
#'
#' Understanding or obtaining the false detection fate
#' or false discovery rate (\code{FDR}):
#'
#' \itemize{
#'
#'   \item Definition:
#'   \code{FDR} is the conditional probability
#'   for the condition being \code{FALSE}
#'   given a positive decision:
#'
#'   \code{FDR = p(condition = FALSE | decision = positive)}
#'
#'
#'   \item Perspective:
#'   \code{FDR} further classifies
#'   the subset of \code{\link{dec.pos}} individuals
#'   by condition (\code{FDR = fa/dec.pos = fa/(hi + fa)}).
#'
#'
#'   \item Alternative names:
#'   false discovery rate
#'
#'
#'   \item Relationships:
#'
#'   a. \code{FDR} is the complement of the
#'   positive predictive value \code{\link{PPV}}:
#'
#'   \code{FDR = 1 - PPV}
#'
#'   b. \code{FDR} is the opposite conditional probability
#'   -- but not the complement --
#'   of the false alarm rate \code{\link{fart}}:
#'
#'   \code{fart = p(decision = positive | condition = FALSE)}
#'
#'   \item In terms of frequencies,
#'   \code{FDR} is the ratio of
#'   \code{\link{fa}} divided by \code{\link{dec.pos}}
#'   (i.e., \code{\link{hi} + \link{fa}}):
#'
#'   \code{FDR = fa/dec.pos = fa/(hi + fa)}
#'
#'
#'   \item Dependencies:
#'   \code{FDR} is a feature of a decision process
#'   or diagnostic procedure and
#'   a measure of incorrect decisions (positive decisions
#'   that are actually \code{FALSE}).
#'
#'   However, due to being a conditional probability,
#'   the value of \code{FDR} is not intrinsic to
#'   the decision process, but also depends on the
#'   condition's prevalence value \code{\link{prev}}.
#'
#' }
#'
#' @examples
#' FDR <- .45     # => sets a false detection rate (FDR) of 45%
#' FDR <- 45/100  # => (condition = FALSE) for 45 out of 100 people with (decision = positive)
#' is_prob(FDR)   # => TRUE (as FDR is a probability)
#'
#' @family probabilities
#'
#' @seealso
#' \code{\link{prob}} contains current probability information;
#' \code{\link{comp_prob}} computes current probability information;
#' \code{\link{num}} contains basic numeric parameters;
#' \code{\link{init_num}} initializes basic numeric parameters;
#' \code{\link{freq}} contains current frequency information;
#' \code{\link{comp_freq}} computes current frequency information;
#' \code{\link{is_prob}} verifies probability inputs.
#'
#' @references
#' Consult \href{https://en.wikipedia.org/wiki/Positive_and_negative_predictive_values}{Wikipedia} for additional information.
#'

FDR <- (1 - PPV)  # default of false discorvery rate (FDR)


## (3) NPV: negative predictive value ------------

#' The negative predictive value of a decision process or diagnostic procedure.
#'
#' \code{NPV} defines some decision's negative predictive value (NPV):
#' The conditional probability of the condition being \code{FALSE}
#' provided that the decision is negative.
#'
#' Understanding or obtaining the negative predictive value \code{NPV}:
#'
#' \itemize{
#'
#'   \item Definition:
#'   \code{NPV} is the conditional probability
#'   for the condition being \code{FALSE}
#'   given a negative decision:
#'
#'   \code{NPV = p(condition = FALSE | decision = negative)}
#'
#'   or the probability of a negative decision being correct.
#'
#'   \item Perspective:
#'   \code{NPV} further classifies
#'   the subset of \code{\link{dec.neg}} individuals
#'   by condition (\code{NPV = cr/dec.neg = cr/(mi + cr)}).
#'
#'   \item Alternative names:
#'   true omission rate
#'
#'   \item Relationships:
#'
#'   a. \code{NPV} is the complement of the
#'   false omission rate \code{\link{FOR}}:
#'
#'   \code{NPV = 1 - FOR}
#'
#'   b. \code{NPV} is the opposite conditional probability
#'   -- but not the complement --
#'   of the specificity \code{\link{spec}}:
#'
#'   \code{spec = p(decision = negative | condition = FALSE)}
#'
#'
#'   \item In terms of frequencies,
#'   \code{NPV} is the ratio of
#'   \code{\link{cr}} divided by \code{\link{dec.neg}}
#'   (i.e., \code{\link{cr} + \link{mi}}):
#'
#'   \code{NPV = cr/dec.neg = cr/(cr + mi)}
#'
#'
#'   \item Dependencies:
#'   \code{NPV} is a feature of a decision process
#'   or diagnostic procedure and
#'   -- similar to the specificity \code{\link{spec}} --
#'   a measure of correct decisions (negative decisions
#'   that are actually FALSE).
#'
#'   However, due to being a conditional probability,
#'   the value of \code{NPV} is not intrinsic to
#'   the decision process, but also depends on the
#'   condition's prevalence value \code{\link{prev}}.
#'
#' }
#'
#' @examples
#' NPV <- .95     # => sets a negative predictive value of 95%
#' NPV <- 95/100  # => (condition = FALSE) for 95 out of 100 people with (decision = negative)
#' is_prob(NPV)   # => TRUE (as NPV is a probability)
#'
#' @references
#' Consult \href{https://en.wikipedia.org/wiki/Positive_and_negative_predictive_values}{Wikipedia} for additional information.
#'
#' @family probabilities
#'
#' @seealso
#' \code{\link{comp_NPV}} computes \code{NPV};
#' \code{\link{prob}} contains current probability information;
#' \code{\link{comp_prob}} computes current probability information;
#' \code{\link{num}} contains basic numeric parameters;
#' \code{\link{init_num}} initializes basic numeric parameters;
#' \code{\link{comp_freq}} computes current frequency information;
#' \code{\link{is_prob}} verifies probability inputs.
#'

NPV <- 1/2  # default of negative predictive value (NPV)


## (4) FOR: False omission rate = 1 - NPV --------

#' The false omission rate (FOR) of a decision process or diagnostic procedure.
#'
#' \code{FOR} defines a decision's false omission rate (\code{FOR}):
#' The conditional probability of the condition being \code{TRUE}
#' provided that the decision is negative.
#'
#' Understanding or obtaining the false omission rate \code{FOR}:
#'
#' \itemize{
#'
#'   \item Definition:
#'   \code{FOR} is the so-called false omission rate:
#'   The conditional probability for the condition being \code{TRUE}
#'   given a negative decision:
#'
#'   \code{FOR = p(condition = TRUE | decision = negative)}
#'
#'   \item Perspective:
#'   \code{FOR} further classifies
#'   the subset of \code{\link{dec.neg}} individuals
#'   by condition (\code{FOR = mi/dec.neg = mi/(mi + cr)}).
#'
#'   \item Alternative names:
#'   none?
#'
#'   \item Relationships:
#'
#'   a. \code{FOR} is the complement of the
#'   negative predictive value \code{\link{NPV}}:
#'
#'   \code{FOR = 1 - NPV}
#'
#'   b. \code{FOR} is the opposite conditional probability
#'   -- but not the complement --
#'   of the miss rate \code{\link{mirt}}
#'   (aka. false negative rate \code{FDR}):
#'
#'   \code{mirt = p(decision = negative | condition = TRUE)}
#'
#'   \item In terms of frequencies,
#'   \code{FOR} is the ratio of
#'   \code{\link{mi}} divided by \code{\link{dec.neg}}
#'   (i.e., \code{\link{mi} + \link{cr}}):
#'
#'   \code{NPV = mi/dec.neg = mi/(mi + cr)}
#'
#'   \item Dependencies:
#'   \code{FOR} is a feature of a decision process
#'   or diagnostic procedure and a measure of incorrect
#'   decisions (negative decisions that are actually \code{FALSE}).
#'
#'   However, due to being a conditional probability,
#'   the value of \code{FOR} is not intrinsic to
#'   the decision process, but also depends on the
#'   condition's prevalence value \code{\link{prev}}.
#'
#' }
#'
#' @examples
#' FOR <- .05     # => sets a false omission rate of 5%
#' FOR <- 5/100   # => (condition = TRUE) for 5 out of 100 people with (decision = negative)
#' is_prob(FOR)   # => TRUE (as FOR is a probability)
#'
#' @references
#' Consult \href{https://en.wikipedia.org/wiki/Positive_and_negative_predictive_values}{Wikipedia} for additional information.
#'
#' @family probabilities
#'
#' @seealso
#' \code{\link{comp_FOR}} computes \code{FOR} as the complement of \code{\link{NPV}};
#' \code{\link{prob}} contains current probability information;
#' \code{\link{comp_prob}} computes current probability information;
#' \code{\link{num}} contains basic numeric parameters;
#' \code{\link{init_num}} initializes basic numeric parameters;
#' \code{\link{comp_freq}} computes current frequency information;
#' \code{\link{is_prob}} verifies probability inputs.
#'

FOR <- 1 - NPV  # default of false omission rate (FOR)





## C: Define probabilities by accuracy: ----------

## (1) Accuracy acc: --------

#' Accuracy (acc) is the probability of a correct decision.
#'
#' \code{acc} defines overall accuracy
#' as the probability of correspondence between a positive decision
#' and true condition (i.e., the proportion of correct classification
#' decisions or of \code{\link{dec.cor}} cases).
#'
#' Importantly, correct decisions \code{\link{dec.cor}}
#' are not necessarily positive decisions \code{\link{dec.pos}}.
#'
#' Understanding or obtaining the accuracy metric \code{acc}:
#'
#' \itemize{
#'
#'   \item Definition:
#'   \code{acc} is the (non-conditional) probability:
#'
#'   \code{acc = p(dec.cor) = dec.cor/N}
#'
#'   or the base rate (or baseline probability)
#'   of a decision being correct, but not necessarily positive.
#'
#'   \code{acc} values range
#'   from 0 (no correct decision/prediction)
#'   to 1 (perfect decision/prediction).
#'
#'   \item Computation: \code{acc} can be computed in several ways:
#'
#'    (a) from \code{\link{prob}}: \code{acc = (prev x sens) + [(1 - prev) x spec]}
#'
#'    (b) from \code{\link{freq}}: \code{acc = dec.cor/N = (hi + cr)/(hi + mi + fa + cr)}
#'
#'    (c) as complement of the error rate \code{\link{err}}: \code{acc = 1 - err}
#'
#'    When frequencies in \code{\link{freq}} are not rounded, (b) coincides with (a) and (c).
#'
#'   \item Perspective:
#'   \code{acc} classifies a population of \code{\link{N}} individuals
#'   by accuracy/correspondence (\code{acc = dec.cor/N}).
#'
#'   \code{acc} is the "by accuracy" or "by correspondence" counterpart
#'   to \code{\link{prev}} (which adopts a "by condition" perspective) and
#'   to \code{\link{ppod}} (which adopts a "by decision" perspective).
#'
#'   \item Alternative names:
#'   base rate of correct decisions,
#'   non-erroneous cases
#'
#'   \item In terms of frequencies,
#'   \code{acc} is the ratio of
#'   \code{\link{dec.cor}} (i.e., \code{\link{hi} + \link{cr}})
#'   divided by \code{\link{N}} (i.e.,
#'   \code{\link{hi} + \link{mi}} + \code{\link{fa} + \link{cr}}):
#'
#'   \code{acc = dec.cor/N = (hi + cr)/(hi + mi + fa + cr)}
#'
#'   \item Dependencies:
#'   \code{acc} is a feature of both the environment (true condition) and
#'   of the decision process or diagnostic procedure. It reflects the
#'   correspondence of decisions to conditions.
#'
#' }
#'
#' See \code{\link{accu}} for other accuracy metrics
#' and several possible interpretations of accuracy.
#'
#' @aliases
#' accurate correct
#'
#' @examples
#' acc <- .50     # => sets a rate of correct decisions of 50%
#' acc <- 50/100  # => (dec.cor) for 50 out of 100 individuals
#' is_prob(acc)   # => TRUE (as acc is a probability)
#'
#' @family probabilities
#' @family metrics
#'
#' @seealso
#' \code{\link{comp_acc}} computes accuracy from probabilities;
#' \code{\link{accu}} lists all accuracy metrics;
#' \code{\link{comp_accu_prob}} computes exact accuracy metrics from probabilities;
#' \code{\link{comp_accu_freq}} computes accuracy metrics from frequencies;
#' \code{\link{comp_sens}} and \code{\link{comp_PPV}} compute related probabilities;
#' \code{\link{is_extreme_prob_set}} verifies extreme cases;
#' \code{\link{comp_complement}} computes a probability's complement;
#' \code{\link{is_complement}} verifies probability complements;
#' \code{\link{comp_prob}} computes current probability information;
#' \code{\link{prob}} contains current probability information;
#' \code{\link{is_prob}} verifies probabilities.
#'
#' @references
#' Consult \href{https://en.wikipedia.org/wiki/Accuracy_and_precision}{Wikipedia:Accuracy_and_precision} for additional information.
#'

acc <- 1/2  # initialize to random accuracy


## (2) Error rate err: --------

#' Error rate (err) as the probability of an incorrect decision.
#'
#' \code{err} defines the error rate as the complement of
#' accuracy \code{\link{acc}} or lack of correspondence
#' of decisions to conditions.
#'
#' Definition:
#'
#' \code{err = (1 - \link{acc})}
#'
#' When \code{\link{freq}} are not rounded (\code{round = FALSE}) then
#'
#' \code{err = \link{dec.err}/\link{N} = (\link{mi} + \link{fa})/\link{N}}
#'
#' \code{err} is currently not included in \code{\link{prob}},
#' but shown in plots.
#'
#' See \code{err}'s complement of accuracy \code{\link{acc}}
#' for computation and
#' \code{\link{accu}} for current accuracy metrics
#' and several possible interpretations of accuracy.
#'
#' @examples
#' err <- .50     # => sets a rate of incorrect decisions of 50%
#' err <- 50/100  # => (dec.err) for 50 out of 100 individuals
#' is_prob(err)   # => TRUE (as err is a probability)
#'
#' @family probabilities
#' @family metrics
#'
#' @seealso
#' \code{\link{acc}} provides overall accuracy;
#' \code{\link{comp_acc}} computes accuracy from probabilities;
#' \code{\link{accu}} lists current accuracy metrics;
#' \code{\link{comp_accu_prob}} computes exact accuracy metrics from probabilities;
#' \code{\link{comp_accu_freq}} computes accuracy metrics from frequencies;
#' \code{\link{comp_sens}} and \code{\link{comp_PPV}} compute related probabilities;
#' \code{\link{is_extreme_prob_set}} verifies extreme cases;
#' \code{\link{comp_complement}} computes a probability's complement;
#' \code{\link{is_complement}} verifies probability complements;
#' \code{\link{comp_prob}} computes current probability information;
#' \code{\link{prob}} contains current probability information;
#' \code{\link{is_prob}} verifies probabilities.
#'

err <- (1 - acc)  # initialize err to complement of accuracy

## (*) Done: -------------------------------------

## - Add err to prob.  [2018 09 05]
## - Add acc to prob.  [2018 09 04]
## - Clean up code.    [2018 08 20]

## (+) ToDo: -------------------------------------

## - Check documentation
##   (for correctness and consistency).

## eof. ------------------------------------------
