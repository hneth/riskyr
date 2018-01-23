## init_prob.R | riskyR
## 2018 01 22
## -----------------------------------------------
## Define and initialize basic probabilities
## -----------------------------------------------

## -----------------------------------------------
## Table of current terminology:

# probabilities (9):                frequencies (9):
# ------------------                ------------------
# (A) basic:
#                                          N
# prev                              n.true | n.false

# sens = hit rate = TPR             hi = TP
# mirt = miss rate = FNR            mi = FN
# fart = false alarm rate = FPR     fa = FP
# spec = true negative rate = TNR   cr = TN


# (B) derived:
#                                   dec.pos | dec.neg

# PPV = pos. pred. value
# FDR = false detection rate
# FOR = false omission rate
# NPV = neg. pred. value

## -----------------------------------------------
## Two basic directions:

## 1: Bayesian: starting with 3 basic probabilities:
## - given:   prev;  sens, spec
## - derived: all other values

## 2: Natural frequencies:
## - given:   N = hi, mi, fa, cr
## - derived: all other values

## -----------------------------------------------
## A: Basic probabilities:
## -----------------------------------------------
## (0) prev:

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
#'   or the (baseline) probability of the condition's occurrence.
#'
#'   \item Alternative names:
#'   \code{baseline}, proportion affected,
#'   rate of condition = \code{TRUE} cases
#'
#'   to be distinguished from the incidence rate (i.e., new cases within a certain time period)
#'
#'   \item In natural frequencies,
#'   \code{prev} is the ratio of individuals for which
#'   \code{condition = TRUE} divided by the
#'   number \code{\link{N}} of all individuals
#'   in the population:
#'
#'   \code{prev = n(condition = TRUE) / \link{N}}
#'
#'   \item \code{prev} is a feature of the population
#'   and condition, but independent of the decision process
#'   or diagnostic procedure.
#'
#'   \item The value of \code{prev} does not depend
#'   on features of the decision process or diagnostic procedure.
#'   However, \code{prev} must be taken into account when
#'   computing the conditional probabilities
#'   \code{\link{sens}}, \code{\link{spec}}, \code{\link{fart}},
#'   \code{\link{ppv}}, and \code{\link{npv}}
#'   (as derived parameters).
#'
#' }
#'
#' @aliases
#' baseline
#'
#' @examples
#' prev <- .10     # => sets a prevalence value of 10%
#' prev <- 10/100  # => (condition = TRUE) for 10 out of 100 individuals
#' is_prob(prev)   # => TRUE (as prev is a probability)
#'
#' @family basic parameters
#'
#' @seealso
#' \code{\link{num}} contains basic numeric variables;
#' \code{\link{init_num}} initializes basic numeric variables;
#' \code{\link{is_prob}} verifies probability inputs;
#' \code{\link{comp_prob}} computes derived probabilities;
#' \code{\link{comp_freq}} computes natural frequencies from probabilities
#'
#' @references
#' Consult \href{https://en.wikipedia.org/wiki/Prevalence}{Wikipedia} for additional information.

prev <- .10 # default prevalence

## -----------------------------------------------
## (1) sens:

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
#'   \item Alternative names:
#'   \code{recall}, true positive rate (\code{TPR}),
#'   hit rate (\code{HR}), probability of detection
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
#'   \item In natural frequencies,
#'   \code{sens} is the ratio of individuals for which
#'   \code{decision = positive} divided by the number of
#'   individuals for which \code{condition = TRUE}:
#'
#'   \code{sens = n(decision = positive) / n(condition = TRUE)}
#'
#'   \item \code{sens} is a feature of a decision process
#'   or diagnostic procedure and a measure of
#'   correct decisions (true positives).
#'
#'   However, due to being a conditional probability,
#'   the value of \code{sens} also depends on the
#'   condition's prevalence value \code{\link{prev}}.
#'
#' }
#'
#' @aliases
#' recall
#' HR
#' TPR
#'
#' @references
#' Consult \href{https://en.wikipedia.org/wiki/Sensitivity_and_specificity}{Wikipedia} for additional information.
#'
#' @family basic parameters
#'
#' @seealso
#' \code{\link{num}} contains basic numeric variables;
#' \code{\link{init_num}} initializes basic numeric variables;
#' \code{\link{is_prob}} verifies probability inputs;
#' \code{\link{comp_prob}} computes derived probabilities;
#' \code{\link{comp_freq}} computes natural frequencies from probabilities
#'
#' @examples
#' sens <- .85     # => sets a sensitivity value of 85%
#' sens <- 85/100  # => (decision = positive) for 85 people out of 100 people for which (condition = TRUE)
#' is_prob(sens)   # => TRUE (as sens is a probability)

sens <- .85  # default sensitivity

## -----------------------------------------------
## (2) mirt:

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
#'   \item Alternative names:
#'   false negative rate (\code{FNR}),
#'   rate of Type-II errors
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
#'   \item In natural frequencies,
#'   \code{mirt} is the ratio of individuals for which
#'   \code{decision = negative}
#'   (number of misses \code{link{mi}}) divided by the number of
#'   individuals for which \code{condition = TRUE}
#'   (\code{\link{n.true}}):
#'
#'   \code{mirt = n(decision = negative) / n(condition = TRUE)}
#'
#'   \item \code{mirt} is a feature of a decision process
#'   or diagnostic procedure and a measure of
#'   incorrect decisions (false negatives).
#'
#'   However, due to being a conditional probability,
#'   the value of \code{mirt} also depends on the
#'   condition's prevalence value \code{\link{prev}}.
#'
#' }
#'
#' @aliases
#' FNR
#'
#' @references
#' Consult \href{https://en.wikipedia.org/wiki/Sensitivity_and_specificity}{Wikipedia} for additional information.
#'
#' @family basic parameters
#'
#' @seealso
#' \code{\link{num}} contains basic numeric variables;
#' \code{\link{init_num}} initializes basic numeric variables;
#' \code{\link{is_prob}} verifies probability inputs;
#' \code{\link{comp_prob}} computes derived probabilities;
#' \code{\link{comp_freq}} computes natural frequencies from probabilities
#'
#' @examples
#' mirt <- .15     # => sets a miss rate of 15%
#' mirt <- 15/100  # => (decision = negative) for 15 people out of 100 people for which (condition = TRUE)
#' is_prob(mirt)   # => TRUE (as mirt is a probability)

mirt <- 1 - sens  # default miss rate

## -----------------------------------------------
## (3) spec:

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
#'   \item Alternative names:
#'   true negative rate (\code{TNR}), correct rejection rate
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
#'   \item In natural frequencies,
#'   \code{spec} is the ratio of individuals for which
#'   \code{decision = negative} divided by the number of
#'   individuals for which \code{condition = FALSE}:
#'
#'   \code{spec = n(decision = negative) / n(condition = FALSE)}
#'
#'   \item \code{spec} is a feature of a decision process
#'   or diagnostic procedure and a measure of
#'   correct decisions (true negatives).
#'
#'   However, due to being a conditional probability,
#'   the value of \code{spec} also depends on the
#'   condition's prevalence value \code{\link{prev}}.
#'
#' }
#'
#' @aliases
#' TNR
#'
#' @references
#' Consult \href{https://en.wikipedia.org/wiki/Sensitivity_and_specificity}{Wikipedia} for additional information.
#'
#' @family basic parameters
#'
#' @seealso
#' \code{\link{num}} contains basic numeric variables;
#' \code{\link{init_num}} initializes basic numeric variables;
#' \code{\link{is_prob}} verifies probability inputs;
#' \code{\link{comp_prob}} computes derived probabilities;
#' \code{\link{comp_freq}} computes natural frequencies from probabilities;
#' \code{\link{comp_spec}} computes \code{spec}
#' as the complement of \code{\link{fart}}
#'
#' @examples
#' spec <- .75     # => sets a specificity value of 75%
#' spec <- 75/100  # => (decision = negative) for 75 people out of 100 people for which (condition = FALSE)
#' is_prob(spec)   # => TRUE (as spec is a probability)

spec <- .75 # default specificity

## -----------------------------------------------
## (4) fart:

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
#'   \item Alternative names:
#'   \code{fallout}, false positive rate (\code{FPR}),
#'   rate of type-I errors
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
#'   \code{FDR = p(condition = FALSE | decision = negative)}
#'
#'   \item In natural frequencies,
#'   \code{fart} is the ratio of individuals for which
#'   \code{decision = positive} divided by the number of
#'   individuals for which \code{condition = FALSE}:
#'
#'   \code{fart = n(decision = positive) / n(condition = FALSE)}
#'
#'   \item \code{fart} is a feature of a decision process
#'   or diagnostic procedure and a measure of
#'   incorrect decisions (false positives).
#'
#'   However, due to being a conditional probability,
#'   the value of \code{fart} also depends on the
#'   condition's prevalence value \code{\link{prev}}.
#'
#' }
#'
#' @aliases
#' fallout
#' FPR
#'
#' @references
#' Consult \href{https://en.wikipedia.org/wiki/Sensitivity_and_specificity}{Wikipedia} for additional information.
#'
#' @family basic parameters
#'
#' @seealso
#' \code{\link{num}} contains basic numeric variables;
#' \code{\link{init_num}} initializes basic numeric variables;
#' \code{\link{is_prob}} verifies probability inputs;
#' \code{\link{comp_prob}} computes derived probabilities;
#' \code{\link{comp_freq}} computes natural frequencies from probabilities;
#' \code{\link{comp_fart}} computes \code{fart}
#' as the complement of \code{\link{spec}}
#'
#' @examples
#' fart <- .25     # => sets a false alarm rate of 25%
#' fart <- 25/100  # => (decision = positive) for 25 people out of 100 people for which (condition = FALSE)
#' is_prob(fart)   # => TRUE (as fart is a probability)

fart <- 1 - spec   # default false alarm rate


## -----------------------------------------------
## B: Derived probabilities (PVs):
## -----------------------------------------------
## (1) PPV: positive predictive value

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
#'   \item Alternative names:
#'   \code{precision}
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
#'   \item In natural frequencies,
#'   \code{PPV} is the ratio of individuals for which
#'   \code{decision = positive} and \code{condition = TRUE}
#'   divided by the number of all individuals for which
#'   \code{decision = positive}:
#'
#'   \code{PPV = n(decision = positive & condition = TRUE) / n(decision = positive)}
#'
#'   \code{PPV = n.hi / (n.hi + n.fa) }
#'
#'   \item \code{PPV} is a feature of a decision process
#'   or diagnostic procedure and
#'   -- similar to the sensitivity \code{\link{sens}} --
#'   a measure of correct decisions (positive decisions
#'   that are actually TRUE).
#'
#'   However, due to being a conditional probability,
#'   the value of \code{PPV} also depends on the
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
#' PPV <- 55/100  # => (condition = TRUE) for 55 people out of 100 people for which (decision = positive)
#' is_prob(PPV)   # => TRUE (as PPV is a probability)
#'
#' @family functions computing probabilities
#'
#' @seealso
#' \code{\link{num}} contains basic numeric variables;
#' \code{\link{comp_PPV}} and \code{\link{comp_PPV_freq}} compute PPVs;
#' \code{\link{comp_prob}} computes derived probabilities;
#' \code{\link{is_prob}} verifies probability inputs;
#' \code{\link{comp_prob}} computes derived probabilities;
#' \code{\link{comp_freq}} computes natural frequencies from probabilities

PPV <- NA  # default of positive predictive value (PPV)


## -----------------------------------------------
## (2) FDR: false detection rate = 1 - PPV

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
#'   \item In natural frequencies,
#'   \code{FDR} is the ratio of individuals for which
#'   \code{decision = positive} and \code{condition = FALSE}
#'   (aka. false positives)
#'   divided by the number of all individuals for which
#'   \code{decision = positive} (true positives and false positives):
#'
#'   \code{FDR = n(decision = positive & condition = FALSE) / n(decision = positive)}
#'
#'   \code{FDR = n.fa / (n.hi + n.fa) }
#'
#'   \item \code{FDR} is a feature of a decision process
#'   or diagnostic procedure and
#'   a measure of incorrect decisions (positive decisions
#'   that are actually \code{FALSE}).
#'
#'   However, due to being a conditional probability,
#'   the value of \code{FDR} also depends on the
#'   condition's prevalence value \code{\link{prev}}.
#'
#' }
#'
#' @references
#' Consult \href{https://en.wikipedia.org/wiki/Positive_and_negative_predictive_values}{Wikipedia} for additional information.
#'
#' @family functions computing probabilities
#'
#' @seealso
#' \code{\link{comp_FDR}} computes \code{FDR} as the complement of \code{\link{PPV}};
#' \code{\link{num}} contains basic numeric variables;
#' \code{\link{comp_PPV}} and \code{\link{comp_PPV_freq}} compute PPVs;
#' \code{\link{comp_prob}} computes derived probabilities;
#' \code{\link{is_prob}} verifies probability inputs;
#' \code{\link{comp_prob}} computes derived probabilities;
#' \code{\link{comp_freq}} computes natural frequencies from probabilities.
#'
#' @examples
#' FDR <- .45     # => sets a false discovery rate (FDR) of 45%
#' FDR <- 45/100  # => (condition = FALSE) for 45 people out of 100 people for which (decision = positive)
#' is_prob(FDR)   # => TRUE (as FDR is a probability)

FDR <- NA  # default of false discorvery rate (FDR)


## -----------------------------------------------
## (3) NPV: negative predictive value

#' The negative predictive value of a decision process or diagnostic procedure.
#'
#' \code{NPV} defines some decision's negative predictive value (NPV):
#' The conditional probability of the condition being FALSE
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
#'   \item In natural frequencies,
#'   \code{NPV} is the ratio of individuals for which
#'   \code{decision = negative} and \code{condition = FALSE}
#'   divided by the number of all individuals for which
#'   \code{decision = negative}:
#'
#'   \code{NPV = n(decision = negative & condition = FALSE) / n(decision = negative)}
#'
#'   \code{NPV = n.cr / (n.mi + n.cr)}
#'
#'   \item \code{NPV} is a feature of a decision process
#'   or diagnostic procedure and
#'   -- similar to the specificity \code{\link{spec}} --
#'   a measure of correct decisions (negative decisions
#'   that are actually FALSE).
#'
#'   However, due to being a conditional probability,
#'   the value of \code{NPV} also depends on the
#'   condition's prevalence value \code{\link{prev}}.
#'
#' }
#'
#' @references
#' Consult \href{https://en.wikipedia.org/wiki/Positive_and_negative_predictive_values}{Wikipedia} for additional information.
#'
#' @family functions computing probabilities
#'
#' @seealso
#' \code{\link{num}} contains basic numeric variables;
#' \code{\link{comp_NPV}} and \code{\link{comp_NPV_freq}} compute PPVs;
#' \code{\link{comp_prob}} computes derived probabilities;
#' \code{\link{is_prob}} verifies probability inputs;
#' \code{\link{comp_prob}} computes derived probabilities;
#' \code{\link{comp_freq}} computes natural frequencies from probabilities.
#'
#' @examples
#' NPV <- .95     # => sets a negative predictive value of 95%
#' NPV <- 95/100  # => (condition = FALSE) for 95 people out of 100 people for which (decision = negative)
#' is_prob(NPV)   # => TRUE (as NPV is a probability)

NPV <- NA  # default of negative predictive value (NPV)


## -----------------------------------------------
## (4) FOR: False omission rate = 1 - NPV

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
#'   \item In natural frequencies,
#'   \code{FOR} is the ratio of individuals for which
#'   \code{decision = negative} and \code{condition = TRUE}
#'   (aka. false negatives)
#'   divided by the number of all individuals for which
#'   \code{decision = negative} (true negatives and false negatives):
#'
#'   \code{FOR = n(decision = negative & condition = TRUE) / n(decision = negative)}
#'
#'   \code{FOR = n.mi / (n.mi + n.cr)}
#'
#'   \item \code{FOR} is a feature of a decision process
#'   or diagnostic procedure and a measure of incorrect
#'   decisions (negative decisions that are actually \code{FALSE}).
#'
#'   However, due to being a conditional probability,
#'   the value of \code{FOR} also depends on the
#'   condition's prevalence value \code{\link{prev}}.
#'
#' }
#'
#' @references
#' Consult \href{https://en.wikipedia.org/wiki/Positive_and_negative_predictive_values}{Wikipedia} for additional information.
#'
#' @family functions computing probabilities
#'
#' @seealso
#' \code{\link{comp_FOR}} computes \code{FOR} as the complement of \code{\link{NPV}};
#' \code{\link{num}} contains basic numeric variables;
#' \code{\link{comp_NPV}} and \code{\link{comp_NPV_freq}} compute PPVs;
#' \code{\link{comp_prob}} computes derived probabilities;
#' \code{\link{is_prob}} verifies probability inputs;
#' \code{\link{comp_prob}} computes derived probabilities;
#' \code{\link{comp_freq}} computes natural frequencies from probabilities
#'
#' @examples
#' FOR <- .05     # => sets a false omission rate of 5%
#' FOR <- 5/100   # => (condition = TRUE) for 5 people out of 100 people for which (decision = negative)
#' is_prob(FOR)   # => TRUE (as FOR is a probability)

FOR <- NA  # default of false omission rate (FOR)


## -----------------------------------------------
## (+) ToDo:

## - Correct formulas mentioned in
##   "in natural frequencies" sections
##   after writing corresponding functions
##   in comp_prob_freq.R.

## -----------------------------------------------
## eof.
