## init_freq.R | riskyr
## 2018 11 08
## Define and initialize ALL frequencies
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


## (A) BASIC frequencies: ----------

##   (0) population size N: --------

#' Number of individuals in the population.
#'
#' \code{N} is a frequency that describes the
#' number of individuals in the current population
#' (i.e., the overall number of cases considered).
#'
#' Key relationships:
#'
#' \enumerate{
#'
#' \item to probabilities:
#' A population of \code{\link{N}} individuals can be split into 2 subsets
#' in 2 different ways:
#'
#' \enumerate{
#'   \item by condition:
#'   The frequency \code{\link{cond.true}} depends on the prevalence \code{\link{prev}}
#'   and
#'   the frequency \code{\link{cond.false}} depends on the prevalence's complement \code{1 - \link{prev}}.
#'
#'   \item by decision:
#'   The frequency \code{\link{dec.pos}} depends on the proportion of positive decisions \code{\link{ppod}}
#'   and
#'   the frequency \code{\link{dec.neg}} depends on the proportion of negative decisions \code{1 - \link{ppod}}.
#'
#' }
#'
#' The population size \code{\link{N}} is a free parameter (independent of the
#' essential probabilities \code{\link{prev}}, \code{\link{sens}}, and \code{\link{spec}}).
#'
#' If \code{\link{N}} is unknown, a suitable minimum value can be computed by \code{\link{comp_min_N}}.
#'
#'   \item to other frequencies:
#'   In a population of size \code{\link{N}}
#'   the following relationships hold:
#'
#'   \itemize{
#'
#'     \item \code{\link{N} = \link{cond.true} + \link{cond.false}} (by condition)
#'
#'     \item \code{\link{N} = \link{dec.pos} + \link{dec.neg}} (by decision)
#'
#'     \item \code{\link{N} = \link{dec.cor} + \link{dec.err}} (by correspondence of decision to condition)
#'
#'     \item \code{\link{N} = \link{hi} + \link{mi} + \link{fa} + \link{cr}} (by condition x decision)
#'   }
#'
#' }
#'
#' Current frequency information is computed by
#' \code{\link{comp_freq}} and contained in a list
#' \code{\link{freq}}.
#'
#'
#' @references
#' Consult \href{https://en.wikipedia.org/wiki/Statistical_population}{Wikipedia: Statistical population} for additional information.
#'
#' @family frequencies
#'
#' @seealso
#' \code{\link{is_freq}} verifies frequencies;
#' \code{\link{num}} contains basic numeric parameters;
#' \code{\link{init_num}} initializes basic numeric parameters;
#' \code{\link{freq}} contains current frequency information;
#' \code{\link{comp_freq}} computes current frequency information;
#' \code{\link{prob}} contains current probability information;
#' \code{\link{comp_prob}} computes current probability information.
#'
#' @examples
#' N <- 1000   # => sets a population size of 1000
#' is_freq(N)  # => TRUE
#' is_prob(N)  # => FALSE (as N is no probability)

N <- 0  # default population size N



## ***: 4 ESSENTIAL frequencies: SDT cases/classes of hi mi fa cr  -------

##   (1) hi*** = TP: -------

#' Frequency of hits or true positives (TP).
#'
#' \code{hi} is the frequency of hits
#' or true positives (\code{TP})
#' in a population of \code{\link{N}} individuals.
#'
#' Definition: \code{hi}
#' is the frequency of individuals for which
#' \code{Condition = TRUE} and \code{Decision = TRUE} (positive).
#'
#' \code{hi} is a measure of correct classifications,
#' not an individual case.
#'
#' Relationships:
#' \enumerate{
#' \item to probabilities:
#' The frequency \code{hi} depends on the sensitivity \code{\link{sens}}
#' (aka. hit rate or true positive rate, TPR)
#' and is conditional on the prevalence \code{\link{prev}}.
#'
#' \item to other frequencies:
#' In a population of size \code{\link{N}}
#' the following relationships hold:
#'
#'   \itemize{
#'
#'     \item \code{\link{N} = \link{cond.true} + \link{cond.false}} (by condition)
#'
#'     \item \code{\link{N} = \link{dec.pos} + \link{dec.neg}} (by decision)
#'
#'     \item \code{\link{N} = \link{dec.cor} + \link{dec.err}} (by correspondence of decision to condition)
#'
#'     \item \code{\link{N} = \link{hi} + \link{mi} + \link{fa} + \link{cr}} (by condition x decision)
#'    }
#' }
#'
#' @aliases
#' TP
#'
#' @family frequencies
#' @family essential parameters
#'
#' @seealso
#' \code{\link{sens}} is the probability of hits or hit rate \code{\link{HR}};
#' \code{\link{num}} contains basic numeric parameters;
#' \code{\link{init_num}} initializes basic numeric parameters;
#' \code{\link{freq}} contains current frequency information;
#' \code{\link{comp_freq}} computes current frequency information;
#' \code{\link{prob}} contains current probability information;
#' \code{\link{comp_prob}} computes current probability information;
#' \code{\link{is_freq}} verifies frequencies.

hi <- 0  # default hits (TP)


##   (2) mi*** = FN: -------

#' Frequency of misses or false negatives (FN).
#'
#' \code{mi} is the frequency of misses
#' or false negatives (\code{FN})
#' in a population of \code{\link{N}} individuals.
#'
#' Definition:
#' \code{mi} is the frequency of individuals for which
#' \code{Condition = TRUE} and \code{Decision = FALSE} (negative).
#'
#' \code{mi} is a measure of incorrect classifications
#' (type-II errors), not an individual case.
#'
#' Relationships:
#' \enumerate{
#' \item to probabilities:
#' The frequency \code{mi} depends on the miss rate \code{\link{mirt}}
#' (aka. false negative rate, FNR)
#' and is conditional on the prevalence \code{\link{prev}}.
#'
#' \item to other frequencies:
#' In a population of size \code{\link{N}}
#' the following relationships hold:
#'
#'   \itemize{
#'
#'     \item \code{\link{N} = \link{cond.true} + \link{cond.false}} (by condition)
#'
#'     \item \code{\link{N} = \link{dec.pos} + \link{dec.neg}} (by decision)
#'
#'     \item \code{\link{N} = \link{dec.cor} + \link{dec.err}} (by correspondence of decision to condition)
#'
#'     \item \code{\link{N} = \link{hi} + \link{mi} + \link{fa} + \link{cr}} (by condition x decision)
#'    }
#' }
#'
#' @aliases
#' FN
#' type-II-errors
#'
#' @family essential parameters
#' @family frequencies
#'
#' @seealso
#' \code{\link{mirt}} is the probability or rate of misses;
#' \code{\link{num}} contains basic numeric parameters;
#' \code{\link{init_num}} initializes basic numeric parameters;
#' \code{\link{freq}} contains current frequency information;
#' \code{\link{comp_freq}} computes current frequency information;
#' \code{\link{prob}} contains current probability information;
#' \code{\link{comp_prob}} computes current probability information;
#' \code{\link{is_freq}} verifies frequencies.

mi <- 0  # default misses (FN)


##   (3) fa*** = FP: -------

#' Frequency of false alarms or false positives (FP).
#'
#' \code{fa} is the frequency of false alarms
#' or false positives (\code{FP})
#' in a population of \code{\link{N}} individuals.
#'
#' Definition:
#' \code{fa} is the frequency of individuals for which
#' \code{Condition = FALSE} and \code{Decision = TRUE} (positive).
#'
#' \code{fa} is a measure of incorrect classifications
#' (type-I-errors), not an individual case.
#'
#' Relationships:
#' \enumerate{
#' \item to probabilities:
#' The frequency \code{fa} depends on the false alarm rate \code{\link{fart}}
#' (aka. false positive rate, FPR)
#' and is conditional on the prevalence \code{\link{prev}}.
#'
#' \item to other frequencies:
#' In a population of size \code{\link{N}}
#' the following relationships hold:
#'
#'   \itemize{
#'
#'     \item \code{\link{N} = \link{cond.true} + \link{cond.false}} (by condition)
#'
#'     \item \code{\link{N} = \link{dec.pos} + \link{dec.neg}} (by decision)
#'
#'     \item \code{\link{N} = \link{dec.cor} + \link{dec.err}} (by correspondence of decision to condition)
#'
#'     \item \code{\link{N} = \link{hi} + \link{mi} + \link{fa} + \link{cr}} (by condition x decision)
#'    }
#' }
#'
#' @aliases
#' FP
#' type-I-errors
#'
#' @family essential parameters
#' @family frequencies
#'
#' @seealso
#' \code{\link{fart}} is the probability of false alarms
#' (aka. false positive rate \code{\link{FPR}} or \code{\link{fallout}});
#' \code{\link{num}} contains basic numeric parameters;
#' \code{\link{init_num}} initializes basic numeric parameters;
#' \code{\link{freq}} contains current frequency information;
#' \code{\link{comp_freq}} computes current frequency information;
#' \code{\link{prob}} contains current probability information;
#' \code{\link{comp_prob}} computes current probability information;
#' \code{\link{is_freq}} verifies frequencies.

fa <- 0  # default false alarms (FP)


##   (4) cr*** = TN: -------

#' Frequency of correct rejections or true negatives (TN).
#'
#' \code{cr} is the frequency of correct rejections
#' or true negatives (\code{TN})
#' in a population of \code{\link{N}} individuals.
#'
#' Definition:
#' \code{cr} is the frequency of individuals for which
#' \code{Condition = FALSE} and \code{Decision = FALSE} (negative).
#'
#' \code{cr} is a measure of correct classifications,
#' not an individual case.
#'
#' Relationships:
#' \enumerate{
#' \item to probabilities:
#' The frequency \code{cr} depends on the specificity \code{\link{spec}}
#' (aka. true negative rate, TNR)
#' and is conditional on the prevalence \code{\link{prev}}.
#'
#' \item to other frequencies:
#' In a population of size \code{\link{N}}
#' the following relationships hold:
#'
#'   \itemize{
#'
#'     \item \code{\link{N} = \link{cond.true} + \link{cond.false}} (by condition)
#'
#'     \item \code{\link{N} = \link{dec.pos} + \link{dec.neg}} (by decision)
#'
#'     \item \code{\link{N} = \link{dec.cor} + \link{dec.err}} (by correspondence of decision to condition)
#'
#'     \item \code{\link{N} = \link{hi} + \link{mi} + \link{fa} + \link{cr}} (by condition x decision)
#'    }
#' }
#'
#' @aliases TN
#'
#' @family essential parameters
#' @family frequencies
#'
#' @seealso
#' \code{\link{spec}} is the specificity or correct rejection rate
#' (aka. true negative rate \code{\link{TNR}});
#' \code{\link{num}} contains basic numeric parameters;
#' \code{\link{init_num}} initializes basic numeric parameters;
#' \code{\link{freq}} contains current frequency information;
#' \code{\link{comp_freq}} computes current frequency information;
#' \code{\link{prob}} contains current probability information;
#' \code{\link{comp_prob}} computes current probability information;
#' \code{\link{is_freq}} verifies frequencies.

cr <- 0  # default correct rejections (TN)



## (B) COMBINED frequencies: ---------

## 3 perspectives: Each combines 2 pairs of essential frequencies: --------

## (a) by condition: cond.true vs. cond.false  ---------
##     (= 2 columns of confusion matrix)


##   (5) cond.true -------

#' Number of individuals for which the condition is true.
#'
#' \code{cond.true} is a frequency that describes the
#' number of individuals in the current population \code{\link{N}}
#' for which the condition is \code{TRUE} (i.e., actually true cases).
#'
#' Key relationships:
#'
#' \enumerate{
#'
#' \item to probabilities:
#' The frequency of \code{cond.true} individuals depends on the population size \code{\link{N}} and
#' the condition's prevalence \code{\link{prev}} and is split further into two subsets of
#' \code{\link{hi}} by the sensitivity \code{\link{sens}} and
#' \code{\link{mi}} by the miss rate \code{\link{mirt}}.
#'
#' Perspectives:
#'
#' \enumerate{
#'   \item by condition:
#'
#'   The frequency \code{\link{cond.true}} is determined by the population size \code{\link{N}} times the prevalence \code{\link{prev}}:
#'
#'   \code{ \link{cond.true} = \link{N} x \link{prev}}
#'
#'   \item by decision:
#'
#'   a. The frequency \code{\link{hi}} is determined by \code{\link{cond.true}} times the sensitivity \code{\link{sens}}
#'   (aka. hit rate \code{\link{HR}}):
#'
#'   \code{ \link{hi} = \link{cond.true} x \link{sens}}
#'
#'
#'   b. The frequency \code{\link{mi}} is determined by \code{\link{cond.true}} times the miss rate \code{\link{mirt} = (1 - \link{sens})}:
#'
#'   \code{ \link{mi} = \link{cond.true} x \link{mirt}  =  \link{cond.true} x (1 - \link{sens})}
#'
#' }
#'
#'
#'   \item to other frequencies:
#'   In a population of size \code{\link{N}} the following relationships hold:
#'
#'   \itemize{
#'
#'     \item \code{\link{N} = \link{cond.true} + \link{cond.false}} (by condition)
#'
#'     \item \code{\link{N} = \link{dec.pos} + \link{dec.neg}} (by decision)
#'
#'     \item \code{\link{N} = \link{dec.cor} + \link{dec.err}} (by correspondence of decision to condition)
#'
#'     \item \code{\link{N} = \link{hi} + \link{mi} + \link{fa} + \link{cr}} (by condition x decision)
#'   }
#'
#' }
#'
#' Current frequency information is computed by
#' \code{\link{comp_freq}} and contained in a list
#' \code{\link{freq}}.
#'
#'
#' @references
#' Consult \href{https://en.wikipedia.org/wiki/Confusion_matrix}{Wikipedia: Confusion matrix} for additional information.
#'
#' @family frequencies
#'
#' @seealso
#' \code{\link{is_freq}} verifies frequencies;
#' \code{\link{num}} contains basic numeric parameters;
#' \code{\link{init_num}} initializes basic numeric parameters;
#' \code{\link{freq}} contains current frequency information;
#' \code{\link{comp_freq}} computes current frequency information;
#' \code{\link{prob}} contains current probability information;
#' \code{\link{comp_prob}} computes current probability information.
#'
#' @examples
#' cond.true <- 1000 * .10   # => sets cond.true to 10% of 1000 = 100 cases.
#' is_freq(cond.true)        # => TRUE
#' is_prob(cond.true)        # => FALSE, as cond.true is no probability (but prev and sens are)

cond.true <- 0  # default frequency of true cases


##   (6) cond.false -------

#' Number of individuals for which the condition is false.
#'
#' \code{cond.false} is a frequency that describes the
#' number of individuals in the current population \code{\link{N}}
#' for which the condition is \code{FALSE} (i.e., actually false cases).
#'
#' Key relationships:
#'
#' \enumerate{
#'
#' \item to probabilities:
#' The frequency of \code{cond.false} individuals depends on the population size \code{\link{N}} and
#' the complement of the condition's prevalence \code{1 - \link{prev}} and is split further into two subsets of
#' \code{\link{fa}} by the false alarm rate \code{\link{fart}} and
#' \code{\link{cr}} by the specificity \code{\link{spec}}.
#'
#' Perspectives:
#'
#' \enumerate{
#'   \item by condition:
#'
#'   The frequency \code{\link{cond.false}} is determined by the population size \code{\link{N}} times the complement of the prevalence \code{(1 - \link{prev})}:
#'
#'   \code{\link{cond.false}= \link{N} x (1 - \link{prev})}
#'
#'   \item by decision:
#'
#'   a. The frequency \code{\link{fa}} is determined by \code{\link{cond.false}} times the false alarm rate \code{\link{fart} = (1 - \link{spec})}
#'   (aka. \code{\link{FPR}}):
#'
#'   \code{\link{fa} = \link{cond.false} x \link{fart} = \link{cond.false} x (1 - \link{spec}) }
#'
#'
#'   b. The frequency \code{\link{cr}} is determined by \code{\link{cond.false}} times the specificity \code{\link{spec} = (1 - \link{fart})}:
#'
#'   \code{\link{cr}  =  \link{cond.false} x \link{spec}  =  \link{cond.false} x (1 - \link{fart}) }
#'
#' }
#'
#'
#'   \item to other frequencies:
#'   In a population of size \code{\link{N}} the following relationships hold:
#'
#'   \itemize{
#'
#'     \item \code{\link{N} = \link{cond.true} + \link{cond.false}} (by condition)
#'
#'     \item \code{\link{N} = \link{dec.pos} + \link{dec.neg}} (by decision)
#'
#'     \item \code{\link{N} = \link{dec.cor} + \link{dec.err}} (by correspondence of decision to condition)
#'
#'     \item \code{\link{N} = \link{hi} + \link{mi} + \link{fa} + \link{cr}} (by condition x decision)
#'   }
#'
#' }
#'
#' Current frequency information is computed by
#' \code{\link{comp_freq}} and contained in a list
#' \code{\link{freq}}.
#'
#'
#' @references
#' Consult \href{https://en.wikipedia.org/wiki/Confusion_matrix}{Wikipedia: Confusion matrix} for additional information.
#'
#' @family frequencies
#'
#' @seealso
#' \code{\link{is_freq}} verifies frequencies;
#' \code{\link{num}} contains basic numeric parameters;
#' \code{\link{init_num}} initializes basic numeric parameters;
#' \code{\link{freq}} contains current frequency information;
#' \code{\link{comp_freq}} computes current frequency information;
#' \code{\link{prob}} contains current probability information;
#' \code{\link{comp_prob}} computes current probability information.
#'
#' @examples
#' cond.false <- 1000 * .90  # => sets cond.false to 90% of 1000 = 900 cases.
#' is_freq(cond.false)       # => TRUE
#' is_prob(cond.false)       # => FALSE, as cond.false is no probability [but (1 - prev) and spec are]

cond.false <- 0  # default frequency of false cases




## (b) by decision: dec.pos vs. dec.neg ----------
##     (= 2 rows of confusion matrix)


##   (7) dec.pos  -------

#' Number of individuals for which the decision is positive.
#'
#' \code{dec.pos} is a frequency that describes the
#' number of individuals in the current population \code{\link{N}}
#' for which the decision is positive (i.e., called or predicted cases).
#'
#' Key relationships:
#'
#' \enumerate{
#'
#' \item to probabilities:
#' The frequency of \code{dec.pos} individuals depends on the population size \code{\link{N}} and
#' the decision's proportion of positive decisions \code{\link{ppod}} and is split further into two subsets of
#' \code{\link{hi}} by the positive predictive value \code{\link{PPV}} and
#' \code{\link{fa}} by the false detection rate \code{\link{FDR} = 1 - \link{PPV}}.
#'
#' Perspectives:
#'
#' \enumerate{
#'   \item by condition:
#'
#'   The frequency \code{\link{dec.pos}} is determined by the population size \code{\link{N}} times
#'   the proportion of positive decisions \code{\link{ppod}}:
#'
#'   \code{\link{dec.pos}  =  \link{N} x \link{ppod}}
#'
#'   \item by decision:
#'
#'   a. The frequency \code{\link{hi}} is determined by \code{\link{dec.pos}} times the positive predictive value \code{\link{PPV}}
#'   (aka. \code{\link{precision}}):
#'
#'   \code{\link{hi}  =  \link{dec.pos} x \link{PPV}}
#'
#'
#'   b. The frequency \code{\link{fa}} is determined by \code{\link{dec.pos}} times the false detection rate \code{\link{FDR} = (1 - \link{PPV})}:
#'
#'   \code{\link{fa}  =  \link{dec.pos} x \link{FDR}  =  \link{dec.pos} x (1 - \link{PPV})}
#'
#' }
#'
#'
#'   \item to other frequencies:
#'   In a population of size \code{\link{N}} the following relationships hold:
#'
#'   \itemize{
#'
#'     \item \code{\link{N}  =  \link{cond.true} + \link{cond.false}} (by condition)
#'
#'     \item \code{\link{N}  =  \link{dec.pos} + \link{dec.neg}} (by decision)
#'
#'     \item \code{\link{N} = \link{dec.cor} + \link{dec.err}} (by correspondence of decision to condition)
#'
#'     \item \code{\link{N}  =  \link{hi} + \link{mi} + \link{fa} + \link{cr}} (by condition x decision)
#'   }
#'
#' }
#'
#' Current frequency information is computed by
#' \code{\link{comp_freq}} and contained in a list
#' \code{\link{freq}}.
#'
#'
#' @references
#' Consult \href{https://en.wikipedia.org/wiki/Confusion_matrix}{Wikipedia: Confusion matrix} for additional information.
#'
#' @family frequencies
#'
#' @seealso
#' \code{\link{is_freq}} verifies frequencies;
#' \code{\link{num}} contains basic numeric parameters;
#' \code{\link{init_num}} initializes basic numeric parameters;
#' \code{\link{freq}} contains current frequency information;
#' \code{\link{comp_freq}} computes current frequency information;
#' \code{\link{prob}} contains current probability information;
#' \code{\link{comp_prob}} computes current probability information.
#'
#' @examples
#' dec.pos <- 1000 * .33   # => sets dec.pos to 33% of 1000 = 330 cases.
#' is_freq(dec.pos)        # => TRUE
#' is_prob(dec.pos)        # => FALSE, as dec.pos is no probability (but ppod and PPV are)

dec.pos <- 0  # default frequency of positive decisions


##   (8) dec.neg -------

#' Number of individuals for which the decision is negative.
#'
#' \code{dec.neg} is a frequency that describes the
#' number of individuals in the current population \code{\link{N}}
#' for which the decision is negative (i.e., cases not called or not predicted).
#'
#' Key relationships:
#'
#' \enumerate{
#'
#' \item to probabilities:
#' The frequency of \code{dec.neg} individuals depends on the population size \code{\link{N}} and
#' the decision's proportion of negative decisions \code{(1 - \link{ppod})} and is split further into two subsets of
#' \code{\link{cr}} by the negative predictive value \code{\link{NPV}} and
#' \code{\link{mi}} by the false omission rate \code{\link{FOR} = 1 - \link{NPV}}.
#'
#' Perspectives:
#'
#' \enumerate{
#'   \item by condition:
#'
#'   The frequency \code{\link{dec.neg}} is determined by the population size \code{\link{N}} times
#'   the proportion of negative decisions \code{(1 - \link{ppod})}:
#'
#'   \code{\link{dec.neg}  =  \link{N} x (1 - \link{ppod})}
#'
#'   \item by decision:
#'
#'   a. The frequency \code{\link{cr}} is determined by \code{\link{dec.neg}} times the negative predictive value \code{\link{NPV}}:
#'
#'   \code{\link{cr}  =  \link{dec.neg} x \link{NPV}}
#'
#'
#'   b. The frequency \code{\link{mi}} is determined by \code{\link{dec.neg}} times the false omission rate \code{\link{FOR} = (1 - \link{NPV})}:
#'
#'   \code{\link{mi}  =  \link{dec.neg} x \link{FOR}  =  \link{dec.neg} x (1 - \link{NPV})}
#'
#' }
#'
#'
#'   \item to other frequencies:
#'   In a population of size \code{\link{N}} the following relationships hold:
#'
#'   \itemize{
#'
#'     \item \code{\link{N}  =  \link{cond.true} + \link{cond.false}} (by condition)
#'
#'     \item \code{\link{N}  =  \link{dec.pos} + \link{dec.neg}} (by decision)
#'
#'     \item \code{\link{N} = \link{dec.cor} + \link{dec.err}} (by correspondence of decision to condition)
#'
#'     \item \code{\link{N}  =  \link{hi} + \link{mi} + \link{fa} + \link{cr}} (by condition x decision)
#'   }
#'
#' }
#'
#' Current frequency information is computed by
#' \code{\link{comp_freq}} and contained in a list
#' \code{\link{freq}}.
#'
#'
#' @references
#' Consult \href{https://en.wikipedia.org/wiki/Confusion_matrix}{Wikipedia: Confusion matrix} for additional information.
#'
#' @family frequencies
#'
#' @seealso
#' \code{\link{is_freq}} verifies frequencies;
#' \code{\link{num}} contains basic numeric parameters;
#' \code{\link{init_num}} initializes basic numeric parameters;
#' \code{\link{freq}} contains current frequency information;
#' \code{\link{comp_freq}} computes current frequency information;
#' \code{\link{prob}} contains current probability information;
#' \code{\link{comp_prob}} computes current probability information.
#'
#' @examples
#' dec.neg <- 1000 * .67   # => sets dec.neg to 67% of 1000 = 670 cases.
#' is_freq(dec.neg)        # => TRUE
#' is_prob(dec.neg)        # => FALSE, as dec.neg is no probability (but ppod, NPV and FOR are)

dec.neg <- 0  # default frequency of negative decisions



## (c) by accuracy/correspondence of decision to condition: ---------
##     dec.cor vs. dec.err (= 2 diagonals of confusion matrix)

##   (9) dec.cor --------

# NOTE: "dec.cor" should better be called "dec.acc"
#       (for consistency with probabilities "acc" vs. "err")!

#' Number of individuals for which the decision is correct.
#'
#' \code{dec.cor} is a frequency that describes the
#' number of individuals in the current population \code{\link{N}}
#' for which the decision is correct/accurate
#' (i.e., cases in which the decision corresponds to the condition).
#'
#' Key relationships:
#'
#' \enumerate{
#'
#' \item to probabilities:
#' The frequency of \code{dec.cor} individuals depends on the population size \code{\link{N}} and
#' the accuracy \code{\link{acc}}.
#'
#' \item to other frequencies:
#' In a population of size \code{\link{N}} the following relationships hold:
#'
#'   \itemize{
#'
#'     \item \code{\link{N}  =  \link{cond.true} + \link{cond.false}} (by condition)
#'
#'     \item \code{\link{N}  =  \link{dec.pos} + \link{dec.neg}} (by decision)
#'
#'     \item \code{\link{N} = \link{dec.cor} + \link{dec.err}} (by correspondence of decision to condition)
#'
#'     \item \code{\link{dec.cor} = \link{hi} + \link{cr}}
#'
#'     \item \code{\link{dec.err} = \link{mi} + \link{fa}}
#'
#'     \item \code{\link{N}  =  \link{hi} + \link{mi} + \link{fa} + \link{cr}} (by condition x decision)
#'   }
#'
#' \item correspondence:
#' When not rounding the frequencies of \code{\link{freq}} then
#'
#' \code{dec.cor = N x acc = hi + cr}
#'
#' (i.e., \code{dec.cor} corresponds to the sum of true positives \code{\link{hi}} and true negatives \code{\link{cr}}.
#'
#' }
#'
#' Current frequency information is computed by
#' \code{\link{comp_freq}} and contained in a list
#' \code{\link{freq}}.
#'
#'
#' @references
#' Consult \href{https://en.wikipedia.org/wiki/Confusion_matrix}{Wikipedia: Confusion matrix} for additional information.
#'
#' @family frequencies
#'
#' @seealso
#' \code{\link{is_freq}} verifies frequencies;
#' \code{\link{num}} contains basic numeric parameters;
#' \code{\link{init_num}} initializes basic numeric parameters;
#' \code{\link{freq}} contains current frequency information;
#' \code{\link{comp_freq}} computes current frequency information;
#' \code{\link{prob}} contains current probability information;
#' \code{\link{comp_prob}} computes current probability information.
#'
#' @examples
#' dec.cor <- 1000 * .50   # => sets dec.cor to 50% of 1000 = 500 cases.
#' is_freq(dec.cor)        # => TRUE
#' is_prob(dec.cor)        # => FALSE, as dec.cor is no probability (but acc, bacc/wacc ARE)

dec.cor <- 0  # default frequency of negative decisions



##  (10) dec.err --------

#' Number of individuals for which the decision is erroneous.
#'
#' \code{dec.err} is a frequency that describes the
#' number of individuals in the current population \code{\link{N}}
#' for which the decision is incorrect or erroneous (i.e., cases in which the
#' decision does not correspond to the condition).
#'
#' Key relationships:
#'
#' \enumerate{
#'
#' \item to probabilities:
#' The frequency of \code{dec.err} individuals depends on the population size \code{\link{N}} and
#' is equal to the sum of false negatives \code{\link{mi}} and false positives \code{\link{fa}}.
#'
#' \item to other frequencies:
#'In a population of size \code{\link{N}} the following relationships hold:
#'
#'   \itemize{
#'
#'     \item \code{\link{N}  =  \link{cond.true} + \link{cond.false}} (by condition)
#'
#'     \item \code{\link{N}  =  \link{dec.pos} + \link{dec.neg}} (by decision)
#'
#'     \item \code{\link{N} = \link{dec.cor} + \link{dec.err}} (by correspondence of decision to condition)
#'
#'     \item \code{\link{dec.cor} = \link{hi} + \link{cr}}
#'
#'     \item \code{\link{dec.err} = \link{mi} + \link{fa}}
#'
#'     \item \code{\link{N}  =  \link{hi} + \link{mi} + \link{fa} + \link{cr}} (by condition x decision)
#'   }
#'
#' }
#'
#' Current frequency information is computed by
#' \code{\link{comp_freq}} and contained in a list
#' \code{\link{freq}}.
#'
#'
#' @references
#' Consult \href{https://en.wikipedia.org/wiki/Confusion_matrix}{Wikipedia: Confusion matrix} for additional information.
#'
#' @family frequencies
#'
#' @seealso
#' \code{\link{is_freq}} verifies frequencies;
#' \code{\link{num}} contains basic numeric parameters;
#' \code{\link{init_num}} initializes basic numeric parameters;
#' \code{\link{freq}} contains current frequency information;
#' \code{\link{comp_freq}} computes current frequency information;
#' \code{\link{prob}} contains current probability information;
#' \code{\link{comp_prob}} computes current probability information.
#'
#' @examples
#' dec.err <- 1000 * .50   # => sets dec.err to 50% of 1000 = 500 cases.
#' is_freq(dec.err)        # => TRUE
#' is_prob(dec.err)        # => FALSE, as dec.err is no probability (but acc, bacc/wacc ARE)

dec.err <- 0  # default frequency of negative decisions



## (*) Done: -----------

## - Clean up code [2018 09 02].


## (+) ToDo: ----------

## - ...

## eof. ------------------------------------------

