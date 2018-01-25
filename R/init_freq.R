## init_freq.R | riskyR
## 2018 01 25
## -----------------------------------------------
## Define and initialize ALL frequencies
## -----------------------------------------------

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

## 1: Bayesian: starting with 3 basic probabilities:
## - given:   prev;  sens, spec
## - derived: all other values

## 2: Natural frequencies:
## - given:   N = hi, mi, fa, cr
## - derived: all other values

## -----------------------------------------------

## -----------------------------------------------
## (A) Define and initializes BASIC frequencies:
## -----------------------------------------------
## (0) N: population size

#' Number of individuals in the population.
#'
#' \code{N} is a frequency that describes the
#' number of individuals in the current population
#' (i.e., the overall number of cases considered).
#'
#' \emph{Relationships}:
#' \enumerate{
#'
#' \item to probabilities:
#' A population of \code{N} individuals can be split into 2 subsets
#' in 2 different ways:
#'
#' \enumerate{
#' \item by condition:
#' The frequency \code{\link{cond.true}} depends on the prevalence \code{\link{prev}}
#' and
#' the frequency \code{\link{cond.false}} depends on the prevalence complement \code{1 - \link{prev}}.
#'
#' \item by decision:
#' The frequency \code{\link{dec.pos}} depends on the prevalence \code{\link{prev}}
#' and
#' the frequency \code{\link{dec.neg}} depends on the prevalence complement \code{1 - \link{prev}}.
#'
#' }
#'
#' The frequency \code{cr} depends on the specificity \code{\link{spec}}
#' (aka. true negative rate, TNR)
#' and is conditional on the prevalence \code{\link{prev}}.
#'
#' \item to other frequencies:
#' In a population of size \code{\link{N}}
#' the following relationships hold:
#'
#' \itemize{
#'
#'   \item \code{\link{N} = \link{cond.true} + \link{cond.false}}
#'
#'   \item \code{\link{N} = \link{dec.pos} + \link{dec.neg}}
#'
#'   \item \code{\link{N} = \link{hi} + \link{mi} + \link{fa} + \link{cr}}
#' }
#'
#' }
#'
#' The current frequency information is computed by
#' \code{\link{comp_freq}} and contained in a list
#' \code{\link{freq}}.
#'
#' If \code{N} is unknown, a suitable minimum value
#' can be computed by \code{\link{comp_min_N}}.
#'
#' @references
#' Consult \href{https://en.wikipedia.org/wiki/Statistical_population}{Wikipedia} for additional information.
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

## -----------------------------------------------
## ***: 4 essential frequencies: hi mi fa cr
## -----------------------------------------------
## (1) hi*** = TP:

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
#'   \item \code{\link{N} = \link{cond.true} + \link{cond.false}}
#'
#'   \item \code{\link{N} = \link{dec.pos} + \link{dec.neg}}
#'
#'   \item \code{\link{N} = \link{hi} + \link{mi} + \link{fa} + \link{cr}}
#'    }
#' }
#'
#' @aliases TP
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

## -----------------------------------------------
## (2) mi*** = FN:

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
#'   \item \code{\link{N} = \link{cond.true} + \link{cond.false}}
#'
#'   \item \code{\link{N} = \link{dec.pos} + \link{dec.neg}}
#'
#'   \item \code{\link{N} = \link{hi} + \link{mi} + \link{fa} + \link{cr}}
#'    }
#' }
#'
#' @aliases FN
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

## -----------------------------------------------
## (3) fa*** = FP:

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
#'   \item \code{\link{N} = \link{cond.true} + \link{cond.false}}
#'
#'   \item \code{\link{N} = \link{dec.pos} + \link{dec.neg}}
#'
#'   \item \code{\link{N} = \link{hi} + \link{mi} + \link{fa} + \link{cr}}
#'    }
#' }
#'
#' @aliases FP
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

## -----------------------------------------------
## (4) cr*** = TN:

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
#'   \item \code{\link{N} = \link{cond.true} + \link{cond.false}}
#'
#'   \item \code{\link{N} = \link{dec.pos} + \link{dec.neg}}
#'
#'   \item \code{\link{N} = \link{hi} + \link{mi} + \link{fa} + \link{cr}}
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

## -----------------------------------------------
## (B) Define and initialize COMBINED frequencies:
## -----------------------------------------------

# +++ here now +++

## -----------------------------------------------
## (a) cond.true cond.false
## -----------------------------------------------

## -----------------------------------------------
## (b) dec.pos dec.neg
## -----------------------------------------------

## -----------------------------------------------
## (+) ToDo:


## -----------------------------------------------
## eof.
