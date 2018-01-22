## comp_prob.R | riskyR
## 2018 01 22
## -----------------------------------------------
## Compute current probabilities (prob) based on num
## (using only the necessary parameters of num):

## Note: Always use num (essential) rather than env (NON-essential)!

## -----------------------------------------------
## (A) ToDo: Compute basic parameters (prev, sens, spec, fart)
##           from existing frequencies!

## -----------------------------------------------
## (B) Define predictive values (PPV and NPV)
##     with corresponding documentation:
## -----------------------------------------------
## (a) PPV:

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

PPV <- NA # default positive predictive value (PPV)

## -----------------------------------------------
## (b) NPV:

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
#' \code{\link{comp_freq}} computes natural frequencies from probabilities
#'
#' @examples
#' NPV <- .95     # => sets a negative predictive value of 95%
#' NPV <- 95/100  # => (condition = FALSE) for 95 people out of 100 people for which (decision = negative)
#' is_prob(NPV)   # => TRUE (as NPV is a probability)

NPV <- NA # default negative predictive value (NPV)

## -----------------------------------------------
## (c) FDR: # complement of PPV

#' The false discovery or false detection rate of a decision process or diagnostic procedure.
#'
#' \code{FDR} defines a decision's false discovery or
#' false detection rate (\code{FDR}):
#' The conditional probability of the condition being \code{FALSE}
#' provided that the decision is positive.
#'
#' Understanding or obtaining the false discovery or
#' false detection rate (\code{FDR}):
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
#'   \code{sens = p(decision = positive | condition = FALSE)}
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
#' \code{\link{comp_freq}} computes natural frequencies from probabilities
#'
#' @examples
#' FDR <- .45     # => sets a false discovery rate (FDR) of 45%
#' FDR <- 45/100  # => (condition = FALSE) for 45 people out of 100 people for which (decision = positive)
#' is_prob(FDR)   # => TRUE (as FDR is a probability)

FDR <- NA # default false discorvery rate (FDR)

## -----------------------------------------------
## (d) FOR: False omission rate = complement of NPV

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
#'   \code{FOR} is the conditional probability
#'   for the condition being \code{TRUE}
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
#'   of the missing rate (\code{1 - \link{sens}}):
#'
#'   \code{missing rate = p(decision = negative | condition = TRUE)}
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

FOR <- NA # default false omission rate (FOR)

## -----------------------------------------------
## (C) Compute predictive values (PVs)
##  C1: from probabilities:
## -----------------------------------------------
## 1. Positive predictive value (PPV) from probabilities:

comp_PPV <- function(prev = num$prev, sens = num$sens, spec = num$spec) {

  PPV <- NA # initialize

  ## ToDo: Add condition
  ## if (is_valid(prev, sens, spec, fart)) { ... }

  ## PPV = hits / positive decision = hits / (hits + false alarms):
  hi <- (prev * sens)
  fa <- (1 - prev) * (1 - spec)

  PPV <- hi / (hi + fa)

  ## Check:
  if (is.nan(PPV)) {
    warning("PPV is NaN.")
  }

  return(PPV)
}

## Check:
## for extreme values:
## comp_PPV(0, 0, 1)  # => NaN, as hi = 0 and fa = 0:  0/0

## -----------------------------------------------
## 2. False discovery/detection rate (FDR = complement of PPV):

comp_FDR <- function(prev = num$prev, sens = num$sens, spec = num$spec) {

  PPV <- NA # initialize
  FDR <- NA

  ## ToDo: Add condition
  ## if (is_valid(prev, sens, spec, fart)) { ... }

  PPV <- comp_PPV(prev, sens, spec)
  FDR <- (1 - PPV) # FDR is the complement of PPV

  return(FDR)
}

## -----------------------------------------------
## 3. Negative predictive value (NPV) from probabilities:

comp_NPV <- function(prev = num$prev, sens = num$sens, spec = num$spec) {

  NPV <- NA # initialize

  ## ToDo: Add condition
  ## if (is_valid(prev, sens, spec, fart)) { ... }

  ## NPV = cr / negative decision = cr / (cr + mi):
  cr <- (1 - prev) * spec
  mi <- prev * (1 - sens)

  NPV <- cr / (cr + mi)

  ## Check:
  if (is.nan(NPV)) {
    warning("NPV is NaN.")
  }

  return(NPV)
}

## Check:
## for extreme values:
## comp_NPV(1, 1, 1)  # => NaN, as cr = 0 and mi = 0: 0/0
## comp_NPV(1, 1, 0)  # => NaN, as cr = 0 and mi = 0: 0/0

## -----------------------------------------------
## 4. False omission rate (FOR = complement of NPV):

comp_FOR <- function(prev = num$prev, sens = num$sens, spec = num$spec) {

  NPV <- NA # initialize
  FOR <- NA

  ## ToDo: Add condition
  ## if (is_valid(prev, sens, spec, fart)) { ... }

  NPV <- comp_NPV(prev, sens, spec)
  FOR <- (1 - NPV) # FOR is the complement of NPV

  return(FOR)
}

## -----------------------------------------------
## (C) Compute predictive values
##     from frequencies (various versions):

## ToDo: Add alternative ways to compute probabilities
##       from frequencies (based on different elements of freq)!
##
## Moved to separate file: comp_prob_freq.R !!!

## -----------------------------------------------
## (D) Compute the set of ALL current probabilities:
##     So far: Compute current values of PPV and NPV
##     as functions of prev, sens, and spec (using Bayes):

#' Compute derived probabilities from basic probabilities.
#'
#' \code{comp_prob} is a function that computes derived probabilities
#' (typically conditional probabilities) from basic probabilities --
#' \code{\link{prev}} and \code{\link{sens}}, and
#' \code{\link{spec}} or \code{\link{fart}} (\code{spec = 1 - fart}).
#'
#' By default, \code{comp_prob} assumes that sufficient
#' basic probabilities (e.g., \code{\link{prev}}, \code{\link{sens}},
#' and either \code{\link{spec}} or \code{\link{fart}}) are provided
#' as inputs and then computes and returns derived probabilities (e.g., the
#' predictive values \code{\link{PPV}} and \code{\link{NPV}}, as well
#' as their complements \code{\link{FDR}} and \code{\link{FOR}})
#' as its output (a list of probabilities \code{\link{prob}}).
#'
#' \code{comp_prob} is the probability counterpart to the
#' frequency function \code{\link{comp_freq}}.
#'
#' Extreme probabilities (sets containing two or more
#' probabilities of 0 or 1) may yield unexpected values
#' (e.g., predictive values \code{\link{PPV}} or \code{\link{NPV}}
#' turning \code{NaN} when \code{\link{is_extreme}}
#' evaluates to \code{TRUE}).
#'
#' @param prev The condition's prevalence value \code{\link{prev}}
#' (i.e., the probability of condition being \code{TRUE}).
#' @param sens The decision's sensitivity value \code{\link{sens}}
#' (i.e., the conditional probability of a positive decision
#' provided that the condition is \code{TRUE}).
#' @param spec The decision's specificity value \code{\link{spec}}
#' (i.e., the conditional probability
#' of a negative decision provided that the condition is \code{FALSE}).
#' \code{spec} is optional when is complement \code{fart} is provided.
#' @param fart The decision's false alarm rate \code{\link{fart}}
#' (i.e., the conditional probability
#' of a positive decision provided that the condition is \code{FALSE}).
#' \code{fart} is optional when its complement \code{spec} is provided.
#'
#' @return A list \code{prob} containing 8 probability values.
#'
#' @examples
#' comp_prob()             # => ok, using current defaults
#' length(comp_prob())     # => 8
#'
#' # Ways to succeed:
#' comp_prob(.999, 1, 1)   # => ok
#' comp_prob(1, .999, 1)   # => ok
#'
#' # Watch out for extreme cases:
#' comp_prob(1, 0, 1)      # => ok, but with warnings (as PPV & FDR are NaN)
#' comp_prob(1, 0, 0)      # => ok, but with warnings (as PPV & FDR are NaN)
#' comp_prob(1, 0, NA, 0)  # => ok, but with warnings (as PPV & FDR are NaN)
#' comp_prob(1, 0, NA, 1)  # => ok, but with warnings (as PPV & FDR are NaN)
#'
#' comp_prob(1, 1, 0)      # => ok, but with warnings (as NPV & FOR are NaN)
#' comp_prob(1, 1, 1)      # => ok, but with warnings (as NPV & FOR are NaN)
#' comp_prob(1, 1, 1, NA)  # => ok, but with warnings (as NPV & FOR are NaN)
#' comp_prob(1, 1, NA, 1)  # => ok, but with warnings (as NPV & FOR are NaN)
#'
#' # Ways to fail:
#' comp_prob(NA, 1, 1, NA)  # => NAs + warning: prev not numeric
#' comp_prob(1, NA, 1, NA)  # => NAs + warning: sens not numeric
#' comp_prob(8,  1, 1, NA)  # => NAs + warning: prev no probability
#' comp_prob(1,  8, 1, NA)  # => NAs + warning: sens no probability
#' comp_prob(1,  1, 1,  1)  # => NAs and warning: is_complement not in tolerated range
#'
#' @family functions computing probabilities
#'
#' @seealso
#' \code{\link{num}} contains basic numeric variables;
#' \code{\link{init_num}} initializes basic numeric variables;
#' \code{\link{freq}} contains current frequency information;
#' \code{\link{prob}} contains current probability information;
#' \code{\link{comp_prob}} computes current probability information;
#' \code{\link{is_valid}} verifies the validity of probability inputs;
#' \code{\link{comp_complement}} computes a complementary probability (if missing);
#' \code{\link{comp_min_N}} computes a suitable population size \code{\link{N}} (if missing)

comp_prob <- function(prev = num$prev, sens = num$sens,
                      spec = num$spec, fart = NA) {

  ## (0) Initialize prob as a list:
  prob <- list(

    ## (a) basic probability parameters:
    "prev" = NA,  # simple p
    "sens" = NA,  # conditional p
    "spec" = NA,  # conditional p: 1 - fart
    "fart" = NA,  # conditional p: 1 - spec

    ## (b) derived predictive values (PVs):
    "PPV" = NA,   # conditional p: reversal of sens
    "NPV" = NA,   # conditional p: reversal of spec
    "FDR" = NA,   # conditional p: 1 - PPV
    "FOR" = NA    # conditional p: 1 - NPV
  )

  ## (1) Only if basic quadruple of probabilities is valid:
  if (is_valid(prev, sens, spec, fart)) {

    ## (2) Compute missing fart or spec (4th argument) value (if applicable):
    cur.spec.fart <- comp_complement(spec, fart)
    spec <- cur.spec.fart[1] # 1st argument
    fart <- cur.spec.fart[2] # 2nd argument

    ## (3) Issue a warning if probabilities describe an extreme case:
    is_extreme(prev, sens, spec, fart)  # prints a warning if TRUE

    ## (4) Assign all values of prob based on current parameter values:
    ## (a) basic probability parameters:
    prob$prev <- prev
    prob$sens <- sens
    prob$spec <- spec
    prob$fart <- fart
    ## (b) derived predictive values (PVs):
    prob$PPV <- comp_PPV(prev, sens, spec) # Note: using probabilistic version (Bayes)
    prob$NPV <- comp_NPV(prev, sens, spec)
    prob$FDR <- comp_FDR(prev, sens, spec)
    prob$FOR <- comp_FOR(prev, sens, spec)

    ## (5) Check derived PVs:
    if ( is.na(prob$PPV) | is.nan(prob$PPV) | !is_prob(prob$PPV) |
         is.na(prob$NPV) | is.nan(prob$NPV) | !is_prob(prob$NPV) |
         is.na(prob$FDR) | is.nan(prob$FDR) | !is_prob(prob$FDR) |
         is.na(prob$FOR) | is.nan(prob$FOR) | !is_prob(prob$FOR) ) {

      warning( "Some PVs are peculiar. Check for extreme probabilities!" )

    }

  } # if (is_valid(prev, sens, spec, fart))

  ## (6) Return the entire list prob:
  return(prob)

}

## Check:
{
  # comp_prob()          # => ok, using current defaults
  # length(comp_prob())  # => 8
  #
  # # Ways to succeed:
  # comp_prob(.999, 1, 1)   # => ok
  # comp_prob(1, .999, 1)   # => ok
  #
  # # Watch out for extreme cases:
  # comp_prob(1, 0, 1)      # => ok, but with warnings (as PPV & FDR are NaN)
  # comp_prob(1, 0, 0)      # => ok, but with warnings (as PPV & FDR are NaN)
  # comp_prob(1, 0, NA, 0)  # => ok, but with warnings (as PPV & FDR are NaN)
  # comp_prob(1, 0, NA, 1)  # => ok, but with warnings (as PPV & FDR are NaN)
  #
  # comp_prob(1, 1, 0)      # => ok, but with warnings (as NPV & FOR are NaN)
  # comp_prob(1, 1, 1)      # => ok, but with warnings (as NPV & FOR are NaN)
  # comp_prob(1, 1, 1, NA)  # => ok, but with warnings (as NPV & FOR are NaN)
  # comp_prob(1, 1, NA, 1)  # => ok, but with warnings (as NPV & FOR are NaN)
  #
  # # Ways to fail:
  # comp_prob(NA, 1, 1, NA)  # => NAs + warning: prev not numeric
  # comp_prob(1, NA, 1, NA)  # => NAs + warning: sens not numeric
  # comp_prob(8,  1, 1, NA)  # => NAs + warning: prev no probability
  # comp_prob(1,  8, 1, NA)  # => NAs + warning: sens no probability
  # comp_prob(1,  1, 1,  1)  # => NAs and warning: is_complement not in tolerated range
}

## -----------------------------------------------
## (E) Apply to initialize prob:

#' List current probability information.
#'
#' \code{prob} is a list of named numeric variables
#' containing 4 basic (1 non-conditional and 3 conditional) probabilities
#' and 4 derived (conditional) probabilities:
#'
#' \enumerate{
#'
#'  \item the condition's prevalence value \code{\link{prev}}
#'  (i.e., the probability of condition being \code{TRUE}).
#'
#'  \item the decision's sensitivity value \code{\link{sens}}
#'  (i.e., the conditional probability of a positive decision
#'  provided that the condition is \code{TRUE}).
#'
#'  \item the decision's specificity value \code{\link{spec}}
#'  (i.e., the conditional probability
#'  of a negative decision provided that the condition is \code{FALSE}).
#'
#'  \item the decision's false alarm rate \code{\link{fart}}
#'  (i.e., the conditional probability
#'  of a positive decision provided that the condition is \code{FALSE}).
#'
#'  \item the decision's positive predictive value \code{\link{PPV}}
#' (i.e., the conditional probability of the condition being \code{TRUE}
#' provided that the decision is positive)
#'
#'  \item the decision's negative predictive value \code{\link{NPV}}
#' (i.e., the conditional probability of the condition being \code{FALSE}
#' provided that the decision is negative)
#'
#'  \item the decision's false discovery or false detection rate \code{\link{FDR}}
#' (i.e., the conditional probability of the condition being \code{FALSE}
#' provided that the decision is positive)
#'
#'  \item the decision's false omission rate \code{\link{FOR}}
#' (i.e., the conditional probability of the condition being \code{TRUE}
#' provided that the decision is negative)
#'
#' }
#'
#' These probabilities are computed from basic probabilities
#' (contained in \code{\link{num}}) and computed by using
#' \code{\link{comp_prob}}.
#'
#' The list \code{prob} is the probability counterpart
#' to the list containing frequency information \code{\link{freq}}.
#'
#' Note that inputs of extreme probabilities (of 0 or 1)
#' may yield unexpected values (e.g., an \code{\link{NPV}}
#' value of NaN when \code{\link{is_perfect}}
#' evaluates to \code{TRUE}).
#'
#' @examples
#' prob <- comp_prob()  # => initialize prob to default parameters
#' prob                 # => show current values
#' length(prob)         # => 8
#'
#' @family lists containing scenario settings
#'
#' @seealso
#' \code{\link{comp_freq}} computes current frequency information;
#' \code{\link{num}} contains basic numeric variables;
#' \code{\link{init_num}} initializes basic numeric variables;
#' \code{\link{prob}} contains current probability information

prob <- comp_prob()  # => initialize prob to default parameters
# prob               # => show current values
# length(prob)       # => 8

## -----------------------------------------------
## Compute either PPV or NPV for an entire matrix of values
## (when sens and spec are given as vectors):

comp_PV_matrix <- function(prev, sens, spec, metric = "PPV") {

  # Initialize matrix as df:
  n.rows <- length(sens)
  n.cols <- length(spec)
  matrix <- as.data.frame(matrix(NA,
                                 nrow = n.rows,
                                 ncol = n.cols))
  names(matrix) <- sens

  ## Loop through rows and columns of matrix:
  for (row in 1:n.rows) {
    for (col in 1:n.cols) {

      cell.val <- NA # initialize current cell value

      if (metric == "PPV") {cell.val <- comp_PPV(prev, sens[row], spec[col])} # compute PPV
      if (metric == "NPV") {cell.val <- comp_NPV(prev, sens[row], spec[col])} # compute NPV

      matrix[row, col] <- cell.val # store result in matrix

    }
  }

  return(matrix)

}

## -----------------------------------------------
## (+) ToDo:

## - Document comp_PPV, comp_NPV, ... etc.
##
## - Allow using fart instead of spec in all functions
##   (defined above)
##
## - Add alternative ways to compute probabilities
##   from frequencies (based on various elements of freq)!
##
## - Compute alternative prob from freq with
##   a. N of dec.pos (rather than N of fa) and
##   b. N of dec.neg (rather than N of mi) provided.
##
## - Compute basic parameters (probabilities and frequencies)
##   from MIX of existing probabilities and frequencies!

## -----------------------------------------------
## eof.
