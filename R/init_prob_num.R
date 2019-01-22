## init_prob_num.R | riskyr
## 2019 01 22
## Define and initialize probability information prob
## by using basic parameter values of num:
## -----------------------------------------------

## (1) Initialize prob as a list (NA) of 11 probabilities (3 essential ones): ----------

## init_prob: Initialize prob list ------

init_prob <- function() {

  ## (0) Initialize prob as a list:
  prob <- list(

    ## (a) by condition: 3 essential + 2 optional probabilities:

    "prev" = NA,  # simple p of cond_true

    "sens" = NA,  # conditional p
    "mirt" = NA,  # conditional p: 1 - sens
    "spec" = NA,  # conditional p:
    "fart" = NA,  # conditional p: 1 - spec

    ## (b) by decision: 5 derived probabilities and predictive values (PVs):

    "ppod" = NA,  # simple p of dec_pos

    "PPV" = NA,   # conditional p: reversal of sens
    "FDR" = NA,   # conditional p: 1 - PPV
    "NPV" = NA,   # conditional p: reversal of spec
    "FOR" = NA,   # conditional p: 1 - NPV

    ## (c) by accuracy/correspondence of decision to condition:

    "acc" = NA,      # accuracy as probability of a correct decision/prediction
    "p_acc_hi" = NA, # p(hi|acc)  # aka. acc-hi  "p(hi | dec_cor)"
    "p_err_fa" = NA  # p(fa|err)  # aka. err-fa  "p(fa | dec_err)"

  )

  ## Return entire list prob:
  return(prob)

}

## Check: --------
# init_prob()          # initializes empty prob
# length(init_prob())  # => 13 probabilities


## (2) Compute the set of ALL current probabilities: ----------

## So far: Compute current values of PPV and NPV
##         as functions of prev, sens, and spec (using Bayes).

## Missing: Additional accuracy info, some of which can be viewed as probabilities
##          (see accu and comp_accu.R).


## comp_prob: Documentation --------

#' Compute probabilities from (3 essential) probabilities.
#'
#' \code{comp_prob} computes current probability information
#' from 3 essential probabilities
#' (\code{\link{prev}},
#' \code{\link{sens}} or \code{\link{mirt}},
#' \code{\link{spec}} or \code{\link{fart}}).
#' It returns a list of 13 probabilities \code{\link{prob}}
#' as its output.
#'
#' \code{comp_prob} assumes that a sufficient and
#' consistent set of essential probabilities
#' (i.e., \code{\link{prev}} and
#' either \code{\link{sens}} or its complement \code{\link{mirt}}, and
#' either \code{\link{spec}} or its complement \code{\link{fart}})
#' is provided.
#'
#' \code{comp_prob} computes and returns a full set of basic and
#' various derived probabilities (e.g.,
#' the probability of a positive decision \code{\link{ppod}},
#' the probability of a correct decision \code{\link{acc}},
#' the predictive values \code{\link{PPV}} and \code{\link{NPV}}, as well
#' as their complements \code{\link{FDR}} and \code{\link{FOR}})
#' in its output of a list \code{\link{prob}}.
#'
#' Extreme probabilities (sets containing two or more
#' probabilities of 0 or 1) may yield unexpected values
#' (e.g., predictive values \code{\link{PPV}} or \code{\link{NPV}}
#' turning \code{NaN} when \code{\link{is_extreme_prob_set}}
#' evaluates to \code{TRUE}).
#'
#' \code{comp_prob} is the probability counterpart to the
#' frequency function \code{\link{comp_freq}}.
#'
#'
#' Key relationships between probabilities and frequencies:
#'
#' \itemize{
#'
#' \item Three perspectives on a population:
#'
#' A population of \code{\link{N}} individuals can be split into 2 subsets of frequencies
#' in 3 different ways:
#'
#'    \enumerate{
#'
#'    \item by condition:
#'
#'    \code{\link{N} = \link{cond_true} + \link{cond_false}}
#'
#'    The frequency \code{\link{cond_true}} depends on the prevalence \code{\link{prev}}
#'    and
#'    the frequency \code{\link{cond_false}} depends on the prevalence's complement \code{1 - \link{prev}}.
#'
#'    \item by decision:
#'
#'    \code{\link{N} = \link{dec_pos} + \link{dec_neg}}
#'
#'    The frequency \code{\link{dec_pos}} depends on the proportion of positive decisions \code{\link{ppod}}
#'    and
#'    the frequency \code{\link{dec_neg}} depends on the proportion of negative decisions \code{1 - \link{ppod}}.
#'
#'    \item by accuracy (i.e., correspondence of decision to condition):
#'
#'    \code{\link{N} = \link{dec_cor} + \link{dec_err}}
#'
#'    }
#'
#' Each perspective combines 2 pairs of the 4 essential probabilities (hi, mi, fa, cr).
#'
#' When providing probabilities, the population size \code{\link{N}} is a free parameter (independent of the
#' essential probabilities \code{\link{prev}}, \code{\link{sens}}, and \code{\link{spec}}).
#'
#' If \code{\link{N}} is unknown (\code{NA}), a suitable minimum value can be computed by \code{\link{comp_min_N}}.
#'
#'
#' \item Defining probabilities in terms of frequencies:
#'
#' Probabilities \emph{are} -- determine, describe, or are defined as -- the relationships between frequencies.
#' Thus, they can be computed as ratios between frequencies:
#'
#'   \enumerate{
#'
#'   \item prevalence \code{\link{prev}}:
#'
#'   \code{\link{prev} = \link{cond_true}/\link{N}  =  (\link{hi} + \link{mi}) / (\link{hi} + \link{mi} + \link{fa} + \link{cr})}
#'
#'
#'   \item sensitivity \code{\link{sens}}:
#'
#'   \code{\link{sens} = \link{hi}/\link{cond_true}  =  \link{hi} / (\link{hi} + \link{mi})  =  (1 - \link{mirt})}
#'
#'
#'   \item miss rate \code{\link{mirt}}:
#'
#'   \code{\link{mirt} = \link{mi}/\link{cond_true}  =  \link{mi} / (\link{hi} + \link{mi})  =  (1 - \link{sens})}
#'
#'
#'   \item specificity \code{\link{spec}}:
#'
#'   \code{\link{spec} = \link{cr}/\link{cond_false}  =  \link{cr} / (\link{fa} + \link{cr})  =  (1 - \link{fart})}
#'
#'
#'   \item false alarm rate \code{\link{fart}}:
#'
#'   \code{\link{fart} = \link{fa}/\link{cond_false}  =  \link{fa} / (\link{fa} + \link{cr})  =  (1 - \link{spec})}
#'
#'
#'   \item proportion of positive decisions \code{\link{ppod}}:
#'
#'   \code{\link{ppod} = \link{dec_pos}/\link{N}  =  (\link{hi} + \link{fa}) / (\link{hi} + \link{mi} + \link{fa} + \link{cr})}
#'
#'
#'   \item positive predictive value \code{\link{PPV}}:
#'
#'   \code{\link{PPV} = \link{hi}/\link{dec_pos}  =  \link{hi} / (\link{hi} + \link{fa})  =  (1 - \link{FDR})}
#'
#'
#'   \item negative predictive value \code{\link{NPV}}:
#'
#'   \code{\link{NPV} = \link{cr}/\link{dec_neg}  =  \link{cr} / (\link{mi} + \link{cr})  =  (1 - \link{FOR})}
#'
#'
#'   \item false detection rate \code{\link{FDR}}:
#'
#'   \code{\link{FDR} = \link{fa}/\link{dec_pos}  =  \link{fa} / (\link{hi} + \link{fa})  =  (1 - \link{PPV})}
#'
#'
#'   \item false omission rate \code{\link{FOR}}:
#'
#'   \code{\link{FOR} = \link{mi}/\link{dec_neg}  =  \link{mi} / (\link{mi} + \link{cr})  =  (1 - \link{NPV})}
#'
#'
#'   \item accuracy \code{\link{acc}}:
#'
#'   \code{\link{acc} = \link{dec_cor}/\link{N}  =  (\link{hi} + \link{cr}) / (\link{hi} + \link{mi} + \link{fa} + \link{cr})}
#'
#'
#'   \item rate of hits, given accuracy \code{p_acc_hi}:
#'
#'   \code{p_acc_hi = \link{hi}/\link{dec_cor} = (1 - \link{cr}/\link{dec_cor})}
#'
#'
#'   \item rate of false alarms, given inaccuracy \code{p_err_fa}:
#'
#'   \code{p_err_fa = \link{fa}/\link{dec_err} = (1 - \link{mi}/\link{dec_err})}
#'
#'   }
#'
#'    Note: When frequencies are rounded (by \code{round = TRUE} in \code{\link{comp_freq}}),
#'    probabilities computed from \code{\link{freq}} may differ from exact probabilities.
#'
#' }
#'
#'
#' Functions translating between representational formats:
#' \code{\link{comp_prob_prob}}, \code{\link{comp_prob_freq}},
#' \code{\link{comp_freq_prob}}, \code{\link{comp_freq_freq}}
#' (see documentation of \code{\link{comp_prob_prob}} for details).
#'
#' @param prev The condition's prevalence value \code{\link{prev}}
#' (i.e., the probability of the condition being \code{TRUE}).
#'
#' @param sens The decision's sensitivity value \code{\link{sens}}
#' (i.e., the conditional probability of a positive decision
#' provided that the condition is \code{TRUE}).
#' \code{sens} is optional when its complement \code{\link{mirt}} is provided.
#'
#' @param mirt The decision's miss rate value \code{\link{mirt}}
#' (i.e., the conditional probability of a negative decision
#' provided that the condition is \code{TRUE}).
#' \code{mirt} is optional when its complement \code{\link{sens}} is provided.
#'
#' @param spec The decision's specificity value \code{\link{spec}}
#' (i.e., the conditional probability
#' of a negative decision provided that the condition is \code{FALSE}).
#' \code{spec} is optional when its complement \code{\link{fart}} is provided.
#'
#' @param fart The decision's false alarm rate \code{\link{fart}}
#' (i.e., the conditional probability
#' of a positive decision provided that the condition is \code{FALSE}).
#' \code{fart} is optional when its complement \code{\link{spec}} is provided.
#'
#' @param tol A numeric tolerance value for \code{\link{is_complement}}.
#' Default: \code{tol = .01}.
#'
#' @return A list \code{\link{prob}} containing 13 probability values.
#'
#' @examples
#' # Basics:
#' comp_prob(prev = .11, sens = .88, spec = .77)                        # => ok: PPV = 0.3210614
#' comp_prob(prev = .11, sens = NA, mirt = .12, spec = NA, fart = .23)  # => ok: PPV = 0.3210614
#' comp_prob()          # => ok, using current defaults
#' length(comp_prob())  # => 13 probabilities
#'
#' # Ways to work:
#' comp_prob(.99, sens = .99, spec = .99)              # => ok: PPV = 0.999898
#' comp_prob(.99, sens = .90, spec = NA, fart = .10)   # => ok: PPV = 0.9988789
#'
#' # Watch out for extreme cases:
#' comp_prob(1, sens = 0, spec = 1)      # => ok, but with warnings (as PPV & FDR are NaN)
#' comp_prob(1, sens = 0, spec = 0)      # => ok, but with warnings (as PPV & FDR are NaN)
#' comp_prob(1, sens = 0, spec = NA, fart = 0)  # => ok, but with warnings (as PPV & FDR are NaN)
#' comp_prob(1, sens = 0, spec = NA, fart = 1)  # => ok, but with warnings (as PPV & FDR are NaN)
#'
#' # Watch out for extreme cases:
#' comp_prob(1, sens = 0, spec = 1)      # => ok, but with warnings (as PPV & FDR are NaN)
#' comp_prob(1, sens = 0, spec = 0)      # => ok, but with warnings (as PPV & FDR are NaN)
#' comp_prob(1, sens = 0, spec = NA, fart = 0)  # => ok, but with warnings (as PPV & FDR are NaN)
#' comp_prob(1, sens = 0, spec = NA, fart = 1)  # => ok, but with warnings (as PPV & FDR are NaN)
#'
#' comp_prob(1, sens = 1, spec = 0)      # => ok, but with warnings (as NPV & FOR are NaN)
#' comp_prob(1, sens = 1, spec = 1)      # => ok, but with warnings (as NPV & FOR are NaN)
#' comp_prob(1, sens = 1, spec = NA, fart = 0)  # => ok, but with warnings (as NPV & FOR are NaN)
#' comp_prob(1, sens = 1, spec = NA, fart = 1)  # => ok, but with warnings (as NPV & FOR are NaN)
#'
#' # Ways to fail:
#' comp_prob(NA, 1, 1, NA)  # => only warning: invalid set (prev not numeric)
#' comp_prob(8,  1, 1, NA)  # => only warning: prev no probability
#' comp_prob(1,  8, 1, NA)  # => only warning: sens no probability
#' comp_prob(1,  1, 1,  1)  # => only warning: is_complement not in tolerated range
#'
#' @family functions computing probabilities
#'
#' @seealso
#' \code{\link{prob}} contains current probability information;
#' \code{\link{accu}} contains current accuracy information;
#' \code{\link{num}} contains basic numeric parameters;
#' \code{\link{init_num}} initializes basic numeric parameters;
#' \code{\link{pal}} contains current color information;
#' \code{\link{txt}} contains current text information;
#' \code{\link{freq}} contains current frequency information;
#' \code{\link{comp_freq}} computes frequencies from probabilities;
#' \code{\link{is_valid_prob_set}} verifies sets of probability inputs;
#' \code{\link{is_extreme_prob_set}} verifies sets of extreme probabilities;
#' \code{\link{comp_min_N}} computes a suitable minimum population size \code{\link{N}};
#' \code{\link{comp_freq_freq}} computes current frequency information from (4 essential) frequencies;
#' \code{\link{comp_freq_prob}} computes current frequency information from (3 essential) probabilities;
#' \code{\link{comp_prob_freq}} computes current probability information from (4 essential) frequencies;
#' \code{\link{comp_prob_prob}} computes current probability information from (3 essential) probabilities.
#'
#' @export

## comp_prob: Definition --------

comp_prob <- function(prev = num$prev,             # probabilities:
                      sens = num$sens, mirt = NA,  # 3 essential (prev, sens, spec)
                      spec = num$spec, fart = NA,  # 2 optional  (      mirt, fart)
                      tol = .01                    # tolerance for is_complement
) {

  ## (A) If a valid set of probabilities was provided:
  if (is_valid_prob_set(prev = prev, sens = sens, mirt = mirt, spec = spec, fart = fart, tol = tol)) {

    ## (0) Initialize prob as a list:
    prob <- init_prob()  # initialize prob (containing only NA values)

    ## (2) Compute the complete quintet of probabilities:
    prob_quintet <- comp_complete_prob_set(prev = prev, sens = sens, mirt = mirt, spec = spec, fart = fart)
    sens <- prob_quintet[2] # gets sens (if not provided)
    mirt <- prob_quintet[3] # gets mirt (if not provided)
    spec <- prob_quintet[4] # gets spec (if not provided)
    fart <- prob_quintet[5] # gets fart (if not provided)

    ## (3) Issue a warning if probabilities describe an extreme case:
    is_extreme_prob_set(prev = prev, sens = sens, spec = spec)  # prints a warning if TRUE

    ## (4) Assign all values of prob based on current parameter values:

    ## (a) by condition: basic probability parameters:
    prob$prev <- prev
    prob$sens <- sens
    prob$mirt <- mirt
    prob$spec <- spec
    prob$fart <- fart

    ## (b) by decision: derived probabilities and predictive values (PVs):
    prob$ppod <- comp_ppod(prev, sens, spec)  # Note: using probabilistic versions (Bayes)
    prob$PPV  <- comp_PPV(prev, sens, spec)
    prob$NPV  <- comp_NPV(prev, sens, spec)
    prob$FDR  <- comp_FDR(prev, sens, spec)   # FDR = (1 - PPV)
    prob$FOR  <- comp_FOR(prev, sens, spec)   # FOR = (1 - NPV)

    ## (c) by accuracy/correspondence of decision to condition:
    prob$acc      <- comp_acc(prev, sens, spec)
    prob$p_acc_hi <- (prob$prev * prob$sens)/(prob$acc)  # p(hi | acc)
    # prob$p_err_fa <- ((1 - prob$prev) * (1 - prob$spec))/(1 - prob$acc)  # p(fa | err) computed from scratch OR:
    prob$p_err_fa <- (1 - (prob$prev * (1 - prob$sens))/(1 - prob$acc))    # p(fa | err) computed as complement of p(mi | err)

    ## (5) Check derived prob values:
    if ( is.na(prob$ppod) | is.nan(prob$ppod) | !is_prob(prob$ppod) |
         is.na(prob$PPV)  | is.nan(prob$PPV)  | !is_prob(prob$PPV)  |
         is.na(prob$NPV)  | is.nan(prob$NPV)  | !is_prob(prob$NPV)  |
         is.na(prob$FDR)  | is.nan(prob$FDR)  | !is_prob(prob$FDR)  |
         is.na(prob$FOR)  | is.nan(prob$FOR)  | !is_prob(prob$FOR)  |
         is.na(prob$acc)  | is.nan(prob$acc)  | !is_prob(prob$acc)  |
         is.na(prob$p_acc_hi) | is.nan(prob$p_acc_hi) | !is_prob(prob$p_acc_hi) |
         is.na(prob$p_err_fa) | is.nan(prob$p_err_fa) | !is_prob(prob$p_err_fa)
         ) {

      warning( "Some derived prob values are peculiar. Check for extreme probabilities!")
    }

    ## (6) Return the entire list prob:
    return(prob)

  } else { # (B) NO valid set of probabilities was provided:

    warning("Invalid probabilities. Please enter a valid set of essential probabilities.")

  }  # if (is_valid_prob_set(...

}

## Check: --------
# # Basics:
# comp_prob(prev = .11, sens = .88, spec = .77)                        # => ok: PPV = 0.3210614
# comp_prob(prev = .11, sens = NA, mirt = .12, spec = NA, fart = .23)  # => ok: PPV = 0.3210614
# comp_prob()          # => ok, using current defaults
# length(comp_prob())  # => 13 probabilities
#
# # Ways to succeed:
# comp_prob(.999, 1, 1)   # => ok
# comp_prob(1, .999, 1)   # => ok
#
# # Watch out for extreme cases:
# comp_prob(1, sens = 0, spec = 1)      # => ok, but with warnings (as PPV & FDR are NaN)
# comp_prob(1, sens = 0, spec = 0)      # => ok, but with warnings (as PPV & FDR are NaN)
# comp_prob(1, sens = 0, spec = NA, fart = 0)  # => ok, but with warnings (as PPV & FDR are NaN)
# comp_prob(1, sens = 0, spec = NA, fart = 1)  # => ok, but with warnings (as PPV & FDR are NaN)
#
# comp_prob(1, sens = 1, spec = 0)      # => ok, but with warnings (as NPV & FOR are NaN)
# comp_prob(1, sens = 1, spec = 1)      # => ok, but with warnings (as NPV & FOR are NaN)
# comp_prob(1, sens = 1, spec = NA, fart = 0)  # => ok, but with warnings (as NPV & FOR are NaN)
# comp_prob(1, sens = 1, spec = NA, fart = 1)  # => ok, but with warnings (as NPV & FOR are NaN)
#
# # Ways to fail:
# comp_prob(NA, 1, 1, NA)  # => only warning: invalid set (prev not numeric)
# comp_prob(8,  1, 1, NA)  # => only warning: prev no probability
# comp_prob(1,  8, 1, NA)  # => only warning: sens no probability
# comp_prob(1,  1, 1,  1)  # => only warning: is_complement not in tolerated range



## (3) Apply to initialize prob: -----------------

## prob: List of current probability information. ------

#' List current probability information.
#'
#' \code{prob} is a list of named numeric variables
#' containing 3 essential (1 non-conditional \code{\link{prev}} and
#' 2 conditional \code{\link{sens}} and \code{\link{spec}}) probabilities
#' and 8 derived (\code{\link{ppod}} and \code{\link{acc}},
#' as well as 6 conditional) probabilities:
#'
#' \code{prob} currently contains the following probabilities:
#'
#' \enumerate{
#'
#'  \item the condition's prevalence \code{\link{prev}}
#'  (i.e., the probability of the condition being \code{TRUE}):
#'  \code{prev = \link{cond_true}/\link{N}}.
#'
#'  \item the decision's sensitivity \code{\link{sens}}
#'  (i.e., the conditional probability of a positive decision
#'  provided that the condition is \code{TRUE}).
#'
#'  \item the decision's miss rate \code{\link{mirt}}
#'  (i.e., the conditional probability of a negative decision
#'  provided that the condition is \code{TRUE}).
#'
#'  \item the decision's specificity \code{\link{spec}}
#'  (i.e., the conditional probability
#'  of a negative decision provided that the condition is \code{FALSE}).
#'
#'  \item the decision's false alarm rate \code{\link{fart}}
#'  (i.e., the conditional probability
#'  of a positive decision provided that the condition is \code{FALSE}).
#'
#'
#'  \item the proportion (baseline probability or rate)
#'  of the decision being positive \code{\link{ppod}}
#'  (but not necessarily true):
#'  \code{ppod = \link{dec_pos}/\link{N}}.
#'
#'  \item the decision's positive predictive value \code{\link{PPV}}
#'  (i.e., the conditional probability of the condition being \code{TRUE}
#'  provided that the decision is positive).
#'
#'  \item the decision's false detection (or false discovery) rate \code{\link{FDR}}
#'  (i.e., the conditional probability of the condition being \code{FALSE}
#'  provided that the decision is positive).
#'
#'  \item the decision's negative predictive value \code{\link{NPV}}
#'  (i.e., the conditional probability of the condition being \code{FALSE}
#'  provided that the decision is negative).
#'
#'  \item the decision's false omission rate \code{\link{FOR}}
#'  (i.e., the conditional probability of the condition being \code{TRUE}
#'  provided that the decision is negative).
#'
#'
#'  \item the accuracy \code{\link{acc}}
#'  (i.e., probability of correct decisions \code{\link{dec_cor}} or
#'  correspondence of decisions to conditions).
#'
#'  \item the conditional probability \code{p_acc_hi}
#'  (i.e., the probability of \code{\link{hi}} given that
#'  the decision is correct \code{\link{dec_cor}}).
#'
#'  \item the conditional probability \code{p_err_fa}
#'  (i.e., the probability of \code{\link{fa}} given that
#'  the decision is erroneous \code{\link{dec_err}}).
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
#' value of NaN when \code{\link{is_extreme_prob_set}}
#' evaluates to \code{TRUE}).
#'
#'
#' Key relationships between frequencies and probabilities
#' (see documentation of \code{\link{comp_freq}} or \code{\link{comp_prob}} for details):
#'
#' \itemize{
#'
#'   \item Three perspectives on a population:
#'
#'   by condition / by decision / by accuracy.
#'
#'   \item Defining probabilities in terms of frequencies:
#'
#'   Probabilities can be computed as ratios between frequencies, but beware of rounding issues.
#'
#' }
#'
#' Functions translating between representational formats:
#' \code{\link{comp_prob_prob}}, \code{\link{comp_prob_freq}},
#' \code{\link{comp_freq_prob}}, \code{\link{comp_freq_freq}}
#' (see documentation of \code{\link{comp_prob_prob}} for details).
#'
#' Visualizations of current probability information
#' are provided by \code{\link{plot_area}},
#' \code{\link{plot_prism}}, and \code{\link{plot_curve}}.
#'
#' @examples
#' prob <- comp_prob()  # => initialize prob to default parameters
#' prob                 # => show current values
#' length(prob)         # => 13
#'
#' @family lists containing current scenario information
#'
#' @seealso
#' \code{\link{num}} contains basic numeric parameters;
#' \code{\link{init_num}} initializes basic numeric parameters;
#' \code{\link{txt}} contains current text information;
#' \code{\link{init_txt}} initializes text information;
#' \code{\link{pal}} contains current color information;
#' \code{\link{init_pal}} initializes color information;
#' \code{\link{freq}} contains current frequency information;
#' \code{\link{comp_freq}} computes current frequency information;
#' \code{\link{prob}} contains current probability information;
#' \code{\link{comp_prob}} computes current probability information;
#' \code{\link{accu}} contains current accuracy information.
#'
#' @export

prob <- comp_prob()  # => initialize prob to default parameters

## Check: --------
# prob               # => show current values
# length(prob)       # => 13

## (*) Done: -----------

## - Add p_acc_hi and p_err_fa to prob.  [2018 09 30]
## - Add accuracy acc to prob.           [2018 09 04]
## - Clean up code.                      [2018 08 31]

## (+) ToDo: ----------

## - Document comp_PPV, comp_NPV, ... etc.
##
## - Allow using fart instead of spec in all functions
##   (defined above)
##
## - Add alternative ways to compute probabilities
##   from frequencies (based on various elements of freq)!
##
## - Compute alternative prob from freq with
##   a. N of dec_pos (rather than N of fa) and
##   b. N of dec_neg (rather than N of mi) provided.
##
## - Compute basic parameters (probabilities and frequencies)
##   from MIX of existing probabilities and frequencies!

## eof. ------------------------------------------
