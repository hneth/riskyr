## start_riskyr.R | riskyr
## 2018 02 03
## -----------------------------------------------
## (1) Initialize the package:

.onAttach <- function(libname, pkgname) {
  packageStartupMessage("Welcome to riskyr!")
}

## -----------------------------------------------
## (2) Run some code:

start_riskyr <- function(...) {

  ## (1) Initialize riskyr.lst:
  riskyr.lst <- list(txt = NULL,
                     pal = NULL,
                     num = NULL,
                     prob = NULL,
                     freq = NULL,
                     accu = NULL)

  ## (2) Source some files:
  # source("./R/comp_util.R")  # utility functions
  # source("./R/init_txt.R")   # 1. initialize txt
  # source("./R/init_pal.R")   # 2. initialize pal
  # source("./R/init_num.R")   # 3. initialize num

  ## (3) Initialize basic scenario settings
  ##     (using init_xxx functions and defaults):
  txt <- init_txt()
  pal <- init_pal()
  num <- init_num()

  ## (4) Compute derived data structures and variables:
  # source("./R/comp_freq.R")  # 1. derive freq (list)
  # source("./R/comp_prob.R")  # 2. derive prob (list)
  # source("./R/comp_popu.R")  # 3. derive popu (data frame)
  prob <- comp_prob()
  freq <- comp_freq()
  accu <- comp_accu()

  ## (5) Insert computed values into riskyr.lst:
  riskyr.lst <- list(txt = txt,
                     pal = pal,
                     num = num,
                     prob = prob,
                     freq = freq,
                     accu = accu)

  ## (6) Message:
  message("Ready to riskyr it...")

  ## (7) Return entire riskyr.lst:
  return(riskyr.lst)

}

## -----------------------------------------------
## (3) Apply:

scenario <- start_riskyr()
# scenario

## -----------------------------------------------
## (4) As proper init function:

init_riskyr <- function(prev = NA,             # probabilities
                        sens = NA, mirt = NA,
                        spec = NA, fart = NA,  # was: num$fart,
                        N = freq$N
){

  ## (A) If a valid set of probabilities was provided:
  if (is_valid_prob_set(prev = prev, sens = sens, mirt = mirt, spec = spec, fart = fart, tol = .01)) {

    ## (0) Compute the complete quintet of probabilities:
    prob_quintet <- comp_complete_prob_set(prev, sens, mirt, spec, fart)
    sens <- prob_quintet[2] # gets sens (if not provided)
    mirt <- prob_quintet[3] # gets mirt (if not provided)
    spec <- prob_quintet[4] # gets spec (if not provided)
    fart <- prob_quintet[5] # gets fart (if not provided)


    ## (1) Initialize riskyr.lst:
    riskyr.lst <- list(txt = NULL,
                       pal = NULL,
                       num = NULL,
                       prob = NULL,
                       freq = NULL,
                       accu = NULL)

    ## (2) Source some files:
    # source("./R/comp_util.R")  # utility functions
    # source("./R/init_txt.R")   # 1. initialize txt
    # source("./R/init_pal.R")   # 2. initialize pal
    # source("./R/init_num.R")   # 3. initialize num

    ## (3) Initialize basic scenario settings
    ##     (using init_xxx functions and defaults):
    txt <- init_txt()
    pal <- init_pal()
    num <- init_num(prev = prev, sens = sens, spec = spec, fart = fart, N = N)  # pass arguments along!

    ## (4) Compute derived data structures and variables:
    # source("./R/comp_freq.R")  # 1. derive freq (list)
    # source("./R/comp_prob.R")  # 2. derive prob (list)
    # source("./R/comp_popu.R")  # 3. derive popu (data frame)
    prob <- comp_prob(prev = prev, sens = sens, spec = spec, fart = fart)   # pass arguments along!
    freq <- comp_freq(prev = prev, sens = sens, spec = spec, N = N)         # pass arguments along!
    accu <- comp_accu(hi = freq$hi, mi = freq$mi, fa = freq$fa, cr = freq$cr)

    ## (5) Insert computed values into riskyr.lst:
    riskyr.lst <- list(txt = txt,
                       pal = pal,
                       num = num,
                       prob = prob,
                       freq = freq,
                       accu = accu)

    ## (6) Message:
    message("Ready to riskyr it...")

    ## (7) Return entire riskyr.lst:
    return(riskyr.lst)

  }

  else { # (B) NO valid set of probabilities was provided:

    warning("Please enter a valid set of probabilities.")

  }

}


## -----------------------------------------------
## (3) Import ready-made and worked out example data
##     (in both ui.R and server.R):

# datasets <- read.csv2("./data/scenarios.csv", stringsAsFactors = FALSE)

## -----------------------------------------------
## (+) ToDo:

## - consider defining an object class riskyr
##   that binds all elements of the current scenario
##   similar to the (now deprecated) env list.

## -----------------------------------------------
## eof.
