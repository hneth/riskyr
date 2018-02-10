## start_riskyr.R | riskyr
## 2018 02 10
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

    ## (3) Initialize basic scenario settings:
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
##     (for both ui.R and server.R):

# scenarios <- NULL  # initialize scenarios

## Working (except for German Umlauts):
# scenarios <- read.csv2("./data-raw/scenarios_7.csv", stringsAsFactors = FALSE)

## Not working any better:
# scenarios <- read.csv2("./data_sources/scenarios_6_win.csv", stringsAsFactors = FALSE)
# scenarios <- read.csv2("./data_sources/scenarios_6_tab.txt", stringsAsFactors = FALSE)
# scenarios <- read.table("./data_sources/scenarios_6_tab.txt", sep = "\t", stringsAsFactors = FALSE, fileEncoding = "UTF-16")

## Check:
{
  # dim(scenarios)
  # names(scenarios)
  ## View(scenarios)
}

## Note that German Umlauts are corrupted.

## Write out to ./data/ directory:
# write.csv2(scenarios, file = "./data/scenarios.csv")  # as .csv file
# save(scenarios, file = "./data/scenarios.RData")  # as .RData file

## Using devtools:
# devtools::use_data(scenarios, overwrite = TRUE)
# devtools::use_data_raw()

## -----------------------------------------------
## (4) Read in again (from ./data/):

scenarios <- NULL  # re-initialize scenarios

## Load data:
# scenarios <- read.csv2("./data/scenarios.csv", stringsAsFactors = FALSE) # from .csv file
# load("./data/scenarios.RData") # from .RData file
load("./data/scenarios.rda") # from .rda file

## Check:
{
  # dim(scenarios) # extra first column "X"
  # names(scenarios)
  ## View(scenarios)
}

## -----------------------------------------------
## (5) Initialize an imported scenario:

init_scen <- function(num) {

  ## (1) Check that num is valid.
  if (num < 1 || num > nrow(scenarios)) {
    warning(paste0("Please enter a valid scenario number (from 1 to ", nrow(scenarios), ")."))
  } else {

  ## (2) Initialize riskyr.lst:
  riskyr.lst <- list(txt = NULL,
                     pal = NULL,
                     num = NULL,
                     prob = NULL,
                     freq = NULL,
                     accu = NULL)

  ## (3) Text labels:
  txt <- init_txt(scen.lbl = scenarios$scen.lbl[num],
                  scen.txt = scenarios$scen.txt[num],
                  scen.src = scenarios$scen.src[num],
                  scen.apa = scenarios$scen.apa[num],
                  scen.lng = scenarios$scen.lng[num],
                  popu.lbl = scenarios$popu.lbl[num],
                  cond.lbl = scenarios$cond.lbl[num],
                  cond.true.lbl = scenarios$cond.true.lbl[num],
                  cond.false.lbl = scenarios$cond.false.lbl[num],
                  dec.lbl = scenarios$dec.lbl[num],
                  dec.pos.lbl = scenarios$dec.pos.lbl[num],
                  dec.neg.lbl = scenarios$dec.neg.lbl[num],
                  hi.lbl = scenarios$hi.lbl[num],
                  mi.lbl = scenarios$mi.lbl[num],
                  fa.lbl = scenarios$fa.lbl[num],
                  cr.lbl = scenarios$cr.lbl[num]
  )

  ## (4) Initialize basic scenario settings:
  prev <- scenarios$prev[num]
  sens <- scenarios$sens[num]
  spec <- scenarios$spec[num]
  fart <- scenarios$fart[num]
  N <- scenarios$N[num]

  ## (5) Initialize num based on these parameters:
  num <- init_num(prev = prev, sens = sens, spec = spec, fart = fart, N = N)

  ## (6) Compute derived data structures and variables:
  prob <- comp_prob(prev = prev, sens = sens, spec = spec, fart = fart)   # pass arguments along!
  freq <- comp_freq(prev = prev, sens = sens, spec = spec, N = N)         # pass arguments along!
  accu <- comp_accu(hi = freq$hi, mi = freq$mi, fa = freq$fa, cr = freq$cr)

  ## (7) Insert computed values into riskyr.lst:
  riskyr.lst <- list(txt = txt,
                     pal = pal,
                     num = num,
                     prob = prob,
                     freq = freq,
                     accu = accu)

  ## (8) Message:
  message("Ready to riskyr this scenario...")

  ## (9) Return entire riskyr.lst:
  return(riskyr.lst)

  }

}

## Check:
{
  # init_scen(21)
  #
  #   x <- 21
  #   txt <- init_scen(x)$txt
  #   num <- init_scen(x)$num
  #   prob <- init_scen(x)$prob
  #   freq <- init_scen(x)$freq
  #   accu <- init_scen(x)$accu
}

## -----------------------------------------------
## (+) ToDo:

## - consider defining an object class riskyr
##   that binds all elements of the current scenario
##   similar to the (now deprecated) env list.

## -----------------------------------------------
## eof.
