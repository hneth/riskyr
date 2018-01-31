## start_riskyr.R | riskyr
## 2018 01 31
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
                     freq = NULL,
                     prob = NULL)

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
  freq <- comp_freq()
  prob <- comp_prob()

  ## (5) Insert computed values into riskyr.lst:
  riskyr.lst <- list(txt = txt,
                     pal = pal,
                     num = num,
                     freq = freq,
                     prob = prob)

  ## (6) Message:
  message("Ready to riskyr it...")

  ## (7) Return entire riskyr.lst:
  return(riskyr.lst)

}

## Apply:
scenario <- start_riskyr()
# scenario

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
