## init_riskyr.R | riskyr
## 2018 01 20
## -----------------------------------------------
## (1) Initialize the package:

.onAttach <- function(libname, pkgname) {
  packageStartupMessage("Welcome to riskyr!")
}

## -----------------------------------------------
## (2) Run some code:

init_riskyr <- function(...) {

  # source("./R/comp_util.R")  # utility functions

  ## A: Initialize basic variables:
  source("./R/init_txt.R")   # 1. initialize txt
  source("./R/init_pal.R")   # 2. initialize pal
  source("./R/init_num.R")   # 3. initialize num

  ## B: Compute derived data structures and variables:
  source("./R/comp_freq.R")  # 1. derive freq (list)
  source("./R/comp_prob.R")  # 2. derive prob (list)
  source("./R/comp_popu.R")  # 3. derive popu (data frame)

  ## Return riskyr object (as a list of lists?)

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
