## init_riskyr.R | riskyr
## 2018 01 19
## -----------------------------------------------
## (1) Initialize the package:

.onAttach <- function(libname, pkgname) {
  packageStartupMessage("Welcome to riskyr!")
}

## -----------------------------------------------
## (2) Run some code:

init_riskyr <- function() {

  # source("./R/comp_util.R")  # utility functions

  source("./R/init_txt.R")   # initialize txt
  source("./R/init_pal.R")   # initialize pal
  source("./R/init_num.R")   # initialize num

  source("./R/comp_freq.R")  # derive freq (list)
  source("./R/comp_prob.R")  # derive prob (list)
  source("./R/comp_popu.R")  # derive popu (data frame)

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
