## start_riskyr.R | riskyr
## 2018 01 20
## -----------------------------------------------
## (1) Initialize the package:

.onAttach <- function(libname, pkgname) {
  packageStartupMessage("Welcome to riskyr!")
}

## -----------------------------------------------
## (2) Run some code:

start_riskyr <- function(...) {


  ## (1) Initialize riskyr.lst:
  riskyr.lst <- list(mes = "NA",
                     txt = NULL,
                     num = NULL
                     )

  ## (2) Source some files:
  # source("./R/comp_util.R")  # utility functions

  ## (3) Initialize basic variables:
  # source("./R/init_txt.R")   # 1. initialize txt
  txt <- init_txt()

  # source("./R/init_pal.R")   # 2. initialize pal

  # source("./R/init_num.R")   # 3. initialize num
  num <- init_num()

  ## B: Compute derived data structures and variables:
  # source("./R/comp_freq.R")  # 1. derive freq (list)
  # source("./R/comp_prob.R")  # 2. derive prob (list)
  # source("./R/comp_popu.R")  # 3. derive popu (data frame)


  ## (7) Insert computed values into riskyr.lst:
  riskyr.lst <- list(txt = txt,
                     num = num
  )

  ## (8) Message:
  message("Ready to riskyr it...")

  ## (9) Return entire riskyr.lst:
  return(riskyr.lst)

}

## Apply:
scenario <- start_riskyr()
# scenario$txt

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
