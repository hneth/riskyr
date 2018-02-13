## start_riskyr.R | riskyr
## 2018 02 13
## -----------------------------------------------
## (1) Initialize the package:

.onAttach <- function(libname, pkgname) {

  packageStartupMessage("Welcome to riskyr!")
  packageStartupMessage("Ready to riskyr it...")

}

## -----------------------------------------------
## (2) Run some code when starting riskyr:

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
  # source("./R/comp_freq.R")  # 1. derive freq (list)
  # source("./R/comp_prob.R")  # 2. derive prob (list)
  # source("./R/comp_popu.R")  # 3. derive popu (data frame)


  ## (3) Initialize basic scenario settings
  ##     (using init_xxx functions and defaults):
  txt <- init_txt()
  pal <- init_pal()
  num <- init_num()

  ## (4) Compute derived data structures and variables:
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
## (+) ToDo:

## - ...

## -----------------------------------------------
## eof.
