## start_riskyr.R | riskyr
## 2018 12 12
## Final functions and start-up settings:
## -----------------------------------------------


## (1) Open package guide: -----------------------

#' Opens the riskyr package guides
#'
#' @importFrom utils vignette
#' @importFrom utils browseVignettes
#'
#' @export

riskyr.guide <- function() {

  # utils::vignette(topic = "User Guide", package = "riskyr")
  utils::browseVignettes(package = "riskyr")

}

## (2) Run some code when starting riskyr: -------

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
  accu <- comp_accu_prob()  # was: comp_accu() which used to call comp_accu_freq()

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

## (3) Initialize package: ------

.onAttach <- function(libname, pkgname) {



  ## Welcome message: ------

  pkg_version <- utils::packageVersion("riskyr", lib.loc = NULL)
  # welcome_message <- paste0("Welcome to riskyr (v", pkg_version, ")!")
  welcome_message <- paste0("Welcome to riskyr!")

  packageStartupMessage(welcome_message)

  ## User guidance: ------

  # packageStartupMessage("riskyr.guide() opens user guides.")
  # packageStartupMessage("citation('riskyr') provides citation info.")

  ## Roll riskyr dice: ------
  ## (to illustrate the underlying notion of "risk")
  dice <- sample(1:6, 1)

  # if (dice == 0) {packageStartupMessage("citation('riskyr') provides citation info.")}

  if (dice == 1) {
    pkg_version <- utils::packageVersion("riskyr", lib.loc = NULL)
    pkg_message <- paste0("Running riskyr (v", pkg_version, ")...")

    packageStartupMessage(" ")
    packageStartupMessage(pkg_message)
    packageStartupMessage(" ")
  }

  if (dice == 2) {
    packageStartupMessage(" ")
    packageStartupMessage("Ready to roll riskyr...")
    packageStartupMessage(" ")
  }

  if (dice == 3) {
    packageStartupMessage(" ")
    packageStartupMessage("Ready to riskyr it...")
    packageStartupMessage(" ")
  }

  if (dice == 4) {
    packageStartupMessage("             ")
    packageStartupMessage("   _____     ")
    packageStartupMessage("  /\\ r  \\  ")
    packageStartupMessage(" /y \\__i_\\ ")
    packageStartupMessage(" \\ r/s   /  ")
    packageStartupMessage("  \\/___k/   ")
    packageStartupMessage("             ")
  }

  if (dice == 5) {
    packageStartupMessage("       ")
    packageStartupMessage(" hi fa ")
    packageStartupMessage("   +   ")
    packageStartupMessage(" mi cr ")
    packageStartupMessage("       ")
  }

  if (dice == 6) {
    packageStartupMessage("              ")
    packageStartupMessage("      N       ")
    packageStartupMessage("    /  \\     ")
    packageStartupMessage("  T      F    ")
    packageStartupMessage(" / \\    / \\ ")
    packageStartupMessage("hi  mi fa  cr ")
    packageStartupMessage("              ")
  }

  packageStartupMessage("riskyr.guide() opens user guides.")

}


## (*) Done: ----------

## - Clean up code.  [2018 08 22].

## (+) ToDo: ----------

## - ...

## eof. ------------------------------------------
