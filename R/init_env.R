## init_env.R | riskyR
## 2018 01 07
## -----------------------------------------------
## Initialize an environment (env) that contains
## all scenario-specific user inputs and customizations:

## -----------------------------------------------
## (1) Basic functions on probabilities:
## Specificity (spec) is the complement of the false alarm rate (fart):

get_fart <- function(spec) {

  if ((spec < 0) | (spec > 1)) {
    warning( "Warning: spec is no probability (range from 0 to 1)." )
  }

  fart <- NA # initialize

  fart <- 1 - spec # compute complement

  return(fart)
}

get_spec <- function(fart) {

  if ((fart < 0) | (fart > 1)) {
    warning( "Warning: fart is no probability (range from 0 to 1)." )
  }

  spec <- NA # initialize

  spec <- 1 - fart # compute complement

  return(spec)
}

## Checks:
{
  # get_fart(2)
  # get_fart(1/3)
  # get_spec(get_fart(2/3))
}

## -----------------------------------------------
## (2) Define and initialize val:

## The minimal set of numeric input parameters
## consists of 3 probabilities (+ 1 complement):

## Define some defaults for val:
val.def <- list("prev" = .15, # prevalence in target population = p(condition TRUE)
                "sens" = .85, # sensitivity = p(decision POS | condition TRUE)  [conditional p]
                "spec" = .75, # specificity = p(decision NEG | condition FALSE) [conditional p]
                "fart" = NA   # false alarm rate = 1 - spec                [complement of spec]
                )

init_val <- function(prev = val.def$prev, sens = val.def$sens, spec = val.def$spec, fart = val.def$fart) {

  ## (a) Define val as a list of 4 probabilities:
  val <- list("prev" = NA, # prevalence in target population = p(condition TRUE)
             "sens" = NA, # sensitivity = p(decision POS | condition TRUE)  [conditional p]
             "spec" = NA, # specificity = p(decision NEG | condition FALSE) [conditional p]
             "fart" = NA   # false alarm rate = 1 - spec                [complement of spec]
             )

  ## (b) Compute missing (4th) argument value (if applicable):
  if (is.na(fart)) {fart <- get_fart(spec)}
  if (is.na(spec)) {spec <- get_spec(fart)}

  ## (c) Initialize val with current arguments:
  val$prev <- prev
  val$sens <- sens
  val$spec <- spec
  val$fart <- fart

  ## (d) Return the entire list val:
  return(val)

}

## Apply:
val <- init_val()
# val

## -----------------------------------------------
## (3) Compute 4th parameter of val (if NA):
##     (moved to init_val() above)
{
# if (is.na(val$fart)) {
#   val$fart <- get_fart(val$spec)
# }
#
# if (is.na(val$spec)) {
#   val$spec <- get_spec(val$fart)
# }
}

## -----------------------------------------------
## (4) Determine a good number for population size N:
##     Criterion: All 4 SDT cells should have a minimal frequency of min.freq:

get_min_N <- function(prev, sens, spec, min.freq = 1) {

  N <- 10^0 # initialize

  ## Compute frequency of 4 SDT cases:
  n.hi <- N * prev * sens
  n.mi <- N * prev * (1 - sens)
  n.cr <- N * (1 - prev) * spec
  n.fa <- N * (1 - prev) * (1 - spec)

  ## Freq of 4 SDT cases:
  while ((n.hi > 0 & n.hi < min.freq) |
         (n.mi > 0 & n.mi < min.freq) |
         (n.cr > 0 & n.cr < min.freq) |
         (n.fa > 0 & n.fa < min.freq)) {

    N <- (N * 10) # multiply N by 10

    ## Update frequency of 4 SDT cases:
    n.hi <- N * prev * sens
    n.mi <- N * prev * (1 - sens)
    n.cr <- N * (1 - prev) * spec
    n.fa <- N * (1 - prev) * (1 - spec)

  }

  ## Return number N:
  return(N)

}

## Checks:
{
  # get_min_N(0, 0, 0) # => 1
  # get_min_N(1, 1, 1) # => 1
  # get_min_N(1, 1, 1, min.freq = 10) # =>  10
  # get_min_N(1, 1, 1, min.freq = 99) # => 100
  # get_min_N(.1, .1, .1)       # => 100       = 10^2
  # get_min_N(.001, .1, .1)     # => 10 000    = 10^4
  # get_min_N(.001, .001, .1)   # => 1 000 000 = 10^6
  # get_min_N(.001, .001, .001) # => 1 000 000 = 10^6
}

## -----------------------------------------------
## (5) Define and initialize environment (env):

## A complete environment (env) consists of:
## 1. 4 basic parameter values (val)
## 2. 1 population size value (N)
## 3. current text information (labels and descriptions) (txt)
## 4. current color information (pal)
##
## env = val + N + txt + pal

## (a) env as a list of lists:
##   (short definition, but complex object):

# env <- list(val,
#             "N" = NA,
#             txt,
#             pal
#             )

## (b) Define and initialize env as a list of scalars:
##    (long definition, but simple object):

init_env <- function(cur.val = val, cur.N = NA, cur.txt = txt, cur.pal = pal) {

  ## (a) Define and initialize env as a list of scalars:
  env <- list(

    ## 1. val:
    "prev" = cur.val$prev,
    "sens" = cur.val$sens,
    "spec" = cur.val$spec,
    "fart" = cur.val$fart,

    ## 2. N:
    "N" = NA,

    ## 3. txt:
    "name" = cur.txt$scenario.lbl, # Note: "name", rather than "lbl".
    "text" = cur.txt$scenario.text,
    "source" = cur.txt$scenario.source,
    "popu.lbl" = cur.txt$target.population.lbl,
    "condition.lbl" = cur.txt$condition.lbl,
    "cond.true.lbl" = cur.txt$cond.true.lbl,
    "cond.false.lbl" = cur.txt$cond.false.lbl,
    "decision.lbl" = cur.txt$decision.lbl,
    "dec.true.lbl" = cur.txt$dec.true.lbl,
    "dec.false.lbl" = cur.txt$dec.false.lbl,
    "sdt.hi.lbl" = cur.txt$sdt.hi.lbl,
    "sdt.mi.lbl" = cur.txt$sdt.mi.lbl,
    "sdt.fa.lbl" = cur.txt$sdt.fa.lbl,
    "sdt.cr.lbl" = cur.txt$sdt.cr.lbl,

    ## 4. pal:
    "pal.hi" = cur.pal["hi"],
    "pal.mi" = cur.pal["mi"],
    "pal.fa" = cur.pal["fa"],
    "pal.cr" = cur.pal["cr"],
    "pal.ppv" = cur.pal["ppv"],
    "pal.npv" = cur.pal["npv"]
  )

  ## (b) Compute a good N for current env (if NA):
  if (is.na(env$N)) {
    env$N <- get_min_N(prev = env$prev,
                       sens = env$sens,
                       spec = env$spec
                       )
  }

  ## (c) Return the entire list env:
  return(env)

}

## Apply:
env <- init_env()

## Checks:
# length(env) == length(val) + 1 + length(txt) + length(pal)

## -----------------------------------------------
## older version of env:

# env <- list("name" = "Demo",  # name (e.g., HIV, mammography, ...)
#            "N" = 100,        # N in population
#            "prev" = .15,     # prevalence in population = p(condition TRUE)
#            "sens" = .85,     # sensitivity = p(decision positive | condition TRUE)
#            "spec" = .75,     # specificity = p(decision negative | condition FALSE)
#            "source" = "source information" # information source (e.g., citation)
#            )

## -----------------------------------------------
## Import ready-made and worked out example data
## (in both ui.R and server.R):

# datasets <- read.csv2("./data/scenarios.csv", stringsAsFactors = FALSE)

## -----------------------------------------------
## (+) ToDo:

## - re-organize "scenarios.xls" according to data structure of env.
## - read in pre-defined datasets ("scenarios.csv") from "/data".

## -----------------------------------------------
## eof.
