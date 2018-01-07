## init_env.R | riskyR
## 2018 01 07
## -----------------------------------------------
## Initialize environment env:

## The minimal set of numeric parameters consists of 3 probabilities:
val <- list("prev" = .15, # prevalence in target population = p(condition TRUE)
            "sens" = .85, # sensitivity = p(decision POS | condition TRUE)  [conditional]
            "spec" = .75, # specificity = p(decision NEG | condition FALSE) [conditional]
            "fart" = NA   # false alarm rate = 1 - spec
            )

## -----------------------------------------------
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
  # get_fart(val$spec)
  # get_spec(get_fart(val$spec))
}

## -----------------------------------------------
## Compute 4th parameter of val (if NA):

if (is.na(val$fart)) {
  val$fart <- get_fart(val$spec)
}

if (is.na(val$spec)) {
  val$spec <- get_spec(val$fart)
}

## -----------------------------------------------
## Determine a good number for population size N:
## Criterion: All 4 SDT cells should have a minimal frequency of min.freq:

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
## Define initial environment (env):

## A complete environment (env) consists of:
## 1. 4 basic parameter values (val)
## 2. 1 population size value (N)
## 3. current text information (labels and descriptions) (txt)
## 4. current color information (pal)

## env = val + N + txt + pal

## (a) env as a list of lists:
##   (short definition, but complex object):

# env <- list(val,
#             "N" = NA,
#             txt,
#             pal
#             )

## (b) env as a list of scalars:
##    (long definition, but simple object):

env <- list(

  ## 1. val:
  "prev" = val$prev,
  "sens" = val$sens,
  "spec" = val$spec,
  "fart" = val$fart,

  ## 2. N:
  "N" = NA,

  ## 3. txt:
  "name" = txt$scenario.name,
  "text" = txt$scenario.text,
  "source" = txt$scenario.source,
  "popu.lbl" = txt$target.population.lbl,
  "condition.lbl" = txt$condition.lbl,
  "cond.true.lbl" = txt$cond.true.lbl,
  "cond.false.lbl" = txt$cond.false.lbl,
  "decision.lbl" = txt$decision.lbl,
  "dec.true.lbl" = txt$dec.true.lbl,
  "dec.false.lbl" = txt$dec.false.lbl,
  "sdt.hi.lbl" = txt$sdt.hi.lbl,
  "sdt.mi.lbl" = txt$sdt.mi.lbl,
  "sdt.fa.lbl" = txt$sdt.fa.lbl,
  "sdt.cr.lbl" = txt$sdt.cr.lbl,

  # 4. pal:
  "pal.hi" = pal["hi"],
  "pal.mi" = pal["mi"],
  "pal.fa" = pal["fa"],
  "pal.cr" = pal["cr"],
  "pal.ppv" = pal["ppv"],
  "pal.npv" = pal["npv"]

)

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
## Compute N of env (if NA):

if (is.na(env$N)) {
  env$N <- get_min_N(prev = env$prev)
}

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
