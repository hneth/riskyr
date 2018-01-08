## init_env.R | riskyR
## 2018 01 08
## -----------------------------------------------
## Initialize an environment (env) that contains
## all scenario-specific user inputs and customizations:

## Note: With the introduction of N into num
##       env has become entirely OPTIONAL and NON-essential.
##
##       It is kept to store ALL settings in a single list
##       and as it contains all info to be stored in the
##       external file of pre-defined datasets (scenario.xls).

## -----------------------------------------------
## (5) Define and initialize environment (env):

## A complete environment (env) consists of:
## 1. 4 basic parameter values (from num[1:4])
## 2. 1 population size value  (from num$N)
## 3. current text information (labels and descriptions) (txt)
## 4. current color information (pal)
## Thus,
##        env = num + txt + pal

## (a) env as a list of lists:
##     (short definition, but complex object):

# env <- list(num,
#             "N" = NA,
#             txt,
#             pal
#             )

## (b) Define and initialize env as a list of scalars:
##    (long definition, but simple object):

init_env <- function(cur.num = num, cur.txt = txt, cur.pal = pal) {

  ## (a) Define and initialize env as a list of scalars:
  env <- list(

    ## 1. num[1:4]:
    "prev" = cur.num$prev,
    "sens" = cur.num$sens,
    "spec" = cur.num$spec,
    "fart" = cur.num$fart,

    ## 2. N:
    "N" = cur.num$N,

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

  ## WAS moved to init_num():
  ## (x) Compute a good N for current env (if NA):
  # if (is.na(env$N)) {
  #   env$N <- get_min_N(prev = env$prev,
  #                      sens = env$sens,
  #                      spec = env$spec
  #                      )
  # }

  ## (c) Return the entire list env:
  return(env)

}

## Apply:
env <- init_env()
# env

## Note: As env is non-essential, all code should still work
##       when env is deleted or remains undefined!

## Checks:
# length(env) == length(val) + 1 + length(txt) + length(pal)

## -----------------------------------------------
## Old version of env:

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

## - as env is now NON-essential, avoid using it in all calls!
## - re-organize "scenarios.xls" according to data structure of env.
## - read in pre-defined datasets ("scenarios.csv") from "/data".

## -----------------------------------------------
## eof.
