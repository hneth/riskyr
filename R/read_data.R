## read_data.R | riskyr
## 2018 02 14
## -----------------------------------------------
## Read data file for scenario information:


## -----------------------------------------------
## (1) Do ONCE (for any new data file):
## -----------------------------------------------
## Import ready-made and worked out example data
## (for both ui.R and server.R):


# df.scenarios <- NULL  # initialize df of scenarios

## Working (except for German Umlauts):
# df.scenarios <- read.csv2("./data-raw/scenarios_7.csv", stringsAsFactors = FALSE)

## Not working any better:
# df.scenarios <- read.csv2("./data_sources/scenarios_6_win.csv", stringsAsFactors = FALSE)
# df.scenarios <- read.csv2("./data_sources/scenarios_6_tab.txt", stringsAsFactors = FALSE)
# df.scenarios <- read.table("./data_sources/scenarios_6_tab.txt", sep = "\t", stringsAsFactors = FALSE, fileEncoding = "UTF-16")

## Check:
{
  # dim(df.scenarios)
  # names(df.scenarios)
  ## View(df.scenarios)
}

## Note that German Umlauts are corrupted.

## Write out to ./data/ directory:
# write.csv2(df.scenarios, file = "./data/df.scenarios.csv")  # as .csv file
# save(scenarios.df, file = "./data/df.scenarios.RData")  # as .RData file

## Using devtools:
# devtools::use_data(df.scenarios, overwrite = TRUE)
# devtools::use_data_raw()


## -----------------------------------------------
## (2) Do EVERY TIME the package loads:
## -----------------------------------------------
## Read in again (from ./data/):

df.scenarios <- NULL  # re-initialize scenarios.df

## Load data:
# df.scenarios <- read.csv2("./data/scenarios.csv", stringsAsFactors = FALSE) # from .csv file
# load("./data/scenarios.RData") # from .RData file

load("./data/df.scenarios.rda") # load scenarios.df from .rda file (as data frame)

## Check:
{
  # dim(df.scenarios) # extra first column "X"
  # names(df.scenarios)
  ## View(df.scenarios)
}


## -----------------------------------------------
## (+) ToDo:

## - Find better ways of dealing with German Umlauts.

## -----------------------------------------------
## eof.
