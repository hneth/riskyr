## read_data.R | riskyr
## 2018 02 13
## -----------------------------------------------
## Read data file for scenario information:


## -----------------------------------------------
## (1) Do ONCE (for any new data file):
## -----------------------------------------------
## Import ready-made and worked out example data
## (for both ui.R and server.R):


# scenarios.df <- NULL  # initialize df of scenarios

## Working (except for German Umlauts):
# scenarios.df <- read.csv2("./data-raw/scenarios_7.csv", stringsAsFactors = FALSE)

## Not working any better:
# scenarios.df <- read.csv2("./data_sources/scenarios_6_win.csv", stringsAsFactors = FALSE)
# scenarios.df <- read.csv2("./data_sources/scenarios_6_tab.txt", stringsAsFactors = FALSE)
# scenarios.df <- read.table("./data_sources/scenarios_6_tab.txt", sep = "\t", stringsAsFactors = FALSE, fileEncoding = "UTF-16")

## Check:
{
  # dim(scenarios.df)
  # names(scenarios.df)
  ## View(scenarios.df)
}

## Note that German Umlauts are corrupted.

## Write out to ./data/ directory:
# write.csv2(scenarios.df, file = "./data/scenarios.df.csv")  # as .csv file
# save(scenarios.df, file = "./data/scenarios.df.RData")  # as .RData file

## Using devtools:
# devtools::use_data(scenarios.df, overwrite = TRUE)
# devtools::use_data_raw()



## -----------------------------------------------
## (2) Do EVERY TIME the package loads:
## -----------------------------------------------
## Read in again (from ./data/):

scenarios.df <- NULL  # re-initialize scenarios.df

## Load data:
# scenarios.df <- read.csv2("./data/scenarios.csv", stringsAsFactors = FALSE) # from .csv file
# load("./data/scenarios.RData") # from .RData file

load("./data/scenarios.df.rda") # load scenarios.df from .rda file (as data frame)

## Check:
{
  # dim(scenarios.df) # extra first column "X"
  # names(scenarios.df)
  ## View(scenarios.df)
}


## -----------------------------------------------
## (+) ToDo:

## - Find better ways of dealing with German Umlauts.

## -----------------------------------------------
## eof.
