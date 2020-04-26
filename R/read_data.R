## read_data.R | riskyr
## 2020 04 24
## Read data file for scenario information:
## -----------------------------------------------

## Part A: Do ONCE (for any new data file): ----------

## (1) Import ready-made and worked out example data
##     (for both ui.R and server.R):

# df_scenarios <- NULL  # initialize df of scenarios

## Working (except for German Umlauts):
# df_scenarios <- read.csv2("./data-raw/scenarios_7.csv", stringsAsFactors = FALSE)  # riskyr 0.1.0 [2018 02 08]
# df_scenarios <- read.csv2("./data-raw/scenarios_13.csv", stringsAsFactors = FALSE) # riskyr 0.2.0 [2018 12 20]

### Not working any better:
## df_scenarios <- read.csv2("./data_sources/scenarios_6_win.csv", stringsAsFactors = FALSE)
## df_scenarios <- read.csv2("./data_sources/scenarios_6_tab.txt", stringsAsFactors = FALSE)
## df_scenarios <- read.table("./data_sources/scenarios_6_tab.txt", sep = "\t", stringsAsFactors = FALSE, fileEncoding = "UTF-16")

## (2) Check:
# dim(df_scenarios)     # 25 rows x 21 variables
# names(df_scenarios)
# View(df_scenarios)

## Note that German Umlauts are corrupted.

## (3) Write out df_scenarios to ./data/ directory:
## write.csv2(df_scenarios, file = "./data/df_scenarios.csv")  # 1. as .csv file   (not included in release)
## save(df_scenarios, file = "./data/df_scenarios.RData")      # 2. as .RData file (obsolete; no longer included)

### OLD: Using devtools:
# devtools::use_data(df_scenarios, overwrite = TRUE) # deprecated
# devtools::use_data_raw() # deprecated

## NEW: Using usethis:
# usethis::use_data(df_scenarios, overwrite = TRUE)


## Part B: Do EVERY TIME the package loads: ----------

## Read in again (from ./data/):

df_scenarios <- NULL  # re-initialize scenarios_df

load("./data/df_scenarios.rda")  # load df_scenarios from .rda file (as data frame)

## Check:
# dim(df_scenarios)     # 25 rows x 21 variables
# names(df_scenarios)
## View(df_scenarios)

## Older ways to load data:
# df_scenarios <- read.csv2("./data/scenarios.csv", stringsAsFactors = FALSE) # from .csv file
# load("./data/scenarios.RData") # from .RData file

## Check/HACK: -----------

# scenarios_df <- df_scenarios # use OBSOLETE name for defining objects (in riskyr_class.R)

## (+) ToDo: -------------------------------------

## - Find better ways of dealing with German Umlauts.

## eof. ------------------------------------------
