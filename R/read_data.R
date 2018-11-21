## read_data.R | riskyr
## 2018 02 14
## Read data file for scenario information:
## -----------------------------------------------


## (1) Do ONCE (for any new data file): ----------

## Import ready-made and worked out example data
## (for both ui.R and server.R):


# df_scenarios <- NULL  # initialize df of scenarios

## Working (except for German Umlauts):
# df_scenarios <- read.csv2("./data-raw/scenarios_7.csv", stringsAsFactors = FALSE)

## Not working any better:
# df_scenarios <- read.csv2("./data_sources/scenarios_6_win.csv", stringsAsFactors = FALSE)
# df_scenarios <- read.csv2("./data_sources/scenarios_6_tab.txt", stringsAsFactors = FALSE)
# df_scenarios <- read.table("./data_sources/scenarios_6_tab.txt", sep = "\t", stringsAsFactors = FALSE, fileEncoding = "UTF-16")

## Check:
# dim(df_scenarios)     # 26 rows x 21 variables
# names(df_scenarios)
## View(df_scenarios)

## Note that German Umlauts are corrupted.

## Write out to ./data/ directory:
# write.csv2(df_scenarios, file = "./data/df_scenarios.csv")  # as .csv file
# save(scenarios_df, file = "./data/df_scenarios.RData")  # as .RData file

## OLD: Using devtools:
# devtools::use_data(df_scenarios, overwrite = TRUE) # deprecated
# devtools::use_data_raw() # deprecated

## NEW: Using usethis:
# usethis::use_data(df_scenarios, overwrite = TRUE)


## (2) Do EVERY TIME the package loads: ----------

## Read in again (from ./data/):

df_scenarios <- NULL  # re-initialize scenarios_df

load("./data/df_scenarios.rda")  # load scenarios_df from .rda file (as data frame)

## Check:
# dim(df_scenarios)     # 26 rows x 21 variables
# names(df_scenarios)
## View(df_scenarios)


## Older ways to load data:
# df_scenarios <- read.csv2("./data/scenarios.csv", stringsAsFactors = FALSE) # from .csv file
# load("./data/scenarios.RData") # from .RData file


## ToDo: HACK: to be removed later: -----------

# scenarios_df <- df_scenarios # use OBSOLETE name for defining objects (in riskyr_class.R)


## (+) ToDo: -------------------------------------

## - Find better ways of dealing with German Umlauts.

## eof. ------------------------------------------
