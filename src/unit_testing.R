### unit_testing ###

# Testing if methods behave as expteceted

# function that outputs one time series for each frequency (by chance)
# test if methods and benchmarks work like planned based on different characteristics
# in particular: check seasonalityTest and whether seasonality is detected
# check if frequency is correctly detected. 

rm(list=ls())
graphics.off()

# libraries
library(forecast)
library(ggplot2)

source("src/my_utils_v2.R")

############################
### unit test: give_fh() ###

# check Monthly data
df_Mo <- give_sam(readRDS(file="data/M4_Monthly.rds"))

# check weekly data
df_We <- give_sam(readRDS(file="data/M4_Weekly.rds"))

# expected result: weekly data, hence fh ought to 13
give_fh(df_We) # wrong result, rewrite

## Check Hourly data
df_Ho <- give_sam(readRDS("data/M4_Hourly.rds"))

# expected result: 48
give_fh(df_Ho)

## Check Daily data
df_Da <- give_sam(readRDS("data/M4_Daily.rds"))

# expected result: 14
give_fh(df_Da)


### unit test: Seasonality Test



