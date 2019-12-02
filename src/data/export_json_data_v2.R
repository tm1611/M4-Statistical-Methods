###########################
# Export rds to json data #
###########################

# pre
rm(list=ls())
graphics.off()

#libraries import 
library(M4comp2018)
library(jsonlite)
library(tidyverse)
library(parallel)
library(anytime)
library(purrr)

library(xts)
library(zoo)

source("src/ts_to_json.R")

#m4_info_df <- read_csv("M4-info.txt")

## Filter by period and frequency  
data(M4)
#df <- Filter(function(df) df$period=="Daily" & df$type=="Daily", M4)
df <- Filter(function(df) df$period=="Daily", M4)
rm(M4)
as.character(df[[1]]$period); as.character(df[[1]]$type);length(df)

# draw random sample 
#my_df <- give_sam(df, size=10, seed=16)

# train file
json1 <- map(1:length(df), ts_to_json, df, test_data=FALSE, true_dates=FALSE, domain_cat=TRUE)

sink("data/json/m4_daily_domain_train.json")
json1 <- lapply(json1, cat)
sink()

# test file
json2 <- map(1:length(df), ts_to_json, df, test_data=TRUE, true_dates=FALSE, domain_cat=TRUE)

sink("data/json/m4_daily_domain_test.json")
json2 <- lapply(json2, cat)
sink()
