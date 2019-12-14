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

#source("src/ts_to_json.R")

#m4_info_df <- read_csv("M4-info.txt")

## Filter by period and frequency  
data(M4)
df <- Filter(function(df) df$period=="Monthly" & df$type=="Micro", M4)
#df <- Filter(function(df) df$period=="Monthly", M4)
rm(M4)
as.character(df[[1]]$period); as.character(df[[1]]$type);length(df)

give_sam <- function(input, size=1, seed=16){
  set.seed(seed)
  idx <- sample(1:length(input), size=size, replace=FALSE)
  input[idx]
}

# draw random sample 
df <- give_sam(df, size=10975, seed=51)
as.character(df[[1]]$period); as.character(df[[1]]$type);length(df)

ts_to_json <- function(idx, df, test_data=FALSE, true_dates=FALSE, domain_cat=FALSE){
  train <- df[[idx]]$x
  test <- df[[idx]]$xx
  item_id <- df[[idx]]$st
  
  if (domain_cat == FALSE){
    feat_static_cat <- list(idx)
  } else {
    feat_static_cat <- c(idx, as.numeric(df[[idx]]$type))
  }
  
  if (true_dates == FALSE){
    start <- "1750-01-01 00:00:00"
  } else {
    start <- paste0(as.character(as.Date(time(train))[1]), " 00:00:00") 
  }
  
  if (test_data==FALSE){
    target=train
  } else {
    target=c(train, test)
  }
  
  my_list <- list(
    start=start,
    item_id=item_id,
    target=target,
    feat_static_cat = feat_static_cat
  )
  
  json <- paste0(toJSON(my_list, auto_unbox=TRUE), "\n")
  #json <- toJSON(my_list, auto_unbox=TRUE, pretty=FALSE)
  
  return(json)
}

# train file
json1 <- map(1:length(df), ts_to_json, df, test_data=FALSE, true_dates=FALSE, domain_cat=FALSE)

sink("data/json/m4_monthly_micro_atm_train.json")
json1 <- lapply(json1, cat)
sink()

# test file
json2 <- map(1:length(df), ts_to_json, df, test_data=TRUE, true_dates=FALSE, domain_cat=FALSE)

sink("data/json/m4_monthly_micro_atm_test.json")
json2 <- lapply(json2, cat)
sink()
