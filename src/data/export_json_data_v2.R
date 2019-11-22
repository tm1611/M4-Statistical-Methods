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

# Read data
#m4_info_df <- read_csv("M4-info.txt")

# Required format: "1750-01-01 00:00:00"


###############################
# Filter according own wishes #
###############################

## Filter by period and frequency  
data(M4)
df <- Filter(function(df) df$period=="Yearly" & df$type=="Macro", M4)

## Filter by frequency
#df <- Filter(function(df) df$period=="Yearly", M4)

# remove M4
rm(M4)

# give a seeded sample 
#give_sam <- function(input, size=1, seed=16){
 # set.seed(seed)
  #idx <- sample(1:length(input), size=size, replace=FALSE)
  #input[idx]
#}

# draw random sample (easier to use)
#my_df <- give_sam(df, size=10, seed=16)


# Check length
length(df)

###########################################
# ts_to_json
###########################################

ts_to_json <- function(idx, df, test_data=FALSE, true_dates=FALSE, domain_cat=FALSE){
  train <- df[[idx]]$x
  test <- df[[idx]]$xx
  
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
    target=target,
    feat_static_cat = feat_static_cat
  )
  
  json <- paste0(toJSON(my_list, auto_unbox=TRUE), "\n")
  #json <- toJSON(my_list, auto_unbox=TRUE, pretty=FALSE)
  
  return(json)
}

# train file
json1 <- map(1:length(df), ts_to_json, df, test_data=FALSE, true_dates=FALSE, domain_cat=FALSE)

sink("data/json/m4_yearly_macro_train.json")
json1 <- lapply(json1, cat)
sink()

# test file
json2 <- map(1:length(df), ts_to_json, df, test_data=TRUE, true_dates=FALSE, domain_cat=FALSE)

sink("data/json/m4_yearly_macro_test.json")
json2 <- lapply(json2, cat)
sink()

###########################################
