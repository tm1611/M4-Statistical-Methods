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

#################
### load data ###

data(M4)
df <- Filter(function(df) df$period=="Yearly" & df$type=="Other", M4)
#df <- Filter(function(df) df$period=="Monthly", M4)
#df <- give_sam(df, 1)
rm(M4)
as.character(df[[1]]$period); as.character(df[[1]]$type);length(df)


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
  
  if (length(target) >= 300){
    target <- xts::last(target, 300)
  } else {
    target <- target
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

as.character(df[[1]]$period); as.character(df[[1]]$type);length(df)

##################
### train file ###
##################
json1 <- map(1:length(df), ts_to_json, df, test_data=FALSE, true_dates=FALSE, domain_cat=FALSE)

sink("data/json/m4_yearly_other_train.json")
json1 <- lapply(json1, cat)
sink()

### test file ###
json2 <- map(1:length(df), ts_to_json, df, test_data=TRUE, true_dates=FALSE, domain_cat=FALSE)

sink("data/json/m4_yearly_other_test.json")
json2 <- lapply(json2, cat)
sink()
