# libraries
# (to do: check if all are necessary here)
library(M4comp2018)
library(jsonlite)
library(tidyverse)
library(parallel)
library(anytime)
library(purrr)

library(xts)
library(zoo)

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

# sample function
give_sam <- function(input, size=1, seed=16){
  set.seed(seed)
  idx <- sample(1:length(input), size=size, replace=FALSE)
  input[idx]
}