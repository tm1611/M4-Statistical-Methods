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

library(xts)
library(zoo)

# Read data
#m4_info_df <- read_csv("M4-info.txt")

# Required format: "1750-01-01 00:00:00"

data(M4)

###############################
# Filter according own wishes #
###############################

## Filter by period and frequency  
# df <- Filter(function(df) df$period=="Weekly" & df$type=="Industry", M4)

## Filter by frequency
data <- Filter(function(df) df$period=="Monthly", M4)

# remove M4
rm(M4)

# give a seeded sample 
give_sam <- function(input, size=1, seed=16){
  set.seed(seed)
  idx <- sample(1:length(input), size=size, replace=FALSE)
  input[idx]
}

# draw random sample (easier to use)
#my_df <- give_sam(df, size=10, seed=16)
df <- data[1:2]

# Check length
length(df)

# Add index to data (lazy way)
#for (i in 1:length(df)){
#  df[[i]]$idx <- i
#} 


### ts_to_json #################################
ts_to_json <- function(input, true_dates=FALSE, domain_cat=FALSE){
  train <- input$x
  test <- input$xx
  idx <- input$idx
  
  if (domain_cat == FALSE){
    feat_static_cat <- c(idx)
  } else {
    feat_static_cat <- c(idx, as.numeric(input$type))
  }
  
  if (true_dates == FALSE){
    start <- "1750-01-01 00:00:00"
  } else {
    start <- paste0(as.character(as.Date(time(train))[1]), " 00:00:00") 
  }
  
  json <- (paste0(toJSON(
    list(
      start=start,
      target=c(train, test),
      feat_static_cat = feat_static_cat
    ),
    auto_unbox = TRUE
  ), "\n"))
  return(json)
}

json1 <- map(df, ts_to_json, true_dates=TRUE, domain_cat=TRUE)
lapply(json1, cat)

########################

idx <- 1
input <- df[[idx]]

ts_to_json <- function(idx, df, true_dates=FALSE, domain_cat=FALSE){
  input <- df[[idx]]
  train <- input$x
  test <- input$xx
  
  if (domain_cat == FALSE){
    feat_static_cat <- c(idx)
  } else {
    feat_static_cat <- c(idx, as.numeric(input$type))
  }
  
  if (true_dates == FALSE){
    start <- "1750-01-01 00:00:00"
  } else {
    start <- paste0(as.character(as.Date(time(train))[1]), " 00:00:00") 
  }
  
  my_list <- list(
    start=start,
    target=c(train, test),
    feat_static_cat = feat_static_cat
  )
  
  json <- paste0(toJSON(my_list, auto_unbox = TRUE), "\n")
           
  return(json)
}

json <- map(1:length(df), ts_to_json, df, TRUE, FALSE)

sink("data/json/test_file.json")
lapply(json, cat)
sink()

###########################################

idx <- 1
input <- df[[idx]]

train <- input$x
test <- input$xx

feat_static_cat <- c(idx)
feat_static_cat <- c(idx, as.numeric(input$type))

start <- "1750-01-01 00:00:00"
#start <- paste0(as.character(as.Date(time(train))[1]), " 00:00:00") 

my_list <- list(
  start=start,
  target=c(train, test),
  feat_static_cat = feat_static_cat
)

my_json <- toJSON(my_list, auto_unbox = TRUE)

write(json, "data/json/test_file2.json")



