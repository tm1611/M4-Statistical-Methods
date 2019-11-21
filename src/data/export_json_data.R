# export rds to json data

# pre
rm(list=ls())
graphics.off()

#libraries import 
library(M4comp2018)
library(jsonlite)
library(tidyverse)


data(M4)
df <- Filter(function(df) df$period=="Monthly" & df$type=="Micro", M4)
length(df)

entry <- df[[16]]

target <- entry$x
time(target)

series <- c(entry$x, entry$xx)
head(entry$x)

## Example
x <- list( start = ,
           target =
