# data filter

# tasks: 
## 1. Filter data by frequency and domain
## 2. export to csv that it can be used by python

# pre
rm(list=ls())
graphics.off()

#libraries import 
library(M4comp2018)
library(forecast)

# load data
data(M4)

# Weekly data
df <- Filter(function(df) df$period=="Weekly" & df$typ=="Industry", M4)
length(df)


#saveRDS(M4_Weekly, file="data/M4_Weekly.rds")
#rm(M4_Weekly)

