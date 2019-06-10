### Benchmarks_Daily
# V1

# pre
rm(list=ls())
graphics.off()

### libraries ###
library(forecast)
library(ggplot2)

# load data
M4_Daily <- readRDS(file= "data/M4_Daily.rds")

# load my_utils
source("my_utils.R")

### initialize ###
names_benchmarks <- c("Naive", "sNaive", "Naive2", "SES", "Holt", "Damped", "Theta", "Comb")
data_train <- data_test <- M4_Daily
n_fc <- length(names_benchmarks)
n_series <- length(data_train)
fh <- M4_Daily[[1]]$h

Total_MASE <- array(NA,dim = c(n_fc, fh, n_series))
Total_sMAPE <- Total_MASE

### Results ### 

# forecasts and measures
for (i in 1:length(data_train)){
  # initialize
  insample <- data_train[[i]]$x
  outsample <- data_test[[i]]$xx
  # estimate forecasts
  fc <- benchmarks(input = insample, fh = fh)
  
  # sMAPE
  for(j in 1:n_fc){
    Total_sMAPE[j,,i] <- cal_sMAPE(outsample, fc[[j]])
  }
  
  # MASE
  for (j in 1:n_fc){
    Total_MASE[j,,i] <- cal_MASE(insample, outsample, fc[[j]])
  }
} 

# sMAPE 
for (i in 1:n_fc){
  print(paste(names_benchmarks[i],"-", round(mean(Total_sMAPE[i,,]),4)))
}

# MASE
for (i in 1:n_fc){
  print(paste(names_benchmarks[i], "-", round(mean(Total_MASE[i,,]), 4)))
}

# OWA 
for(i in 1:n_fc){
  rel_MASE <- (mean(Total_MASE[i,,]) / mean(Total_MASE[3,,]))
  rel_sMAPE <- (mean(Total_sMAPE[i,,]) / mean(Total_sMAPE[3,,]))
  owa <- (rel_MASE + rel_sMAPE)/2
  print(paste(names_benchmarks[i], "-",round(owa,4)))
}
