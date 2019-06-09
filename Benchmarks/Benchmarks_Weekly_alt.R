### Alternative way to get benchmarks ###
### Benchmarks_Weekly_alt

rm(list=ls())
graphics.off()
library(forecast)
library(ggplot2)
source("src/my_utils.R")

# load data
df <- readRDS(file= "data/M4_Weekly.rds")

# initialize values 
fh <- NA
insample <- NA
outsample <- NA
fc_names <- c("Naive", "sNaive", "Naive2", "SES", "Holt", "Damped", "Theta", "Comb")
Total_sMAPE <- Total_MASE <- matrix(data = NA, nrow = length(df), ncol = length(fc_names))
colnames(Total_sMAPE) <- colnames(Total_MASE) <- fc_names

# Forecasts
for (i in 1:length(df)){
  insample <- df[[i]]$x
  outsample <- df[[i]]$xx
  fh <- df[[i]]$h
  
  fc <- benchmarks(input=insample, fh=fh)
  Total_sMAPE[i,] <- unlist(lapply(fc, cal_sMAPE, outsample=outsample))
  Total_MASE[i,] <- unlist(lapply(fc, cal_MASE, insample=insample, outsample=outsample))
}

## Calculate final sMAPE, MASE, OWA
colMeans(Total_sMAPE)
colMeans(Total_MASE)

rel_sMAPE <- Total_sMAPE / Total_sMAPE[,1]
rel_MASE <- Total_MASE / Total_MASE[,1]

OWA <- (rel_sMAPE + rel_MASE) / 2
colMeans(OWA)





