### Benchmarks_Hourly_alt
rm(list=ls())
graphics.off()
library(forecast)
library(ggplot2)
source("src/my_utils.R")

# load data
df <- give_sam(readRDS(file= "data/M4_Hourly.rds"),size = 10, seed = 16)
df <- readRDS(file= "data/M4_Hourly.rds")

# initialize values 
fc_names <- c("Naive", "sNaive", "Naive2", "SES", "Holt", "Damped", "Theta", "Comb")
Total_sMAPE <- Total_MASE <- matrix(data = NA, nrow = length(df), ncol = length(fc_names))
colnames(Total_sMAPE) <- colnames(Total_MASE) <- fc_names

# forecasts
for (i in 1:length(df)){
  output <- wrapper_fun(df[[i]], benchmarks)
  Total_sMAPE[i,] <- output$sMAPE
  Total_MASE[i,] <- output$MASE
}

## Calculate means of sMAPE and MASE
sMAPE_mean <- round( (colMeans(Total_sMAPE)*100),4 )
MASE_mean <- round( colMeans(Total_MASE),4 )

# Calculate mean OWA
rel_sMAPE <- Total_sMAPE / Total_sMAPE[,"Naive2"]
rel_MASE <- Total_MASE / Total_MASE[,"Naive2"]
OWA <- (rel_sMAPE + rel_MASE) / 2
OWA_mean <- round( colMeans(OWA),4 )

# results
data.frame(sMAPE_mean, MASE_mean, OWA_mean)

