### My_Benchmarks_Weekly
rm(list=ls())
graphics.off()
library(forecast)
library(ggplot2)
source("src/my_utils.R")

# load data
my_data <- "data/M4_Weekly.rds"
df <- give_sam(readRDS(file=my_data),size = 20, seed = 16)
df <- readRDS(file=my_data)
length(df)

# plot a random series 
df_sam <- give_sam(df, size=1)[[1]]
fc_sam <- my_benchmarks(df_sam$x, fh=df_sam$h)

autoplot(df_sam$x) +
  autolayer(df_sam$xx, series="outsample") +
  autolayer(fc_sam$Naive2, series="Naive2") +
  autolayer(fc_sam$Comb, series="Comb") +
  autolayer(fc_sam$ARIMA, series="Auto.ARIMA") + 
  autolayer(fc_sam$ETS, series="ETS") +
  autolayer(fc_sam$ETSARIMA, series="ETSARIMA")

# intialize values
fc_names <- c("Naive2", "Comb", "ARIMA", "ETS", "ETSARIMA")
Total_sMAPE <- Total_MASE <- matrix(data = NA, nrow = length(df), ncol = length(fc_names))
colnames(Total_sMAPE) <- colnames(Total_MASE) <- fc_names
dim(Total_sMAPE)

# forecasts
for (i in 1:length(df)){
  n <- length(df)

  output <- wrapper_fun(df[[i]], my_benchmarks)
  Total_sMAPE[i,] <- output$sMAPE
  Total_MASE[i,] <- output$MASE
  
  if(i%%10==0){
    pct <- round((i/n)*100,2)
    print(noquote(paste0(i, "/", n, " - ", pct, "%")))
  } 
  if(i%%n==0){
    print("Done!")
  }
}

## Calculate accuracy measures
print(my_data)
my_accuracy(Total_sMAPE, Total_MASE)

####################
### Save results ###
sn <- rep(NA, length(df))
for (i in 1:length(df)){
  sn[i] <- df[[i]]$st
}

res_weekly <- data.frame(Series=sn, Total_sMAPE, Total_MASE)
write.csv(res_weekly, file="results/benchmarks/results_weekly.csv")

res_weekly_table  <- my_accuracy(Total_sMAPE, Total_MASE)
write.csv(res_weekly_table, file="results/benchmarks/results_weekly_table.csv")

