### Benchmarks_all
rm(list=ls())
graphics.off()
library(forecast)
library(ggplot2)
library(M4comp2018)
source("src/my_utils.R")

# load data
data(M4)
df <- M4
#df <- give_sam(df ,size = 20, seed = 11)
length(df)

# plot a random series 
df_sam <- give_sam(df, size=1, seed = 11)[[1]]
fc_sam <- benchmarks(df_sam$x, fh=df_sam$h)

# historical data -> Can you guess where this is going?
autoplot(df_sam$x) +
  ggtitle(paste("Historical data of series:",df_sam$st)) +
  xlab("Year") +
  ylab("value")

autoplot(df_sam$x) +
  autolayer(df_sam$xx, series="outsample") +
  autolayer(fc_sam$Theta, series="Theta") +
  autolayer(fc_sam$Comb, series="Comb") +
  ggtitle(paste("Forecasting methods:",df_sam$st)) +
  xlab("Year") +
  ylab("value")

# initialize values 
fc_names <- c("Naive", "sNaive", "Naive2", "SES", "Holt", "Damped", "Theta", "Comb")
Total_sMAPE <- Total_MASE <- matrix(data = NA, nrow = length(df), ncol = length(fc_names))
colnames(Total_sMAPE) <- colnames(Total_MASE) <- fc_names
dim(Total_sMAPE)

# forecasts
for (i in 1:length(df)){
  n <- length(df)

  output <- wrapper_fun(df[[i]], benchmarks)
  Total_sMAPE[i,] <- output$sMAPE
  Total_MASE[i,] <- output$MASE
  
  if(i%%10 ==0){
    pct <- round((i/n)*100,4)
    print(noquote(paste0(i, "/",n, " - " ,pct, "%")))
  }
  if(i%%n==0){
    print("Done!")
  }
  
}

## Calculate accuracy measures
my_accuracy(Total_sMAPE, Total_MASE)

####################
### Save results ###
st <- n <- period <- type <- rep(NA, length(df))

for (i in 1:length(df)){
  st[i] <- df[[i]]$st
  n[i] <- df[[i]]$n
  period[i] <- df[[i]]$period
  type[i] <- df[[i]]$type
}

results_benchmarks <- data.frame(st=st, n=n, period=period, type=type, sMAPE=Total_sMAPE, MASE=Total_MASE)
write.csv(results_benchmarks, file="results/M4_benchmarks8/results_benchmarks_all.csv")

results_benchmarks_table <- my_accuracy(Total_sMAPE, Total_MASE)
write.csv(results_benchmarks_table, file="results/M4_benchmarks8/results_benchmarks_all_table.csv")
