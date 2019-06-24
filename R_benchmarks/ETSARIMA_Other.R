### ETSARIMA_Other
rm(list=ls())
graphics.off()
library(forecast)
library(ggplot2)
source("src/my_utils.R")

# load data
my_data <- "data/M4_Other.rds"
df <- give_sam(readRDS(file=my_data),size = 1000, seed = 11) # =max{1000,0.1*data}
#df <- readRDS(file=my_data)
length(df)

## plot random series 
df_sam <- give_sam(df, size=1)[[1]]
fc_sam <- my_benchmarks(df_sam$x, fh=df_sam$h)

autoplot(df_sam$x) +
  autolayer(df_sam$xx, series="outsample") +
  autolayer(fc_sam$Comb, series="Comb") +
  autolayer(fc_sam$ARIMA, series="Auto.ARIMA") + 
  autolayer(fc_sam$ETS, series="ETS") +
  ggtitle(paste("Forecasting methods:",df_sam$st)) +
  xlab("Year") +
  ylab("value")

# intialize values
fc_names <- c("Naive2", "Comb", "ARIMA", "ETS", "ETSARIMA")
Total_sMAPE <- Total_MASE <- matrix(data = NA, nrow = length(df), ncol = length(fc_names))
colnames(Total_sMAPE) <- colnames(Total_MASE) <- fc_names
dim(Total_MASE)

# forecasts
for (i in 1:length(df)){
  n <- length(df)
  output <- wrapper_fun(df[[i]], my_benchmarks)
  Total_sMAPE[i,] <- output$sMAPE
  Total_MASE[i,] <- output$MASE
  
  if(i%%1==0){
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

########################
### Save the results ###
st <- n <- period <- rep(NA, length(df))

for (i in 1:length(df)){
  st[i] <- df[[i]]$st
  n[i] <- df[[i]]$n
  period[i] <- df[[i]]$period
}

results_Other <- data.frame(Series=st, insample_n=n, period=period, sMAPE=Total_sMAPE, MASE=Total_MASE)
write.csv(results_Other, file="results/M4_ETSARIMA/results_Other_1000.csv")

results_table_Other <- my_accuracy(Total_sMAPE, Total_MASE)
write.csv(results_table_Other, file="results/M4_ETSARIMA/results_Other_1000_table.csv")

