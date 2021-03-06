### My_Benchmarks_Monthly
rm(list=ls())
graphics.off()
library(forecast)
library(ggplot2)
source("src/my_utils.R")

# load data
my_data <- "data/M4_Monthly.rds"
df <- give_sam(readRDS(file=my_data),size = 4800, seed = 16) # 10%: 4800
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
  if(i%%10==0){
    pct <- round((i/n)*100,2)
    print(noquote(paste0(i, "/", n, " - ", pct, "%")))
  } 
  if(i%%n==0){
    print("Done!")
  }
  
  output <- wrapper_fun(df[[i]], my_benchmarks)
  Total_sMAPE[i,] <- output$sMAPE
  Total_MASE[i,] <- output$MASE
}

## Calculate accuracy measures
print(my_data)
my_accuracy(Total_sMAPE, Total_MASE)

########################
### Save the results ###
sn <- rep(NA, length(df))

for (i in 1:length(df)){
  sn[i] <- df[[i]]$st
}

res_monthly <- data.frame(Series=sn, sMAPE=Total_sMAPE, MASE=Total_MASE)
write.csv(res_monthly, file="results/benchmarks/results_monthly_10pc.csv")

res_table_monthly_10pc <- my_accuracy(Total_sMAPE, Total_MASE)
write.csv(res_table_monthly_10pc, file="results/benchmarks/results_table_monthly_10pc.csv")

