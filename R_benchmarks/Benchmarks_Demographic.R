### Benchmarks_Demographics_alt
rm(list=ls())
graphics.off()
library(forecast)
library(ggplot2)
source("src/my_utils.R")

# load data
my_data <- "data/M4_Demographic.rds"
#df <- give_sam(readRDS(file=my_data),size = 20, seed = 11)
df <- readRDS(file=my_data)
length(df)

# plot a random series 
df_sam <- give_sam(df, size=1, seed = 16)[[1]]
fc_sam <- benchmarks(df_sam$x, fh=df_sam$h)

# historical data
autoplot(df_sam$x) +
  ggtitle(paste("Historical data of series:",df_sam$st)) +
  xlab("Year") +
  ylab("value")

print("Can you guess where this is going?")

autoplot(df_sam$x) +
  autolayer(df_sam$xx, series="outsample") +
  autolayer(fc_sam$Naive2, series="Naive2") +
  autolayer(fc_sam$Theta, series="Theta") +
  autolayer(fc_sam$Comb, series="Comb") +
  ggtitle(paste("Forecasting methods:",df_sam$st)) +
  xlab("Year") +
  ylab("value")

fc_names <- c("Naive", "sNaive", "Naive2", "SES", "Holt", "Damped", "Theta", "Comb")
Total_sMAPE <- Total_MASE <- matrix(data = NA, nrow = length(df), ncol = length(fc_names))
colnames(Total_sMAPE) <- colnames(Total_MASE) <- fc_names
dim(Total_sMAPE)

# forecasts
for (i in 1:length(df)){
  N <- length(df)

  output <- wrapper_fun(df[[i]], benchmarks)
  Total_sMAPE[i,] <- output$sMAPE
  Total_MASE[i,] <- output$MASE
  
  if(i%%10 ==0){
    pct <- round((i/N)*100,4)
    print(noquote(paste0(i, "/",N, " - " ,pct, "%")))
  }
  if(i%%N==0){
    print("Done!")
  }
}

## Calculate accuracy measures
print(my_data)
my_accuracy(Total_sMAPE, Total_MASE)

####################
### Save results ###
st <- n <- period <- rep(NA, length(df))

for (i in 1:length(df)){
  st[i] <- df[[i]]$st
  n[i] <- df[[i]]$n
  period[i] <- df[[i]]$period
}

results_bm8_demographic <- data.frame(Series=st, insample_n=n, period=period, sMAPE=Total_sMAPE, MASE=Total_MASE)
write.csv(results_bm8_demographic, file="results/M4_benchmarks8/results_bm8_demographic.csv")

results_table_bm8_demographic <- my_accuracy(Total_sMAPE, Total_MASE)
write.csv(results_table_bm8_demographic, file="results/M4_benchmarks8/results_bm8_demographic_table.csv")








