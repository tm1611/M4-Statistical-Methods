### Benchmarks_Hourly_alt
rm(list=ls())
graphics.off()
library(forecast)
library(ggplot2)
source("src/my_utils.R")

# load data
my_data <- "data/M4_Hourly.rds"
df <- give_sam(readRDS(file=my_data),size = 20, seed = 16)
df <- readRDS(file=my_data)
length(df)

# plot a random series 
df_sam <- give_sam(df, size=1)[[1]]
fc_sam <- benchmarks(df_sam$x, fh=df_sam$h)

autoplot(df_sam$x) +
  autolayer(df_sam$xx, series="test") +
  autolayer(fc_sam$Naive, series="Naive") +
  autolayer(fc_sam$Naive2, series="Naive2") +
  autolayer(fc_sam$Theta, series="Theta") +
  autolayer(fc_sam$Comb, series="Comb") 

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

res_bm8_hourly <- data.frame(Series=sn, sMAPE=Total_sMAPE, MASE=Total_MASE)
write.csv(res_bm8_hourly, file="results/benchmarks/results_bm8_hourly.csv")

res_table_bm8_hourly <- my_accuracy(Total_sMAPE, Total_MASE)
write.csv(res_table_bm8_hourly, file="results/benchmarks/results_bm8_hourly_table.csv")



