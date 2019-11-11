### ETSARIMA_Legaki_Yearly 
rm(list=ls())
graphics.off()
library(forecast)
library(ggplot2)
source("src/my_utils.R")

# load data
my_data <- "data/M4_Yearly.rds"
df <- give_sam(readRDS(file=my_data),size = 24, seed = 1)
df <- readRDS(file=my_data)
length(df)

# plot a random series 
df_sam <- give_sam(df, size=1)[[1]]
fc_sam <- ETSARIMA_LEGAKI(df_sam$x, fh=df_sam$h)

autoplot(df_sam$x) +
  autolayer(df_sam$xx, series="outsample") +
  autolayer(fc_sam$Naive2, series="Naive2") +
  autolayer(fc_sam$ARIMA, series="Auto.ARIMA") + 
  autolayer(fc_sam$ETS, series="ETS") +
  autolayer(fc_sam$ETSARIMA, series="ETSARIMA") +
  autolayer(fc_sam$Legaki, series="Legaki")

# intialize values
fc_names <- c("Naive2", "ARIMA", "ETS", "ETSARIMA", "Legaki")
Total_sMAPE <- Total_MASE <- matrix(data = NA, nrow = length(df), ncol = length(fc_names))
colnames(Total_sMAPE) <- colnames(Total_MASE) <- fc_names
dim(Total_MASE)

# forecasts
for (i in 1:length(df)){
  n <- length(df)
  if(i%%5==0){
    pct <- round((i/n)*100,2)
    print(noquote(paste0(i, "/", n, " - ", pct, "%")))
  } 
  if(i%%n==0){
    print("Done!")
  }
  
  output <- wrapper_fun(df[[i]], ETSARIMA_LEGAKI)
  Total_sMAPE[i,] <- output$sMAPE
  Total_MASE[i,] <- output$MASE
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

res_quarterly <- data.frame(Series=sn, sMAPE=Total_sMAPE, MASE=Total_MASE)
write.csv(res_quarterly, file="results/M4_ETSARIMA_Legaki/ETSARIMA_Legaki_results_yearly.csv")

res_table_quarterly <- my_accuracy(Total_sMAPE, Total_MASE)
write.csv(res_table_quarterly, file="results/M4_ETSARIMA_Legaki/ETSARIMA_Legaki_results_table_yearly.csv")
