### Benchmarks_Monthly_alt
rm(list=ls())
graphics.off()
library(forecast)
library(ggplot2)
source("src/my_utils.R")

my_benchmarks_v3 <- function(input, fh){
  require(forecast)
  
  des_input <- seas_adj(input, fh)$des_input
  SIout <- seas_adj(input, fh)$SIout
  
  f_n2 <- naive(des_input, h=fh)$mean*SIout # naive2
  
  f_ses <- ses(des_input, h=fh)$mean*SIout # ses
  f_holt <- holt(des_input, h=fh, damped=FALSE)$mean*SIout # holt
  f_damped <- holt(des_input, h=fh, damped=TRUE)$mean*SIout # damped holt
  f_hw_a <- hw(input, seasonal ="additive", damped=TRUE, h=fh)$mean
  f_hw_m <- hw(input, seasonal = "multiplicative", damped=TRUE, h=fh)$mean
  
  f_theta <- thetaf(input, h=fh)$mean
  f_arima <- forecast(auto.arima(input), h=fh)$mean # ARIMA
  f_ets <- forecast(ets(input), h=fh)$mean # ETS 
  
  # Combinations
  f_hw_comb <- (f_hw_a + f_hw_m) / 2
  f_comb <- (f_ses + f_holt + f_damped) / 3
  ETSARIMA <- (f_arima + f_ets)/2
  HWARIMA1 <- (f_arima + f_hw_a) / 2
  HWARIMA2 <- (f_arima + f_hw_m) / 2
  AET <- (f_arima + f_ets + f_theta) / 3 # ETSARIMA
  ATHW1 <- (f_arima + f_theta + f_hw_a) / 3
  ATHW2 <- (f_arima + f_theta + f_hw_m) / 3
  
  ATHW_comb <- (f_arima + f_theta + f_hw_a + f_hw_m) / 4
  ATHW_comb2 <- (f_arima + f_theta + f_hw_comb) / 3
  
  AETHW1 <- (f_arima + f_ets + f_theta + f_hw_a) /4
  AETHW2 <- (f_arima + f_ets + f_theta + f_hw_m) /4
  AETHW_comb <- (f_arima + f_ets + f_theta + f_hw_comb) / 4
  
  output <- list(Naive2=f_n2, HW_a=f_hw_a, HW_m=f_hw_m, HW_comb= f_hw_comb,
                 THETA=f_theta, ARIMA=f_arima, ETS=f_ets, 
                 comb=f_comb, 
                 ETSARIMA=ETSARIMA, HWARIMA1=HWARIMA1, HWARIMA2=HWARIMA2,
                 AET=AET, ATHW1=ATHW1, ATHW2=ATHW2, 
                 ATHW_comb=ATHW_comb, ATHW_comb2=ATHW_comb2,
                 AETHW1=AETHW1, AETHW2=AETHW2, AETHW_comb=AETHW_comb)
  return(output)
}

my_data <- "data/M4_Monthly.rds"
df <- give_sam(readRDS(file=my_data),size = 100, seed = 13)
#df <- readRDS(file=my_data)
length(df)

# plot a random series 
df_sam <- give_sam(df, size=1, seed = 16)[[1]]
fc_sam <- my_benchmarks_v3(df_sam$x, fh=df_sam$h)

# historical data -> Can you guess where this is going?
autoplot(df_sam$x) +
  ggtitle(paste("Historical data of series:",df_sam$st)) +
  xlab("Year") +
  ylab("value")

# check plots 
autoplot(df_sam$x) +
  autolayer(df_sam$xx) +
  autolayer(fc_sam$HW_comb) +
  autolayer(fc_sam$HW_m) +
  autolayer(fc_sam$HW_a) +
  ggtitle(paste("Historical data of series:",df_sam$st)) +
  xlab("Year") +
  ylab("value")

autoplot(df_sam$x) +
  autolayer(df_sam$xx) +
  autolayer(fc_sam$THETA) +
  autolayer(fc_sam$ARIMA) +
  autolayer(fc_sam$ETS) +
  autolayer(fc_sam$HW_comb) +
  ggtitle(paste("Historical data of series:",df_sam$st)) +
  xlab("Year") +
  ylab("value")

# initialize values 
fc_names <- c("Naive2","HW_a", "HW_m", "HW_comb",
              "THETA", "ARIMA", "ETS", 
              "comb", 
              "ETSARIMA", "HWARIMA1", "HWARIMA2",
              "AET", "ATHW1", "ATHW2",
              "ATHW_comb", "ATHW_comb2",
              "AETHW1", "AETHW2", "AETHW_comb")
Total_sMAPE <- Total_MASE <- matrix(data = NA, nrow = length(df), ncol = length(fc_names))
colnames(Total_sMAPE) <- colnames(Total_MASE) <- fc_names
dim(Total_sMAPE)

# forecasts
for (i in 1:length(df)){
  n <- length(df)
  
  output <- wrapper_fun(df[[i]], my_benchmarks_v3)
  Total_sMAPE[i,] <- output$sMAPE
  Total_MASE[i,] <- output$MASE
  
  if(i%%2 ==0){
    pct <- round((i/n)*100,4)
    print(noquote(paste0(i, "/",n, " - " ,pct, "%")))
  }
  if(i%%n==0){
    print("Done!")
  }
}

## Calculate accuracy measures
print(my_data)

acc_table <- my_accuracy(Total_sMAPE, Total_MASE)
acc_table
acc_table[order(acc_table$OWA_mean), ]
acc_table[order(acc_table$OWA_M4),]

### Save results ###
sn <- rep(NA, length(df))

for (i in 1:length(df)){
  sn[i] <- df[[i]]$st
}

res_monthly_mybm3 <- data.frame(Series=sn, sMAPE=Total_sMAPE, MASE=Total_MASE)
write.csv(res_bm8_monthly, file="results/benchmarks/results_monthly_mybm3.csv")

res_monthly_mybm3_table <- my_accuracy(Total_sMAPE, Total_MASE)
write.csv(res_monthly_mybm3_table, file="results/benchmarks/results_monthly_mybm3_table.csv")



