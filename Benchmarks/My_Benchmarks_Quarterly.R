### my_benchmarks 
# pre
rm(list=ls())
graphics.off()
library(forecast)
library(ggplot2)
source("src/my_utils.R")

# unit testing 
df <- give_sam(readRDS("data/M4_Quarterly.rds"), seed = 253)

fh <- give_fh(df)
fc <- my_benchmarks(input=df$x, fh=fh)

# plot forecasts
autoplot(df$x) +
  autolayer(df$xx, series="test") +
  autolayer(fc$Comb) +
  autolayer(fc$ARIMA) +
  autolayer(fc$ETS) +
  autolayer(fc$ETSARIMA)

# evaluate forecasts
fc_names <- c("Comb", "ARIMA", "ETS", "ETSARIMA")

for (i in 1:length(fc)){
  name <- fc_names[i]
  sMAPE <- cal_sMAPE(outsample = df$xx, forecasts=fc[[i]])
  MASE
  print(paste(name, ";", round(res, 6)))
}






