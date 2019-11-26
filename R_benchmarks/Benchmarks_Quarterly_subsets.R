### Benchmarks Monthly-subsets ###
rm(list=ls())
graphics.off()

library(M4comp2018)
library(forecast)

library(ggplot2)
library(dplyr)
library(tidyr)

library(tsfeatures)

source("src/my_utils.R")

#################
### load data ###
data(M4)
df <- Filter(function(df) df$period=="Quarterly" & df$type=="Other", M4)
#df <- Filter(function(df) df$period=="Monthly", M4)
#df <- give_sam(df, 10)
rm(M4)
as.character(df[[1]]$period); as.character(df[[1]]$type);length(df)


# plot a random series 
df_sam <- give_sam(df, size=1)[[1]]
fc_sam <- benchmarks(df_sam$x, fh=df_sam$h)

autoplot(df_sam$x) +
  autolayer(df_sam$xx, series="outsample") +
  autolayer(fc_sam$Naive2, series="Naive2") +
  autolayer(fc_sam$SES, series="SES") + 
  autolayer(fc_sam$Theta, series="Theta") +
  autolayer(fc_sam$Comb, series="Comb")


# intialize values
fc_names <- c("Naive", "sNaive", "Naive2", "SES", "Holt", "Damped", "Theta", "Comb")
Total_sMAPE <- Total_MASE <- matrix(data = NA, nrow = length(df), ncol = length(fc_names))
colnames(Total_sMAPE) <- colnames(Total_MASE) <- fc_names
dim(Total_MASE)


#################
### forecasts ###
for (i in 1:length(df)){
  n <- length(df)
  if(i%%5==0){
    pct <- round((i/n)*100,2)
    print(noquote(paste0(i, "/", n, " - ", pct, "%")))
  } 
  if(i%%n==0){
    print("Done!")
  }
  
  output <- wrapper_fun(df[[i]], benchmarks)
  Total_sMAPE[i,] <- output$sMAPE
  Total_MASE[i,] <- output$MASE
}


###################################
### Calculate accuracy measures ###
as.character(df[[1]]$period); as.character(df[[1]]$type);length(df)
my_accuracy(Total_sMAPE, Total_MASE)


#######################################
### tsfeatures ########################

### get end of data
#my_df <- df[3000:3002]
end_dates <- purrr::map(df, 
           function(x){
             end(x$x)
          })
# to do: write function that counts the series by enddate!!!


# tsfeatures
khs_stl <- function(x,...) {
  lambda <- BoxCox.lambda(x, lower=0, upper=1, method='loglik')
  y <- BoxCox(x, lambda)
  c(stl_features(y, s.window='periodic', robust=TRUE, ...), lambda=lambda)
}

# to do: tsfeatures of entire series or only train series for characterisation?
#data <- purrr::map(df,
#                   function(x) {
#                     tspx <- tsp(x$x)
#                     ts(c(x$x,x$xx), start=tspx[1], frequency=tspx[3])
#                   })

data <- purrr::map(df,
                   function(x) {
                     tspx <- tsp(x$x)
                     ts(c(x$x), start=tspx[1], frequency=tspx[3])
                   })

tsfeat <- bind_cols(
  tsfeatures(data, c("frequency", "entropy")),
  tsfeatures(data, "khs_stl", scale=FALSE)) %>%
  select(frequency, entropy, trend, seasonal_strength, e_acf1, lambda) %>%
  replace_na(list(seasonal_strength=0)) %>%
  rename(
    Frequency = frequency,
    Entropy = entropy,
    Trend = trend,
    Season = seasonal_strength,
    ACF1 = e_acf1,
    Lambda = lambda) %>%
  mutate(Period = as.factor(Frequency))


####################################################################
### Save results ###
sn <- rep(NA, length(df))
n <- rep(NA, length(df))

for (i in 1:length(df)){
  sn[i] <- df[[i]]$st
  n[i] <- df[[i]]$n
}

################################
### Save under correct names ###
################################
as.character(df[[1]]$period); as.character(df[[1]]$type);length(df)
my_accuracy(Total_sMAPE, Total_MASE)

results <- data.frame(Series=sn, n=n, tsfeat=tsfeat, sMAPE=Total_sMAPE, MASE=Total_MASE)
write.csv(results, file="results/M4_benchmarks8_Subsets/Benchmarks8_Quarterly_Other.csv")

results_table <- my_accuracy(Total_sMAPE, Total_MASE)
write.csv(results_table, file="results/M4_benchmarks8_Subsets/Benchmarks8_Quarterly_Other_table.csv")
