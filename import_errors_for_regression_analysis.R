# intro
rm(list=ls())
graphics.off()

# libraries
library(M4comp2018)
library(forecast)

library(ggplot2)
library(dplyr)
library(tidyr)

library(tsfeatures)


### Weekly ###

# Import DeepAR errors
error_test_file <- read.csv("error_test_file.csv")

# Merge with ... features ... 

data(M4)
#df <- Filter(function(df) df$period=="Quarterly" & df$type=="Other", M4)
df <- Filter(function(df) df$period=="Weekly", M4)
#df <- give_sam(df, 10)
rm(M4)
as.character(df[[1]]$period); as.character(df[[1]]$type);length(df)

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

tsfeat_weekly <- bind_cols(
  tsfeatures(data, c("frequency", "entropy")),
  tsfeatures(data, "khs_stl", scale=FALSE)) %>%
  select(frequency, entropy, trend, e_acf1, lambda) %>%
  rename(
    Frequency = frequency,
    Entropy = entropy,
    Trend = trend,
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

df_a <- data.frame(item_id=sn, n, tsfeat_weekly)
df_b <- error_test_file

merged_df <- merge(x = df_a, y = df_b, by = "item_id")
dim(merged_df)

