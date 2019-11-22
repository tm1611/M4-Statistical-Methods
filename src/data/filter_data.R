# data filter

# tasks: 
## 1. Filter data by frequency and domain
## 2. export to csv that it can be used by python

# pre
rm(list=ls())
graphics.off()

#libraries import 
library(M4comp2018)

library(tsfeatures)
library(dplyr)
library(tidyr)
library(forecast)

# load data
data(M4)

# Weekly data
df <- Filter(function(df) df$period=="Weekly" & df$type=="Finance", M4)
df <- Filter(function(df) df$period=="Weekly", M4)

length(df)

# one entry
entry <- df[[1]]
autoplot(entry$x)

# function for BoxCox
khs_stl <- function(x,...) {
  lambda <- BoxCox.lambda(x, lower=0, upper=1, method='loglik')
  y <- BoxCox(x, lambda)
  c(stl_features(y, s.window='periodic', robust=TRUE, ...), lambda=lambda)
}

# prep data
data <- purrr::map(df,
                     function(x) {
                       tspx <- tsp(x$x)
                       ts(c(x$x,x$xx), start=tspx[1], frequency=tspx[3])
                     })




# get Kang et al. (2017) features
khs <- bind_cols(
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

khs2 <- bind_cols(
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

dim(khs)
colMeans(data.frame(khs[,2:5]))

dim(khs2)
colMeans(data.frame(khs2[,2:5]))

