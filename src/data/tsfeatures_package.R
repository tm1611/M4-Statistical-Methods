# extract features using tsfeatures
## how to work with ts features package
### start with introduction and work your way towards M4 data. 

# https://cran.r-project.org/web/packages/tsfeatures/vignettes/tsfeatures.html
# https://robjhyndman.com/hyndsight/tspackages/


# pre
rm(list=ls())
graphics.off()

library(tsfeatures)
library(M4comp2018)
library(forecast)
library(tidyverse)

data(M4)

df <- Filter(function(df) df$period=="Weekly" & df$typ=="Industry", M4)
length(df)

mylist <- list(sunspot.year, WWWusage, AirPassengers, USAccDeaths)
tsfeatures(mylist)

# Kang et al. (2017):
## Entropy ("forecastability" prox using Shannon entropy); Noise-signal relationship
## Trend: strength of trend 
## Seasonality: strength of seasonality
## Linearity
## Stability
## Observations

entry <- df[[1]]$x
entry_feat <- tsfeatures(entry)

# entropy
entry_feat$entropy

# trend 
entry_feat$trend

# Seasonal
seasonal_feat <- stl_features(entry)
seasonal_feat

# Reproducing Kang et al. (2017)
## Six features: 
### 1) "Forecastability" (randomness)
### 2) "Strength of trend"
### 3) "Strength of seasonality"
### 4) "Seasonal period"
### 5) "first order autocorr"
### 6) "optimal Box-Cox transformation"

library(tsfeatures)
library(dplyr)
library(tidyr)
library(forecast)

M3data <- purrr::map(Mcomp::M3,
                     function(x) {
                       tspx <- tsp(x$x)
                       ts(c(x$x,x$xx), start=tspx[1], frequency=tspx[3])
                     })
    
khs_stl <- function(x,...) {
  lambda <- BoxCox.lambda(x, lower=0, upper=1, method='loglik')
  y <- BoxCox(x, lambda)
  c(stl_features(y, s.window='periodic', robust=TRUE, ...), lambda=lambda)
}

khs <- bind_cols(
  tsfeatures(M3data, c("frequency", "entropy")),
  tsfeatures(M3data, "khs_stl", scale=FALSE)) %>%
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

df <- data.frame(khs)
df <- df[,1:6]
colMeans(df)

khs %>%
  select(Period, Entropy, Trend, Season, ACF1, Lambda) %>%
  GGally::ggpairs()

# 2-d Feature space (Top of Fig 2)
khs_pca <- khs %>%
  select(-Period) %>%
  prcomp(scale=TRUE)
khs_pca$x %>%
  as_tibble() %>%
  bind_cols(Period=khs$Period) %>%
  ggplot(aes(x=PC1, y=PC2)) +
  geom_point(aes(col=Period))


# Apply this to M4 data: 
