# extract features using tsfeatures
## how to work with ts features package
### start with introduction and work your way towards M4 data. 

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
    function(x){
      tspx <- tsp(x$x)
      ts(c(x$x, x$xx), start=tspx[1], frequency=tspc[3])
    }

