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
error_test_file <- read.csv("data/error_test_file.csv")

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

# add domain #
# to do: function that takes out domain and item_id




####################################################################
# Combine results
sn <- rep(NA, length(df))
n <- rep(NA, length(df))
type <- rep(NA, length(df))

for (i in 1:length(df)){
  sn[i] <- df[[i]]$st
  n[i] <- df[[i]]$n
  type[i] <- df[[i]]$type
}

df_a <- data.frame(item_id=sn, n=n, type=type, tsfeat_weekly)
df_b <- error_test_file

merged_df <- merge(x = df_a, y = df_b, by = "item_id")
dim(merged_df)
head(merged_df)

#######################
# dplyr what you want #

head(merged_df)

merged_df %>% 
  select(item_id, n, type, Entropy, Trend, ACF1, Lambda, MASE, sMAPE, MSIS) -> df_processed

head(df_processed)

# Add OWA m4 #


############################
# change type of type
df_processed$type <- factor(df_processed$type, 
                  levels=c(1,2,3,4,5,6),
                  labels=c("Demographic", "Finance", "Industry",
                           "Macro", "Micro", "Other"))


# descriptive statistcs
df_processed[, purrr::map_lgl(df_processed, is.numeric)] %>% 
  colMeans() %>% 
  round(4)

head(df_processed[,-1])

#############################################################
# Regressions # 

# Check correlation with type and length
summary(lm(formula = MASE ~ type + n, data=df_processed))
summary(lm(formula = sMAPE ~ type + n, data=df_processed))

# linear regression models tsfeatures 
summary(lm(formula = MASE ~  n + Entropy + Trend + ACF1 + Lambda, data=df_processed))
summary(lm(formula = sMAPE ~ n + Entropy + Trend + ACF1 + Lambda, data=df_processed))

# linear regression models types plus tsfeatures
summary(lm(formula = MASE ~ type + n + Entropy + Trend + ACF1 + Lambda, data=df_processed))
summary(lm(formula = sMAPE ~ type + n + Entropy + Trend + ACF1 + Lambda, data=df_processed))








