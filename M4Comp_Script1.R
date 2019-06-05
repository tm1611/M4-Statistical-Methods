### Benchmark methods and Measures

# prep
rm(list=ls())
graphics.off()

# packages 
library(forecast)
library(ggplot2)
#devtools::install_github("carlanetto/M4comp2018")
library(M4comp2018)

names(M4[[1]])

# extract yearly series
yearly_M4 <- Filter(function(l) l$period =="Yearly", M4)
length(yearly_M4)

# number for one of the yearly ts
nr <- 18
train <- yearly_M4[[nr]]$x
test <- yearly_M4[[nr]]$xx

autoplot(train, series="train", col="black") +
  autolayer(test, series="test")

# extract monthly series 
monthly_M4 <- Filter(function(l) l$period=="Monthly", M4)

# check one of the monthly series
train <- monthly_M4[[nr]]$x
test <- monthly_M4[[nr]]$xx

length(train)

autoplot(train, series = "train", col="black") +
  autolayer(test, series = "test")
  
# domain-specific data


# check ts 40773
train <- M4[[40773]]$x
test <- M4[[40773]]$xx

# clear-memory-except
rm(list=setdiff(ls(), c("train","test")))

# plot
autoplot(train, series="train", color="black") +
  autolayer(test, series="test", color="red")

### naive/ seasonal naive methods ###
# Game plan: working meth on one series, extend
# fh: forecast horizon (aka as h in other functions)

# give_fh
give_fh <- function(input){
  if(frequency(input)==1){
    fh <- 6
  } else if(frequency(input)==4){
    fh <- 8
  } else if(frequency(input)==12){
    fh <- 18
  } else if(frequency(input)==52){
    fh <- 13
  } else if(frequency(input)==365){
    fh <- 14
  } else if(frequency(input)==8760){
    fh <- 48
  }
}
fh <- give_fh(train)

f1 <- naive(train, h=fh)
f2 <- snaive(train, h=fh)

autoplot(train) +
  autolayer(f1$mean, series="Naive") +
  autolayer(f2$mean, series="Seasonal Naive") +
  ggtitle("Forecasts from naive methods") +
  guides(colour=guide_legend(title="Forecast"))








