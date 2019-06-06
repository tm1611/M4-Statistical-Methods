#######################################
### M4Comp - Methods and Benchmarks ###
# utility function file 
# See individual files on how to use these functions

## Notes ##
# Avoid dependencies within functions

#############
# libraries #
#library(forecast)
#library(ggplot2)
#library(M4comp2018)

#########################
### utility functions ### 


## give frequency ##
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
  return(fh)
}
# to do:
# unit test for all frequencies 
# How to avoid long else-if structure?
# -> Use pattern 


## Calculate sMAPE ##
cal_sMAPE <- function(outsample, forecasts){ 
  outsample <- as.numeric(outsample)
  forecasts <- as.numeric(forecasts)
  num <- abs(outsample-forecasts)*2
  denom <- abs(outsample) + abs(forecasts)
  
  smape <- mean(num/denom, na.rm=TRUE)
  return(smape)
}
# to do:
# Unit tests


## Calculate MASE ##
cal_MASE <- function(insample, outsample, forecasts){
  frq <- frequency(insample)
  forecastsNaiveSD <- rep(NA, frq)
  
  for (j in (frq+1): length(insample)){
    forecastsNaiveSD <- c(forecastsNaiveSD, insample[j-frq])
  }
  
  masep <- mean(abs(insample - forecastsNaiveSD),na.rm=TRUE)
  outsample <- as.numeric(outsample)
  forecasts <- as.numeric(forecasts)
  
  mase <- mean((abs(outsample - forecasts)) / masep)
  
  return(mase)
}
# to do: 
# Unit tests
# check if naive2 is calculated correctly
# check masep


## Seasonality Test ##
SeasonalityTest <- function(input, ppy){
  tcrit <- 1.645
  if (length(input)<3*ppy){
    test_seasonal <- FALSE
  }else{
    xacf <- acf(input, plot = FALSE)$acf[-1, 1, 1]
    clim <- tcrit/sqrt(length(input)) * sqrt(cumsum(c(1, 2 * xacf^2)))
    test_seasonal <- ( abs(xacf[ppy]) > clim[ppy] )
    
    if (is.na(test_seasonal)==TRUE){ test_seasonal <- FALSE }
  }
  return(test_seasonal)
}
# to do: 
# unit testsing


## Seasonal adjustment function ##
# seasonal adjustment function
seas_adj <- function(input, fh){
  # estimate seasonaly adjused series if seasonal
  ppy <- frequency(input)
  ST <- F
  
  if(ppy > 1){
    ST<- SeasonalityTest(input, ppy)
  }
  
  if(ST==T){
    dec <- decompose(input, type="multiplicative")
    des_input <- input/dec$seasonal
    n_seas <- length(dec$seasonal)
    # 
    SIout <- head(rep(dec$seasonal[(n_seas - ppy+1): n_seas ],fh), fh)
  }else{
    des_input <- input
    SIout <- rep(1, fh)
  }
  output <- list(des_input=des_input, SIout=SIout)
  return(output)
}


# to do:
# can this be further modularized?
# unit tests


## Benchmarks ##
benchmarks <- function(input){ 
  fh <- give_fh(input)
  des_input <- seas_adj(input)$des_input
  SIout <- seas_adj(input)$SIout
  
  f1 <- naive(input, h=fh)$mean # naive
  f2 <- snaive(input, h=fh)$mean # snaive
  f3 <- naive(des_input, h=fh)$mean*SIout # naive2
  f4 <- ses(des_input, h=fh)$mean*SIout # ses
  f5 <- holt(des_input, h=fh, damped=FALSE)$mean*SIout # holt
  f6 <- holt(des_input, h=fh, damped=TRUE)$mean*SIout # damped holt
  f7 <- thetaf(input, h=fh)$mean
  f8 <- (f4+f5+f6)/3 # comb
  
  output <- list(Naive=f1, sNaive=f2, Naive2=f3, SES=f4, Holt=f5, Damped=f6, Theta=f7, Comb=f8)
  return(output)
}
# to do: 
# Unit testing





