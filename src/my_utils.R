#######################################
### M4Comp - Methods and Benchmarks ###
# utility function file 
# See individual files on how to use these functions

## Notes ##
# Avoid dependencies within functions

#########################
### utility functions ### 

# give sample from M4
give_sam <- function(input, size=1, seed=16){
  set.seed(seed)
  idx <- sample(1:length(input), size=size, replace=FALSE)
  input[idx]
}

## give forecast horizon ##
give_fh <- function(input){
  if(input$period=="Yearly"){
    fh <- 6
  } else if(input$period=="Quarterly"){
    fh <- 8
  } else if(input$period=="Monthly"){
    fh <- 18
  } else if(input$period=="Weekly"){
    fh <- 13
  } else if(input$period=="Daily"){
    fh <- 14
  } else if(input$period=="Hourly"){
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
# frequency() seems not to work with daily, ... data


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
seas_adj <- function(input, fh){
  # estimate seasonaly adjusted series if seasonal
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
# Does frequency work on daily data?


## Benchmarks M4Comp
benchmarks <- function(input, fh){ 
  des_input <- seas_adj(input, fh)$des_input
  SIout <- seas_adj(input, fh)$SIout
  
  f1 <- naive(input, h=fh)$mean # naive
  f2 <- snaive(input, h=fh)$mean # snaive
  f3 <- naive(des_input, h=fh)$mean*SIout # naive2
  f4 <- ses(des_input, h=fh)$mean*SIout # ses
  f5 <- holt(des_input, h=fh, damped=FALSE)$mean*SIout # holt
  f6 <- holt(des_input, h=fh, damped=TRUE)$mean*SIout # damped holt
  f7 <- thetaf(input, h=fh)$mean
  f8 <- (f4+f5+f6)/3 # comb
  
  output <- list(Naive=f1, sNaive=f2, Naive2=f3, SES=f4, Holt=f5, Damped=f6, HW=f6b, Theta=f7, Comb=f8)
  return(output)
}
# to do: 
# Unit testing

# benchmarks v2
benchmarks2 <- function(input, fh){ 
  des_input <- seas_adj(input, fh)$des_input
  SIout <- seas_adj(input, fh)$SIout
  
  f9 <- rwf(input, drift=TRUE)$mean # rwf
  f10 <- rwf(input, drift=TRUE, lambda=BoxCox.lambda(input))$mean# rwf, BoxCox
  f11 <- rwf(input, drift=TRUE, lambda=BoxCox.lambda(input), biasadj = TRUE)$mean # rwf biasadj
  f12 <- forecast(auto.arima(input), h=fh)$mean # ARIMA
  f13 <- forecast(ets(input), h=fh)$mean # ETS 
  f14 <- (f12+f13) / 2 # ETSARIMA
  
  output <- list(RWdrift=f9, RWdrift_BC=f10, RWdrift_BC_a=f11, ARIMA=f12, ETS=f13, ETSARIMA=f14)
  return(output)
}

# benchmarks v3
my_benchmarks <- function(input, fh){
  des_input <- seas_adj(input, fh)$des_input
  SIout <- seas_adj(input, fh)$SIout
  f3 <- naive(des_input, h=fh)$mean*SIout # naive2
  
  f4 <- ses(des_input, h=fh)$mean*SIout # ses
  f5 <- holt(des_input, h=fh, damped=FALSE)$mean*SIout # holt
  f6 <- holt(des_input, h=fh, damped=TRUE)$mean*SIout # damped holt
  f8 <- (f4+f5+f6)/3 # comb

  f12 <- forecast(auto.arima(input), h=fh)$mean # ARIMA
  f13 <- forecast(ets(input), h=fh)$mean # ETS 
  f14 <- (f12+f13) / 2 # ETSARIMA
  
  output <- list(Naive2=f3, Comb=f8, ARIMA=f12, ETS=f13, ETSARIMA=f14)
  return(output)
}

# wrapper function
wrapper_fun <- function(input, fun){
  insample <- input$x
  outsample <- input$xx
  fh <- input$h
  
  fc <- fun(input=insample, fh=fh)
  sMAPE <- sapply(fc, cal_sMAPE, outsample=outsample)
  MASE <- sapply(fc, cal_MASE, insample=insample, outsample=outsample)
  output <- list(sMAPE=sMAPE, MASE=MASE)
  return(output)
}

# my accuracy
my_accuracy <- function(Total_sMAPE, Total_MASE){
  sMAPE_mean <- round( (colMeans(Total_sMAPE, na.rm = T)*100),4 )
  MASE_mean <- round( colMeans(Total_MASE, na.rm=T),4 )
  
  # Calculate mean OWA
  rel_sMAPE <- Total_sMAPE / Total_sMAPE[,"Naive2"]
  rel_MASE <- Total_MASE / Total_MASE[,"Naive2"]
  OWA <- (rel_sMAPE + rel_MASE) / 2
  OWA_mean <- round( colMeans(OWA, na.rm=T),4 )
  
  # Calculate mean OWA according M4
  rel_sMAPE_M4 <- colMeans(Total_sMAPE, na.rm=T) / colMeans(Total_sMAPE, na.rm = T)["Naive2"]
  rel_MASE_M4 <- colMeans(Total_MASE, na.rm=T) / colMeans(Total_MASE, na.rm=T)["Naive2"]
  OWA_M4 <- round( ((rel_sMAPE_M4 + rel_MASE_M4) / 2), 4)
  # results
  data.frame(sMAPE_mean, MASE_mean, OWA_mean, OWA_M4)
}

