### my_utils ###
### v2, after unit-testing ###

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

### Seasonality Test ###
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
