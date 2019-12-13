# unpack file M4.rda located within a tar.gz file
# use 

# unpack tar.gz
#untar("M4comp2018_0.2.0.tar.gz", list=TRUE)
#untar("M4comp2018_0.2.0.tar.gz", files="M4comp2018/data/M4.rda")

rm(list=ls())

# read in unpacked file
#readRDS("M4comp2018/data/M4.rda")
load(file = "M4comp2018/data/M4.rda")
load(file = "M4comp2018/data/submission_info.rda")

rm(df)
df <- Filter(function(df) df$period=="Monthly" & df$type=="Other", M4)
#df <- Filter(function(df) df$period=="Weekly", M4)
#df <- give_sam(df, 10)

#rm(M4)
as.character(df[[1]]$period); as.character(df[[1]]$type);length(df)

source("src/my_utils.R")

calc_smyl <- function(input){
  fc_smyl <- input$pt_ff[1,]
  insample <- input$x
  outsample <- input$xx
  
  sMAPE <- cal_sMAPE(outsample=outsample, forecasts=fc_smyl)
  MASE <- cal_MASE(insample=insample, outsample = outsample, forecasts = fc_smyl)

  output <- list(sMAPE=sMAPE, MASE=MASE)
  return(output)
}

fc_names <- c("Smyl")
Total_sMAPE <- Total_MASE <- matrix(data = NA, nrow = length(df), ncol = length(fc_names))
colnames(Total_sMAPE) <- colnames(Total_MASE) <- fc_names
dim(Total_MASE)


for (i in 1:length(df)){
  output <- calc_smyl(df[[i]])
  Total_sMAPE[i,] <- output$sMAPE
  Total_MASE[i,] <- output$MASE
}

sMAPE_mean <- round( (colMeans(Total_sMAPE, na.rm = T)),4 )
MASE_mean <- round( colMeans(Total_MASE, na.rm=T),4 )

as.character(df[[1]]$period); as.character(df[[1]]$type);length(df)
sMAPE_mean
MASE_mean

