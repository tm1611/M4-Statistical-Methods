# unpack file M4.rda located within a tar.gz file
# use 

# unpack tar.gz
#untar("M4comp2018_0.2.0.tar.gz", list=TRUE)
#untar("M4comp2018_0.2.0.tar.gz", files="M4comp2018/data/M4.rda")

# read in unpacked file
#readRDS("M4comp2018/data/M4.rda")
load(file = "M4comp2018/data/M4.rda")
load(file = "M4comp2018/data/submission_info.rda")

df <- Filter(function(df) df$period=="Monthly" & df$type=="Other", M4)
#df <- Filter(function(df) df$period=="Monthly", M4)
#df <- give_sam(df, 10)
rm(M4)
as.character(df[[1]]$period); as.character(df[[1]]$type);length(df)

source("src/my_utils.R")
df <- give_sam(df, 3)

series <- df[[1]]
str(series)

forecast <- series$pt_ff
forecast[1:2,] # first two, i.e. Smyl, and MM


