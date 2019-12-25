### Filter by end date: yearly ###

# pre
rm(list=ls())
graphics.off()

#libraries import 
library(M4comp2018)
library(jsonlite)
library(tidyverse)
library(parallel)
library(anytime)
library(purrr)
library(xts)
library(zoo)

library(forecast)
library(tsfeatures)

source("src/my_utils.R")


####################
##### Get Data #####
data(M4)
#df <- Filter(function(df) df$period=="Yearly" & df$type=="Other", M4)
df <- Filter(function(df) df$period=="Yearly", M4)
rm(M4)

as.character(df[[1]]$period); as.character(df[[1]]$type);length(df)

### get end of data
#end_dates <- purrr::map(df, 
#                        function(x){
#                          end(x$x)
#                        })

item_id <- rep(NA, length(df))
n <- rep(NA, length(df))
end <- rep(NA, length(df))

for (i in 1:length(df)){
  entry <- df[[i]]
  series <- entry$x
  item_id[i] <- df[[i]]$st
  n[i] <- df[[i]]$n
  end[i] <- time(series)[length(series)]
}

df_prop <- data.frame(item_id=item_id, length=n, end=end)
dim(df_prop)

df_prop %>% 
  filter(end > 2019) %>% 
  count()
# Why 254 time series that end after 2019?

df_prop %>% 
  filter(end < 2020) %>% 
  count(end) %>% 
  filter(n > 100) %>% 
  arrange(desc(n)) %>% 
  data.frame() -> df_end_n
  
head(df_end_n, 10)

# distribution
ggplot(df_end_n, aes(end, n)) +
  geom_bar(stat="identity") # 3 peaks: 2009, 1991, 2004

###################
##### Subsets #####


########################
# Subset 1: 2009, N=6029 
df_prop %>% 
  filter(end == "2009") %>% 
  count()

df_prop %>% 
  filter(end == "2009") %>% 
  select(item_id) -> m4_yearly_end2009_ids

target_list1 <- as.character(m4_yearly_end2009_ids[,])
length(target_list1)

m4_yearly_end2009 <- Filter(function(df) df$st %in% target_list1, df)
length(m4_yearly_end2009)


# get etsarima-legaki and benchmarks for this subset
df1 <- m4_yearly_end2009
length(df1)



fc_names <- c("Naive", "sNaive", "Naive2", "SES", "Holt", "Damped", "Theta", "Comb")
Total_sMAPE <- Total_MASE <- matrix(data = NA, nrow = length(df1), ncol = length(fc_names))
colnames(Total_sMAPE) <- colnames(Total_MASE) <- fc_names
dim(Total_MASE)

#################
### forecasts ###
for (i in 1:length(df1)){
  n <- length(df1)
  if(i%%5==0){
    pct <- round((i/n)*100,2)
    print(noquote(paste0(i, "/", n, " - ", pct, "%")))
  } 
  if(i%%n==0){
    print("Done!")
  }
  
  output <- wrapper_fun(df1[[i]], benchmarks)
  Total_sMAPE[i,] <- output$sMAPE
  Total_MASE[i,] <- output$MASE
}

sn <- rep(NA, length(df1))
n <- rep(NA, length(df1))

for (i in 1:length(df1)){
  sn[i] <- df1[[i]]$st
  n[i] <- df1[[i]]$n
}

################################
### Save under correct names ###
################################
as.character(df1[[1]]$period); as.character(df1[[1]]$type);length(df1)
my_accuracy(Total_sMAPE, Total_MASE)

results <- data.frame(Series=sn, n=n, sMAPE=Total_sMAPE, MASE=Total_MASE)
write.csv(results, file="results/M4_benchmarks8_Subsets/Benchmarks8_M4_Yearly_End2009.csv")

results_table <- my_accuracy(Total_sMAPE, Total_MASE)
write.csv(results_table, file="results/M4_benchmarks8_Subsets/Benchmarks8_M4_Yearly_End2009_table.csv")




# intialize values
fc_names <- c("Naive2", "ARIMA", "ETS", "ETSARIMA", "Legaki")
Total_sMAPE <- Total_MASE <- matrix(data = NA, nrow = length(df1), ncol = length(fc_names))
colnames(Total_sMAPE) <- colnames(Total_MASE) <- fc_names
dim(Total_MASE)

### forecasts ###
for (i in 1:length(df1)){
  n <- length(df1)
  if(i%%5==0){
    pct <- round((i/n)*100,2)
    print(noquote(paste0(i, "/", n, " - ", pct, "%")))
  } 
  if(i%%n==0){
    print("Done!")
  }
  
  output <- wrapper_fun(df1[[i]], ETSARIMA_LEGAKI)
  Total_sMAPE[i,] <- output$sMAPE
  Total_MASE[i,] <- output$MASE
}

### Calculate accuracy measures ###
as.character(df1[[1]]$period); as.character(df1[[1]]$type)
my_accuracy(Total_sMAPE, Total_MASE)

# save results
sn <- rep(NA, length(df1))
n <- rep(NA, length(df1))

for (i in 1:length(df1)){
  sn[i] <- df1[[i]]$st
  n[i] <- df1[[i]]$n
}

# save under correct names
as.character(df1[[1]]$period); as.character(df1[[1]]$type);length(df1)
my_accuracy(Total_sMAPE, Total_MASE)

ETSARIMA_Legaki <- data.frame(Series=sn, n=n, sMAPE=Total_sMAPE, MASE=Total_MASE)
write.csv(ETSARIMA_Legaki, file="results/M4_ETSARIMA_Legaki/ETSARIMA_Legaki_Yearly_End2009.csv")

Res_table_quarterly <- my_accuracy(Total_sMAPE, Total_MASE)
write.csv(Res_table_quarterly, file="results/M4_ETSARIMA_Legaki/ETSARIMA_Legaki_Yearly_End2009_table.csv")





########################
# Subset 2: 1991, N=3496
df_prop %>% 
  filter(end == "1991") %>% 
  count()

df_prop %>% 
  filter(end == "1991") %>% 
  select(item_id) -> m4_yearly_end1991_ids

target_list2 <- as.character(m4_yearly_end1991_ids[,])
length(target_list2)

m4_yearly_end1991 <- Filter(function(df) df$st %in% target_list2, df)
length(m4_yearly_end1991)


# get etsarima-legaki for this df
df1 <- m4_yearly_end1991
length(df1)


fc_names <- c("Naive", "sNaive", "Naive2", "SES", "Holt", "Damped", "Theta", "Comb")
Total_sMAPE <- Total_MASE <- matrix(data = NA, nrow = length(df1), ncol = length(fc_names))
colnames(Total_sMAPE) <- colnames(Total_MASE) <- fc_names
dim(Total_MASE)

#################
### forecasts ###
for (i in 1:length(df1)){
  n <- length(df1)
  if(i%%5==0){
    pct <- round((i/n)*100,2)
    print(noquote(paste0(i, "/", n, " - ", pct, "%")))
  } 
  if(i%%n==0){
    print("Done!")
  }
  
  output <- wrapper_fun(df1[[i]], benchmarks)
  Total_sMAPE[i,] <- output$sMAPE
  Total_MASE[i,] <- output$MASE
}

sn <- rep(NA, length(df1))
n <- rep(NA, length(df1))

for (i in 1:length(df1)){
  sn[i] <- df1[[i]]$st
  n[i] <- df1[[i]]$n
}

################################
### Save under correct names ###
################################
as.character(df1[[1]]$period); as.character(df1[[1]]$type);length(df1)
my_accuracy(Total_sMAPE, Total_MASE)
df1[[1]]$x

results <- data.frame(Series=sn, n=n, sMAPE=Total_sMAPE, MASE=Total_MASE)
write.csv(results, file="results/M4_benchmarks8_Subsets/Benchmarks8_M4_Yearly_End1991.csv")

results_table <- my_accuracy(Total_sMAPE, Total_MASE)
write.csv(results_table, file="results/M4_benchmarks8_Subsets/Benchmarks8_M4_Yearly_End1991_table.csv")




# intialize values
fc_names <- c("Naive2", "ARIMA", "ETS", "ETSARIMA", "Legaki")
Total_sMAPE <- Total_MASE <- matrix(data = NA, nrow = length(df1), ncol = length(fc_names))
colnames(Total_sMAPE) <- colnames(Total_MASE) <- fc_names
dim(Total_MASE)

### forecasts ###
for (i in 1:length(df1)){
  n <- length(df1)
  if(i%%5==0){
    pct <- round((i/n)*100,2)
    print(noquote(paste0(i, "/", n, " - ", pct, "%")))
  } 
  if(i%%n==0){
    print("Done!")
  }
  
  output <- wrapper_fun(df1[[i]], ETSARIMA_LEGAKI)
  Total_sMAPE[i,] <- output$sMAPE
  Total_MASE[i,] <- output$MASE
}

### Calculate accuracy measures ###
as.character(df1[[1]]$period); as.character(df1[[1]]$type)
my_accuracy(Total_sMAPE, Total_MASE)

# save results
sn <- rep(NA, length(df1))
n <- rep(NA, length(df1))

for (i in 1:length(df1)){
  sn[i] <- df1[[i]]$st
  n[i] <- df1[[i]]$n
}

# save under correct names
as.character(df1[[1]]$period); as.character(df1[[1]]$type);length(df1)
my_accuracy(Total_sMAPE, Total_MASE)

ETSARIMA_Legaki <- data.frame(Series=sn, n=n, sMAPE=Total_sMAPE, MASE=Total_MASE)
write.csv(ETSARIMA_Legaki, file="results/M4_ETSARIMA_Legaki/ETSARIMA_Legaki_Yearly_End1991.csv")

Res_table_quarterly <- my_accuracy(Total_sMAPE, Total_MASE)
write.csv(Res_table_quarterly, file="results/M4_ETSARIMA_Legaki/ETSARIMA_Legaki_Yearly_End1991_table.csv")











#######################
# Subset 3: 2004, N=984
df_prop %>% 
  filter(end == "2004") %>% 
  count()

df_prop %>% 
  filter(end == "2004") %>% 
  select(item_id) -> m4_yearly_end2004_ids

target_list3 <- as.character(m4_yearly_end2004_ids[,])
length(target_list3)

m4_yearly_end2004 <- Filter(function(df) df$st %in% target_list3, df)
length(m4_yearly_end2004)


# get etsarima-legaki for this df
df1 <- m4_yearly_end2004
length(df1)


fc_names <- c("Naive", "sNaive", "Naive2", "SES", "Holt", "Damped", "Theta", "Comb")
Total_sMAPE <- Total_MASE <- matrix(data = NA, nrow = length(df1), ncol = length(fc_names))
colnames(Total_sMAPE) <- colnames(Total_MASE) <- fc_names
dim(Total_MASE)

#################
### forecasts ###
for (i in 1:length(df1)){
  n <- length(df1)
  if(i%%5==0){
    pct <- round((i/n)*100,2)
    print(noquote(paste0(i, "/", n, " - ", pct, "%")))
  } 
  if(i%%n==0){
    print("Done!")
  }
  
  output <- wrapper_fun(df1[[i]], benchmarks)
  Total_sMAPE[i,] <- output$sMAPE
  Total_MASE[i,] <- output$MASE
}

sn <- rep(NA, length(df1))
n <- rep(NA, length(df1))

for (i in 1:length(df1)){
  sn[i] <- df1[[i]]$st
  n[i] <- df1[[i]]$n
}

################################
### Save under correct names ###
################################
as.character(df1[[1]]$period); as.character(df1[[1]]$type);length(df1)
my_accuracy(Total_sMAPE, Total_MASE)
df1[[1]]$x

results <- data.frame(Series=sn, n=n, sMAPE=Total_sMAPE, MASE=Total_MASE)
write.csv(results, file="results/M4_benchmarks8_Subsets/Benchmarks8_M4_Yearly_End2004.csv")

results_table <- my_accuracy(Total_sMAPE, Total_MASE)
write.csv(results_table, file="results/M4_benchmarks8_Subsets/Benchmarks8_M4_Yearly_End2004_table.csv")






# intialize values
fc_names <- c("Naive2", "ARIMA", "ETS", "ETSARIMA", "Legaki")
Total_sMAPE <- Total_MASE <- matrix(data = NA, nrow = length(df1), ncol = length(fc_names))
colnames(Total_sMAPE) <- colnames(Total_MASE) <- fc_names
dim(Total_MASE)

### forecasts ###
for (i in 1:length(df1)){
  n <- length(df1)
  if(i%%5==0){
    pct <- round((i/n)*100,2)
    print(noquote(paste0(i, "/", n, " - ", pct, "%")))
  } 
  if(i%%n==0){
    print("Done!")
  }
  
  output <- wrapper_fun(df1[[i]], ETSARIMA_LEGAKI)
  Total_sMAPE[i,] <- output$sMAPE
  Total_MASE[i,] <- output$MASE
}

### Calculate accuracy measures ###
as.character(df1[[1]]$period); as.character(df1[[1]]$type)
my_accuracy(Total_sMAPE, Total_MASE)

# save results
sn <- rep(NA, length(df1))
n <- rep(NA, length(df1))

for (i in 1:length(df1)){
  sn[i] <- df1[[i]]$st
  n[i] <- df1[[i]]$n
}

# save under correct names
as.character(df1[[1]]$period); as.character(df1[[1]]$type);length(df1)
my_accuracy(Total_sMAPE, Total_MASE)
df1[[1]]$x

ETSARIMA_Legaki <- data.frame(Series=sn, n=n, sMAPE=Total_sMAPE, MASE=Total_MASE)
write.csv(ETSARIMA_Legaki, file="results/M4_ETSARIMA_Legaki/ETSARIMA_Legaki_Yearly_End2004.csv")

Res_table_quarterly <- my_accuracy(Total_sMAPE, Total_MASE)
write.csv(Res_table_quarterly, file="results/M4_ETSARIMA_Legaki/ETSARIMA_Legaki_Yearly_End2004_table.csv")







###############################
##### Export data to JSON #####

length(m4_yearly_end2009)
length(m4_yearly_end1991)
length(m4_yearly_end2004)

ts_to_json <- function(idx, df, test_data=FALSE, true_dates=FALSE, domain_cat=FALSE){
  train <- df[[idx]]$x
  test <- df[[idx]]$xx
  item_id <- df[[idx]]$st
  
  if (domain_cat == FALSE){
    feat_static_cat <- list(idx)
  } else {
    feat_static_cat <- c(idx, as.numeric(df[[idx]]$type))
  }
  
  if (true_dates == FALSE){
    start <- "1750-01-01 00:00:00"
  } else {
    start <- paste0(as.character(as.Date(time(train))[1]), " 00:00:00") 
  }
  
  if (test_data==FALSE){
    target=train
  } else {
    target=c(train, test)
  }
  
  my_list <- list(
    start=start,
    item_id=item_id,
    target=target,
    feat_static_cat = feat_static_cat
  )
  
  json <- paste0(toJSON(my_list, auto_unbox=TRUE), "\n")
  #json <- toJSON(my_list, auto_unbox=TRUE, pretty=FALSE)
  
  return(json)
}

####################################################
####################################################

# train file
json1 <- map(1:length(m4_yearly_end2004), ts_to_json, m4_yearly_end2004, test_data=FALSE, true_dates=FALSE, domain_cat=FALSE)

sink("data/json/m4_yearly_end2004_train.json")
json1 <- lapply(json1, cat)
sink()

# test file
json2 <- map(1:length(m4_yearly_end2004), ts_to_json, m4_yearly_end2004, test_data=TRUE, true_dates=FALSE, domain_cat=FALSE)

sink("data/json/m4_yearly_end2004_test.json")
json2 <- lapply(json2, cat)
sink()


