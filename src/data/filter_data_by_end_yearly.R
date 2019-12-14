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


########################
# Subset 2: 1991, N=3496
my_df %>% 
  filter(end_year == "1991") %>% 
  count()

my_df %>% 
  filter(end_year == "1991") %>% 
  select(item_id) -> m4_yearly_end1991_ids

target_list2 <- as.character(m4_yearly_end1991_ids[,])
length(target_list2)

m4_yearly_end1991 <- Filter(function(df) df$st %in% target_list2, df)
length(m4_yearly_end1991)

#######################
# Subset 3: 2004, N=984
my_df %>% 
  filter(end_year == "2004") %>% 
  count()

my_df %>% 
  filter(end_year == "2004") %>% 
  select(item_id) -> m4_yearly_end2004_ids

target_list3 <- as.character(m4_yearly_end2004_ids[,])
length(target_list3)

m4_yearly_end2004 <- Filter(function(df) df$st %in% target_list3, df)
length(m4_yearly_end2004)

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


