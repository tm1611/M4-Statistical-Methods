### Filter by end date ###

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

data(M4)
#df <- Filter(function(df) df$period=="Yearly" & df$type=="Other", M4)
df <- Filter(function(df) df$period=="Quarterly", M4)
rm(M4)

as.character(df[[1]]$period)
as.character(df[[1]]$type)
length(df)


item_id <- rep(NA, length(df))
length <- rep(NA, length(df))
end <- rep(NA, length(df))

for (i in 1:length(df)){
  entry <- df[[i]]
  series <- entry$x
  item_id[i] <- df[[i]]$st
  length[i] <- df[[i]]$n
  end[i] <- time(series)[length(series)]
}

df_prop <- data.frame(item_id=item_id, length=length, end=end)
dim(df_prop)
range(df_prop$end)
head(df_prop)

df_prop %>% 
  filter(end < 2020) %>% 
  count(end) %>% 
  filter(n > 50) %>% 
  arrange(desc(n)) %>%
  data.frame() -> df_end_n

df_prop %>% 
  filter(end < 2020) %>% 
  count(end) %>% 
  filter(n > 50) %>% 
  arrange(desc(end))


head(df_end_n, 10)

# distribution of end dates
ggplot(df_end_n, aes(x=end, y=n)) +
  geom_bar(stat="identity")


#####################################################
#####################################################
# Subset 1: N=4126, Q1/2015
df_prop %>% 
  filter(end==2015.25) %>% 
  count()

df_prop %>% 
  filter(end==2015.25) %>% 
  select(item_id) -> m4_quarterly_2015q1_ids

target_list1 <- as.character(m4_quarterly_2015q1_ids[,])
length(target_list1)
head(target_list1)

m4_quarterly_end2015q1 <- Filter(function(df) df$st %in% target_list1, df)
length(m4_quarterly_end2015q1)


#####################################################
#####################################################
# Subset 2: N=1119, Q1/2013
df_prop %>% 
  filter(end==2013.25) %>% 
  count()

df_prop %>% 
  filter(end==2013.25) %>% 
  select(item_id) -> m4_quarterly_2013q1_ids

target_list2 <- as.character(m4_quarterly_2013q1_ids[,])
length(target_list2)

m4_quarterly_end2013q1 <- Filter(function(df) df$st %in% target_list2, df)
length(m4_quarterly_end2013q1)


#####################################################
#####################################################
# Subset 3: N=728, Q3/2003
df_prop %>% 
  filter(end==2003.75) %>% 
  count()

df_prop %>% 
  filter(end==2003.75) %>% 
  select(item_id) -> m4_quarterly_2003q3_ids

target_list3 <- as.character(m4_quarterly_2003q3_ids[,])
length(target_list3)

m4_quarterly_end2003q3 <- Filter(function(df) df$st %in% target_list3, df)
length(m4_quarterly_end2003q3)


####################################################
####################################################
length(m4_quarterly_end2015q1)
length(m4_quarterly_end2013q1)
length(m4_quarterly_end2003q3)

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

# train file
json1 <- map(1:length(m4_quarterly_end2003q3), ts_to_json, m4_quarterly_end2003q3, test_data=FALSE, true_dates=FALSE, domain_cat=FALSE)

sink("data/json/m4_quarterly_end2003q3_train.json")
json1 <- lapply(json1, cat)
sink()

# test file
json2 <- map(1:length(m4_quarterly_end2003q3), ts_to_json, m4_quarterly_end2003q3, test_data=TRUE, true_dates=FALSE, domain_cat=FALSE)

sink("data/json/m4_quarterly_end2003q3_test.json")
json2 <- lapply(json2, cat)
sink()

