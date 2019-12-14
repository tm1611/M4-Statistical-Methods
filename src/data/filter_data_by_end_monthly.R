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
df <- Filter(function(df) df$period=="Monthly", M4)
rm(M4)

# consistency check
as.character(df[[1]]$period); as.character(df[[1]]$type);length(df)

### get end of data
#end_dates <- purrr::map(df, 
#                        function(x){
#                          end(x$x)
#                        })

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
head(df_prop)

# Function that counts the series by enddate!!!
df_prop %>% 
  filter(end < 2020) %>% 
  filter(end > 2007) %>% 
  count(end) %>% 
  filter(n > 50) %>% 
  arrange(desc(n)) %>% 
  round(2) %>% 
  data.frame() -> df_end_n

head(df_end_n, 10)

# distribution of end dates
ggplot(df_end_n, aes(x = end, y=n)) +
  geom_bar(stat="identity")


##################################
##################################
# subset 1: N=15700 - 5/2015
df_prop %>% 
  filter(end > 2015.4) %>% 
  filter(end < 2015.5) %>% 
  count()

df_prop %>% 
  filter(end > 2015.4) %>% 
  filter(end < 2015.5) %>% 
  select(item_id) -> m4_monthly_052015_ids

target_list1 <- as.character(m4_monthly_052015_ids[,])

length(target_list1)
head(target_list1)
class(target_list1)
class(df[[1]]$st)

m4_monthly_end052015 <- Filter(function(df) df$st %in% target_list1, df)
as.character(m4_monthly_end052015[[1]]$period)
as.character(m4_monthly_end052015[[1]]$type)
length(m4_monthly_end052015)

##################################
##################################
# subset 2: N=1807 - 03/2014
df_prop %>% 
  filter(end>2014.2) %>% 
  filter(end<2014.3) %>% 
  count()

df_prop %>% 
  filter(end > 2014.2) %>% 
  filter(end < 2014.3) %>% 
  select(item_id) -> m4_monthly_032014_ids

target_list2 <- as.character(m4_monthly_032014_ids[,])

length(target_list2)
head(target_list2)
class(target_list2)
class(df[[1]]$st)

m4_monthly_end032014 <- Filter(function(df) df$st %in% target_list2, df)
as.character(m4_monthly_end032014[[1]]$period)
as.character(m4_monthly_end032014[[1]]$type)
length(m4_monthly_end032014)


##################################
##################################
# subset 3: 943 - 09/2007
df_prop %>% 
  filter(end > 2007.7) %>% 
  filter(end < 2007.8) %>% 
  count()
  
df_prop %>% 
  filter(end > 2007.7) %>% 
  filter(end < 2007.8) %>% 
  select(item_id) -> m4_monthly_092007_ids

target_list3 <- as.character(m4_monthly_092007_ids[,])

length(target_list3)
head(target_list3)
class(target_list3)
class(df[[1]]$st)

m4_monthly_end092007 <- Filter(function(df) df$st %in% target_list3, df)
as.character(m4_monthly_end092007[[1]]$period)
as.character(m4_monthly_end092007[[1]]$type)
length(m4_monthly_end092007)


##################################
##################################
# Export data to JSON

length(m4_monthly_end052015) # df1
length(m4_monthly_end032014) # df2
length(m4_monthly_end092007) # df3

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

###################################
###################################

# train file
json1 <- map(1:length(m4_monthly_end092007), ts_to_json, m4_monthly_end092007, test_data=FALSE, true_dates=FALSE, domain_cat=FALSE)

sink("data/json/m4_monthly_end092007_train.json")
json1 <- lapply(json1, cat)
sink()

# test file
json2 <- map(1:length(m4_monthly_end092007), ts_to_json, m4_monthly_end092007, test_data=TRUE, true_dates=FALSE, domain_cat=FALSE)

sink("data/json/m4_monthly_end092007_test.json")
json2 <- lapply(json2, cat)
sink()






