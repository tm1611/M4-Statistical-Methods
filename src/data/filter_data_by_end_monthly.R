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
library(ggplot2)

library(xts)
library(zoo)


data(M4)
#df <- Filter(function(df) df$period=="Yearly" & df$type=="Other", M4)
df <- Filter(function(df) df$period=="Monthly", M4)
rm(M4)

#df <- give_sam(df, 1)
#df <- df[1:100]
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

my_df <- data.frame(item_id=item_id, length=length, end=end)
dim(my_df)
head(my_df)

# Function that counts the series by enddate!!!
my_df %>% 
  count(end) %>% 
  arrange(desc(n)) %>% 
  head(10) %>% 
  data.frame()

# subset 1: N=15700 - 5/2015
my_df %>% 
  filter(end > 2015.4) %>% 
  filter(end < 2015.5) %>% 
  count()

# subset 2: N=1224 - 2014
my_df %>% 
  filter(end==2014) %>% 
  count()

# subset 2b: N=1807 - 03/2014
my_df %>% 
  filter(end>2014.2) %>% 
  filter(end<2014.3) %>% 
  count()

# subset 3: 943
my_df %>% 
  filter(end > 2007.7) %>% 
  filter(end < 2007.8) %>% 
  count()
  

# filter and get top7
my_df %>% 
  count(end_year) %>% 
  filter(end_year <= 2020) %>% 
  arrange(desc(n)) 

# end_date distribution
my_df %>% 
  count(end) %>%
  filter(end <= 2020) %>% 
  filter(n > 250) %>% 
  data.frame()

dim(df_end_n)
head(df_end_n)
str(df_end_n)
summary(df_end_n)

df_end_n$end <- round(df_end_n$end,2)

ggplot(data=df_end_n, aes(x=end, y=n)) +
  geom_histogram(stat="identity")

my_df %>% 
  count(end) %>%
  filter(end <= 2020) %>% 
  ggplot(aes(end)) +
  geom_histogram()

### To do: Export data to JSONlines
### To do: Own Benchmarks on the data



