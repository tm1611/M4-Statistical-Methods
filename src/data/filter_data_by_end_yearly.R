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
df <- Filter(function(df) df$period=="Yearly", M4)
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
n <- rep(NA, length(df))
end <- rep(NA, length(df))

for (i in 1:length(df)){
  entry <- df[[i]]
  series <- entry$x
  item_id[i] <- df[[i]]$st
  n[i] <- df[[i]]$n
  end[i] <- time(series)[length(series)]
}

my_df <- data.frame(item_id=item_id, n=n, end_year=end)
dim(my_df)

# filter by year
#df_asc$end_year=="2009"

# to do: write function that counts the series by enddate!!!
my_df %>% 
  count(end_year) %>%
  filter(end_year <= 2020) %>% 
  ggplot(aes(end_year, n)) +
  geom_bar(stat="identity")

# filter and get top7
my_df %>% 
  count(end_year) %>% 
  filter(end_year <= 2020) %>% 
  arrange(desc(n)) %>% 
  head(20)

# get data ending 1991, 2004, 2009  
my_df %>% 
  filter(end_year == "1991") -> m4_yearly_end1991

my_df %>% 
  filter(end_year == "2004") -> m4_yearly_end2004

my_df %>% 
  filter(end_year == "2009") -> m4_yearly_end2009

dim(m4_yearly_end1991)
dim(m4_yearly_end2004)
dim(m4_yearly_end2009)


# To do: Filter by multiple values and cut the end!

