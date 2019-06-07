### M4Comp_data_prep ###

# pre
rm(list=ls())
graphics.off()

#libraries
library(M4comp2018)

# load data
data(M4)

### Subsets by period ###

# Yearly data
M4_Yearly <- Filter(function(df) df$period=="Yearly", M4)
length(M4_Yearly)
saveRDS(M4_Yearly, file="data/M4_Yearly.rds")
rm(M4_Yearly)

# Quarterly data
M4_Quarterly <- Filter(function(df) df$period=="Quarterly", M4)
length(M4_Quarterly)
saveRDS(M4_Quarterly, file="data/M4_Quarterly.rds")
rm(M4_Quarterly)

# Monthly data
M4_Monthly <- Filter(function(df) df$period=="Monthly", M4)
length(M4_Monthly)
saveRDS(M4_Monthly, file="data/M4_Monthly.rds")
rm(M4_Monthly)

# Weekly data
M4_Weekly <- Filter(function(df) df$period=="Weekly", M4)
length(M4_Weekly)
saveRDS(M4_Weekly, file="data/M4_Weekly.rds")
rm(M4_Weekly)

# Daily data
M4_Daily <- Filter(function(df) df$period=="Daily", M4)
length(M4_Daily)
saveRDS(M4_Daily, file="data/M4_Daily.rds")
rm(M4_Daily)

# Hourly data
M4_Hourly <- Filter(function(df) df$period=="Hourly", M4)
length(M4_Hourly)
saveRDS(M4_Hourly, file="data/M4_Hourly.rds")
rm(M4_Hourly)





