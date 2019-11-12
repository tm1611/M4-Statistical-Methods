# Export ts to csv 
## Step towards exporting series

rm(list=ls())
graphics.off()

ts2csv <- function(x) {
  fname <- paste0("data/export/", deparse(substitute(x)), ".csv")
  readr::write_csv(tsibble::as_tsibble(x, pivot_longer = FALSE), fname)
}

library(fpp2)
ts2csv(auscafe)    # Univariate monthly data
ts2csv(visnights)  # Multivariate quarterly data
ts2csv(elecdemand) # Multivariate half-hourly data

# How to apply this to M4? 
## Check how to import data directly from M4 data?
## which format for gluonts?

my_data <- "data/M4_Yearly_Demographic.rds"
rds <- readRDS(file=my_data)

rds_sub <- rds[1:10]

# one series 
entry1 <- ts(c(rds_sub[[1]]$x, rds_sub[[1]]$xx), start(rds_sub[[1]]$x))
entry2 <- ts(c(rds_sub[[2]]$x, rds_sub[[2]]$xx), start(rds_sub[[2]]$x))
entry3 <- ts(c(rds_sub[[3]]$x, rds_sub[[3]]$xx), start(rds_sub[[3]]$x))

my_list <- list(entry1, entry2, entry3)


# 
h1 <- ts(data=cbind(1:24), start=c(2001, 1), frequency=12)
h2 <- ts(data=cbind(1:24, 25:48), start=c(2001, 1), frequency=12)
h3 <- ts(data=cbind(1:4, 5:8, 9:12), start=c(2001, 1), frequency=4)
colnames(h2) <- c("aa", "bb")
colnames(h3) <- c("cc", "dd", "ee")
h1; h2; h3


test <- list(t1 = h1, t2 = h2, t3 = h3)


