### Regression Analysis of Benchmarks 8
# Analysis of sMAPE, MASE results 

#pre
rm(list=ls())
graphics.off()

# libraries
library(ggplot2)
library(broom)

# Load data
df <- read.csv("results/M4_benchmarks8/results_benchmarks_all.csv")

# check data
dim(df)
head(df)
str(df)

# change period, type to factor
df$period <- factor(df$period,
                    levels=c(1,2,3,4,5,6),
                    labels=c("Daily", "Hourly", "Monthly",
                             "Quarterly", "Weekly", "Yearly"))


df$type <- factor(df$type, 
                  levels=c(1,2,3,4,5,6),
                  labels=c("Demographic", "Finance", "Industry",
                           "Macro", "Micro", "Other"))

### Vis ###

# histograms 
ggplot(df, aes(x=n)) + geom_histogram()
  
# boxplots
ggplot(df, aes(x=period, y=n)) +
  geom_boxplot() +
  ggtitle("Boxplots: Series length by period") +
  coord_flip() 

ggplot(df, aes(x=type, y=n)) +
  geom_boxplot() + 
  ggtitle("Boxplots: Series length by domain") +
  coord_flip()

# relations length (n)? 
idx <- sample(x = df$X, size = 5000)

ggplot(df[idx,], aes(x=n, y=sMAPE.Naive2)) +
  ggtitle("Naive2 Forecast") + ylab("sMAPE") +
  geom_point()

### Regression Analysis sMAPE ###
# make_list function 
make_list <- function(lm_object){
  list(Coef=round(summary(lm_object)$coefficients,8), 
       r.squared = summary(lm_object)$r.squared, 
       r.squared.adj = summary(lm_object)$adj.r.squared,
       res.std.error = summary(lm_object)$sigma,
       fstatistic_val = summary(lm_object)$fstatistic[1],
       fstatistic_numdf = summary(lm_object)$fstatistic[2],
       fstatistic_dendf = summary(lm_object)$fstatistic[3])
}

# Calculate individual Weighted Average
make_WA <- function(sMAPE, MASE){
  rel_sMAPE <- sMAPE/df$sMAPE.Naive2
  rel_MASE <- MASE/df$MASE.Naive2
  WA <- (rel_sMAPE + rel_MASE)/2
  return(WA)
} 

df$WA.Theta <- make_WA(df$sMAPE.Theta, df$MASE.Theta)
df$WA.Comb <- make_WA(df$sMAPE.Comb, df$MASE.Comb)

# sMAPE.Theta 
fit1 <- lm(log(sMAPE.Theta) ~ log(n) + period + type, data = df)
summary(fit1)

l1 <- make_list(fit1)
write.csv(l1, file="results/Reg_benchmarks8/sMAPE_Theta.csv")

# sMape_Comb
fit2 <- lm(log(sMAPE.Comb) ~ log(n) + period + type, data = df)
summary(fit2)

l2 <- make_list(fit2)
write.csv(l2, file="results/Reg_benchmarks8/sMAPE_Comb.csv")

### Regression of MASE ###
# MASE_Theta
fit3 <- lm(log(MASE.Theta) ~ log(n) + period + type, data = df)
summary(fit3)

l3 <- make_list(fit3)
write.csv(l3, file="results/Reg_benchmarks8/MASE_Theta.csv")

# MASE_Comb
fit4 <- lm(log(MASE.Comb) ~ log(n) + period + type, data=df)
summary(fit4)

l4 <- make_list(fit4)
write.csv(l4, file="results/Reg_benchmarks8/MASE_Comb.csv")

### Regression of Weighted average ###
fit5 <- lm(WA.Theta ~ log(n) + period + type, data=df)
summary(fit5)

fit6 <- lm(WA.Comb ~ log(n) + period + type, data=df)
summary(fit6)