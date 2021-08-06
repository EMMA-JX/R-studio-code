library(UsingR)
setwd("/Users/estherji") 

#Part 1
hos <- read.csv("hospital.csv", header=FALSE)
head(hos)
# 2
typeof(hos)
a <- as.numeric(as.matrix(hos))
a
table(a)

hist(a, ylim=c(0,25),breaks = seq(0,17,1), xlab = "Number of days in hospital", main="Number of days in hospital distribution")


# 3

data <- c(mean(a), median(a), sd(a), quantile(a,0.25), quantile(a,0.75), min(a), max(a) )

datas <- matrix(data, nrow = 1, ncol = 7, byrow = TRUE)

dimnames(datas) <- list(
  c("durations of hospital "),
  c("mean", "median", "standard deviation", "first quartiles", "third quartiles", "minimum", "maximum"))



write.csv(datas,"hospital.csv")

#4


#(a) What percentage of patients are in the hospital for less than 10 days


pnorm(10, mean = 5, sd = 3)

#(b) What percentage of patients are in the hospital between 3 and 10 days? 

b <- pnorm(10, mean = 5, sd = 3) - pnorm(3, mean = 5, sd = 3)
b

#r^2


a <- c(20, 30, 24, 28, 28)
b <- c(20, 32, 22, 26, 30)

lm(b-a)

yhat = a* 1.188 -4.875

ybar = mean(y)



sum((yhat - bar)**2) / sum((y - ybar)**2)


