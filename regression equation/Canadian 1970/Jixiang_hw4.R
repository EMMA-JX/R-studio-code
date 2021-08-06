library(UsingR)
setwd("/Users/estherji") 

#Part 1
canada <- read.csv("Canadian1970.csv", header=TRUE)
head(canada)
#(1)
plot(canada$Education.Level..years.,canada$Prestige.Score)

cor(canada$Prestige.Score, canada$Education.Level..years.)
#0.8501769
#(2)
lg = lm(canada$Prestige.Score ~ canada$Education.Level..years.)

summary(lm(canada$Prestige.Score ~ canada$Education.Level..years.))
#residual plot

par(mfrow = c(2,2))

plot(lm(canada$Prestige.Score ~ canada$Education.Level..years.))

par(mfrow = c(1,1))
#outlier
plot(lg, 4)

#3
rg = lm(canada$Prestige.Score~canada$Education.Level..years.+canada$Income....+canada$Percent.of.Workforce.that.are.Women)

summary(rg)

#4
summary(rg)
confint (rg)


#5
#residual plot

par(mfrow = c(2,2))


plot(lm(canada$Prestige.Score ~ canada$Education.Level..years.))

par(mfrow = c(1,1))

