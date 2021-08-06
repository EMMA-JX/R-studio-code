library(UsingR)
library(sampling)
library(dplyr)
setwd("/Users/estherji") 

#Part 1
degree <- read.csv("Bachelor_Degree_Majors.csv", header=TRUE)
head(degree)
#Validation
any(is.na(degree))



#a subset that male older than 25 
m.over25 <- subset(degree, Sex == 'Male' & Age.Group == '25 and older')
#numeric of male older than 25 with degree
num.m25 <- as.numeric(gsub(",", "", m.over25$Bachelor.s.Degree.Holders))
#sum of male over 25 with degree with all state
a <- sum(num.m25)
a

#a subset that female older than 25 
fm.over25 <- subset(degree, Sex == 'Female' & Age.Group == '25 and older')
#numeric of female older than 25 with degree
num.fm25 <- as.numeric(gsub(",", "", fm.over25$Bachelor.s.Degree.Holders))
#sum of female over 25 with degree with all state
b <- sum(num.fm25)
b
part.a <- c(b,a)
## Plot pie chart 

slice.labels <- list("Female with Bachelor Degree", "Male with Bachelor Degree")
slice.labels
slice.percents <- round(part.a/sum(part.a)*100)
slice.labels <- paste(slice.labels, slice.percents)
slice.labels <- paste(slice.labels, "%", sep="")
slice.labels
pie(part.a, labels = slice.labels, col=hcl(c(0, 100, 200)))

######
par(mfrow = c(2,2))
#pie with science major by Sex
#numeric of male older than 25 with science degree
num.m25 <- as.numeric(gsub(",", "", m.over25$Science.and.Engineering))
#numeric of female older than 25 with Science degree
num.fm25 <- as.numeric(gsub(",", "", fm.over25$Science.and.Engineering))
a <- sum(num.m25)
b <- sum(num.fm25)
a
b
part.a <- c(b,a)
## Plot pie chart 

slice.labels <- list("Female with Science and Engineering Major", "Male with Science and Engineering Major")
slice.labels
slice.percents <- round(part.a/sum(part.a)*100)
slice.labels <- paste(slice.labels, slice.percents)
slice.labels <- paste(slice.labels, "%", sep="")
slice.labels
pie(part.a, labels = slice.labels, col=hcl(c(0, 100, 200)))


#pie with business major by Sex
#numeric of male older than 25 with business degree
num.m25 <- as.numeric(gsub(",", "", m.over25$Business))
#numeric of female older than 25 with business degree
num.fm25 <- as.numeric(gsub(",", "", fm.over25$Business))
a <- sum(num.m25)
b <- sum(num.fm25)
a
b
part.a <- c(b,a)
## Plot pie chart 

slice.labels <- list("Female with Business Major", "Male with Business Major")
slice.labels
slice.percents <- round(part.a/sum(part.a)*100)
slice.labels <- paste(slice.labels, slice.percents)
slice.labels <- paste(slice.labels, "%", sep="")
slice.labels
pie(part.a, labels = slice.labels, col=hcl(c(0, 100, 200)))


#
#pie with education major by Sex
#numeric of male older than 25 with education degree
num.m25 <- as.numeric(gsub(",", "", m.over25$Education))
#numeric of female older than 25 with education degree
num.fm25 <- as.numeric(gsub(",", "", fm.over25$Education))
a <- sum(num.m25)
b <- sum(num.fm25)
a
b
part.a <- c(b,a)
## Plot pie chart 

slice.labels <- list("Female with education Major", "Male with education Major")
slice.labels
slice.percents <- round(part.a/sum(part.a)*100)
slice.labels <- paste(slice.labels, slice.percents)
slice.labels <- paste(slice.labels, "%", sep="")
slice.labels
pie(part.a, labels = slice.labels, col=hcl(c(0, 100, 200)))



#pie with Arts, Humanities and Others major by Sex
#numeric of male older than 25 with Arts, Humanities and Others degree
num.m25 <- as.numeric(gsub(",", "", m.over25$Arts..Humanities.and.Others))
#numeric of female older than 25 with Arts, Humanities and Others degree
num.fm25 <- as.numeric(gsub(",", "", fm.over25$Arts..Humanities.and.Others))
a <- sum(num.m25)
b <- sum(num.fm25)
a
b
part.a <- c(b,a)
## Plot pie chart 

slice.labels <- list("Female with Arts, Humanities and Others Major", "Male with Arts, Humanities and Others Major")
slice.labels
slice.percents <- round(part.a/sum(part.a)*100)
slice.labels <- paste(slice.labels, slice.percents)
slice.labels <- paste(slice.labels, "%", sep="")
slice.labels
pie(part.a, labels = slice.labels, col=hcl(c(0, 100, 200)))


#	Mean，mini, max of the number of people above 25 with a bachelor's degree, within all states.


over25 <- subset(degree, Age.Group == '25 and older' & Sex == 'Total')

num.over25 <- as.numeric(gsub(",", "", over25$Bachelor.s.Degree.Holders))

num.over25
summary.over25 <- summary(num.over25)

boxplot(num.over25, col=hcl(0), xaxt = "n",
        xlab = "number of people with a bachelor's degree", horizontal = TRUE)
axis(side = 1, at = fivenum(num.over25), labels = TRUE, las=2)

#3.	In mosaicplot Compare New York State and California, choose Science and Engineering major and Business major as age above 25. 
par(mfrow = c(1,1))
over25.c <- subset(degree, Age.Group == '25 and older' & Sex == 'Total' & State == 'California')
over25.c
a <-table(over25.c$Science.and.Engineering, over25.c$Business)
a

over25.n <- subset(degree, Age.Group == '25 and older' & Sex == 'Total' & State == 'New York')
over25.n
b <-table(over25.n$Science.and.Engineering, over25.n$Business)
b

c.s <- as.numeric(gsub(",", "", over25.c$Science.and.Engineering))
c.s
c.b <- as.numeric(gsub(",", "", over25.c$Business))
c.b
n.s <- as.numeric(gsub(",", "", over25.n$Science.and.Engineering))
n.b <- as.numeric(gsub(",", "", over25.n$Business))


x <- matrix(c(c.s,c.b,n.s,n.b), nrow=2,byrow = TRUE)
x



rownames(x) <- c("California", "New York")
colnames(x) <- c("Science and Engineering major", "bussiness major")
x


mosaicplot(x, color=c("red", "blue"))



#
#Draw 1000 samples of this data with size 15. 
par(mfrow = c(2,2))
samples <- 1000
sample.size <- 15

xbar <- numeric(samples)
for (i in 1:samples){
  xbar[i] <- mean(sample(num.m25, size = sample.size, replace = TRUE))
}

hist(xbar, prob = T, main = paste("Sample Size =", sample.size),
     xlab="Mean =  683719  SD =  203212 ")
cat("Sample Size = ", sample.size, " Mean = ", mean(xbar),
    " SD = ", sd(xbar), "\n")

#####'
######Draw 2000 samples of this data with size 60. 
samples <- 2000
sample.size <- 60

xbar <- numeric(samples)
for (i in 1:samples){
  xbar[i] <- mean(sample(num.m25, size = sample.size, replace = TRUE))
}

hist(xbar, prob = T, main = paste("Sample Size =", sample.size),
     xlab="Mean =  688721  SD =  104871 ")
cat("Sample Size = ", sample.size, " Mean = ", mean(xbar),
    " SD = ", sd(xbar), "\n")

######Draw 10000 samples of this data with size 100. 
samples <- 10000
sample.size <- 100

xbar <- numeric(samples)
for (i in 1:samples){
  xbar[i] <- mean(sample(num.m25, size = sample.size, replace = TRUE))
}

hist(xbar, prob = T, main = paste("Sample Size =", sample.size),
     xlab="Mean =  688174  SD =  80746 ")
cat("Sample Size = ", sample.size, " Mean = ", mean(xbar),
    " SD = ", sd(xbar), "\n")

######
######Draw 10000 samples of this data with size 200. 
samples <- 10000
sample.size <- 200

xbar <- numeric(samples)
for (i in 1:samples){
  xbar[i] <- mean(sample(num.m25, size = sample.size, replace = TRUE))
}

hist(xbar, prob = T, main = paste("Sample Size =", sample.size),
     xlab="Mean =  687955  SD =  57430")
cat("Sample Size = ", sample.size, " Mean = ", mean(xbar),
    " SD = ", sd(xbar), "\n")


#####



mean(num.m25)
sd(num.m25)

#Compare their means sd 
#The means of the population and the means of sample mean distributions are close. 
#As the number of sample size increases, the standard deviation of the 
#sample mean distribution decreases

#Show how various sampling methods (using at least 3 sampling methods) can be applied on your data. What are your conclusions if these samples are used instead of the whole dataset.  

  
head(degree)
N <- nrow(degree)
N
n <- 50


#a) srswr, show frequencies for sex
s <- srswr(n, N)
tmp <- degree[s != 0,]
sample.1 <- tmp[rep(1:nrow(tmp), s[s!=0]), ]
sample.1

table(sample.1$Sex)
table(sample.1$Bachelor.s.Degree.Holders)


#b) systematic sampling. show frequencies for sex
k <- ceiling (N/n)
r <- sample(k,1)
s <- seq(r, by = k, length = n)
sample.2 <- degree[s,]
head(sample.2)

table(sample.2$Sex)

#------
#c) Stratified sampling based on sex
#order data  
data <- degree[order(degree$Sex),]
freq <- table(data$Sex)
freq

#the sample sizes are not integers
st.sizes <- floor(n * freq / sum(freq))
sum(st.sizes)

st.1 <- strata(data, stratanames = c("Sex"),
               size = st.sizes, method ="srswor",
               description = T)

#need to use getdata
sample.3 <- getdata(data, st.1)

#Comparison of the results of the three methods
table(sample.1$Sex)
table(sample.2$Sex)
table(sample.3$Sex)


#Because of the method of systematic and Stratified, the proportion of the selected sexes is very close to the original data, therefore, the conclusion should be very similar to the conclusion with all the data

#Implementation of additional feature(s) not mentioned above (20 points)

t.science <- sum(as.numeric(gsub(",", "", over25$Science.and.Engineering)))
t.bussiness <- sum(as.numeric(gsub(",", "", over25$Business)))
t.edu <- sum(as.numeric(gsub(",", "", over25$Education)))
t.art <- sum(as.numeric(gsub(",", "", over25$Arts..Humanities.and.Others)))
x <- cbind(t.science, t.bussiness, t.edu, t.art)

par(mfrow = c(1,1))
barplot(t(x), beside=TRUE, legend.text=TRUE, 
        col=c("lightyellow","cadetblue1","pink", "gray"),
        main = "Major choices",
        args.legend = list(x = "topright"))

#As you can see, the most students study science, followed by art and business, but very few choose education

prop.table(x)

