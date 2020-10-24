# https://github.com/jaejungca/statistics
# Simple Regression

## Type of scale: Nominal, ordinal, interval, and ratio

y = c(7,3,5,6,1,2) # y= # of cup of coffee; ratio scale
x = c(1,0,1,1,0,0) # x= baby cried; nominal

summary(y)
summary(x)
plot(x,y)
abline(h = mean(y), col='blue', lty=2)
abline(v = mean(x), col='purple', lty=2)
abline(lm(y~x), col='red')

## Original model:               y = b0 + b1x + u
## Estimated model (prediction): yhat = b0hat + b1hatx
## Estimated model (prediction): yhat = 2 + 4x
## H0: b0 = 0; t = b0hat/se 
## H0: b1 = 0; t = b1hat/se
lm <- lm(y~x)
typeof(lm)
class(lm)
summary(lm)

# R-squared Illustration

library(readxl)
data_ch2 <- read_excel('data_ch2.xlsx')
plot(data_ch2$x1, data_ch2$y, xlim = c(-3,3))
plot(data_ch2$x2, data_ch2$y, xlim = c(-3,3))

summary(lm(y~x1, data = data_ch2))
summary(lm(y~x2, data = data_ch2))

# Data set: charity

library(readxl)
charity <- read_xls("charity.xls")
class(charity)
head(charity)
attach(charity)

## Q1.What is the average gift in the sample of 4268 people? 
## What percentage of people gave no gift?

mean(gift)
summary(gift)
hist(gift)
hist(gift, col = 'red', xlim = c(0,50))
sum(gift==0)/length(gift)


## Q2. Estimate the model that gift is a function of mails sent.
## = regressing gift on mailsyear

a = lm(gift~mailsyear)

 ## gift_hat = 2.014 + 2.650mailsyear
 ## predict the amount of gift when the organization sent out 5 mails
  ## gift_hat (x=5) = 2.014 + 2.650x(5) 
    gift_hat (x=5) = 2.014 + 2.650*(5) = 15.264
    gift_hat (x=0) = 2.014 + 2.650*(0) = 2.014
    
summary(gift); # scale: ratio
summary(mailsyear) # scale: ratio
plot(mailsyear,gift, ylim = c(0,50))
abline(a, col='blue')
abline(h = mean(gift), col="red", lty=2)
abline(v = mean(mailsyear), col="purple", lty=2)

## Q3. Interpret the slope coefficient. If each mailing costs one guilder (Dutch currency), is the charity expected to make a net gain on each mailing? Does this mean the charity makes a net gain on every mailing? Explain.

  ## gift_hat = 2.014 + 2.650mailsyear
  ## predict the amount of gift when the organization sent out 5 mails
  ## gift_hat (x=5) = 2.014 + 2.650x(5) 
  gift_hat (x=5) = 2.014 + 2.650*(5) = 15.264
  gift_hat (x=0) = 2.014 + 2.650*(0) = 2.014
  
  net gain = revenue - cost = 2.650 - 1 = 1.650 on average

## Q4. What is the smallest predicted charitable contribution in the sample? Using this simple regression analysis, can you ever predict zero for gift?

gift_hat = a$fitted.values
head(gift_hat)
summary(gift_hat)
hist(gift_hat)

res = a$residuals
summary(res)
sum(res)
hist(res)
error = data.frame(gift, gift_hat, res)
View(error)
  
# Simulation: Unbiased and variance

## Unbiased part

set.seed(100)
II = 10000 # number of different samples
N = 30 # sample size
beta0 = 1 # true value in population
beta1 = 2 # true value in population
beta0mat = matrix(0,II,1) # to save beta0_hat
beta1mat = matrix(0,II,1) # to save beta1_hat
for(i in 1:II){
  x = rnorm(N,0,2)
  e = rnorm(N,0,1)
  y = beta0 + beta1*x + e
  beta0mat[i]= summary(lm(y~x))$coefficients[1,1]
  beta1mat[i]= summary(lm(y~x))$coefficients[2,1]
}

mean(beta0mat)
mean(beta1mat)
