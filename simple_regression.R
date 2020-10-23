# Simple Regression
y = c(7,3,5,6,1,2) # y= # of cup of coffee
x = c(1,0,1,1,0,0) # x= baby cried


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




## Q1.What is the average gift in the sample of 4268 people? 
## What percentage of people gave no gift?


## Q2. Estimate the model that gift is a function of mails sent.


## Q3. Interpret the slope coefficient. If each mailing costs one guilder (Dutch currency), is the charity expected to make a net gain on each mailing? Does this mean the charity makes a net gain on every mailing? Explain.


## Q4. What is the smallest predicted charitable contribution in the sample? Using this simple regression analysis, can you ever predict zero for gift?


# Simulation: Unbiased and variance

## Unbiased part