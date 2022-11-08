rm(list=ls())
#install.packages("tidyverse")
library("tidyverse")

# Enter the data
data <- read.csv ("incumbents_subset.csv")

# Q1.1. Use lm() function to fit a regression model where the outcome variable
# is voteshare and the explanatory variable is difflog
lm(voteshare ~ difflog, data = data)
diffvote.lm <- lm(voteshare ~ difflog, data = data)
summary(diffvote.lm)

# Q1.2. Use plot() and abline () to make a scatterplot and add the regression line 
pdf("plot1_difflog_voteshare.pdf")
plot(data$difflog, data$voteshare, main 
     = "Impact of difference in campaign spending on incumbent party voteshare")
abline(diffvote.lm)
dev.off()

# Q1.3. calculate the residuals and save as separate object
diffvote.res <- resid(diffvote.lm)
# Plot the residuals to check that the assumptions of the model have been satisfied.
preds.diffvote <- predict(diffvote.lm)
segments(data$difflog, data$voteshare, data$difflog, preds.diffvote)
# Check that residuals are zero in expectation:
pdf("plot3_density of residuals.pdf")
plot(density(data$voteshare-preds.diffvote),
     main = "Density of residuals",
     ylab = "Y", xlab = "X",
     cex.axis=1.5, cex.lab=2,
     cex.main=1.5, lwd=3)
dev.off()
# We would expect the residuals to be randomly scattered without showing any 
# systematic patterns when plotted against the predictor variable:
pdf("plot4_residual against predictor.pdf")
plot(data$difflog, diffvote.res, 
     ylab = "Residuals", xlab= "Difference in Campaign Spending")
dev.off()
# Q1.4. Write the prediction equation. Using Y = b0 + b1X1 and extracting the coefficients
# from diffvote.lm in Q1.1, we have:
# Incumbent party voteshare = 0.58 + 0.04*Difference in Campaign Spending, or
# Y = 0.58 + 0.04X

# Q2.1. Use lm() function to fit a regression model where the outcome variable
# is presvote and the explanatory variable is difflog
lm(presvote ~ difflog, data = data)
diffpres.lm <- lm(presvote ~ difflog, data = data)
summary(diffpres.lm)

# Q2.2. Use plot() and abline () to make a scatterplot and add the regression line
pdf("plot2_difflog_presvote.pdf")
plot(data$difflog, data$presvote, main 
     = "Impact of difference in campaign spending on incumbent party's presidential voteshare")
abline(diffpres.lm)
dev.off()

# Q2.3. calculate the residuals and save as separate object
diffpres.res <- resid(diffpres.lm)
# Plot the residuals to check that the assumptions of the model have been satisfied.
preds.diffpres <- predict(diffpres.lm)
segments(data$difflog, data$presvote, data$difflog, preds.diffpres)
plot(density(data$presvote-preds.diffpres),
     main = "Density of residuals",
     ylab = "Y", xlab = "X",
     cex.axis=1.5, cex.lab=2,
     cex.main=1.5, lwd=3)
# We see that the residuals are zero in expectation.
# We would expect the residuals to be randomly scattered without showing any 
# systematic patterns when plotted against the predictor variable:
plot(data$difflog, diffpres.res, 
     ylab = "Residuals", xlab= "Difference in Campaign Spending")

# Q2.4. Write the prediction equation. Using Y = b0 + b1X1 and extracting the coefficients
# from diffpres.lm in Q2.1, we have:
# Incumbent party's presidential voteshare = 0.51 + 0.02*Difference in Campaign Spending, or
# Y = 0.58 + 0.04X

# Q3.1. Use lm() function to fit a regression model where the outcome variable
# is voteshare and the explanatory variable is presvote
lm(voteshare ~ presvote, data = data)
presvoteshare.lm <- lm(voteshare ~ presvote, data = data)
summary(presvoteshare.lm)

# Q3.2. Use plot() and abline () to make a scatterplot and add the regression line
pdf("plot3_presvote_voteshare.pdf")
plot(data$presvote, data$voteshare, main 
     = "Impact of incumbent party's presidential voteshare on incumbent party voteshare")
abline(presvoteshare.lm)
dev.off()

# Q3.3. Write the prediction equation. Using Y = b0 + b1X1 and extracting the coefficients
# from presvoteshare.lm in Q3.1, we have:
# Incumbent party voteshare = 0.44 + 0.39*incumbent party's presidential voteshare, or
# Y = 0.44 + 0.39X

# Q4.1. Run a regression where the outcome variable is the residuals from 
# Question 1 and the explanatory variable is the residuals from Question 2.
lm(diffvote.res ~ diffpres.res, data = data)
residuals.lm <- lm(diffvote.res ~ diffpres.res, data = data)
summary(residuals.lm)

# Q4.2. Use plot() and abline () to make a scatterplot and add the regression line
pdf("plotres_diffpres_diffvote.pdf")
plot(diffpres.res, diffvote.res, 
     ylab = "Residuals_voteshare", xlab= "Residuals_presvote")
abline(residuals.lm)
dev.off()

# Q4.3. Write the prediction equation:
# Residuals of incumbent party voteshare = -4.860e-18 + 2.569e-01*residuals of incumbent party's presidential voteshare, or
# Y = -4.860e-18 + 2.569e-01X

# Q5.1. Run a regression where the outcome variable is the incumbent's voteshare
# and the explanatory variables are difflog and presvote.
lm(voteshare ~ difflog + presvote, data = data)
reg2 <- lm(voteshare ~ difflog + presvote, data = data)
summary(reg2)

# Q5.2. Write the prediction equation: Y = b0 + b1X1 + b2X2
# Incumbent party voteshare = 0.45 + 0.04*difference in campaign spending + 
# 0.26*incumbent party's presidential voteshare, or
# Y = 0.45 + 0.04X1 + 0.26X2

# Q5.3. 