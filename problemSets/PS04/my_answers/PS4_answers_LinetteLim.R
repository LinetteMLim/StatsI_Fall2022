rm(list=ls())
#install.packages("tidyverse")
library("tidyverse")
#install.packages("car")
library("car")
data(Prestige)
help(Prestige)

summary(Prestige$type)

# Q1a. Create a new variable professional by recoding the variable type so that 
# professionals are coded as 1, and blue and white collar workers are coded as 0 (Hint: ifelse).
Prestige$prof <- ifelse(Prestige$type == "prof", 1, 0)

table(type = Prestige$type,
     Professional = Prestige$prof)

# Q1b. Run a linear model with prestige as an outcome and income, professional, 
# and the interaction of the two as predictors (Note: this is a continuous × 
# dummy interaction.)

mod <- lm(prestige ~ income + prof + income:prof, 
          data = Prestige)
summary(mod)

# Q1c. Write the prediction equation based on the result.

# Prestige= beta0 +beta1*income +beta2*prof + beta3 *income*prof
#         = 21.14 + 0.003*income + 37.78*prof - 0.002*income*prof

# Prestige(prof)= 21.14 + 0.003*income + 37.78*1 - 0.002*income*1
#               = 58.92 + 0.001*income

# Prestige(non-prof)= 21.14 + 0.003*income + 37.78*0 - 0.002*income*0
#                   = 21.14 + 0.003*income

# Subbing in random values, it appears that if incomes are high (e.g. 100,000), 
# the prestige score is higher for non-professionals (blue collared and white collared)
# than for professionals. However, at low levels of income (1000), the prestige score is 
# higher for professionals than for non-professionals.

# Q1d. Interpret the coefficient for income.
# For every 1000 units (dollars) increase in income, prestige increases by 3.1709 units.

# Q1e. Interpret the coefficient for professional.
# The coefficient for professional is 37.78. A positive regression coefficient means 
# that prestige is higher for the dummy variable 'professional' than for the reference group (bc and wc).
# The regression coefficient is statistically significant, which means the prestige discrepancy with
# the reference group is also statistically significant.

# Q1f. What is the effect of a $1,000 increase in income on prestige score for 
# professional occupations? In other words, we are interested in the marginal effect 
# of income when the variable professional takes the value of 1. Calculate the 
# change in yˆ associated with a $1,000 increase in income based on your answer for (c).

# Let us plug in income values of 2000 and 1000 to the prediction equation in 1c.
# Prestige(professionals)_2000 = 58.92 + 0.001*2000 = 60.92
# Prestige(professionals)_1000 = 58.92 + 0.001*1000 = 59.92
# Prestige(professionals)_2000 - Prestige(professionals)_1000 = 60.92-59.92 = 1
# For professional occupations, a $1,000 increase in income increases prestige score by 1 unit.

# Q1g. What is the effect of changing one’s occupations from non-professional to 
# professional when her income is $6,000? We are interested in the marginal effect 
# of professional jobs when the variable income takes the value of 6,000. Calculate 
# the change in yˆ based on your answer for (c).

# Let us plug in income value of 6000 to the prediction equation in 1c.

# Prestige(professionals)     = 58.92 + 0.001*6000 = 64.92
# Prestige(non-professionals) = 21.14 + 0.003*6000 = 39.14
# Prestige(professionals) - Prestige(non-professionals) = 64.92 - 39.14 = 25.78

# Holding income constant at $6000, a non-professional switching to a professional 
# occupation will gain 25.78 units in prestige score.

#2a. Use the results from a linear regression to determine whether having these 
# yard signs in a precinct affects vote share (e.g., conduct a hypothesis test with alpha = .05).
# H0: beta.yard = 0
# Ha: beta.yard /= 0
# We will do a t.test, so we need coefficient estimate, test statistic, SE, and p-value
T.test.yard <- 0.042/0.016
n <- 131
k <- 2
p.values.yard <- 2*pt(abs(T.test.yard) , n-k, lower.tail = F)
p.values.yard
# The p value, at 0.0097, is smaller than α = .05, so we reject the null hypothesis 
# that there is no discernible linear relationship between yard signs and Cuccinelli's vote share.

#2b. Use the results to determine whether being next to precincts with these yard 
# signs affects vote share (e.g., conduct a hypothesis test with α = .05).
# H0: beta.adjacent = 0
# Ha: beta.adjacent /= 0
# We will do a t.test, so we need coefficient estimate, test statistic, SE, and p-value
T.test.adjacent <- 0.042/0.013
p.values.adjacent <- 2*pt(abs(T.test.adjacent) , n-k, lower.tail = F)
p.values.adjacent
# The p value, at 0.0015, is smaller than α = .05, so we reject the null hypothesis 
# that there is no discernible linear relationship between adjacent precincts with yard signs and Cuccinelli's vote share.

#2c. Interpret the coefficient for the constant term substantively.
# It indicates that if all the explanatory variables in the model (precincts with yard signs, precincts adjacent to yard signs)
# are zero, then the value of the dependent variable will be equal to the constant term. In other words, in the absence
# of the signs treatment, the proportion of the vote share for Cuccinelli already stands at 0.302. 

#2d. Evaluate the model fit for this regression. What does this tell us about the 
# importance of yard signs versus other factors that are not modeled?

# The R squared for the model is 0.094. Generally, the closer the R squared is to 1,
# the better the model fit. The lower the R squared value, the smaller the proportion of the variance
# for a dependent variable (in this case, Cuccinelli's vote share) that can be explained by the independent variables
# (yard signs and precincts adjacent to yard signs) in the a regression model. This suggests that the model 
# might benefit from the inclusion of other factors, for example, party identity and campaign spending. 

# However, we have to be careful not to overinterpret what a low R squared value means for a model's predictive power. 
# This is because as more variables are added to the model, R squared cannot decrease. We can perform other checks, such as 
# by plotting studentized residuals, to ascertain if the low R squared value could be due to outliers or non-linearity.
