rm(list=ls())
#install.packages("tidyverse")
library("tidyverse")
setwd("/Users/linettelim/Documents/GitHub/StatsI_Fall2022/problemSets/PS02/my_answers")
## Create data

A = matrix(
  
  # Taking sequence of elements
  c(14, 6, 7, 7, 7, 1),
  
  # No of rows
  nrow = 2,
  
  # No of columns
  ncol = 3,		
  
  # By default matrices are in column-wise order
  # So this parameter decides how to arrange the matrix
  byrow = TRUE		
)

# Naming rows
rownames(A) = c("Upper class", "Lower class")

# Naming columns
colnames(A) = c("Not stopped", "Bribe requested", "Stopped or warned")

cat("The 2x3 matrix:\n")
print(A)

sum(A[1, ])
sum(A[2, ])
sum(A[ ,1])
sum(A[ ,2])
sum(A[ ,3])

sum.row <- sum(A[1, ]) + sum(A[2, ])
sum.column <- sum(A[ ,1]) + sum(A[ ,2]) + sum(A[ ,3])

B = matrix(
  
  # Taking sequence of elements
  c(14, 6, 7, 27, 7, 7, 1, 15, 21, 13, 8, 42),
  
  # No of rows
  nrow = 3,
  
  # No of columns
  ncol = 4,		
  
  # By default matrices are in column-wise order
  # So this parameter decides how to arrange the matrix
  byrow = TRUE		
)

# Naming rows
rownames(B) = c("Upper class", "Lower class", "Column total")

# Naming columns
colnames(B) = c("Not stopped", "Bribe requested", "Stopped or warned", "Row total")

cat("The 3x4 matrix:\n")
print(B)


# Fe = (Row total / grand total) * column total

c1 <- (21/42)*27
c2 <- (13/42)*27
c3 <- (8/42)*27
c4 <- (21/42)*15
c5 <- (13/42)*15
c6 <- (8/42)*15

chi.square <- ((14-c1)^2/c1) + ((6-c2)^2/c2) + ((7-c3)^2/c3) + ((7-c4)^2/c4) + ((7-c5)^2/c5) + ((1-c6)^2/c6)
print(chi.square)

# To check if the calculation 'by hand' in R is correct, we run the formula:
chisq.test(A)

# 1(a) The chi square statistic is 3.79.

# df = (rows -1)(columns -1)

df <- (2-1)*(3-1)
df

p.value <- pchisq(chi.square, df = 2, lower.tail=FALSE)
print(p.value)

# To confirm if p-value is 0.15, we convert the matrix (A) into a table and run
# some code. The summary() function reaffirms the chi sq statistic as 3.79
# and the p-value as 0.15.

table <- as.table(A)
print(table)
summary(table)

# 1(b) The p-value is 0.15. This is larger than the significance level alpha = 0.1,
# so we do not have enough evidence to reject the null hypothesis, that there is
# no relationship between the two variables, socioeconomic class and treatment by
# police officers. 

# To access the standardized residuals, you can assign the output of your chisq.test() 
# to an object, and print out using your_object$stdres 

model <- chisq.test(table)
model$stdres

# 1(c) It is also possible to calculate by hand in r. First, we calculate the
# row and column proportions:

c1.rowprop <- 27/42 
c1.colprop <- 21/42
c2.rowprop <- 27/42
c2.colprop <- 13/42
c3.rowprop <- 27/42 
c3.colprop <- 8/42
c4.rowprop <- 15/42
c4.colprop <- 21/42
c5.rowprop <- 15/42
c5.colprop <- 13/42
c6.rowprop <- 15/42
c6.colprop <- 8/42

# Next, we can find out the standardized residuals using the following formula:

z1 <- (14-c1)/sqrt((c1*(1-c1.rowprop)*(1-c1.colprop)))
z2 <- (6-c2)/sqrt((c2*(1-c2.rowprop)*(1-c2.colprop)))
z3 <- (7-c3)/sqrt((c3*(1-c3.rowprop)*(1-c3.colprop)))
z4 <- (7-c4)/sqrt((c4*(1-c4.rowprop)*(1-c4.colprop)))
z5 <- (7-c5)/sqrt((c5*(1-c5.rowprop)*(1-c5.colprop)))
z6 <- (1-c6)/sqrt((c6*(1-c6.rowprop)*(1-c6.colprop)))

# Finally we can arrange the standardized residuals for each cell in the matrix:

C = matrix(
  
  # Taking sequence of elements
  c(z1, z2, z3, z4, z5, z6),
  
  # No of rows
  nrow = 2,
  
  # No of columns
  ncol = 3,		
  
  # By default matrices are in column-wise order
  # So this parameter decides how to arrange the matrix
  byrow = TRUE		
)

# Naming rows
rownames(C) = c("Upper class", "Lower class")

# Naming columns
colnames(C) = c("Not stopped", "Bribe requested", "Stopped or warned")

cat("The 2x3 matrix:\n")
print(C)

# We are arrived at the same standardized residuals through calculation by hand in R. 

X <- c("upper", "upper", "upper", "lower", "lower", "lower")
Y1 <- c(14, 6, 7, 7, 7, 1)
Y2 <- c("not.stopped", "bribe.requested", "stopped.warned","not.stopped", "bribe.requested", "stopped.warned")

dataframe <- data.frame(X, Y1, Y2)
names(dataframe) <- c('Class', 'Cases', 'Treatment')
print(dataframe)
pdf("plot_class_treatment.pdf")
ggplot(dataframe, aes(x=X, y=Y1, colour=Y2, shape=Y2)) + 
  geom_point()  + 
  theme_bw() + 
  theme(axis.title = element_text(size=20),
        axis.text = element_text(size=15),
        legend.title = element_text(size=17),
        legend.text = element_text(size=15)) 
dev.off()
# Both classes have a similar likelihood of being targeted for bribes. It also looks
# like (a) the upper class are more likely to be not stopped when flouting
# traffic rules. However, it seems puzzling that at the same time, (b) a member of 
# the upper class is also more likely to be stopped or warned than someone deemed lower 
# class. Since the interpretation is unclear, let us explore further. I will overwrite 
# the values for Y1 ('Cases') with the proportion of cases corresponding to class 
# instead of the absolute number of cases.

X <- c("upper", "upper", "upper", "lower", "lower", "lower")
Y1 <- c(14/42, 6/42, 7/42, 7/42, 7/42, 1/42)
sum(Y1)
Y2 <- c("not.stopped", "bribe.requested", "stopped.warned","not.stopped", "bribe.requested", "stopped.warned")

dataframe <- data.frame(X, Y1, Y2)
names(dataframe) <- c('Class', 'Cases', 'Treatment')
print(dataframe)

ggplot(dataframe, aes(x=X, y=Y1, colour=Y2, shape=Y2)) + 
  geom_point()  + 
  theme_bw() + 
  theme(axis.title = element_text(size=20),
        axis.text = element_text(size=15),
        legend.title = element_text(size=17),
        legend.text = element_text(size=15)) 

# 1(d) The plot looks similar to the plot with absolute instead of proportional values
# for Y1 ('Cases'). Since the finding was inconclusive, let us try using standardized
# residuals to interpret the results. We use the following formula:
pdf("corrplot_class_treatment.pdf")
install.packages("corrplot")
library(corrplot)
corrplot(model$stdres, is.cor = FALSE)
dev.off()
# The positive residuals are in blue, signifying a positive association between 
# the corresponding row and column variables.

# In the plot, we can see a strong positive association between being lower class
# and being asked to pay a bribe. There is also a strong positive association 
# between being upper class and being stopped or warned. This seems to suggest that
# the lower class are often stymied by 'informal' ways of doing things, possibly due to
# a combination of low social capital and lack of literacy in the law, while the upper
# class are somewhat protected by and benefit from formal institutions like the law
# and the police force.

# The negative residuals are in red. This implies a negative association between
# the corresponding row and column variables. From the plot, we see the upper class 
# is less likely (or 'not associated' with) to be asked to pay a bribe, while the 
# lower class is less likely to be stopped or warned. 

# Taken together, this suggests that there are different traffic offense resolution 
# methods for the upper class and the lower class. The lower class is let go with 
# a bribe, while the upper class is more likely to be formally warned or sanctioned.

# 2(a)

## Clear global environment, load packages, set working directory and import data.
rm(list=ls())
library(tidyverse) 
setwd("/Users/linettelim/Documents/GitHub/StatsI_Fall2022/problemSets/PS02/")
data <- read.csv ("https://raw.githubusercontent.com/kosukeimai/qss/master/PREDICTION/women.csv")

mean(data$water[data$reserved == 1]) - mean(data$water[data$reserved == 0])

# We find that the reservation policy increased the number of drinking water facilities 
# in a GP on average by about 9 (new or repaired).

# 2(a) Therefore, the null hypothesis is that there is zero treatment effect, i.e., 
# H0 : B1 = 0
# Ha: B1 =/= 0

# 2(b) Bivariate regression
lm(water ~ reserved, data = data)
fit.data <- lm(water ~ reserved, data = data)
summary(fit.data)

# Here, we see that the p-value = 0.0197. This is less than the typical threshold 
# of 5%, so we reject the null hypothesis that treatment effect (of reservation 
# policy on new or repaired drinking water facilities) is zero.

# 2(c) Coefficient estimate

confint(fit.data)

# We find that the point estimate of the slope coefficient is 9.252, the standard error
# is 3.948, and the t-statistic for the estimated slope coefficient is 2.344. The slope 
# coefficient is equal to the difference-in-means estimator earlier calculated. When the 
# explanatory variable is binary, the estimated average treatment effect equals the 
# estimated slope coefficient. The result suggests that having reservations for women is
# estimated to increase the number of drinking water facilities by 9.25 facilities with 
# a 95% confidence interval of [1.49, 17.02].
