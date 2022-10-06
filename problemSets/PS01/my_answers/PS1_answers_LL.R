rm(list=ls())
#install.packages("tidyverse")
library("tidyverse")
setwd("/Users/linettelim/Documents/GitHub/StatsI_Fall2022/problemSets/PS01/template/")
y <- c(105, 69, 86, 100, 82, 111, 104, 110, 87, 108, 87, 90, 94, 113, 112, 98, 80, 97, 95, 111, 114, 89, 95, 126, 98)
summary(y)
sd <- sd(y)
sd
y.bar <- mean(y)
y.bar
se <- sd(y)/sqrt(length(y)) 
se
# Check distribution
qqnorm(y)
qqline(y,
       distribution = qnorm)

# Question 1a. Calculate 90 percent confidence intervals

CI_lower <- qnorm(0.05, 
                  mean = mean(y), 
                  sd = (sd(y)/sqrt(length(y)))
)

CI_upper <- qnorm(0.95,
                  mean = mean(y),
                  sd = (sd(y)/sqrt(length(y)))
)

matrix(c(CI_lower, CI_upper), ncol = 2,
       dimnames = list("",c("Lower", "Upper")))

# A way to check the working
t.test(y, conf.level = 0.9, alternative = "two.sided")

# With 90 percent confidence, the average student IQ in the school is between 
# 94 and 103.

# Question 1b. We are told the average IQ score among all the schools in the 
# country is 100.

# let mu = 100
# H0: y.bar <= mu
# Ha: y.bar > mu

upper <- pnorm(y.bar, mean = 100, sd = se, lower.tail = FALSE)
upper

#Check answer using z score

z.score <- (y.bar - 100) / se
pnorm(z.score, lower.tail = FALSE)

# Check answer using t test 

t.test(y, mu = 100, 
       data = data,
       var.equal = FALSE, 
       alternative = "greater", 
       conf.level = .95)

# The p-value is 0.72. This is larger than the significance level alpha = 0.05,
# so we cannot reject the null hypothesis. There is not enough statistical
# evidence to support Ha, that is, that the average IQ in the school is higher than 
# the average IQ score (100) among all the schools in the country.

# Question 2.1 

expenditure <- read.table("https://raw.githubusercontent.com/ASDS-TCD/StatsI_Fall2022/main/datasets/expenditure.txt", header=T)

# create scatterplot of Y and X1 
pdf("plot1_Y_X1.pdf")
plot(expenditure$X1, expenditure$Y)
dev.off()
# Fig 1 shows a weak, positive, linear relationship between Y, per capita 
# the expenditure on shelters/housing assistance in state, and X1, the per capita
# personal income in state. There are a number of outliers in the data that need to be investigated.

# create scatterplot of Y and X2 
pdf("plot_Y_X2.pdf")
plot(expenditure$X2, expenditure$Y)
dev.off()
# Fig 2 shows a weak, positive, linear relationship between Y, per capita 
# expenditure on shelters/housing assistance in state, and X2, the number 
# of residents per 100,000 that are ”financially insecure” in state. 
# There are a number of outliers in the data that need to be investigated.

# create scatterplot of Y and X3 
pdf("plot_Y_X3.pdf")
plot(expenditure$X3, expenditure$Y)
dev.off()
# Fig 3 shows a moderately strong, positive, linear relationship between Y, per capita 
# expenditure on shelters/housing assistance in state, and X3, the number 
# of people per thousand residing in urban areas in state. This suggests that
# the greater the number of urban residents in state, the greater the 
# expenditure on shelters/housing assistance in state. There are a few outliers
# in the data. 

# create scatterplot of X1 and Y 
pdf("plot_X1_Y.pdf")
plot(expenditure$Y, expenditure$X1)
dev.off()
# Fig 4 shows a weak, positive, linear relationship between Y, per capita 
# expenditure on shelters/housing assistance in state, and X1, the per capita
# personal income in state. There are a number of outliers in the data that 
# needs to be investigated.

# create scatterplot of X1 and X2 
pdf("plot_X1_X2.pdf")
plot(expenditure$X2, expenditure$X1)
dev.off()
# Fig 5. The points on the scatter plot seem to be scattered randomly, suggesting
# there is no relationship or no correlation between X2, the number of residents per 100,000 that are 
# ”financially insecure” in state, and X1, the per capita personal 
# income in state.

# create scatterplot of X1 and X3 
pdf("plot_X1_X3.pdf")
plot(expenditure$X3, expenditure$X1)
dev.off()
# Fig 6 shows a moderately strong, positive, linear association between X3, the 
# number of people per thousand residing in urban areas in state, and X1, the 
# per capita personal income in state. This suggests that the greater the number
# of people living in urban areas in state, the greater the per capita personal 
# income in state. There are a few outliers that need to be investigated.

# create scatterplot of X2 and Y 
pdf("plot_X2_Y.pdf")
plot(expenditure$Y, expenditure$X2)
dev.off()
# Fig 7 shows a weak, positive, linear relationship between Y, per capita 
# expenditure on shelters/housing assistance in state, and X2, the number 
# of residents per 100,000 that are ”financially insecure” in state. 
# There are a number of outliers in the data that need to be investigated.

# create scatterplot of X2 and X1
pdf("plot_X2_X1.pdf")
plot(expenditure$X1, expenditure$X2)
dev.off()
# Fig 8. The points on the scatter plot seem to be scattered randomly, suggesting
# there is no relationship or no correlation between X1, the per capita personal 
# income in state, and X2, the number of residents per 100,000 that are 
# ”financially insecure” in state.

# create scatterplot of X2 and X3
pdf("plot_X2_X3.pdf")
plot(expenditure$X3, expenditure$X2)
dev.off()
# Fig 9. The points on the scatter plot seem to be scattered randomly, suggesting
# there is no relationship or no correlation between X3, the number of people
# per thousand residing in urban areas in state, and X2, the number of residents
# per 100,000 that are ”financially insecure” in state.

# create scatterplot of X3 and Y 
pdf("plot_X3_Y.pdf")
plot(expenditure$Y, expenditure$X3)
dev.off()
# Fig 10 shows a weak, positive, linear relationship between Y, per capita 
# expenditure on shelters/housing assistance in state, and X3, the number 
# of people per thousand residing in urban areas in state. There are a number 
# of outliers in the data. 

# create scatterplot of X3 and X1 
pdf("plot_X3_X1.pdf")
plot(expenditure$X1, expenditure$X3)
dev.off()
# Fig 11 shows a moderately strong, positive, linear association between X1,
# the per capita personal income in state, and X3, the number of people per thousand
# residing in urban areas in state. This suggests that the greater the per capita personal 
# income in state, the greater the number of people living in urban areas in state.
# There are a number of outliers in the data that need to be investigated.

# create scatterplot of X3 and X2 
pdf("plot_X3_X2.pdf")
plot(expenditure$X2, expenditure$X3)
dev.off()
# Fig 12. The points on the scatter plot seem to be scattered randomly, suggesting
# there is no relationship or no correlation between X2, Number of residents per 
# 100,000 that are ”financially insecure” in state, and X3, the number of people
# per thousand residing in urban areas in state.

# We plot 12 graphs to detect an association between the variables, letting Y, X1
# X2, and X3 take turns to be the response variable.

# Question 2.2

# We begin by wrangling the data.

Region_1 <- expenditure[expenditure$Region == "1",]
Region_2 <- expenditure[expenditure$Region == "2",]
Region_3 <- expenditure[expenditure$Region == "3",]
Region_4 <- expenditure[expenditure$Region == "4",]

Region_1_Expend <- mean(Region_1$Y)
Region_2_Expend <- mean(Region_2$Y)
Region_3_Expend <- mean(Region_3$Y)
Region_4_Expend <- mean(Region_4$Y)

Region_1_Expend
Region_2_Expend
Region_3_Expend
Region_4_Expend

# Based on r's calculations, we see that on average Region 4 has the highest per
# capita expenditure on housing assistance, at 88.3 (assumed to be) USD.

# We will use a boxplot to check the minimum and maximum points, as well as the 
# median, and the first and third quartile for the four regions.

library(ggplot2)
expenditure %>%
  filter(Region %in% c("1", "2", "3", "4")) %>%
  group_by(Region) %>%
  ggplot(aes(factor(Region), Y)) +
  geom_boxplot()

# The boxplot for Region 4, or the West, is observably higher than that of the 
# other three regions, suggesting that it has on average, the highest per capita 
# expenditure on housing assistance. Looking at the boxplots, 75% of the states in the West 
# have a per capita expenditure on housing assistance of between 79 (assumed to 
# be) and 129 USD. This is compared to between 68 and 120 for Region 1 
# (Northeast), between 60 and 98 for Region 3 (South), and between 79 and 85 for 
# Region 2 (North Central). While Region 4 has on average, the highest per capita 
# expenditure on housing assistance, it has the longest boxplot and the longest 
# whiskers, suggesting that the states within the region also have the 
# largest disparity, or distribution, in the level of housing assistance expenditure per capita. 
# Region 2 has the shortest boxplot and shortest whiskers, suggesting it has the 
# smallest disparity in the level of housing assistance expenditure per capita.
# Region 2 is also interesting as it is the only region with outliers - it has
# three outliers.

# Question 2.3

plot(expenditure$X1, expenditure$Y, col='red', pch=19, cex=1.3,
     xlab='Personal income', ylab='Expenditure on shelters/housing assistance', main='Scatterplot of Y and X1')

# The scatterplot (Fig. 2) shows a weak to medium positive, linear correlation between Y, 
# the per capita expenditure on shelters/housing assistance in state, and X1, 
# the per capita personal income in state. This suggests there is a likelihood 
# that with increasing per capita personal income in state, there is 
# increasing per capita expenditure on shelters/housing in state. There are a 
# number of outliers in the data that need to be investigated.

pdf("plot_3_variables.pdf")
ggplot(data=expenditure, mapping = aes(x = X1, y = Y)) + 
  geom_point(aes(color = factor(Region))) +
  theme_light() +
  scale_color_discrete(labels = c("Northeast", "North Central", "South", "West")) +
  ggtitle("Relationship between Expenditure on shelters and Personal income by Region") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Per capita personal income") +
  ylab("Per capita expenditure on shelters/housing")
dev.off()

# From the graph, we can observe more clearly the differences between the four regions. There
# are generally lower levels of per capita personal income and per capita 
# expenditure on shelters/housing in the South. The Northeast tends to have
# higher levels of per capita personal income, but overall, per capita expenditure on 
# shelters/housing is not particularly high, even though there are three states that 
# are outliers in terms of shelter expenditure. The North Central lies somewhat in between 
# the spread of the South and the Northeast. The points for the West look rather 
# peculiar - with the majority of the data points concentrated between 1600 and
# 2100 on the x-axis. This seems to suggest there might be some form of basic income
# or minimum wage policy in many states in the West. In the West, unlike the other
# three regions, personal income levels do not seem to have a strong correlation 
# with spending on housing/shelters.