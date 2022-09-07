# Import Libraries needed for this analysis task in R---------------------------
library(dplyr)
library(tidyverse)
library(ggplot2)
library(plotly)
library(corrplot)
library(olsrr)
library(reshape2)
library(lattice)
library(moments)
library(Hmisc)
library(gridExtra)
library(easystats)


# Import data for this analysis task in R---------------------------------------
# Setting manual import to ensure notebook can be used in any external environment
# Need to load turtle_sales.csv which may not be stored in the same directory
# In an external environment.
sales <- read.csv(file.choose(), header = TRUE)

# View Dataframe
glimpse(sales)

# Create a new data frame from a subset of the sales data frame
# Remove unnecessary columns. 
sales2 <- subset (sales, select = -c(Ranking, Year, Genre, Publisher))

# View the data frame.
head(as_tibble (sales2))

# Descriptive statistics
summary(sales2)

# Gathering Initial insights from the data set----------------------------------
# Visualizing the data for insights
# Create a scatter plot to visually determine relation between variables
# Scatter plots with trendlines added to aid visualisation
plot1 <- ggplot(data=sales2,mapping=aes(x=Global_Sales, y=NA_Sales)) + 
  geom_point(color = "green", alpha = .5, size = 3) + geom_smooth(method = "lm")
plot1
plot2 <- ggplot(data=sales2,mapping=aes(x=Global_Sales, y=EU_Sales)) + 
  geom_point(color = "red", alpha = .5, size = 3) + geom_smooth(method = "lm")
plot2
plot3 <- ggplot(data=sales2,mapping=aes(x=NA_Sales, y=EU_Sales)) + 
  geom_point(color = "yellow", alpha = .5, size = 3) + geom_smooth(method = "lm")
plot3
# Scatter plots show there may be some linear correlation between the variables
# Plots also suggest presence of many outliers in the data

# View the scatter plots above altogether
grid.arrange(plot1, plot2, plot3, nrow=2, ncol=2)


# Create a correlation Matrix & visually check for Multicollinearity
# Create new DataFrame which only contains numerical values
sales3 <- subset(sales2, select = -c(Product, Platform))

# Create correlation matrix
corrplot(cor(sales3), method = 'number')
# Note some strong correlations across the board 
# Esp between NA_Sales & Global_Sales
# Makes sense given NA_Sales are likely a very large component of Global_Sales
# NA is a very large games market

# Testing for Multicollinearity with VIF values to statistically confirm
# The visual confirmations of correlation 
# Using scatterplots & Correlation Matrix
# Create a fitted Multiple Regression Model
my_model <- lm(Global_Sales ~ EU_Sales + NA_Sales, data=sales3)
summary(my_model)

# Now Test for VIF & Tolerance
ols_vif_tol(my_model)
# VIF is below 3 = benchmark for accepting that there is no Multicollinearity

# Further test for multicollinearity using Eigenvalues & Condition Index
ols_eigen_cindex(my_model)
# Condition Index outputs are low
# Even though Variance is relatively high
# Given Condition Index Numbers are well below 10
# We can rule out Multicollinearity affirmatively.

# Create Histograms to visualize distribution of data
qplot(NA_Sales, bins=20, data=sales2)
qplot(EU_Sales, bins=20, data=sales2)
qplot(Global_Sales, bins=20, data=sales2)
# Both Regions and Global Sales show very strong right skew
# This implies that mean is greater than median sales 
# Which in turn implies that most of the sales are driven by a handful of products
# Out of the total products being offered

# Create boxplots to summarise the statistical characteristics of the distributions
# Reshaping the dataframe to aggregate by product
data_long <- melt(sales2)
head(data_long)

# Create boxplots for all variables on a single chart
bwplot(value ~ variable, data_long)
# No real outliers to 'Clean'

# Determining the impact on sales per product id--------------------------------
# Aggregate the total sales per product id
# Determine the impact on sales per product_id
df <- sales2 %>% group_by(Product) %>% summarise(across(.cols = c(NA_Sales, EU_Sales, Global_Sales), list(sum = sum)))

# Explore the DataFrame.
head(as_tibble (df))
# Count the number of products
n <- nrow(df)
# Print
cat('Number of Unique Products :', n)
# 175 Unique products sold

# Calculate Total Sales per region
sum(df$NA_Sales_sum)
# GBP 885.62mio
sum(df$EU_Sales_sum)
# GBP 578.61mio
sum(df$Global_Sales_sum)
# GBP 1877.81mio
# Sum of Global_sales is higher than sums of NA_Sales & EU_Sales combined
# Indicates there are sales in other regions not present in the dataset

# Return top 25% products sold in each region and total value of such sales.
top_quart_NA <- head(sort(df$NA_Sales_sum,decreasing=TRUE),n=44)
top_quart_EU <- head(sort(df$EU_Sales_sum,decreasing=TRUE),n=44)
top_quart_Global <- head(sort(df$Global_Sales_sum,decreasing=TRUE),n=44)
# Calculate what their sales add up to
sum(top_quart_NA)
# 25% of total products sold (44 products) generate GBP 476.83mio in NA_Sales
# This is ~53% of total sales in NA

sum(top_quart_EU)
# 25% of total products sold (44 products) generate GBP 323.75mio in EU_Sales
# This is ~56% of total sales in EU

sum(top_quart_Global)
# 25% of total products sold (44 products) generate GBP 943.03mio in Global_Sales
# This is ~50% of total Global Sales.

# We can see that sales revenues are highly concentrated
# From a small number of products out of the total offered
# This confirms our finding previously taken from data distribution visualisation

# Clean and Manipulate the data for this analysis task in R---------------------
# Clean and manipulate data in R
# Check min, max and mean values of all the sales data
# Display output in DataFrame

# Use the DataFrame df previously created which aggregated sales data by product
summary(df)

# Check for normality using QQ Plots
# NA Sales
qqnorm(df$NA_Sales_sum)
qqline(df$NA_Sales_sum)
# Data points dont follow the reference line well implying we should not assume normality

# EU Sales
qqnorm(df$EU_Sales_sum)
qqline(df$EU_Sales_sum)
# Data points dont follow the reference line well implying we should not assume normality 

#Global Sales
qqnorm(df$Global_Sales_sum)
qqline(df$Global_Sales_sum)
# Data point dont follw the reference line well implying we should not assume normality 

# Perform Shapiro-Wilk test
# This will mathematically show if sales data is normally distributed
shapiro.test((df$NA_Sales_sum))
shapiro.test((df$EU_Sales_sum))
shapiro.test((df$Global_Sales_sum))
# p values from all above are very close to 0 and well below 0.05
# This implies that the distributions are NOT normally distributed

# Skewness and Kurtosis.
# NA Sales
skewness(df$NA_Sales_sum) 
kurtosis(df$NA_Sales_sum)
# Skewness > 1 implies strong skew in data
# Kurtosis > 3 implies presence of large outliers

# EU Sales
skewness(df$EU_Sales_sum)
kurtosis(df$EU_Sales_sum)
# Skewness > 1 implies strong skew in data
# Kurtosis > 3 implies presence of large outliers

#Global Sales
skewness(df$Global_Sales_sum)
kurtosis(df$Global_Sales_sum)
# Kurtosis is greater than 3 across all three sales data
# This suggests that data is not platykurtic
# As such there are likely many extreme outliers in the data
# Skewness in all 3 sales data is greater than 1
# This implies that there is significant positive skew. 
# This also confirms the visualisation of distribution earlier of sales data.

# Determine correlation on the aggregated sales data
df2 <- subset(df, select = -c(Product))
corrplot(cor(df2), method = 'number')
# Some very strong correlations again esp between NA & Global Sales as seen previously

# Exploring simple linear relationships in R------------------------------------
# In having cleaned and aggregated the data earlier,
# There appeared to be strong possible relationships between NA, EU & Global Sales
# Investigate further by creating a simple and multiple linear regression model
# We first test for the pre-conditions for suitability of linear regression

# Test for Normality using histograms
# We have seen previously that the data is normally distributed
# Confirmed visually and mathematically using Shapiro-Wilki test.
# We can see broadly the sales data is normally distributed.

# Lets also try to fit them all on one histogram to reconfirm.
df3 <- df
df3 <- data.frame(NA_Sales_sum = rnorm(1000, 250, 10), 
                 EU_Sales_sum = rnorm(1000, 260, 10), 
                 Global_Sales_sum = rnorm(1000, 270, 10))

df3 %>% pivot_longer(everything()) %>% 
  ggplot(aes(x = value, fill = name)) + 
  geom_histogram(position = "identity", alpha = 0.5)
# Confirmed that broadly these are NOT normal distributions
# Still may be suitable for linear regression
# We are also aware from earlier that the scatter plots show linearity 
# This means may be worthwhile running linear regression models

# Create a linear regression model between variables
# EU_Sales_Sum & NA_Sales_Sum
EU.NA.lm <- lm(NA_Sales_sum ~ EU_Sales_sum, data = df)
summary(EU.NA.lm)
# Not very significant R Squared at 0.3856
# Very low p-value at 2.2e-16 which indicates good model fit

# Testing the predictive power of the model
plot(EU.NA.lm$residuals)
# Residuals do plot well as cant see any pattern. 
# From these results we can say there is not a very strong but statistically significant
# Positive linear relationship between EU_Sales & NA_Sales (p-value < 0.001)
# Final Test before forecasting is to see fit
plot(df$NA_Sales_sum, df$EU_Sales_sum)
abline(coefficients(EU.NA.lm))
# Line fitting is not great.
# Try log transformation of the variables
df$log_NA_Sales_sum = log(df$NA_Sales_sum)
df$log_EU_Sales_sum = log(df$EU_Sales_sum)
df$log_Global_Sales_sum = log(df$Global_Sales_sum)
head(as_tibble(df))

# Now re-create the model with log transformed variables.
Log.EU.NA.lm <- lm(log_NA_Sales_sum ~ EU_Sales_sum, data=df)
summary(Log.EU.NA.lm)
# Summary Stats are not very promising
# Very low R-squared though not directly comparable with non log model
# Lot worse than the non-log model
# Try line fitting
plot(df$log_NA_Sales_sum, df$EU_Sales_sum)
abline(coefficients(Log.EU.NA.lm))
# Line fit doesn't improve so reject the log transformation model
# Return to simple linear regression model for the other variables

# NA_Sales_sum & Global_Sales_sum
G.NA.lm <- lm(Global_Sales_sum ~ NA_Sales_sum, data = df)
summary(G.NA.lm)
# Good R-Squared of 0.8395
# Very low p-value at 2.2e-16 which indicates statistical significance of model

# Testing the predictive power of the model
plot(G.NA.lm$residuals)
# Residuals do plot well as cant see any pattern. 
# From these results we can say there is a reasonably strong & significant
# Positive relationship between EU_Sales & NA_Sales (p-value < 0.001)

# Final Test before forecasting is to see fit
plot(df$NA_Sales_sum, df$Global_Sales_sum)
abline(coefficients(G.NA.lm))
# Line fit is much better & very strong.

# EU_Sales & Global_Sales_sum
G.EU.lm <- lm(Global_Sales_sum ~ EU_Sales_sum, data = df)
summary(G.EU.lm)
# Reasonable R-Squared of 0.7201
# Strongest relationship is between NA_Sales & Global_Sales which is expected
# Very low p-value at 2.2e-16 which indicates model is statistically significant

# Testing the predictive power of the model
plot(G.EU.lm$residuals)
# Residuals do plot well as cant see any pattern. 
# From these results we can say there is a reasonably strong & significant

# Final Test before forecasting is to see fit
plot(df$Global_Sales_sum, df$EU_Sales_sum)
abline(coefficients(G.EU.lm))
# Very bad line fit which means poor predictive power

# Exploring multiple linear relationships in R----------------------------------
# Given Simple Linear Regression Models had mixed results
# Relationships and statistical significance were mostly acceptable 
# Predictive power were mostly poor
# We should also test for multiple linear regression given just 3 variables.
# Remind myself of the data shape
head(as_tibble (df))

# Run correlation between the variables
df %>% cor()

# Remove the log values & Product columns as they are creating noise in the cor output
mlr_df <- subset(df, select = -c(log_NA_Sales_sum, log_EU_Sales_sum, 
                                 log_Global_Sales_sum, Product))

# Re-run correlation between the variables
mlr_df %>% cor()
# Neater output

# Create Multiple linear regression model
mlr_model.lm <- lm(Global_Sales_sum ~ EU_Sales_sum + NA_Sales_sum, data = mlr_df)
summary(mlr_model.lm)

# Very strong output with adjusted R Squared of 0.9664
# This means that 96.64% of changes amongst the set of variables is caused by changes
# within each other.
# This is expected given Global Sales is an output of a summation function of the
# Other variables.
# The estimated effect of EU_Sales on GLobal_Sales is 1.1992
# This means for every 1% growth of sales in EU, Global Sales should grow by 1.192%
# Estimated effect of NA_Sales on Global_sales is 1.13
# This means for every 1% growth of sales in NA, Global Sales should grow by 1.13%
# Standard errors are low so predictive fit should be good.
# Based on the standard errors, we can say the predictions are on a normal distribution basis
# For EU Sales predictions should be within 4.67% of actual observation
# For NA_Sales, predictions should be within 3.16% of actual observation

# Checking for homoscedasticity
# This will check whether the observed data meets our model assumptions
par(mfrow=c(2,2))
plot(mlr_model.lm)
# Mean of Residuals (red lines) from MLR are mostly horizontal & centered around x = 0
# This means there are no outliers or biases in the data which would make this MLR invalid
# The Normal Q-Q Plot shows real residuals from our MLR form a good fit for the most part
# So the real residuals fit well vs the theoretical residuals
# There is no homoscedasticity

# Check whether the regression residuals is normally distributed
ols_test_normality(mlr_model.lm)
# All p values are 0 which mathematically confirms that model errors are normally distributed.

# Get a global overview of model assumptions
performance::check_model(mlr_model.lm)
# All parameters inside acceptable output guidelines.

# These all confirm that the MLR model here is fit for purpose & ready to use.

# Making predictions in R-------------------------------------------------------
# Predictions of Global_Sales based on on given values for other variables
NA_Sales_sum <- c(2.26)
EU_Sales_sum <- c(0.97)
predict_Global_Sales_sum <- data.frame(NA_Sales_sum, EU_Sales_sum)
predict(mlr_model.lm, newdata = predict_Global_Sales_sum)

# Compare predictions versus observed values in R-------------------------------
# Plotting predicted values vs actual observed values from the base data
plot(predict(mlr_model.lm), mlr_df$Global_Sales_sum, xlab = 'Predicted Global_Sales_sum', ylab = 'Observed_Global_Sales_sum')
abline(a=0, b=1)
# Very strong fit between predicted values and observed values as visualised

# We can also present this into a DataFrame.
predicted_actual <- data.frame(Global_Sales_sum=mlr_df$Global_Sales_sum, Predicted_Global_Sales_sum=predict(mlr_model.lm))
head(predicted_actual)
# Bring in the other two variables into this new DataFrame.
# This will display every value of observed variable vs Predicted value of Global_Sales
Combined_Pred_Act <- merge(mlr_df, predicted_actual, by = 'Global_Sales_sum', all=TRUE)
# View this new DataFrame
head(Combined_Pred_Act)
tail(Combined_Pred_Act)

# Recommendation for Turtle Games-----------------------------------------------
# Recommend using this Model for making predictions about future sales.
# The Model can be tweaked to solve for any of the 3 variables.
# Currently it is set up to predict Global Sales against any input of NA_Sales & EU_Sales.