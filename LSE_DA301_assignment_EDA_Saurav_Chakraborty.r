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
library(data.table)
library(treemap)


# Import data for this analysis task in R---------------------------------------
# Setting manual import to ensure notebook can be used in any external environment
# Need to load turtle_sales.csv which may not be stored in the same directory
# In an external environment.
sales <- read.csv(file.choose(), header = TRUE)

# View Dataframe
glimpse(sales)

# Create a new data frame from a subset of the sales data frame
# Remove unnecessary columns. 
sales <- subset(sales, select = -c(Ranking, Year, Genre, Publisher))

# View the data frame.
head(as_tibble (sales))

# Descriptive statistics
summary(sales)
# Confirms we have sufficient data to run standard statistical tests such as Shapiro Wilks

# Gathering Initial insights from the data set----------------------------------
# Visualizing the data for insights
# Create a scatter plot to visually determine relation between variables
# Scatter plots with trendlines added to aid visualisation
plot1 <- ggplot(data=sales,mapping=aes(x=Global_Sales, y=NA_Sales)) + 
  geom_point(color = "green", alpha = .5, size = 3) + geom_smooth(method = "lm")
plot1
plot2 <- ggplot(data=sales,mapping=aes(x=Global_Sales, y=EU_Sales)) + 
  geom_point(color = "red", alpha = .5, size = 3) + geom_smooth(method = "lm")
plot2
plot3 <- ggplot(data=sales,mapping=aes(x=NA_Sales, y=EU_Sales)) + 
  geom_point(color = "yellow", alpha = .5, size = 3) + geom_smooth(method = "lm")
plot3
# Scatter plots show there may be some linear correlation between the variables
# Plots also suggest presence of many outliers in the data

# View the scatter plots above altogether
grid.arrange(plot1, plot2, plot3, nrow=2, ncol=2)


# Create a correlation Matrix & visually check for Multicollinearity
# Create new DataFrame which only contains numerical values
sales2 <- subset(sales, select = -c(Product, Platform))

# Create correlation matrix
corrplot(cor(sales2), method = 'number')
# Note some strong correlations across the board 
# Esp between NA_Sales & Global_Sales
# Makes sense given NA_Sales are likely a very large component of Global_Sales
# NA is a very large games market

# Testing for Multicollinearity with VIF values to statistically confirm
# The visual confirmations of correlation 
# Using scatterplots & Correlation Matrix
# Create a fitted Multiple Regression Model
my_model <- lm(Global_Sales ~ EU_Sales + NA_Sales, data=sales2)
summary(my_model)
# Shows very strong results in Adj R Squred but problems with p values
# Could generate large errors in the predictions. Will revist.

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

# Create boxplots for all numeric variables on a single chart
boxplot(sales2, main="Looking for outliers in Sales Data")
# Lots of outliers identified
# No real outliers to 'Clean'
# "Outliers" represent the skew which we will explore below

# Determining the impact on sales per product id--------------------------------
# Aggregate the total sales per product id
# Determine the impact on sales per product_id
df <- sales %>% group_by(Product) %>% summarise(across(.cols = c(NA_Sales, EU_Sales, Global_Sales), list(sum = sum)))

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
# Visualise this concentration further in a treemap
df$label <- paste(df$Global_Sales_sum, df$Product, sep = "\n")
tm <- treemap(df,index=c("label"),vSize="Global_Sales_sum",vColor="Global_Sales_sum",type="index",palette = "Greens",title="Global Sales by Product",fontsize.title=14)


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


# Exploring simple linear relationships in R------------------------------------
# In having cleaned and aggregated the data earlier,
# There appeared to be strong possible relationships between NA, EU & Global Sales
# Investigate further by creating a simple and multiple linear regression model
# We first test for the pre-conditions for suitability of linear regression


# We have seen previously that the sales data are NOT normally distributed
# Confirmed visually and mathematically using Shapiro-Wilki test.
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
# Line fit worsens so reject the log transformation model
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

# Remove the log values & Product columns as they are creating noise in the cor output
mlr_df <- subset(df, select = -c(log_NA_Sales_sum, log_EU_Sales_sum, 
                                 log_Global_Sales_sum, Product))


# Create Multiple linear regression model
mlr_model.lm <- lm(Global_Sales_sum ~ EU_Sales_sum + NA_Sales_sum, data = mlr_df)
summary(mlr_model.lm)

# Very strong output with adjusted R Squared of 0.9668
# This means that 96.68% of changes amongst the set of variables is caused by changes within each other
# This is expected given Global Sales is an output of a summation function of the other variables
# The estimated effect of EU_Sales on Global_Sales is 1.19992
# This means for every 1% growth of sales in EU, Global Sales should grow by 1.19992%
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
NA_Sales_sum <- c(34.02)
EU_Sales_sum <- c(23.80)
predict_Global_Sales_sum <- data.frame(NA_Sales_sum, EU_Sales_sum)
predict(mlr_model.lm, newdata = predict_Global_Sales_sum)


# Compare all predictions versus observed values in R---------------------------
# Plotting all predicted values vs actual observed values from the base data
plot(predict(mlr_model.lm), mlr_df$Global_Sales_sum, xlab = 'Predicted Global_Sales_sum', ylab = 'Observed_Global_Sales_sum')
abline(a=0, b=1)
# Very strong fit between predicted values and observed values as visualised

# We can also present this into a DataFrame.
predicted_actual <- data.frame(Global_Sales_sum=mlr_df$Global_Sales_sum, Predicted_Global_Sales_sum=predict(mlr_model.lm))
head(predicted_actual)
# Bring in the other two variables into this new DataFrame.
# This will display every value of observed variable vs Predicted value of Global_Sales
Combined_Pred_Act <- merge(mlr_df, predicted_actual, by = 'Global_Sales_sum', all=TRUE)
Combined_Pred_Act <- subset(Combined_Pred_Act, select = -c(label))
# View this new DataFrame with all observed and predicted values
view(Combined_Pred_Act)

# Selecting for the predicted values to extract observed values
# Take previous example where predicted value for global sales was 68.05655 
# Filter for one of the known and given variables
# Retrieve all other known variables + prediction
sample = filter(Combined_Pred_Act, NA_Sales_sum=='34.02')
View(sample)
# Trying another example laid out in the assignment
sample = filter(Combined_Pred_Act, EU_Sales_sum=='0.65')
View(sample)

# Recommendation for Turtle Games-----------------------------------------------
# Recommend using this Model for making predictions about future sales.
# The Model can be tweaked to solve for any of the 3 variables.
# Currently it is set up to predict Global Sales against any input of NA_Sales & EU_Sales.

# Extension Work----
# Worthwhile quickly checking the other independent variables which were discarded at the outset.
# Re-introduce the base data
# This is done as I avoided creating too many dataframes in the earlier work 
# This method is more memory efficient
# Choose turtle_sales.csv from your computer
sales <- read.csv(file.choose(), header = TRUE)
glimpse(sales)

# Remove unnecessary columns
sales <- subset (sales, select = -c(Ranking, Year))

# Group by Platform-----
edf <- sales %>% group_by(Platform) %>% summarise(across(.cols = c(NA_Sales, EU_Sales, Global_Sales), list(sum = sum)))
head(as_tibble(edf))

# View distribution
qplot(NA_Sales_sum, bins=20, data=edf)
qplot(EU_Sales_sum, bins=20, data=edf)
qplot(Global_Sales_sum, bins=20, data=edf)
# Not normal and looks multi-modal distributions

# Shapiro Wilks Test
shapiro.test((edf$NA_Sales_sum))
shapiro.test((edf$EU_Sales_sum))
shapiro.test((edf$Global_Sales_sum))
# P values across the board are < 0.05 
# Confirms that these are not normally distributed

# MLR for Platform
mlr_model.lm <- lm(Global_Sales_sum ~ EU_Sales_sum + NA_Sales_sum, data = edf)
summary(mlr_model.lm)
ols_test_normality(mlr_model.lm)
performance::check_model(mlr_model.lm)
# Reject this as have lot of errors / poor residual output

# Group by Genre-----
edf2 <- sales %>% group_by(Genre) %>% summarise(across(.cols = c(NA_Sales, EU_Sales, Global_Sales), list(sum = sum)))
head(as_tibble(edf2))

# View distribution
qplot(NA_Sales_sum, bins=20, data=edf2)
qplot(EU_Sales_sum, bins=20, data=edf2)
qplot(Global_Sales_sum, bins=20, data=edf2)
# Not normal distributions

# Shapiro Wilks Test
shapiro.test((edf2$NA_Sales_sum))
shapiro.test((edf2$EU_Sales_sum))
shapiro.test((edf2$Global_Sales_sum))
# Much better output but visualisation shows that these are not normal distributions

# MLR for Genre
mlr_model.lm <- lm(Global_Sales_sum ~ EU_Sales_sum + NA_Sales_sum, data = edf2)
summary(mlr_model.lm)
ols_test_normality(mlr_model.lm)
performance::check_model(mlr_model.lm)
# Good fit but iffy residuals

# Group by Publisher----
edf3 <- sales %>% group_by(Publisher) %>% summarise(across(.cols = c(NA_Sales, EU_Sales, Global_Sales), list(sum = sum)))
head(as_tibble(edf3))

# View distribution
qplot(NA_Sales_sum, bins=20, data=edf3)
qplot(EU_Sales_sum, bins=20, data=edf3)
qplot(Global_Sales_sum, bins=20, data=edf3)
# Even more skewed than product grouping
# Confirms just a handful of products/publishers generate most revenue

# Lets view top 5 Publisher share of Global Sales
top5 <- top_n(edf3, 5, Global_Sales_sum)
top5 %>% summarise(sum(Global_Sales_sum))
# Lets view top 10 Publishers share of Global Sales
top10 <- top_n(edf3, 10, Global_Sales_sum)
top10 %>% summarise(sum(Global_Sales_sum))
# Lets view top 15 Publishers share of Global Sales
top15 <- top_n(edf3, 15, Global_Sales_sum)
top15 %>% summarise(sum(Global_Sales_sum))
# Lets view Total Global Sales 
edf3 %>% summarise(sum(Global_Sales_sum))
# Lets view total Number of Publishers
nrow(edf3)
# Shows that top 5 Publishers account for GBP 1531mio of Global Sales
# Top 10 publishers account for GBP 1757mio of total Global Sales
# Top 15 publishers account for GBP 1824mio of total Global Sales
# Total Global Sales of GBP 1878mio
# Total number of Publishers is 24
# A Further action point could be to trim the offerings here

# Visualise the Analysis 
# Create a treemap to display the distribution of Global sales by Publisher
edf3$label <- paste(edf3$Global_Sales_sum, edf3$Publisher, sep = "\n")
tm <- treemap(edf3,index=c("label"),vSize="Global_Sales_sum",vColor="Global_Sales_sum",type="value",palette="Reds",title="Share of Global Sales by Publisher",fontsize.title=14)

# View the top 10 Publishers by Global Sales in the table
edf3[order(-edf3$Global_Sales_sum),]

# Shapiro Wilks Test
shapiro.test((edf3$NA_Sales_sum))
shapiro.test((edf3$EU_Sales_sum))
shapiro.test((edf3$Global_Sales_sum))

# MLR for Publisher
mlr_model.lm <- lm(Global_Sales_sum ~ EU_Sales_sum + NA_Sales_sum, data = edf3)
summary(mlr_model.lm)
ols_test_normality(mlr_model.lm)
performance::check_model(mlr_model.lm)
# Residuals output is poor
# Very high R Squared
# Strong collinearity needs to be noted
# No real world useability produced