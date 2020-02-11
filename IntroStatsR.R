#' ---
#' title: "Introductory Statistics with R"
#' author: "Clay Ford, UVA Library StatLab"
#' date: "Spring 2020"
#' ---
#' 


# To submit a line of code, place your cursor in the line and hit Ctrl + Enter
# (Win/Linux) or Cmd + Enter (Mac)


# load packages -----------------------------------------------------------

library(tidyverse)
library(binom)

# Supress the numerous start up messages these two packages produce
suppressPackageStartupMessages(library(Hmisc))
suppressPackageStartupMessages(library(mosaic))


# Getting data into R -----------------------------------------------------

# Results from the US Census American Community Survey, 2012.
# https://www.census.gov/programs-surveys/acs
# From the openintro package

# Import the acs12.csv file; the result is a data frame
acs12 <- read.csv("https://github.com/clayford/IntroStatsR/raw/master/data/acs12.csv")

# str() shows us the structure of the data frame
str(acs12)

# NOTE: Factors are integers with character labels. Good to think of them as
# categorical variables. By default the read.csv() function automatically
# converts all columns containing text into factors.
#
# Can also click on the name in the Environment window to view the data.
#
# The summary function will provide summaries for all columns in a data frame.
# NA means "Not available", or missing. May not be useful for data with many
# columns.
summary(acs12)

# What am I typically looking for? 
# - Unusual values (too low, too high)
# - missing data (NA's)
# - big differences between mean and median (skewed data?)
# - consistent Factor level names ("male" vs "Male")
# - order of Factor levels
# - excess zeroes, or some other value
# - wrong data types (numbers stored as Factors, etc)
#
# Another option to get all summaries is the describe function from the Hmisc
# package
Hmisc::describe(acs12)


# We can also use summary on individual columns. Use the "$" to access columns
# in a data frame.
summary(acs12$income)
summary(acs12$employment)

# Notice that RStudio allows autocompletion of column names after you type the
# "$".


# YOUR TURN #0 ------------------------------------------------------------

# Try summary on the time_to_work and edu columns, or any other columns of
# interest.




# Counts and Proportions --------------------------------------------------

# Counts - the most fundamental statistic

# The table() function generates counts of unique values for a given vector,
# such as a column in a data frame. By default is excludes missing data.
table(acs12$employment)

# To see if there are missing data, set exclude = NULL:
table(acs12$employment, exclude = NULL)

# Can also exclude other values, for example
table(acs12$employment, exclude = "not in labor force")
table(acs12$employment, exclude = c("not in labor force", NA))

# summary() returns NAs by default (for Factors)
summary(acs12$employment)

# quick way to visualize counts, as long as the column is a Factor:
plot(acs12$employment)

# Or we can use barplot() with table(); table() will count anything that can be
# converted to a factor (integers, numeric, character).

# counts of subjects by number of hours they work each week
table(acs12$hrs_work)
barplot(table(acs12$hrs_work))


# We can also count things satisfying a condition. 
#
#  ==  EQUAL
#  !=  NOT EQUALS
#  >   GREATER THAN
#  <   LESS THAN
#  >=  GREATER THAN OR EQUAL
#  <=  LESS THAN OR EQUAL

# How many people are age 60?
sum(acs12$age == 60)

# How many people are over age 60
sum(acs12$age > 60)

# How many people commute more than 60 minutes to work?
sum(acs12$time_to_work > 60)

# In certain cases, R does not skip missing values unless told to do so.
# Set na.rm = TRUE
sum(acs12$time_to_work > 60, na.rm = TRUE)

# How many people do NOT have employment = "employed"
sum(acs12$employment != "employed", na.rm=TRUE)

# Combine conditions
#    & (AND) 
#    | (OR)
#
# How many married AND work more than 50 hours
sum(acs12$married == "yes" & acs12$hrs_work > 50, na.rm = TRUE)

# How many under age 18 OR over age 65
sum(acs12$age < 18 | acs12$age > 65)


# Proportions
#
# What proportion of people are over age 60? The mean of 0s and 1s is the
# proportion of 1s.
mean(acs12$age > 60)

# What proportion of people commute over 60 minutes to work?
mean(acs12$time_to_work > 60, na.rm = TRUE)

# Use prop.table to get proportions from tables
#
# NOTE: these are proportions of non-missing!
prop.table(table(acs12$employment))

# Notice the difference if we include the missings
prop.table(table(acs12$employment, exclude = NULL))

# or using summary
prop.table(summary(acs12$employment))

# Quick note about pipes. If the dplyr package is loaded, we can use pipes
# instead of nesting functions. For example:
table(acs12$employment) %>% prop.table()

# Use Ctrl + Shift + M or Cmd + Shift + M to enter %>% 
#
# Pipes take the output of one function and feed it to the first argument of the
# next function.

table(acs12$employment) %>% prop.table() %>% round(2)


# Confidence intervals of proportions
#
# These are just estimates. How certain are we? Another sample would yield
# slightly different results.
#
# Confidence intervals help us quantify the uncertainty. They provide an
# estimated lower and upper bound of our estimate.
#
# Example: how certain are we about 0.53 employed?
#
# The prop.test() function returns a 95% confidence interval for an estimated
# proportion.
#
# 95% Confidence Interval theory: sample the data, calculate a 95% confidence
# interval, repeat many times. About 95% of confidence intervals will contain
# the "true" value you're estimating. See Appendix for a demo.
#
# The prop.test() function requires number of "successes" (ie, number employed)
# and total number of "trials" (ie, number of respondents).

# Number of Employed: 843
sum(acs12$employment == "employed", na.rm = TRUE)

# Total number who responded (ie, total not missing): 1605
sum(table(acs12$employment))

# x = number of "successes", n = number of "trials"
prop.test(x = 843, n = 1605)

# 95 percent confidence interval: (0.50, 0.55)
#
# can save into an object
p.out <- prop.test(x = 843, n = 1605)

# access the confidence interval
p.out$conf.int

# The mosaic version of prop.test allows us to do the following:
mosaic::prop.test(acs12$employment == "employed")

# Note: placing the package name and two colons before a function tells R to use
# the function in that package.
#
# The prop.test function in the stats package that comes with R cannot do this.
# stats::prop.test(acs12$employment == "employed")
#
# The binom.confint() function from the binom package allows us to get
# confidence intervals for all three employment levels. method = "prop.test"
# returns traditional confidence intervals
binom.confint(x = table(acs12$employment), 
              n = sum(table(acs12$employment)), 
              method = "prop.test")

# The binom.confint() function provides a 11 different methods for calculating
# CIs. Why use other methods? When proportion estimate is near boundary (ie, 0
# or 1). Notice these are equal out to 3 decimal places.
binom.confint(x = 843,n = 1605)


# Sometimes we derive proportions from numeric data. Example: proportion of
# people working more than 40 hours
mean(acs12$hrs_work > 40, na.rm = TRUE)

# How certain are we about that proportion? 
# The mosaic version of prop.test allows us to do the following:
mosaic::prop.test(acs12$hrs_work > 40)

# 95 percent confidence interval: (0.21, 0.27)




# YOUR TURN #1 ------------------------------------------------------------

# (1) What proportion of ACS respondents are married? 

# (2) As a population estimate, how certain is it? Calculate a confidence
# interval





# Means and Medians -------------------------------------------------------

# The mean and median are two measures of center.

# calling summary() on a numeric variable returns the mean and median
summary(acs12$hrs_work)

# The median is the middle of the sorted data
# The mean is the "balance point" of the data
# Symmetric data have similar means and medians.


# The mean and median functions; if any data is missing the result is NA
mean(acs12$hrs_work)
median(acs12$hrs_work)

# specify na.rm = TRUE to ignore missing data
mean(acs12$hrs_work, na.rm = TRUE)
median(acs12$hrs_work, na.rm = TRUE)

# How much data is missing? The is.na() function can help us answer this.
sum(is.na(acs12$hrs_work))   # count
mean(is.na(acs12$hrs_work))  # proportion

# How much data is NOT missing? Precede is.na() with !
sum(!is.na(acs12$hrs_work))
mean(!is.na(acs12$hrs_work))

# histograms are good to visualize the distribution of numeric variables
hist(acs12$hrs_work)

# Numeric measures of spread include the standard deviation...
sd(acs12$hrs_work, na.rm = TRUE)

# ...the Interquartile Range (difference between 75th and 25th quartiles)
IQR(acs12$hrs_work, na.rm = TRUE)

# The IQR may be preferred with highly skewed data, but can always report both.

# Recall the estimated mean of hrs_work
mean(acs12$hrs_work, na.rm = TRUE)

# The mean of 37.977 is just an estimate. How certain are we about the mean?
# Another sample would yield slightly different results. The t.test() function
# returns a 95% confidence interval for a mean. 
# Notice we do not need na.rm = TRUE.
t.test(acs12$hrs_work)

# CI: (37.1, 38.8)

# We can save into an object and access the confidence interval directly
t.out <- t.test(acs12$hrs_work)
t.out$conf.int

# The smean.cl.normal() function from the Hmisc package returns just a
# confidence interval:
Hmisc::smean.cl.normal(acs12$hrs_work)

# Confidence intervals for medians are a little tricker.

# The smedian.hilow() function from the Hmisc package returns a confidence
# interval for the median. "computes the sample median and a selected pair of
# outer quantiles having equal tail areas."
Hmisc::smedian.hilow(acs12$hrs_work)

# The wilcox.test() function returns a CI for medians, with conf.int = TRUE.
# Note the estimate is a "pseudomedian".
wilcox.test(acs12$hrs_work, conf.int = TRUE)

# Bootstrapping is another approach for estimating uncertainty and calculating
# confidence intervals. See Appendix below for more information on basic
# bootstrapping.



# YOUR TURN #2 ------------------------------------------------------------

# (1) What is the mean income of ACS respondents? As a population estimate, how
# certain is it? Calculate a confidence interval.


# (2) How many respondents reported an income of 0?


# (3) What proportion of people reported earning more than $100,000? As a
# population estimate, how certain is it? Calculate a confidence interval.




# Comparing two proportions -----------------------------------------------

# We often want to compare two proportions.

# Example: Is there a difference between the proportion of people with
# disabilities between citizens and non-citizens?

# cross tabulation of citizenship and disability using table()
table(acs12$citizen, acs12$disability)

# can also use xtabs(); the table includes row and column labels
# read the "~" as "tabulate the data by"
xtabs(~ citizen + disability, data = acs12)

# prop.table() calculates proportions; margin = 1 means across rows; margin = 2
# means down the columns
xtabs(~ citizen + disability, data = acs12) %>% 
  prop.table(margin = 1) %>% 
  round(3)

# About 0.11 of non-citizens have a disability. About 0.17 of citizens have a
# disability. That's a difference of about -0.06. How certain are we about that
# difference? Would another random sample result in a differece in the opposite
# direction?
#
# Can use prop.test() to answer this. The first argument x takes number of
# "successes" for each group (citizens and non-citizens); the second argument n
# takes number of people surveyed in each group (citizens and non-citizens)
prop.test(x = c(13, 311), n = c(105 + 13, 1571 + 311))

# The p-value says there's about a 15% chance of getting a difference this big,
# or bigger, if there really is no difference between the proportions.
#
# Traditionally, we judge a difference "significant", or not due to chance
# alone, if the p-value is small, say below 0.05.
#
# In this case, we might conclude: 
#
# - "with the current sample size the data were unable to overcome the
#    supposition of no difference in the proportions" 
#
# The 95 percent confidence interval is on the difference of proportions. Since
# it overlaps 0 (barely) we're uncertain about the direction of the difference.
#
#
# To avoid hard-coding numbers, we can use the mosaic version which allows us to
# use formula notation to specify we want to compare the proportion of people
# with disabilities by citizen. Notice we need to specify success = "yes", which
# means we want to compare proportion of people who answered "yes" to
# disability.
mosaic::prop.test(disability ~ citizen, data = acs12, success = "yes")


# A slightly more complicated example...
#
# cross tabulation of citizenship and education
xtabs(~ citizen + edu, data = acs12)

# proportion of education level by citizenship
xtabs(~ citizen + edu, data = acs12) %>% 
  prop.table(margin = 1) %>% 
  round(3)

# About 0.13 of non-citizens are college graduates. About 0.19 of citizens are
# college graduates. That's a difference of -0.06. How certain are we about that
# difference? Would another random sample result in a differece in the opposite
# direction?
#
# We can use prop.test() to answer this. The first argument x takes number of
# "successes"; the second argument n takes number of trials

# Number of success: 15, 344
xtabs(~ citizen + edu, data = acs12)

# Number of trials: 117, 1825. The addmargins() function adds margin totals to a
# table in the specified dimension.
xtabs(~ citizen + edu, data = acs12) %>% 
  addmargins(margin = 2)

# Use values with prop.test()
prop.test(x = c(15, 344), n = c(117, 1825))

# The p-value says there's about a 13% chance of getting a difference this big,
# or bigger, if there really is no difference between the proportions.
#
# The confidence level of the difference in proportions barely overlaps 0.
#
# To avoid entering numbers we can also save table with margins and use with
# prop.test
tab <- xtabs(~ citizen + edu, data = acs12) %>% 
  addmargins(margin = 2)
tab
tab[,"college"]
tab[,"Sum"]
prop.test(x = tab[,"college"], n = tab[,"Sum"])



# YOUR TURN #3 ------------------------------------------------------------

# Compare the proportion of people with disabilities between male and female
# (gender). What is the confidence interval of the difference of proportions?

xtabs(~ gender + disability, data = acs12)


# Comparing two means -----------------------------------------------------

# We often want to compare means between two groups.

# Example: does mean income differ between gender?

# The aggregate() function allows us to calculate a specified statistic for
# groups. 

# Example: Take income, group by gender, and calculate mean for each group
aggregate(income ~ gender, data = acs12, mean)

# The mosaic version of mean allows a formula and "grouping" variable
mosaic::mean(~ income | gender, data = acs12, na.rm = TRUE)

# The mean income of females is about $14,335. The mean income of males is about
# $32,627. That's a difference of -$18,292. How certain are we about that
# difference? Would another random sample result in a differece in the opposite
# direction?

# The t.test() function can help us assess the difference.
t.test(income ~ gender, data = acs12)

# The 95% CI on the difference is (-22702.24, -13880.38)
#
# The t-test is testing that the data from the two groups came from the same
# Normal distribution. The t-test is pretty robust to this assumption with large
# sample sizes, say greater than 30. In other words, we can trust the results
# even if the Normality assumption is suspect.
#
# The p-value is virtually 0. There is just about no chance we would see a
# difference in means this large or larger if the two groups came from the same
# normally distributed population.
#
# Save to an object and extract the CI of the difference in means
t.out <- t.test(income ~ gender, data = acs12)
t.out$conf.int

# It's time for a closer look at the data:
# Proportion of zeros
mean(acs12$income == 0, na.rm=TRUE)

# Proportion of zeros by gender
table(acs12$gender, acs12$income == 0) %>% 
  prop.table(margin = 1) %>% 
  round(2)

# number of income == by employment, by gender
table(acs12$income == 0, acs12$employment, acs12$gender)

# Maybe we should drop the zeroes? We can use the subset() function to only use
# rows for which income is greater than 0.
t.out <- t.test(income ~ gender, data = subset(acs12, income > 0))
t.out
t.out$conf.int

# The expected income difference for females is about (-$34,161, -$20,648) less
# than males.
#
# It's important to note the data is quite skew. 
hist(acs12$income)
hist(acs12$income[acs12$income > 0])

# ggplot makes it pretty easy to see the histograms for both genders
ggplot(subset(acs12, income > 0), aes(x = income)) +
  geom_histogram() +
  facet_wrap(~gender)

# One alternative approach might be a log transformation. This is useful for
# skewed data such as income. NOTE: you can only log transform values greater
# than 0
#
# after log transformation; natural log (base e)
hist(log(acs12$income[acs12$income > 0]))    
# for both genders
ggplot(subset(acs12, income > 0), aes(x = log(income))) +
  geom_histogram() +
  facet_wrap(~gender)

# Now skewed the other direction...
#
# t-test for difference in log-transformed means
t.test(log(income) ~ gender, data = subset(acs12, income > 0))

# The result is still quite large and signficant, but difficult to interpret. We
# need to exponentiate to get data on original scale. First let's save the
# result.
t.out <- t.test(log(income) ~ gender, data = subset(acs12, income > 0))
t.out
t.out$estimate
# Now exponentiate the estimated means
exp(t.out$estimate)

# exponentiating the confidence interval returns the multiplicative effect of
# gender on income.
t.out$conf.int
exp(t.out$conf.int)

# females appear to earn about 0.43 to 0.62 times the income of males
#
# If you doubt the normality assumption, you can try a nonparametric test such
# as the wilcox.test. Instead of assuming the two means come from the same
# Normal distribution, we assume the two means just come from the same
# distribution.
wilcox.test(income ~ gender, data = subset(acs12, income > 0))

# To get a confidence interval, set conf.int = TRUE
wilcox.test(income ~ gender, data = subset(acs12, income > 0), 
            conf.int = TRUE)

# The "difference in location" estimates the median of the difference between a
# sample from x and a sample from y.


# YOUR TURN #4 ------------------------------------------------------------

# How does mean hrs_work differ between gender? What is the confidence interval
# of the difference?




# Comparing more than 2 means ---------------------------------------------

# We often want to compare means between more than two groups.
#
# Example: does mean income differ between race?
#
# Use aggregate to calculate mean income for each race. Let's drop the rows with
# 0 income.
aggregate(income ~ race, data = subset(acs12, income > 0), mean)

# Can also calculate median
aggregate(income ~ race, data = subset(acs12, income > 0), median)

# boxplots are good for visualizing distribution of numeric variables by a
# grouping variable. The black bar is the median (not the mean). The "box" is
# the middle 50% of the data.
boxplot(income ~ race, data = subset(acs12, income > 0))

# We can also easily plot log(income)
boxplot(log(income) ~ race, data = subset(acs12, income > 0))

# It appears mean income may be different between races. Is this due to chance?
#
# The ANOVA procedure is usually emmployed to determine if there is a
# statistically significant difference between the means.
#
# The aov() function works like the t.test() function. Best to save result and
# use summary()
aov.out <- aov(log(income) ~ race, data = subset(acs12, income > 0))
summary(aov.out)

# A small p-value provides against the null hypothesis that all means are the
# same. So where are the differences? This leads to what are commonly called
# "post-hoc" procedures.
#
# A basic one built into R is Tukey's Honestly Significantly Difference,
# TukeyHSD(). It compares all combinations of pairs and tests whether the
# difference is 0. It also reports 95% confidence intervals on the differences.
#
# Call it on the aov object and save. The result is a list of all pairwise
# comparisons along with the estimated difference and confidence intervals and
# an "adjusted" p-value on the "significance" of the difference.
tukey.out <- TukeyHSD(aov.out)
tukey.out

# There is a plotting method for a TukeyHSD object.
plot(tukey.out)

# Exponentiate to get multiplicative effect
exp(tukey.out$race[,"diff"])

# race=black makes about 46 cents for every dollar race=asian earns.



# YOUR TURN #5 ------------------------------------------------------------

# Run an ANOVA of log(income) on edu. Is there a difference in mean log(income)
# values between different levels of education? Go ahead subset acs12 to use
# income greater than 0.





# Association between categorical variables -------------------------------

# Let's create a cross tabulation of race and employment
xtabs(~ race + employment, data = acs12)

# proportion of employment by race
xtabs(~ race + employment, data = acs12) %>% 
  prop.table(margin = 1) %>% 
  round(3)

# Is there an assocation between race and employment? Does knowing race give us
# additional information about the proportions of employment levels?
#
# A common approach to answering this question is the chi-square test. The
# chisq.test() function simply requires a table
xtabs(~ race + employment, data = acs12) %>% 
  chisq.test()

# A small p-value provides evidence against the null hypothesis of no
# association. However it does not tell us where the association is or the
# magnitude of the association.
#
# Also notice the warning. That is due to small expected cell counts. Happens
# when the expected count of a cell is less than 5. Set simulate.p.value = TRUE
# to simulate a p-value using Monte Carlo simulation and not get warning.
c.out <- xtabs(~ race + employment, data = acs12) %>% 
  chisq.test(simulate.p.value = TRUE)
c.out

# Check the residuals of the chisq.test to see where the "unexpected" high/low
# counts are occuring. Residuals over 2 (or less than -2) are usually of
# interest.
c.out$residuals

# It appears we have more unemployed for race=black than would be expected if
# there was no association between race and employment
#
# The mosaicplot() function with base R can help visualize residuals from a
# chi-square test. shade = TRUE colors the tiles where the residuals are
# unusually large. las=1 makes the labels horizontal.
xtabs(~ employment + race, data = acs12) %>% 
  mosaicplot(shade = TRUE, las = 1)

# The width of the boxes corresponds to the proportions of employment. The
# height of the boxes correspond to the proportion of race, within each level of
# employment. Within unemployed, we see a higher than expected proportion of
# race=black.


# YOUR TURN #6 ------------------------------------------------------------

# (1) Create a cross tabulation of employment and education


# (2) Run a chi-square test on the cross tabulation. Is there evidence of
# association?


# (3) Create a mosaic plot of the cross tabulation with residual shading




# Linear association between numeric variables ----------------------------

# Correlation summarizes linear association between two numeric variables.
# Ranges from -1 to 1. No correlation is 0.
#
# See an illustration:
# https://en.wikipedia.org/wiki/Correlation_and_dependence#/media/File:Correlation_examples2.svg
#
# Is age correlated with income?
#
# Let's first plot age versus income
plot(acs12$age, acs12$income)

# Plot income versus age for income > 0
plot(income ~ age, data = subset(acs12, income > 0))

# Correlation between age and income; returns NA because we have missing data.
cor(acs12$age, acs12$income)
summary(acs12$income)

# Set use = "pairwise.complete.obs" to calculate correlation using all complete
# pairs of observations 
cor(acs12$age, acs12$income, use = "pairwise.complete.obs")

# The mosaic version allows us to use the formula interface which allows us to
# subset acs12 for income > 0
mosaic::cor(age ~ income, use = "pairwise.complete.obs", 
            data = subset(acs12, income > 0))

# The cor.test function returns a confidence interval on the correlation
mosaic::cor.test(age ~ income, use = "pairwise.complete.obs", 
                 data = subset(acs12, income > 0))

# Plot log(income) versus age for income > 0
plot(log(income) ~ age, data = subset(acs12, income > 0))

# correlation between age and log(income)
mosaic::cor.test(age ~ log(income), use = "pairwise.complete.obs", 
                 data = subset(acs12, income > 0))


# YOUR TURN #7 ------------------------------------------------------------

# Calculate the correlation, and associated 95% confidence interval, between age
# and hrs_work. Make a scatterplot as well.




# Simple Linear Regression ------------------------------------------------

# Simple linear regression is basically summarizing the relationship between two
# variables as a straight line, using the familiar slope-intercept formula:
# 
# y = a + bx
# 
# This implies we can approximate the mean of y for a given value of x by
# multiplying x by some number and adding a constant value.
#
# It allows us to answer the question, "how does the mean of y change as x
# increases?"

# Example: how does mean income change as hrs_work increases? Regress income on
# hrs_work.
mod <- lm(income ~ hrs_work, data = acs12, subset = income > 0)
summary(mod)

# It appears mean annual income increases by about $1400 for each additional
# hour worked.
#
# The coefficient is just an estimate. Use confint() to get a confidence
# interval. 95% CI: ($1226, $1742)
confint(mod)

# Is this a "good" estimate? One way to judge is to plot the fitted straight
# line over the raw data.
plot(acs12$hrs_work, acs12$income)

# add fitted straight line using the abline() function
abline(mod)

# It doesn't look like a great estimate. The fitted line overpredicts and
# underpredicts over the range of hrs_work.
#
# The differences between the fitted straight line and observed data are called
# the residuals. An assumption of a simple linear model is that these residuals
# are a random sample from a normal distribution with mean 0 and some finite
# standard deviation. This implies we expect an even scatter of raw data around
# the fitted line (ie, constant variance). We can check this assumption by
# calling plot() on the model object.
#
# The first plot assess the constant variance assumption. The second
# assesses the Normality assumption. 
plot(mod, which = 1)
plot(mod, which = 2)

# Neither look very good. It appears the constant variance assumption is
# violated. We like to see a mostly horizontal red line in the first plot, and a
# mostly diagonal line in the second plot.
# 
# One approach to address this is to try a log transformation
mod2 <- lm(log(income) ~ hrs_work, data = acs12, subset = income > 0)

# get estimated coefficients
summary(mod2)

# For every extra hour worked, income increases by about (exp(0.063) - 1) * 100
# = 6.5 percent.
exp(0.063)
(exp(0.063) - 1) * 100

# see confidence intervals
confint(mod2)

# scatterplot with fitted line
plot(acs12$hrs_work, log(acs12$income))
abline(mod2)

# check assumptions
plot(mod2, which = 1)
plot(mod2, which = 2)

# There still appears to be a violation of the constant variance assumption.
# This is basically because there is a non-linear relationship between hrw_work
# and log(income).
#
# We can try to fit a smooth line that follows the data. One approach is called
# regression splines.
#
# 1) Load the splines package (comes with R)
# 2) use the ns() function and specify the number of times the line can "change
#    direction" (usually 3 - 5) as the degrees of freedom (df). 
#    ns = natural spline

library(splines)
mod3 <- lm(log(income) ~ ns(hrs_work, 3), data = acs12, subset = income > 0)
summary(mod3)

# There is really no model interpretation
#
# Plot the fitted line over the raw data
plot(acs12$hrs_work, log(acs12$income))

# Since the prediction is no longer a straight line we cannot use abline().
# Use the predict() function to get fitted values on the range of 0 - 100.
# Notice the newdata argument requires a data frame
y <- predict(mod3, newdata = data.frame(hrs_work = 1:100))

# add line to plot
lines(1:100, y)

# The assumptions appear to be met though row 1411 seems to be unusual
plot(mod3, which = 1)
plot(mod3, which = 2)

# Use model to make a prediction of expected income given hours worked.
predict(mod3, newdata = data.frame(hrs_work = c(20,30,40,50)), 
        interval = "confidence") %>% 
  exp()



# YOUR TURN #8 ------------------------------------------------------------

# (1) Regress log(income) on age. That is, fit lm(log(income) ~ age, data =
#     acs12). Be sure to subset the data: subset = income > 0 & age > 17
#     Plot the data and fitted line.



# (2) Regress log(income) on ns(age, 3). Again, Be sure to subset the data:
#     subset = income > 0 & age > 17. Plot the data and fitted line.



# Appendix: Plotting means and confidence intervals -----------------------

# difference in mean income by gender.
# create a data frame of means and upper and lower CIs
income.ci <- acs12 %>% 
  filter(income > 0) %>% 
  group_by(gender) %>% 
  summarise(mean = mean(income, na.rm = TRUE),
            lower = smean.cl.normal(income)["Lower"],
            upper = smean.cl.normal(income)["Upper"])
income.ci

library(ggplot2)
ggplot(income.ci, aes(x = gender, y = mean)) +
  geom_point() +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.1) +
  scale_y_continuous(labels = scales::dollar) +
  labs(y = "Mean Income", x = "Gender")


# plot proportion of college graduates by race
# first get counts of college grads and total race
tab <- xtabs(~ race + edu, data = acs12) %>%
  addmargins(margin = 2)
# get confidence intervals
binom.out <- binom.confint(x = tab[,"college"],
                           n = tab[,"Sum"], 
                           methods = "prop.test")
# add race to the binom.out data frame
binom.out$race <- rownames(tab)
binom.out

# create the plot
ggplot(binom.out, aes(x = race, y = mean)) +
  geom_point() +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.1) +
  ylim(0,0.5) +
  labs(y = "proportion", title = "proportion of college grads by race") 

# reorder race by mean
ggplot(binom.out, aes(x = reorder(race, mean), y = mean)) +
  geom_point() +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.1) +
  ylim(0,0.5) +
  labs(y = "proportion", x = "race", 
  title = "proportion of college grads by race") 


# Appendix: Demonstration of confidence intervals -------------------------

# 95% Confidence Interval theory: sample the data, calculate a confidence
# interval, repeat many times. About 95% of confidence intervals will contain
# the "true" value you're trying to estimate.

# Simulation to demonstrate

# simulate a population of 10,000 from a Normal distribution with mean 10 and
# standard deviation 1.5.
population <- rnorm(10000, mean = 10, sd = 1.5)

# The TRUE mean value is 10. Pretend we don't know that.

# sample the population (size = 30) and calculate 95% confidence interval of
# mean:
s <- sample(population, size = 30, replace = TRUE)
ci <- t.test(s)$conf.int
ci

# Does confidence interval contain mean?
ci[1] < 10 & ci[2] > 10

# Let's do this 1000 times
result <- logical(length = 1000)  # empty vector to store result (TRUE/FALSE)
for(i in 1:1000){
  s <- sample(population, size = 30, replace = TRUE)
  ci <- t.test(s)$conf.int
  result[i] <- ci[1] < 10 & ci[2] > 10
}

# what proportion of times did confidence interval contain 10?
mean(result)

# The process works about 95% of the time. Hence, the reason we call it a
# "confidence" interval. We're confident the process will return a confidence
# interval that contains the true value.

# Here's another demonstration that produces a plot.

# a random seed that ensures you get the results I got
set.seed(6)

# create an empty data frame containing a counter and upper and lower CI
# values.
dat <- data.frame(i = 1:50, lwr = NA, upr = NA)

# Sample from "population" and calculate CI 50 times
for(i in 1:50){
  s <- sample(population, size = 30, replace = TRUE)
  ci <- t.test(s)$conf.int
  dat[i,"lwr"] <- ci[1]
  dat[i,"upr"] <- ci[2]
}

# create an indicator that indentifies whether or not the CI captured the "True"
# mean of 10.
dat$grp <- ifelse(dat$lwr < 10 & dat$upr > 10, "Captured", "Not Captured")

# Generate plot using the ggplot2 package
library(ggplot2)
ggplot(dat) +
  geom_vline(xintercept = 10, linetype = 2) +
  geom_segment(aes(x = lwr, y = i, xend = upr, yend = i, color = grp)) +
  labs(x = "", y = "", title = "50 random confidence intervals") +
  scale_color_discrete("True Mean")



# Appendix: Basic bootstrapping -------------------------------------------

# Bootstrapping means resampling your data with replacement, calculating a
# statistic of choice (such as a mean) and then repeating many times. The result
# is many means which we then can use to get an estimate of uncertainty of our
# original estimated mean. Because it's based on a resampling, your bootstrapped
# CI will be different from mine, but only slightly.

# Bootstrapping is effective for estimating uncertainty when the usual
# assumptions for calculating standard errors are suspect, or when a standard
# error formula is complex or not available.

# Example data:
x <- c(12, 22, 21, 18, 19, 23, 7)
mean(x)

# Bootstrap "by hand":
# Resample with replacement (ie allow a value to be sampled more than once) and
# estimate mean
mean(sample(x, replace = TRUE))

# repeat 1000 times and store
b <- replicate(n = 1000, mean(sample(x, replace = TRUE)))

# b contains 1000 bootstrapped means
head(b)

# We can take the 0.025 and 0.975 percentiles to get an approximate Confidence
# Interval:
quantile(b, probs = c(0.025, 0.975))

# The Hmisc function smean.cl.boot calculates bootstrapped confidence intervals:
Hmisc::smean.cl.boot(x)

# Calculate a bootstrap CI of hrs_work using the Hmisc function smean.cl.boot.
# If you run it multiple times you will get slightly difference intervals.
smean.cl.boot(acs12$hrs_work, B = 1500)

# To see the bootstrapped estimates, set reps = TRUE
smean.cl.boot(acs12$hrs_work, B = 100, reps = TRUE)

# The reps represent the 100 means that resulted from the 100 bootstrapped
# samples.

# The boot package that comes with R provides the boot() function that allows
# you to bootstap just about any statistic as long as you can write a function
# for it.

# Here we bootstrap the median of income. How certain is that? 
median(acs12$income, na.rm = TRUE)

# load the boot package
library(boot)

# Write a function that calculates the median of a bootstrapped sample. The [i]
# is required to sample the selected indices (rows) generated by the
# re-sampling.
bootMedian <- function(x, i)median(x[i], na.rm = TRUE)

# Run the bootstrap 999 times
boot.out <- boot(data = acs12$income, 
                 statistic = bootMedian, 
                 R = 999)

# Get the percentile confidence interval of the bootstrapped estimates using the
# boot.ci() function, also in the boot package
boot.ci(boot.out, type = "perc")

# Another example: 
# IQR. How to get a confidence interval for the IQR of income?
IQR(acs12$income, na.rm = TRUE)

# Use the bootstrap. First write a function:
bootIQR <- function(x, i)IQR(x[i], na.rm = TRUE)

# Run the bootstrap.
boot.out <- boot(data = acs12$income, 
                 statistic = bootIQR, 
                 R = 999)

# Calculate the percentile confidence interval
boot.ci(boot.out, type = "perc")

# Another example:
# difference in medians

# How to get a confidence interval on the difference in medians?
mosaic::median(~ income | gender, data = acs12, na.rm = TRUE)

# diff() subtracts first element from the second
diff(mosaic::median(~ income | gender, data = acs12, na.rm = TRUE))

# Use the bootstrap. First write a function:
bootMedianDiff <- function(x, i)diff(mosaic::median(~ income | gender,
                                                    data = x[i,], 
                                                    na.rm = TRUE))

# Run the bootstrap.
boot.out <- boot(data = acs12, 
                 statistic = bootMedianDiff, 
                 R = 999)

# Calculate the percentile confidence interval
boot.ci(boot.out, type = "perc")

# Another example:
# 75th percentile

# How to get a confidence interval on the 75th percentile of income?
quantile(acs12$income, probs = 0.75, na.rm = TRUE)

# Use the bootstrap. First write a function:
boot75p <- function(x, i)quantile(x[i], probs = 0.75, na.rm = TRUE)

# Run the bootstrap.
boot.out <- boot(data = acs12$income, 
                 statistic = boot75p, 
                 R = 999)

# Calculate the percentile confidence interval
boot.ci(boot.out, type = "perc")



