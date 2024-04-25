# MTCARS Lab Practical 7 -----------------------------------------
# This practical session is based on the mtcars data which was 
# extracted from the 1974 Motor Trend US magazine and comprises 
# fuel consumption and 10 aspects of automobile design and performance 
# for 32 cars (1973--74 models). 
# The variables of the cars data frame are as follows: 
# mpg  Miles/(US) gallon 
# cyl  Number of cylinders 
# disp  Displacement (cu.in.) 
# hp  Gross horsepower 
# drat  Rear axle ratio 
# wt  Weight (1000 lbs) 
# qsec  1/4 mile time 
# vs  Engine (0 = V-shaped, 1 = straight) 
# am  Transmission (0 = automatic, 1 = manual) 
# gear  Number of forward gears  

# Question 1 -------------------------------------------------------------------
# We would like to examine whether there is a link between the 
# weight of cars and their fuel efficiency as measured by MPG. 
# Devise a hypothesis test that we use for this analysis. 
# Describe the variables you will use for the analysis. 
# What type of variables are they? 
# Do they need to be converted? 
# If they do, convert them to the relevant data types in R. 
# ------------------------------------------------------------------------------

# H0:There is no correlation between the weight of the cars 
# and their fuel efficiency (MPG). 
# This means the correlation coefficient (p) is zero:𝜌= 0
  
# H1: There is a correlation between the weight of the cars 
# and their fuel efficiency (MPG). 
# This means the correlation coefficient (ρ) is not zero: 𝜌≠0

?mtcars

# Check the structure of the data
str(mtcars)

# Specifically check data types for MPG and Weight
class(mtcars$mpg)  # Check if mpg is numeric
class(mtcars$wt)   # Check if wt is numeric

# If not numeric, convert them to numeric
# mtcars$mpg <- as.numeric(mtcars$mpg)
# mtcars$wt <- as.numeric(mtcars$wt)

# Question 2 -------------------------------------------------------------------
# Check whether the variables you are using for the hypothesis test 
# are normally distributed or not. 
# Do this visually and using a relevant statistical analysis test. 
# Then decide on which statistical test you will use. 
# Refer to code to check visually and numerically for distribution. 
# Data is normally distributed. 
# ------------------------------------------------------------------------------

# Load necessary library
library(ggplot2)
library("lattice")

# Histogram for MPG
windows(15,10)
ggplot(mtcars, aes(x=mpg)) + 
  geom_histogram(aes(y=..density..), binwidth=1, colour="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666") +
  ggtitle("Histogram of MPG")

# Histogram for Weight
windows(15,10)
ggplot(mtcars, aes(x=wt)) + 
  geom_histogram(aes(y=..density..), binwidth=0.1, colour="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666") +
  ggtitle("Histogram of Weight")

# Q-Q plot for MPG
windows(15,10)
qqnorm(mtcars$mpg)
qqline(mtcars$mpg, col = "green", lwd = 2)

# Q-Q plot for Weight
windows(15,10)
qqnorm(mtcars$wt)
qqline(mtcars$wt, col = "steelblue", lwd = 2)

# Question 3 ------------------------------------------------------------------ 
# Examine the results of the statistical test and decide on whether there is a 
# statistical correlation between car weight and mpg. Present your findings. 
# -----------------------------------------------------------------------------

# The Pearson correlation coefficient (r) measures the strength and direction 
# of the linear relationship between two continuous variables. 
# It ranges from -1 to 1, where:
# 1 indicates a perfect positive linear relationship,
# 0 indicates no linear relationship, and
# -1 indicates a perfect negative linear relationship.

# Hypothesis for the Pearson Correlation Test
# H0: There is no linear relationship between the weight of the cars and their MPG (r = 0).
# H1: There is a linear relationship between the weight of the cars and their MPG (r ≠ 0).

# Pearson correlation test between wt and mpg in mtcars
cor_test_result <- cor.test(mtcars$wt, mtcars$mpg, method = "pearson")

# Output the results
print(cor_test_result)

# The results from the Pearson's product-moment correlation test show a correlation 
# coefficient (r) of approximately -0.8677. 
# This value indicates a strong negative linear relationship between the weight 
# of the cars (wt) and their fuel efficiency (mpg). 

# The p-value is 1.294 × 10^-10, which is significantly less than the conventional 
# alpha level of 0.05. 
# This very low p-value leads us to reject the null hypothesis and conclude 
# that there is a statistically significant correlation between car weight and fuel efficiency.
# Additionally, the 95% confidence interval for the correlation coefficient ranges from 
# -0.9338 to -0.7441. Since this interval does not contain 0, it further supports 
# the conclusion that there is a significant negative correlation between the two variables.

# Question 4 -------------------------------------------------------------------
# We would like to examine whether there is a link between the horsepower of cars 
# and the type of gearbox fitted to them. 
# Devise a hypothesis test that we use for this analysis. 
# Describe the variables you will use for the analysis. 
# What type of variables are they? Do they need to be converted? 
# If they do, convert them to the relevant data types in R. 
# Is there a correlation between horsepower and gearbox type?  
# ------------------------------------------------------------------------------

# To examine the link between the horsepower of cars (hp) and the type of gearbox 
# fitted to them (am), we can perform a hypothesis test for association between
# the two variables. Since one variable is continuous (horsepower) 
# and the other is categorical (gearbox type), the appropriate test is a 
# two-sample test, such as the t-test (if assumptions are met) 
# or a non-parametric test like the Mann-Whitney U test.

# H0: Mean horsepower is not affected by type of transmission
# H1: Mean horsepower is affected by type of transmission

# Variables for Analysis
# Horsepower (hp): This is a continuous numerical variable that indicates the 
# power of the car's engine.
# Gearbox Type (am): This is a categorical dichotomous variable where '1' represents 
# manual transmission and '0' represents automatic transmission.

# Check the structure of the data
str(mtcars)

# Specifically check data types for horsepower and gearbox type
class(mtcars$hp)  # Horsepower should be numeric
class(mtcars$am)  # Gearbox type should be a factor for a t-test

# If hp is not numeric, convert it
#if (!is.numeric(mtcars$hp)) {
#  mtcars$hp <- as.numeric(mtcars$hp)
# }

# Convert gearbox type to a factor if it's not already
mtcars$am <- as.factor(mtcars$am)

# Perform Shapiro-Wilk test for normality on hp by gearbox type
shapiro.test(mtcars$hp[mtcars$am == 0])  # For automatic cars
shapiro.test(mtcars$hp[mtcars$am == 1])  # For manual cars

# Automatic Cars (am == 0): The p-value is 0.5403, which is above the common 
# alpha level of 0.05, suggesting that the horsepower for automatic cars does 
# not significantly deviate from a normal distribution.
# Manual Cars (am == 1): The p-value is 0.00288, which is below the alpha level 
# of 0.05, indicating that the horsepower for manual cars deviates significantly
# from a normal distribution.

# Given that one group (manual cars) does not appear to be normally distributed,
# it's not appropriate to use a parametric test like the two-sample t-test, 
# which assumes normality in both groups. Instead, a non-parametric test would 
# be more suitable.

# Perform Mann-Whitney U test
wilcox.test(hp ~ am, data=mtcars)

# The results of the Wilcoxon rank sum test with continuity correction indicate
# that the p-value is 0.0457. This is just below the conventional significance 
# level of 0.05, suggesting that there is a statistically significant difference
# in the median horsepower between cars with manual and automatic gearboxes in 
# the mtcars dataset.

# The alternative hypothesis for the Wilcoxon test, which states that the true 
# location shift is not equal to zero, is supported here. This means that the 
# type of gearbox (automatic or manual) is likely associated with a difference 
# in horsepower.

# Practically speaking, this could be interpreted to mean that the type of 
# transmission a car has may be an indicator of its horsepower, with one type 
# (either manual or automatic) typically having higher or lower horsepower than 
# the other. However, the test does not tell us which type has more horsepower; 
# it only indicates that a significant difference exists.

# Given the closeness of the p-value to the threshold, it's also worth noting 
# that this is a marginal finding and could be sensitive to the choice of 
# significance level or the presence of any biases or anomalies in the data. 
# For a more thorough analysis, you could also look at the effect size or the 
# median horsepower values for each group to understand the practical 
# significance of this difference.
