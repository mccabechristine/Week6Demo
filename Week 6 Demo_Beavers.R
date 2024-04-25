## Using statistical methods to examine -------------------------#
## the relationships between variables of interest --------------#

?beavers
str(beaver2)
View(beaver2)

## Question 1 ---------------------------------------------------#
## We would like to examine the mean body temperature of the 
## beavers to determine whether activity has any effect on 
## beaver body temperature --------------------------------------#

## We want to examine the difference in body temp
## during periods of activity to evaluate whether 
## body temperature is affected by activity
## First we need to ensure that sata is in correct format
## Activ should be a factor
## Temp is numerical --------------------------------------------#

## Devise a hypothesis test that we use for this analysis. 

# H0: Body temperature is not affected by the activity
# H1: Body temperature is affected by the activity

# Copying the data to a data frame
# This is not a necessary step
beavers_data <- beaver2
beavers_data
str(beavers_data)

## Describe the variables you will use for the analysis. 
## What type of variables are they? Do they need to be converted? 
## If they do, convert them to the relevant data types in R. ----#

# labels starts with what is assigned to the lower value first
# eg 0 = no, 1 = yes
beavers_data$activ <- factor(beavers_data$activ,
                             labels = c("no", "yes"))
beavers_data
str(beavers_data)
library(ggplot2)
# range(beavers_data)

## Question 2 ---------------------------------------------------#
## Check whether the variables you are using for the hypothesis 
## test are normally distributed or not. Do this visually and 
## using a relevant statistical analysis test. 
## Then decide on which statistical test you will use. ----------# 

windows(16, 10)
ggplot(beavers_data, aes(x=temp))+geom_histogram()+theme_bw()

ggplot(beavers_data, aes(x=temp))+
        geom_histogram(breaks=seq(36, 38, .2))+
        theme_bw()+
        labs(x = "temp", y = "Activity")+
        scale_y_continuous(breaks=seq(0,60,5))

library("lattice")
windows(20,10)
attach(beavers_data)
histogram(~temp | activ,
          data = beavers_data,
          main = "Distribution of beaver activity data",
          xlab = "Temperature (degrees)",
          ylab = "Activity %")
detach(beavers_data)

attach(beavers_data)
windows(15,10)
qqnorm(temp)
# this line represents normal distribution
qqline(temp, col = "red")

opar <- par(no.readonly = TRUE)
windows(20,10)
# arrange plots in 1 rows and 2 column
par(mfrow = c(1, 2))

with(beavers_data, {
  qqnorm(temp[activ == "yes"],
         main = "Beavers active data")
  qqline(temp[activ == "yes"])
})

with(beavers_data, {
  qqnorm(temp[activ == "no"],
         main = "Beavers inactivewindows(16, 10)
ggplot(beavers_data, aes(x=temp))+geom_histogram()+theme_bw()

ggplot(beavers_data, aes(x=temp))+
        geom_histogram(breaks=seq(36, 38, .2))+
        theme_bw()+
        labs(x = "temp", y = "Activity")+
        scale_y_continuous(breaks=seq(0,60,5))

library("lattice")
windows(20,10)
attach(beavers_data)
histogram(~temp | activ,
          data = beavers_data,
          main = "Distribution of beaver activity data",
          xlab = "Temperature (degrees)",
          ylab = "Activity %")
detach(beavers_data)

attach(beavers_data)
windows(15,10)
qqnorm(temp)
# this line represents normal distribution
qqline(temp, col = "red")

opar <- par(no.readonly = TRUE)
windows(20,10)
# arrange plots in 1 rows and 2 column
par(mfrow = c(1, 2))

with(beavers_data, {
  qqnorm(temp[activ == "yes"],
         main = "Beavers active data")
  qqline(temp[activ == "yes"])
})

with(beavers_data, {
  qqnorm(temp[activ == "no"],
         main = "Beavers inactive data")
  qqline(temp[activ == "no"])
})

par(opar) data")
  qqline(temp[activ == "no"])
})

par(opar)

# Formal test of normality
# provided through widely used Shapiro-Wilks test
normality_test <- shapiro.test(beavers_data$temp)
normality_test
normality_test$p.value
# p-value tells us the chances that the sample comes 
# from a normal distribution
# In this example, p-value is clearly lower than 0.05
# so not normally distributed

# this method does not work for a dichotomous variable
# Data needs to be numeric for Shapiro Wilk test

# After consulting the chart, I am examining
# a dependent continuous variable (temp)
# with an independent categorical variable 
# so I use the Mann-Whitney test
# this is also known as the "Wilcox Test"
# Format = wilcox.test(dependent~independent)
attach (beavers_data)
wilcox.test(temp~activ)
wilcox.test(beavers_data,temp~activ)

# p-value is < 0.05 so we reject H0 and conclude that 
# beaver body temperature is affected by activity
# Reject Null hypothesis and accept the alternative hypothesis 
# H0: Body temperature is not affected by the activity - REJECT
# H1: Body temperature is affected by the activity - ACCEPT

# To examine the correlation between variables
# and whether it is positive or negative

# We can use libraries to help improve
# the chart. Also includes correlations between variables
install.packages("psych")
library(psych)

windows( 16, 10)
pairs(beavers_data, labels = colnames(beavers_data),
      main = "Beavers dataset correlation plot")

pairs.panels(beavers_data,
             smooth = TRUE,        # If tRUE, draws less smooth
             scale = FALSE,        # If TRUE, scales the correlation text
             density = TRUE,       # If TRUE, adds density plots and histogram
             ellipse = TRUE,       # If TRUE, draws ellipses
             method = "spearman",  # Correlation method (also "pearson")
             pch = 21,             # pch symbol
             lm = FALSE,           # If TRUE, plots linear fit rather than
             cor = TRUE,           # If TRUE, reports correlations
             jiggle = FALSE,       # If TRUE, data points are juttered
             factor = 2,           # Jittering factor
             hist.col = 4,         # Histogram colour
             stars = TRUE,         # If TRUE, adds significance level with
             ci = TRUE)            # If True, adds confidence levels

