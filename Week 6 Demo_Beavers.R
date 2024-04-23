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

# labels starts with what is assigned to the lawer value first
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
         main = "Beavers inactive data")
  qqline(temp[activ == "no"])
})
  