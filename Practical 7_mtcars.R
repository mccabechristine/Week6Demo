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

# Question 1
# We would like to examine whether there is a link between the 
# weight of cars and their fuel efficiency as measured by MPG. 
# Devise a hypothesis test that we use for this analysis. 
# Describe the variables you will use for the analysis. 
# What type of variables are they? 
# Do they need to be converted? 
# If they do, convert them to the relevant data types in R. 

# H0: Fuel efficiency is not affected by the weight
# H1: Fuel efficiency is affected by the weight

?mtcars
str(mtcars)
View(mtcars)

# summary data can be used 
summary(mtcars)

windows(16, 10)
ggplot(mtcars, aes(x=mpg))+geom_histogram()+theme_bw()

ggplot(mtcars, aes(x=mpg))+
  geom_histogram(breaks=seq(36, 38, .2))+
  theme_bw()+
  labs(x = "MPG", y = "Weight")+
  scale_y_continuous(breaks=seq(0,60,5))

library("lattice")
windows(20,10)
attach(mtcars)
histogram(~mpg | wt,
          data = mtcars,
          main = "Distribution of Car data",
          xlab = "MPG (Miles/US Gallon)",
          ylab = "Weight (pounds)")
detach(mtcars)

attach(mtcars)
windows(15,10)
qqnorm(wt)
# this line represents normal distribution
qqline(wt, col = "red")

opar <- par(no.readonly = TRUE)
windows(20,10)
# arrange plots in 1 rows and 2 column
par(mfrow = c(1, 2))

with(mtcars, {
  qqnorm(mpg[wt == "yes"],
         main = "Cars data")
  qqline(mpg[wt == "yes"])
})

with(mtcars, {
  qqnorm(temp[activ == "no"],
         main = "Beavers inactive data")
  qqline(temp[activ == "no"])
})

par(opar)
