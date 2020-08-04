library(jsonlite)
library(tidyverse)
library(magrittr)
library(qwraps2)
install.packages("qwraps2", repo = "http://cran.rstudio.com")

# Define tables
mc_mpg <- read.csv(file = 'MechaCar_mpg.csv', check.names = F,stringsAsFactors = F)
head(mc_mpg)

mc_susp <- read.csv(file='Suspension_Coil.csv',check.names = F,stringsAsFactors = F)
head(mc_susp)

# MPG Regression
## Multi Regression Analysis with all variables
summary(lm(mpg ~ `vehicle length` + `vehicle weight` +
  `spoiler angle` + `ground clearance` + `AWD`,data=mc_mpg))

## Investigation on ground clearance variable
model <- lm(mpg ~ `ground clearance`,mc_mpg) #create linear model
yvals <- model$coefficients['`ground clearance`']*mc_mpg$`ground clearance` +
  model$coefficients['(I7bgntercept)']
plt <- ggplot(mc_mpg,aes(x=`ground clearance`,y=mpg)) #import dataset into ggplot2
plt + geom_point() + geom_line(aes(y=yvals), color = "red") 
summary(model)

## Investigation on vehicle length variable
model <- lm(mpg ~ `vehicle length`,mc_mpg) #create linear model
yvals <- model$coefficients['`vehicle length`']*mc_mpg$`vehicle length` +
  model$coefficients['(Intercept)']
plt <- ggplot(mc_mpg,aes(x=`vehicle length`,y=mpg)) #import dataset into ggplot2
plt + geom_point() + geom_line(aes(y=yvals), color = "red") 
summary(model)


# Suspension Coil Summary
susp_summary <- mc_susp %>% group_by(Manufacturing_Lot) %>% summarize(Mean=mean(PSI),Median=median(PSI),Variance=var(PSI),SD=sd(PSI))
susp_summary

## Box Plot with the differences between Lots
plt <- ggplot(mc_susp,aes(x=Manufacturing_Lot,y=PSI))
plt + geom_boxplot()

# Suspension Coil T-Test
t.test(mc_susp$PSI, mu=1500)

susp_sample <- mc_susp %>% sample_n(50)
t.test(susp_sample$PSI, mu=1500)

lot1 <- mc_susp %>% filter(Manufacturing_Lot=='Lot1')
lot2 <- mc_susp %>% filter(Manufacturing_Lot=='Lot2')
lot3 <- mc_susp %>% filter(Manufacturing_Lot=='Lot3')
summary(lot3$PSI)

t.test(lot1$PSI,lot2$PSI,paired = T)
t.test(lot1$PSI,lot3$PSI,paired = T)
t.test(lot2$PSI,lot3$PSI,paired = T)
