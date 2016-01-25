#### Section 6 ####

crime <- read.csv("http://people.ucsc.edu/~cdobkin/MLDA%20Crime%20CA.csv")
attach(crime)
options(scipen=999)

#### Making different sized bins ####

plot(,x=days_to_21, y=murder_r)
#very messy

# Get a person's age based off of the 'days to 21' variable
Age = 21 + days_to_21/365

Age_1day = 21 + days_to_21/365
Age_4day = 21 + 4*floor(days_to_21/4)/365 + 2/365
Age_8day = 21 + 8*floor(days_to_21/8)/365 + 4/365
Age_16day = 21 + 16*floor(days_to_21/16)/365 + 8/365
Age_26day = 21 + 26*floor(days_to_21/26)/365 + 13/365
Age_32day = 21 + 32*floor(days_to_21/32)/365 + 16/365
Age_50day = 21 + 50*floor(days_to_21/50)/365 + 25/365
Age_65day = 21 + 65*floor(days_to_21/65)/365 + 32/365
Age_128day = 21 + 128*floor(days_to_21/128)/365 + 64/365
Age_365day = 21 + 365*floor(days_to_21/365)/365 + 182/365

#Merge with data
crime1 <- cbind(crime, Age, Age_1day,Age_4day,Age_8day,Age_16day,Age_26day,
                Age_32day,Age_50day,Age_65day,Age_128day,Age_365day)

attach(crime1)

#Combined graph - look at graphs with different sized bins 
# (question 1 from HW)
install.packages("gridExtra")
library(gridExtra)
library(ggplot2)


#making averages for different bin sizes

#128 day bin
aggdata <- aggregate(crime1, by=list(crime1$Age_128day), FUN=mean,
                     na.rm=TRUE)
plot1a <- ggplot(aggdata, aes(x=Group.1, y= dui_r))+
  geom_point()+
  xlab("Age at Time of Arrest")+
  ylab("Arrest Counts, binwidth = 128 days")+
  theme_bw()
plot1a

#8 day bin 
aggdata <- aggregate(crime1, by=list(crime1$Age_8day), FUN=mean,
                     na.rm=TRUE)
plot1b <- ggplot(aggdata, aes(x=Group.1, y= dui_r))+
  geom_point()+
  xlab("Age at Time of Arrest")+
  ylab("Arrest Counts, binwidth = 8 days")+
  theme_bw()
plot1b

#16 day bin
aggdata <- aggregate(crime1, by=list(crime1$Age_16day), FUN=mean,
                     na.rm=TRUE)
plot1c <- ggplot(aggdata, aes(x=Group.1, y= dui_r))+
  geom_point()+
  xlab("Age at Time of Arrest")+
  ylab("Arrest Counts, binwidth = 16 days")+
  theme_bw()
plot1c

aggdata <- aggregate(crime1, by=list(crime1$Age_26day), FUN=mean,
                     na.rm=TRUE)
plot1d <- ggplot(aggdata, aes(x=Group.1, y= dui_r))+
  geom_point()+
  xlab("Age at Time of Arrest")+
  ylab("Arrest Counts, binwidth = 26 days")+
  theme_bw()
plot1d

grid.arrange(plot1a, plot1b, plot1c, plot1d, ncol=2, main="Choosing Binwidth")

# The next decision is bandwidth

#combined plot with different ranges of the running variable

#Between 17 and 25
aggdata <- aggregate(crime1, by=list(crime1$Age_26day), FUN=mean,
                     na.rm=TRUE)
plot2a <- ggplot(aggdata, aes(x=Group.1, y= dui_r))+
  geom_point()+
  xlab("Age at Time of Arrest")+
  ylab("Arrest Counts")+
  xlim(17,25)+
  theme_bw()
plot1b

#Between 20.5 and 21.5
plot2b <- ggplot(aggdata, aes(x=Group.1, y= dui_r))+
  geom_point()+
  xlab("Age at Time of Arrest")+
  ylab("Arrest Counts")+
  xlim(20.5,21.5)+
  theme_bw()
plot2b

#Between 20 and 22
plot2c <- ggplot(aggdata, aes(x=Group.1, y= dui_r))+
  geom_point()+
  xlab("Age at Time of Arrest")+
  ylab("Arrest Counts")+
  xlim(20,22)+
  theme_bw()
plot3b

#Between 19 and 23
plot2d <- ggplot(aggdata, aes(x=Group.1, y= dui_r))+
  geom_point()+
  xlab("Age at Time of Arrest")+
  ylab("Arrest Counts")+
  xlim(19,23)+
  theme_bw()
plot2d

grid.arrange(plot2a, plot2b, plot2c, plot2d, ncol=2, main="Choosing Bandwidth")

#### Combined plot that looks at different ranges of the outcome variable ####

#0 to 400
aggdata <- aggregate(crime1, by=list(crime1$Age_26day), FUN=mean,
                     na.rm=TRUE)
plot3a <- ggplot(aggdata, aes(x=Group.1, y= dui_r))+
  geom_point()+
  xlab("Age at Time of Arrest")+
  ylab("Arrest Counts")+
  xlim(19,23)+
  ylim(0,400)+
  theme_bw()
plot3a

#180 - 250
plot3b <- ggplot(aggdata, aes(x=Group.1, y= dui_r))+
  geom_point()+
  xlab("Age at Time of Arrest")+
  ylab("Arrest Counts")+
  xlim(19,23)+
  ylim(180,250)+
  theme_bw()
plot3b

#160 - 260
plot3c <- ggplot(aggdata, aes(x=Group.1, y= dui_r))+
  geom_point()+
  xlab("Age at Time of Arrest")+
  ylab("Arrest Counts")+
  xlim(19,23)+
  ylim(160,260)+
  theme_bw()
plot3c

#135, 270
plot3d <- ggplot(aggdata, aes(x=Group.1, y= dui_r))+
  geom_point()+
  xlab("Age at Time of Arrest")+
  ylab("Arrest Counts")+
  xlim(19,23)+
  ylim(135,270)+
  theme_bw()
plot3d

grid.arrange(plot3a, plot3b, plot3c, plot3d, ncol=2, main="Choosing Outcome Range")

# Need to put in regression lines
# We want to run the regressions on the non-binned data
#Let's limit the forcing variable (Age) from 19 to 23
crime2 <- subset(crime1, Age>=19 & Age <=23)
attach(crime2)

#Dummy variables for over 21 or not
z = ifelse(crime2$Age >=21, 1, 0)

#Create other variables
#Age centered
agec = crime2$Age - 21
#agec squared
agec_sq = agec^2
agec_post = agec*z
agec_sq_post = agec_sq*z
agec_cu = agec^3
agec_cu_post = agec_cu*z

#Indicator for birthday
birthday = ifelse(days_to_21==0, 1, 0)

#Run these regressions
reglinear = lm(dui_r ~ z + agec + agec_post)
reglinear_bday = lm(dui_r ~ z + agec + agec_post + birthday)

regquad = lm(dui_r ~ z + agec + agec_post + agec_sq + agec_sq_post)
regquad_bday = lm(dui_r ~ z + agec + agec_post + agec_sq + 
                    agec_sq_post + birthday)

regcubic = lm(dui_r ~ z + agec + agec_post + agec_sq + agec_sq_post +
                agec_cu + agec_cu_post)

regcubic_bday = lm(dui_r ~ z + agec + agec_post + agec_sq + agec_sq_post +
                     agec_cu + agec_cu_post + birthday)

library(stargazer)
stargazer(reglinear, reglinear_bday, regquad, regquad_bday,
          regcubic, regcubic_bday, type="text")

# Want to get the regressions on the plots
aggdata <- aggregate(crime2, by=list(crime2$Age_26day), FUN=mean,
                     na.rm=TRUE)
attach(aggdata)
#Dummy variables for over 21 or not
z = ifelse(aggdata$Age >=21, 1, 0)

#Create other variables
#Age centered
agec = aggdata$Age - 21
#agec squared
agec_sq = agec^2
agec_post = agec*z
agec_sq_post = agec_sq*z
agec_cu = agec^3
agec_cu_post = agec_cu*z

#Indicator for birthday
birthday = ifelse(days_to_21==0, 1, 0)

#Get the predicted values from the regression
pred_arrest_quad = predict(lm(dui_r ~ z + agec + agec_post + agec_sq+
                                agec_sq_post))

#Pull this all together
crime3 <- cbind(aggdata, z,agec, agec_sq, agec_post, agec_sq_post,
                birthday, pred_arrest_quad)
attach(crime3)

#Create the plot

# Add the fitted regression lines
#subset
sub1 <- subset(crime3, Age>=21)
sub2 <- subset(crime3, Age<21)

#Plot
plot4 <- ggplot(aggdata, aes(x=Group.1, y= dui_r))+
  geom_point()+
  geom_line(data = sub1, aes(x= Group.1, y=pred_arrest_quad),color="red")+
  geom_line(data = sub2, aes(x= Group.1, y=pred_arrest_quad),color="red")+
  xlab("Age at Time of Arrest")+
  ylab("Arrest Counts")+
  ggtitle("Age Profile of DUI Arrests")+
  xlim(19,23)+
  ylim(135,270)+
  theme_bw()
plot4
