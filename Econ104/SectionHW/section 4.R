#### Section 4 ####

## IV ##

#Picked half the phone book to get a phone call
#Did everyone in that group talk to an experimenter?
#No

#Estimate the first stage
#What is the effect of being assigned to treatment on treatment status?
#What is the effect of being assigned to get a phone call,
# on actually talking to a experimenter?
attach(voting)
options(scipen=999)
reg1 <- lm(contact ~ treat_real)
summary(reg1)

#How much being assigned to get the treatment affects whether
# or not people got the treatment

# What if there is perfect compliance? Zi = Di

## Reduced form / Intention to Treat (ITT)
reg2 <- lm(vote02 ~ treat_real)
summary(reg2)

# How much does being assigned to the treatment group affect 
# the outcome variable (voting here). How many more people 
# in the treatment voted compared to the control group

# Now get the IV estimate - This is the effect of being contacted
# on voting. How actually getting the treatment on the outcome

IV <- 0.035070/0.4175782031621806


#Reduced form divided by the first stage

#Why might this be a problem? This is simply a rescaling. What if the first stage is really small?
IV2 <- 0.035070/.2
IV3 <- 0.035070/.11
IV4 <- 0.035070/.10


# The IV estimate is about 8 percent
# This a local average treatment effect (LATE)
# This is the effect of getting encouragement to vote over the 
# phone for the type of person that answers the phone
