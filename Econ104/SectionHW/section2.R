#### Section 2 ####

#### Set up same as before ####

voting <- read.csv("~/Desktop/voting.csv")
attach(voting)

#turn of scientific notation
options(scipen=999)

#### Treatment Effects ####

# Simple regression
reg1 <- lm(vote02 ~ treat_real)
summary(reg1)
#This gives the difference in mean voting rates for the treatment and control groups

# Question then, is this causal?

# Adding covariates one by one
reg2 <- lm(vote02 ~ treat_real + age)
summary(reg2)
reg3 <- lm(vote02 ~ treat_real + age + vote98)
summary(reg3)

# Test the hypothesis that people that were called were called were more likely to vote

# A significance level tells us how willing we are to reject the null hypothesis
# when it is actually true. 5 percent pretty standard. This means if the null is true
# we will incorrectly reject it 5 percent of the time, just by chance.

# The null hypothesis is beta1= 0. Specifically, this says that we think there is no difference
# in voting rates between the treatment and control groups.

# The alterative hypothesis is that beta1 ≠ 0. In words, there is a difference in voting rates
# between treatment and control groups. 

# Ran the regression and get some estimate for beta1. Then we do a t-test. To get the t-stat:
tstat <- (0.035070/0.002867)
tstat
# The ciritical value for 95% significance is 1.96. So this is very significant.

# Interpreting the p-value
# The pvalue is the probability of getting a value as big or larger than the one
# we got (in absolute value) 
# So this is very significant. 

# Star system
# three stars *** --> Signficant at the 99 % percent level. The p-val is less than .01
# two stars ** --> 95%, p-val less than 0.05
# one star * --> 90%, p-val less than 0.1

#### Making Tables Look Nicer ####

library(stargazer)
stargazer(reg1, reg2, reg3, type="text", align=T,
          covariate.labels=c("Treatment", "Age", "Vote in 1998"),
          omit.stat=c("f","ser"), title="Something Descriptive")

# For next week download latex and lyx --> lyx.org
