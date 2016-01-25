# Section 1

# Comments start with a # 

# USE A LOT OF COMMENTS

#### SET UP #####

#Set the directory. This is where the data and 
# script files are. Can also set this in R-studio.

setwd("/Users/annikarose/Desktop")

#Import the data. Use header=true for variable names

voting_data <- read.csv("voting.csv", header=TRUE)

# Attach the data so you don't have to keep referring
# to it
attach(voting_data)
 
#### Comparison of means table (balance table) #1 on HW ####

#Turn off scientific notation
options(scipen =999)

# Split the data into treatment and control groups
treatment = subset(voting_data, treat_real==1)
control = subset(voting_data, treat_real==0)

#Perform a t-test to obtain means of treatment and 
#control groups

print(t.test(control$vote98,treatment$vote98))

# Perform regressions to obtain difference in means

reg.vote98 <- lm(vote98 ~ treat_real)
summary(reg.vote98)

# Same info as t-test

#For age
print(t.test(control$age, treatment$age))
reg.age <- lm(age ~ treat_real)
summary(reg.age)

# There is a function to do all of this at once
install.packages("devtools")
library(devtools)
source_url("http://is.gd/balancetable")

#make the table. Can also use the file ="something" option to save as a csv file
mytable <- balance.table(voting_data, treat_real, age, female, vote98)

#### Treatment effects via regressions ####

reg.vote02 <- lm(vote02 ~ treat_real)
summary(reg.vote02)

#Adding covariates one by one (see #5 on hw)
reg1 <- lm(vote02 ~treat_real + vote98)
summary(reg1)
reg2 <- lm(vote02 ~treat_real + vote98 + age)
summary(reg2)
reg3 <- lm(vote02 ~treat_real + vote98 + age + female)
summary(reg3)


#Stargazer - makes the tables for you. Will discuss further next week
library(stargazer)
stargazer(reg.vote98, type="text", align=TRUE)

#Another way to get robust SE - sandwich package
library(sandwich) 
#Now get the correct SE. Replace 'reg1' with the regression you need the errors for
rse.regvote02 <- diag(vcovHC(reg.vote02, type = "HC"))^0.5
rse.reg1 <- diag(vcovHC(reg1, type = "HC"))^0.5
rse.reg2 <- diag(vcovHC(reg2, type = "HC"))^0.5
rse.reg3 <- diag(vcovHC(reg3, type = "HC"))^0.5
#Then you can run stargazer with these standard errors

stargazer(reg.vote02, reg1, reg2, reg3, se=list(rse.regvote02, rse.reg1, rse.reg2, rse.reg3), type="text", align=T)


