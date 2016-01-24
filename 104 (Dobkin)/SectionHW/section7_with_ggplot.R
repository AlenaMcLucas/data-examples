#### Section 7 ####

NHIS <- read.csv("http://people.ucsc.edu/~cdobkin/NHIS%20Data.csv")
attach(NHIS)
options(scipen=999)

#Create bins just like in our last homework

Age = 21 + days_21/365
Age_1day = 21 + days_21/365
Age_4day = 21 + 4*floor(days_21/4)/365 + 2/365
Age_8day = 21 + 8*floor(days_21/8)/365 + 4/365
Age_16day = 21 + 16*floor(days_21/16)/365 + 8/365
Age_26day = 21 + 26*floor(days_21/26)/365 + 13/365
Age_32day = 21 + 32*floor(days_21/32)/365 + 16/365
Age_50day = 21 + 50*floor(days_21/50)/365 + 25/365

#group this together
NHIS1 <- cbind(NHIS, Age, Age_4day,  Age_8day, Age_16day, Age_26day
               ,Age_32day, Age_50day)
attach(NHIS1)

#Create variable, again just like the previous hw
z = ifelse(Age>=21,1,0)
agec = Age - 21
agec_sq = agec^2
agec_cu = agec^3
agec_post = agec*z
agec_sq_post = agec_sq*z
agec_cu_post = agec_cu*z
birthday = ifelse(days_21 == 0, 1, 0)

#group this all together

NHIS2 <- cbind(NHIS1, z, agec, agec_sq, agec_cu, agec_post,
               agec_sq_post, agec_cu_post , birthday)


## Question 1 - Comparison of Means 
#Compare people just over 21 and just under 21

#We know the range from the last hw
NHIS3 = subset(NHIS2, Age>=19 & Age<=23)
attach(NHIS3)

reg.HS_diploma = lm(HS_Diploma ~ z + agec + agec_post, data=NHIS3)
reg.hispanic = lm(hispanic ~ z + agec+ agec_post, data=NHIS3)
#etc
#do this for all characteristics

library(stargazer)
stargazer(reg.HS_diploma, reg.hispanic, align=T,type="text", 
          float.env="table", title= "Summary Statistics
          Around Age Cutoff", omit.stat=c("ser","f","adj.rsq")
)

## Question 2 - death graphs

mort <- read.csv("http://people.ucsc.edu/~cdobkin/mortality%20data.csv")
attach(mort)

#make the same variables as before
z = ifelse(mort$Age >= 21,1,0)
agec= mort$Age -21
agec_sq = agec^2
agec_cu = agec^3
agec_post = agec*z
agec_sq_post = agec_sq*z
agec_cu_post = agec_cu*z
birthday = ifelse(mort$Age>=21 & mort$Age<=21 + 31/365, 1, 0)

#Get the predicted values of the regression
pred_mort_linear = predict(lm(All ~ z + agec + agec_post))

pred_mort_quadratic = predict(lm(All ~ z + agec + agec_post +
                                   agec_sq + agec_sq_post))

pred_mort_cubic = predict(lm(All~ z + agec+ agec_post +
                               agec_sq + agec_sq_post+
                               agec_cu + agec_cu_post))

#group it all together
mort2 <- cbind(mort, pred_mort_linear, pred_mort_quadratic, 
               pred_mort_cubic)
attach(mort2)

#subset the data
sub1<- subset(mort2, Age>=21)
sub2 <- subset(mort2, Age<21)

#Create the plot
library(ggplot2)

#this is the color the points will be
colors= c("black")

plot1 <- ggplot(mort2, aes(Age, All)) +
  #points and lines
  geom_point(aes(color="black")) +      
  geom_line(data=sub1, aes(Age, pred_mort_cubic), color="red") +   
  geom_line(data=sub2, aes(Age, pred_mort_cubic), color="red") +
  #Can also do this for pred_mort_quadratic/pred_mort_cubic
  
  #vertical and horizontal limits
  xlim(19,23)+
  ylim(85,105)+
  
  #labels, legend, and title
  xlab("Age at Time of Death") +
  ylab("Mortality Rate Per 100,000") +
  ggtitle("Age Profile of Mortality, All Causes") +
  theme_bw() +
  scale_color_manual(name = "Legend",
                     labels = c("All Causes"),
                     values = colors)+
  theme(legend.position = "bottom",legend.text=element_text(size=16)) 
plot1


## Question 3 - MVA
attach(mort)

#MVA
pred_mva_linear = predict(lm(MVA ~ z + agec + agec_post))

pred_mva_quadratic = predict(lm(MVA ~ z + agec + agec_post +
                                  agec_sq + agec_sq_post))

pred_mva_cubic = predict(lm(MVA~ z + agec+ agec_post +
                              agec_sq + agec_sq_post+
                              agec_cu + agec_cu_post))

#Alcohol
pred_alcohol_linear = predict(lm(Alcohol ~ z + agec + agec_post))

pred_alcohol_quadratic = predict(lm(Alcohol ~ z + agec + agec_post +
                                      agec_sq + agec_sq_post))

pred_alcohol_cubic = predict(lm(Alcohol~ z + agec+ agec_post +
                                  agec_sq + agec_sq_post+
                                  agec_cu + agec_cu_post))

mort2 <- cbind(mort, z, agec, agec_sq, agec_cu, agec_post,
               agec_sq_post, agec_cu_post, birthday,pred_mva_linear,
               pred_mva_quadratic, pred_mva_cubic,
               pred_alcohol_linear,pred_alcohol_quadratic,
               pred_alcohol_cubic)


#subset the data
sub1 <- subset(mort2, Age>=21)
sub2 <- subset(mort2, Age<21)

#this is the color the points will be
colors= c("brown","grey")

plot2 <- ggplot(mort2, aes(Age, MVA)) +
  #points and lines, MVA
  geom_point(aes(color="brown")) +      
  geom_line(data=sub1, aes(Age, pred_mva_cubic), color="red") +   
  geom_line(data=sub2, aes(Age, pred_mva_cubic), color="red") +
  
  #points and lines, Alcohol
  geom_point(aes(Age, Alcohol, color="grey")) +      
  geom_line(data=sub1, aes(Age, pred_alcohol_cubic), color="blue") +   
  geom_line(data=sub2, aes(Age, pred_alcohol_cubic), color="blue") +
  
  #vertical and horizontal limits
  xlim(19,23)+
  
  #labels, legend, and title
  xlab("Age at Time of Death") +
  ylab("Mortality Rate Per 100,000") +
  ggtitle("Age Profile of Mortality") +
  theme_bw() +
  scale_color_manual(name = "Legend",
                     labels = c("MVA","Alcohol"),
                     values = colors)+
  theme(legend.position = "bottom",legend.text=element_text(size=16)) 
plot2

#Do this for the rest of the causes of death

#Question 4 - regressions

attach(mort2)
reg.All = lm(All ~ z + agec + agec_post + agec_sq + agec_sq_post + birthday)
reg.External = lm(External ~ z + agec + agec_post + agec_sq + agec_sq_post + birthday)
reg.Internal = lm(Internal ~ z + agec + agec_post + agec_sq + agec_sq_post + birthday)
#Continue running regressions with your preferred specification for each of the outcomes then make a table
#with your preferred method of table making.

## Questions 6 and 7 - IV estimates
#We know some people changed their behavior at 21

firststage = lm(drinks_alcohol ~ z + agec+ agec_post + 
                  birthday, data=NHIS3 )
reducedform = lm(All ~ z + agec + agec_post + birthday,
                 data=mort2)

#grab the coefficient from the first stage
phi = coef(summary(firststage))["z","Estimate"]
#grab the coeffcient from the reduced form
rho = coef(summary(reducedform))["z","Estimate"]

#standard error from first stage
se.phi = coef(summary(firststage))["z","Std. Error"]
#standard error from reduced form
se.rho = coef(summary(reducedform))["z","Std. Error"]

rho_sq = rho^2
phi_4 = phi^4

var.phi = se.phi^2
var.rho = se.rho^2
phi_sq= phi^2

iv.var = ((rho_sq/phi_4)*var.phi)+ ((1/phi_sq)*var.rho)
iv.se = sqrt(iv.var)
iv.coef = rho/phi
print(iv.coef)
print(iv.se)
