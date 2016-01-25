## Section 5 ##

### Set Up ###

#Import data
mort_data <- read.csv("http://people.ucsc.edu/~cdobkin/mortality%20data.csv", header=TRUE)

#Attach data
attach(mort_data)

#Turn off scientific
options(scipen=999)

#Look over the dataset
summary(mort_data)

## Setting up the RD graph ##

#Create variables centered at 0, for pre and post
Z = ifelse(Age>=21,1,0)

#Create a polynomial in age
agec = Age - 21
agec_sq = (Age-21)^2
agec_post = agec*Z

agec_sq_post = agec_sq*Z
agec_cu = (Age-21)^3
agec_cu_post = agec_cu*Z

#run a regression with the centered age variable as the 
# indepedent variable
reg1 <- lm(MVA ~ Z + agec + agec_sq + agec_post + agec_sq_post)

## Getting the predicted values ##
pred_MVA_linear <- predict(lm(MVA~ Z + agec +agec_post))
pred_MVA_quad <- predict(lm(MVA~ Z + agec + agec_post + agec_sq + agec_sq_post))
pred_MVA_cubic <- predict(lm(MVA~ Z + agec + agec_post + agec_sq + agec_sq_post + agec_cu + agec_cu_post))

#Bundle together
mort1 <- cbind(mort_data, Z, agec, agec_sq, agec_post, 
               agec_sq_post, agec_cu, agec_cu_post, 
               pred_MVA_linear, pred_MVA_quad, pred_MVA_cubic)

#Subset the data
attach(mort1)
sub1 <- subset(mort1, Age>=21)
sub2<- subset(mort1, Age<21)

#### Graphing with ggplot2 ####

library(ggplot2)
#make the figure

#this is the color the points will be
colors= c("black")

plot1 <- ggplot(mort1, aes(Age, MVA)) +
  #points and lines
  geom_point(aes(color="black")) +      
  geom_line(data=sub1, aes(Age, pred_MVA_cubic), color="red") +   
  geom_line(data=sub2, aes(Age, pred_MVA_cubic), color="red") +
  
  #vertical and horizontal limits
  xlim(19,23)+
  ylim(25,40)+
  
  #labels, legend, and title
  xlab("Age at Time of Death") +
  ylab("Mortality Rate Per 100,000") +
  ggtitle("Age Profile of Mortality") +
  theme_bw() +
  scale_color_manual(name = "Legend",
                     labels = c("MVA"),
                     values = colors)+
  theme(legend.position = "bottom",legend.text=element_text(size=16)) 
plot1

