#Read data into R - drag and drop file to get path, read it in and write it to the file texting
texting <- read.csv(file="C:\\Teaching\\Econ 104 Spring 2015\\Lecture 02\\texting_survey.csv",head=TRUE,sep=",")

#Attach it - this way you dont need to refere to variables via full path
attach(texting)

#Display data does it look right

#Check distributions - do they look right, what does this tell us
hist(Male, main = "Check")
hist(Texts_class,breaks = 50)
hist(GPA,breaks = 50)

#Plot the data
plot(Texts_class,GPA, pch=21, bg="blue",xlab="Texts Per Class",ylab ="GPA for Quarter",main="Correlation Between Texting and GPA")

#Run regression
reg1 <- lm(GPA ~ Texts_class)
#Get details on regression
summary(reg1)
#Get fitted value of regression
reg1_f <- predict(reg1) 

#Put regression line in
lines(Texts_class,reg1_f) 

