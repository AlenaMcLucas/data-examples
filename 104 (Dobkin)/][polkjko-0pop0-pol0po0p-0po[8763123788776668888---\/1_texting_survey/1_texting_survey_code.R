### 2 ###
texting <- read.csv(file="/Users/alenamclucas/Dropbox/ECON/104(Dobkin)/Data/1_texting_survey/texting_survey.csv",head=TRUE,sep=",")
attach(texting)

### 3 ###
library(extrafont)
font_install('fontcm')
loadfonts()

pdf("plot_cm.pdf", family="CM Roman", width=5, height=5)
hist(Male,main = "Figure 3.1 - Male Distribution")
dev.off()
embed_fonts("plot_cm.pdf", outfile="plot_cm_embed.pdf")

hist(Texts_class,breaks = 50,main = "Figure 3.2 - Texts per Class Distribution")
hist(GPA,breaks = 50,main = "Figure 3.3 - GPA Distribution")

plot(Texts_class,GPA, pch=21, bg="blue",xlab="Texts per Class",ylab ="GPA for Quarter",main="Correlation Between Texting and GPA")

### 4 ###
reg1 <- lm(GPA ~ Texts_class)
summary(reg1)
reg1_f <- predict(reg1)
lines(Texts_class,reg1_f)
