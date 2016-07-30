# read in data
nhis = read.csv("data-examples/Econ104/Data/HW3/NHIS_Data.csv", header = TRUE)
attach(nhis)
mort = read.csv("data-examples/Econ104/Data/2_age_mortality/mortality_data.csv", header = TRUE)
attach(mort)

# bin - 30 days
nhis$bin30 = floor(days_21/30)
attach(nhis)
nhis2 = aggregate(nhis, by = list(bin30), FUN = mean)
attach(nhis2)

# create threshold where treatment probability changes
nhis3 = subset(nhis2, drinks_alcohol > 0 & drinks_alcohol < 1)
attach(nhis3)

# create an age column
nhis3$age = days_21/365 + 21
attach(nhis3)

# create an column that contains 1 if a person is over 21 and 0 if they are under 21
nhis3$over21 = ifelse(days_21 >= 0, 1, 0)
attach(nhis3)

# create variables for polynomial regressions
nhis3$agesq = age^2
nhis3$agecu = age^3
nhis3$age21 = age*over21
nhis3$agesq21 = age^2*over21
nhis3$agecu21 = age^3*over21
attach(nhis3)

# linear regression
lm1 = lm(drinks_alcohol ~ age + over21 + age21)
coef1 = coefficients(lm1)
line1 = curve(coef1[1] + coef1[2]*x, from=18, to=21)
line2 = curve(coef1[1] + coef1[3] + (coef1[2] + coef1[4])*x, from=21, to=30)

lm2 = lm(drinks_alcohol ~ age + over21 + age21 + agesq + agesq21)
coef2 = coefficients(lm2)
line3 = curve(coef2[1] + coef2[2]*x + coef2[5]*x*x, from=18, to=21)
line4 = curve(coef2[1] + coef2[3] + (coef2[2] + coef2[4])*x + (coef2[5] + coef2[6])*x*x, from=21, to=30)

lm3 = lm(drinks_alcohol ~ age + over21 + age21 + agesq + agesq21 + agecu + agecu21)
coef3 = coefficients(lm3)
line5 = curve(coef3[1] + coef3[2]*x + coef3[5]*x*x + coef3[7]*x*x*x, from=18, to=21)
line6 = curve(coef3[1] + coef3[3] + (coef3[2] + coef3[4])*x + (coef3[5] + coef3[6])*x*x + (coef3[7] + coef3[8])*x*x*x, from=21, to=30)

par(mfrow=c(2,2))
plot(x = age, y = drinks_alcohol, cex = 0.6, xlim = (c(19,23)), ylim = (c(0.45,0.75)), ylab = "Drinks Alcohol", xlab = "Age", main = "Linear Regression")
lines(line1)
lines(line2)
abline(v = 21, lty = 3, col = 2)
plot(x = age, y = drinks_alcohol, cex = 0.6, xlim = (c(19,23)), ylim = (c(0.45,0.75)), ylab = "Drinks Alcohol", xlab = "Age", main = "Quadratic Regression")
lines(line3)
lines(line4)
abline(v = 21, lty = 3, col = 2)
plot(x = age, y = drinks_alcohol, cex = 0.6, xlim = (c(19,23)), ylim = (c(0.45,0.75)), ylab = "Drinks Alcohol", xlab = "Age", main = "Cubic Regression")
lines(line5)
lines(line6)
abline(v = 21, lty = 3, col = 2)
