#Load required libraries and read xls file
library(psych)
library(car)
library(readxl)
Cleaned_data601 <- read_excel("Cleaned Graduation Factors Data for MD, VA, DC by year, 2015-2021.xlsx")

#-------------------------------------------------------------------------------
# Linear Model: Graduation Rate vs Unemployment Rate
#-------------------------------------------------------------------------------
#Create Linear Model
lmUnemployment = lm(CountyValue ~ Unemp, data = Cleaned_data601, , ,na.omit)

#Check Assumptions: Linearity, some outliers may be a concern but appears linear
plot(Cleaned_data601$Unemp, Cleaned_data601$CountyValue,
     pch=20, col = "blue",
     xlab= "Unemployment Rate", ylab="Graduation Rate",
     main="Graduation Rate vs Unemployment Rate")

#Graphics for Normality Assumption, not perfect but not obvioulsy violated
hist(lmUnemployment$residuals, xlab="Residuals", main = "Historgram of Residuals")
qqnorm(lmUnemployment$residuals)

#Graphics for Independence Assumption, seems random so no violation
plot(lmUnemployment$residuals, type = 'o',
     xlab = "Case number", ylab = "Residual")

#Graphics/Levenes for Homoscedacity Assumption, this is a problem becuase it is 
#statistically different so Homoscedacity is not met
bin <- cut(lmUnemployment$fitted.values, 5)
leveneTest(lmUnemployment$residuals ~ bin)

#Summary of linear Model
summary(lmUnemployment)

#Visualization of LM on scatter plot
plot(Cleaned_data601$Unemp, Cleaned_data601$CountyValue,
     pch=20, col = "blue",
     xlab= "Unemployment Rate", ylab="Graduation Rate",
     main="=Graduation Rate vs Unemployment Rate")
abline(lmUnemployment)


#-------------------------------------------------------------------------------
# Linear Model: Graduation Rate vs Per Pupil Spending
#-------------------------------------------------------------------------------
#Create Linear Model
lmPPspend = lm(CountyValue ~ PPCSTOT, data = Cleaned_data601, , ,na.omit)

#Check Assumptions: Linearity, unclear relationship 
plot(Cleaned_data601$PPCSTOT, Cleaned_data601$CountyValue,
     pch=20, col = "blue",
     xlab= "Per Pupil Spending", ylab="Graduation Rate",
     main="Graduation Rate vs Per Pupil Spending")

#Graphics for Normality Assumption, skewed left, not normal
hist(lmPPspend$residuals, xlab="Residuals", main = "Historgram of Residuals")
qqnorm(lmPPspend$residuals)

#Graphics for Independence Assumption, seems random so no violation
plot(lmPPspend$residuals, type = 'o',
     xlab = "Case number", ylab = "Residual")

#Graphics/Levenes for Homoscedacity Assumption, this is a problem becuase it is 
#statistically different so Homoscedacity is not met
bin <- cut(lmPPspend$fitted.values, 5)
leveneTest(lmPPspend$residuals ~ bin)

#Summary of linear Model
summary(lmPPspend)

#Visualization of LM on scatter plot
plot(Cleaned_data601$PPCSTOT, Cleaned_data601$CountyValue,
     pch=20, col = "blue",
     xlab= "Unemployment Rate", ylab="Graduation Rate",
     main="Graduation Rate vs Per Pupil Spending")
abline(lmPPspend)

#-------------------------------------------------------------------------------
# Linear Model: Graduation Rate vs % of individuals with 3+ risk factors
# Community Resilience Estimates
#-------------------------------------------------------------------------------

lmrisk3 = lm(CountyValue ~ rate3, data = Cleaned_data601, , ,na.omit)

#Summary of linear Model
summary(lmrisk3)

#Visualization of LM on scatter plot
plot(Cleaned_data601$rate3, Cleaned_data601$CountyValue,
     pch=20, col = "blue",
     xlab= "% of population with 3+ risk factors", ylab="Graduation Rate",
     main="Graduation rate vs % of population with 3+ risk factors")
abline(lmrisk3)
