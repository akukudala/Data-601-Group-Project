#-------------------------------------------------------------------------------
# Semester Project: Introduction to Data Science
# Autor: Jessica Reynolds
#-------------------------------------------------------------------------------

#Load required libraries and read xls file
library(psych)
library(car)
library(readxl)
Cleaned_data601<- read_excel("Cleaned Graduation Factors Data for MD, VA, DC by year, 2015-2021.xlsx")
#Removed unwanted Characters/Spaces from Column Names
names(Cleaned_data601) <- gsub(" ", "", names(Cleaned_data601))
names(Cleaned_data601) <- gsub("%", "", names(Cleaned_data601))

riskdf <- read_excel("2019 Risk Factors Data.xlsx", 
                                      sheet = "2019")
names(riskdf)<- gsub(" ", "", names(riskdf))

lmpp2019 = lm(CountyValue ~ PPCSTOT, data = riskdf, , ,na.omit)
summary(lmpp2019)
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

#Outliers
qqPlot(lmUnemployment,labels=row.names(Unemp), id.method="identify",
       simulate=TRUE, main="Q-Q Plot") #shows row 310 and 344 are potential outliers
outlierTest(lmUnemployment)

Cleaned_data601[c(310,344),]

#Summary of linear Model
summary(lmUnemployment)

#Visualization of LM on scatter plot
plot(Cleaned_data601$Unemp, Cleaned_data601$CountyValue,
     pch=20, col = "blue",
     xlab= "Unemployment Rate", ylab="Graduation Rate",
     main="Graduation Rate vs Unemployment Rate")
abline(lmUnemployment)

#-------------------------------------------------------------------------------
# Linear Model: Graduation Rate vs Unemployment Rate Outliers removed
#-------------------------------------------------------------------------------
#Create Linear Model
df <- Cleaned_data601[which(Cleaned_data601$County!="Worcester County"),]
lmdf=lm(CountyValue ~ Unemp, data = df, , ,na.omit)

#Check Assumptions: Linearity, some outliers may be a concern but appears linear
plot(df$Unemp, df$CountyValue,
     pch=20, col = "blue",
     xlab= "Unemployment Rate", ylab="Graduation Rate",
     main="Graduation Rate vs Unemployment Rate")

#Graphics for Normality Assumption, not perfect but not obvioulsy violated
hist(lmdft$residuals, xlab="Residuals", main = "Historgram of Residuals")
qqnorm(lmdf$residuals)

#Graphics for Independence Assumption, seems random so no violation
plot(lmdf$residuals, type = 'o',
     xlab = "Case number", ylab = "Residual")

#Graphics/Levenes for Homoscedacity Assumption, this is a problem becuase it is 
#statistically different so Homoscedacity is not met
bin <- cut(lmdf$fitted.values, 5)
leveneTest(lmdf$residuals ~ bin)

#Outliers
qqPlot(lmdf,labels=row.names(CountyValue), id.method="identify",
       simulate=TRUE, main="Q-Q Plot") #shows row 340 and 306 are potential outliers
outlierTest(lmUnemployment)

Cleaned_data601[c(306,340),]#looks okay

#Summary of linear Model
summary(lmdf)

#Visualization of LM on scatter plot
plot(df$Unemp, df$CountyValue,
     pch=20, col = "blue",
     xlab= "Unemployment Rate", ylab="Graduation Rate",
     main="Graduation Rate vs Unemployment Rate")
abline(lmdf)


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

#including both variables in the model does not improve the correlation
lmboth = lm(CountyValue ~ PPCSTOT + Unemp, data = Cleaned_data601, , ,na.omit)
summary(lmboth)
#-------------------------------------------------------------------------------
# Linear Model: Graduation Rate vs % of individuals with 3+ risk factors
# Community Resilience Estimates
#-------------------------------------------------------------------------------

lmrisk3 = lm(CountyValue ~ rate3, data = riskdf, , ,na.omit)

#Check Assumptions: Linearity
plot(riskdf$rate3, riskdf$CountyValue,
     pch=20, col = "blue",
     xlab= "Estimated % of Population with 3+ Risk Factors", ylab="Graduation Rate",
     main="Graduation rate vs % of population with 3+ risk factors")

#Graphics for Normality Assumption, close to normal
hist(lmrisk3$residuals, xlab="Residuals", main = "Historgram of Residuals")
qqnorm(lmrisk3$residuals)

#Graphics for Independence Assumption, seems random so no violation
plot(lmrisk3$residuals, type = 'o',
     xlab = "Case number", ylab = "Residual")

#Graphics/Levenes for Homoscedacity Assumption, met
bin <- cut(lmrisk3$fitted.values, 5)
leveneTest(lmrisk3$residuals ~ bin)

#Summary of linear Model
summary(lmrisk3)

#Visualization of LM on scatter plot
plot(riskdf$rate3, riskdf$CountyValue,
     pch=20, col = "blue",
     xlab= "% of population with 3+ risk factors", ylab="Graduation Rate",
     main="Graduation rate vs % of population with 3+ risk factors")
abline(lmrisk3)
