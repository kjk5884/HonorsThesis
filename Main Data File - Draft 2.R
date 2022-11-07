#require(baseballr)
#require(readr)
library(ggplot2)
require(dplyr)
library(olsrr)
library(lmtest)
library(MASS)
library(Hmisc)
library(car)

performanceandSalary <- read.csv("~/Thesis Research/performanceandSalary.csv")
correlationTable <- performanceandSalary[c(5:6,8:10)]
rosterwSalary <- read.csv("~/Thesis Research/rosterwSalary.csv")
smallMarketData <- filter(performanceandSalary, (MarketSize) == "Small")

# Initial Model
predictionModelFull <- lm(data=performanceandSalary,W.L.~(payrollMil)+playersto50+tenmilplayers+adjServiceTime+playersto50sq+adjServiceTimeSq)
summary(predictionModelFull)
predictionModelFullSmall <- lm(data=smallMarketData,W.L.~(payrollMil)+playersto50+tenmilplayers+adjServiceTime+playersto50sq+adjServiceTimeSq)
summary(predictionModelFullSmall)

# Models with Highly Paid Players and Payroll
predictionModelHPP <- lm(data=performanceandSalary,W.L.~playersto50+adjServiceTime+playersto50sq)
summary(predictionModelHPP)

predictionModelPayroll <- lm(data=performanceandSalary,W.L.~playersto50+payrollMil+playersto50sq)
summary(predictionModelPayroll)

# Stepwise Model Selection
intercept_only <- lm(data=performanceandSalary,W.L.~1)
stepwiseHPP <- step(intercept_only, direction='both', scope=formula(predictionModelHPP), trace=0)
stepwiseHPP$anova
step(predictionModelHPP)

stepwisePayroll <- step(intercept_only, direction='both', scope=formula(predictionModelPayroll), trace=0)
stepwisePayroll$anova
stepAIC(predictionModelPayroll,direction="both")

# Final Models from Stepwise
finalModelHPP <- lm(data=performanceandSalary,W.L.~playersto50+adjServiceTime+playersto50sq)
summary(finalModelHPP)

finalModelPayroll <- lm(data=performanceandSalary,W.L.~playersto50+adjServiceTime+playersto50sq)
summary(finalModelHPP)

# VIF calculations
vifModelHPP<- lm(data=performanceandSalary,W.L.~playersto50+tenmilplayers+adjServiceTime)
vif(vifModelHPP)
vifModelPayroll<- lm(data=performanceandSalary,W.L.~playersto50+payrollMil+adjServiceTime)
vif(vifModelPayroll)

# Small Market Model
predictionModelSmall <- lm(data=smallMarketData,W.L.~playersto50+adjServiceTime+playersto50sq+adjServiceTimeSq)
predictionModelSmallFinal <- lm(data=smallMarketData,W.L.~playersto50+playersto50sq)
summary(predictionModelSmallFinal)
summary(predictionModelSmall)
step(predictionModelSmall, trace=F)
intercept_onlySmall <- lm(data=smallMarketData,W.L.~1)
stepwiseSmall <- step(intercept_onlySmall, direction='both', scope=formula(predictionModelSmall), trace=0)
anova(stepwiseSmall)
rcorr(as.matrix(correlationTable))
plot(predictionModelSmall$fitted,predictionModelSmall$residuals, ylab="Residuals", xlab="Fitted Values", main = "Residuals vs. Fits")
abline(0,0)

qqnorm(predictionModelSmall$residuals)
qqline(predictionModelSmall$residuals)
shapiro.test(predictionModelSmall$residuals) # p = 0.4871, no evidence of deviating from normal dist
bptest(predictionModelSmall) # p = 0.05572 - no evidence of nonconstant variance

# Check Linearity - Overall Model

# Payroll - Significant
payrollModel <- lm(data=performanceandSalary, W.L.~Payroll)
summary(payrollModel)
payrollModelSmall <- lm(data=smallMarketData, W.L.~Payroll)
summary(payrollModelSmall)
# Players to 50 - Significant
playersto50Model <- lm(data=performanceandSalary, W.L.~playersto50)
summary(playersto50Model) 
playersto50ModelSmall <- lm(data=smallMarketData, W.L.~playersto50)
summary(playersto50ModelSmall)
# Service Time - Significant
serviceTimeModel <- lm(data=performanceandSalary, W.L.~adjServiceTime)
summary(serviceTimeModel)
serviceTimeModelSmall <- lm(data=smallMarketData, W.L.~adjServiceTime)
summary(serviceTimeModelSmall)
# Highly Paid Players - Significant
hppModel <- lm(data=performanceandSalary, W.L.~tenmilplayers)
summary(hppModel)
hppModelSmall <- lm(data=smallMarketData, W.L.~tenmilplayers)
summary(hppModelSmall)

# Independent Errors Assumption

rcorr(as.matrix(correlationTable))
plot(predictionModelFull$fitted,predictionModelFull$residuals, ylab="Residuals", xlab="Fitted Values", main = "Residuals vs. Fits")
abline(0,0)

# Normal Residuals Assumption
qqnorm(predictionModelFull$residuals)
qqline(predictionModelFull$residuals)
shapiro.test(predictionModelFull$residuals) # p = 0.1687, no evidence of deviating from normal dist
ad.test(predictionModelFull$residuals) # p = 0.1211, no evidence of deviating from normal dist

# Constant Variance Assumption
bptest(predictionModelFull) # p = 0.4671 - no evidence of nonconstant variance

# Autocorrelation
dwtest(finalModelFull)

# Check Residuals vs Order - all look independent
plot(payrollModel$residuals, main="Residuals vs Order")

# Check Linearity - Small Market Model
# Payroll - Not Significant
payrollModelSmall <- lm(data=smallMarketData, W.L.~Payroll)
summary(payrollModelSmall)
# Players to 50 - Significant
playersto50ModelSmall <- lm(data=smallMarketData, W.L.~playersto50)
summary(playersto50ModelSmall) 
# Service Time - Significant
serviceTimeModelSmall<- lm(data=smallMarketData, W.L.~adjServiceTime)
summary(serviceTimeModelSmall)
# Highly Paid Players - Not Significant
hppModelSmall <- lm(data=smallMarketData, W.L.~tenmilplayers)
summary(hppModelSmall)


# Check Linearity - Overall Model
# Payroll
ggplot(data=performanceandSalary,aes(x=payrollMil,y=W.L.))+geom_point()+
  geom_smooth(method='lm', formula= y~x)+ ggtitle("Winning Percentage vs. Team Payroll (2012-2019)")+xlab("Payroll (in millions)")
# Players to 50
ggplot(data=smallMarketData,aes(x=playersto50,y=W.L.))+geom_point()+
  geom_smooth(method='lm', formula= y~x)+ ggtitle("Winning Percentage vs. Players to 50 (Small Markets, 2012-2019)")+xlab("Players to 50")
ggplot(data=performanceandSalary,aes(x=playersto50,y=W.L.))+geom_point()+
  geom_smooth(method='lm', formula= y~x)+ ggtitle("Winning Percentage vs. Players to 50 (2012-2019)")+xlab("Players to 50")
# Service Time
ggplot(data=performanceandSalary,aes(x=adjServiceTime,y=W.L.))+geom_point()+
  geom_smooth(method='lm', formula= y~x)+ ggtitle("Winning Percentage vs. Adjusted Service Time (2012-2019)")+xlab("Service Time")
ggplot(data=smallMarketData,aes(x=adjServiceTime,y=W.L.))+geom_point()+
  geom_smooth(method='lm', formula= y~x)+ ggtitle("Winning Percentage vs. Adjusted Service Time (Small Markets, 2012-2019)")+xlab("Service Time")
# Highly Paid Players
ggplot(data=performanceandSalary,aes(x=tenmilplayers,y=W.L.))+geom_point()+
  geom_smooth(method='lm', formula= y~x)+ ggtitle("Winning Percentage vs. Highly Paid Players (2012-2019)")+xlab("Highly Paid Players")

# Ten Million Players by Year Table
tenmilplayers <- performanceandSalary %>%
  group_by(year) %>%
  summarize(
    count = sum(tenmilplayers))

# Logistic Model
predictionModelLogPayroll <- glm(Playoffs ~ Payroll + adjServiceTime + playersto50, family = binomial, data = performanceandSalary)
predictionModelLogHPP <- glm(Playoffs ~ tenmilplayers + adjServiceTime + playersto50, family = binomial, data = performanceandSalary)
stepAIC(predictionModelLogHPP, trace=F)
predictionModelLogHPPFinal <- glm(Playoffs ~ adjServiceTime + playersto50, family = binomial, data = performanceandSalary)
summary(predictionModelLogHPPFinal)
stepAIC(predictionModelLogPayroll, trace=F)
predictionModelLogPayrollFinal <- glm(Playoffs ~ payrollMil+playersto50, family = binomial, data = performanceandSalary)
summary(predictionModelLogPayrollFinal)
