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


intercept_only <- lm(data=performanceandSalary,W.L.~1)
stepwiseHPP <- step(intercept_only, direction='both', scope=formula(predictionModelHPP), trace=0)
stepwiseHPP$anova

stepwisePayroll <- step(intercept_only, direction='both', scope=formula(predictionModelPayroll), trace=0)
stepwisePayroll$anova

predictionModelFull <- lm(data=performanceandSalary,W.L.~(payrollMil)+playersto50+tenmilplayers+adjServiceTime+playersto50sq+adjServiceTimeSq)
summary(predictionModelFull)

predictionModelHPP <- lm(data=performanceandSalary,W.L.~playersto50+tenmilplayers+adjServiceTime+playersto50sq+adjServiceTimeSq)
summary(predictionModelHPP)

predictionModelPayroll <- lm(data=performanceandSalary,W.L.~playersto50+payrollMil+playersto50sq)
summary(predictionModelPayroll)

finalModelHPP <- lm(data=performanceandSalary,W.L.~playersto50+adjServiceTime+playersto50sq)
summary(finalModelHPP)

finalModelPayroll <- lm(data=performanceandSalary,W.L.~playersto50+adjServiceTime+playersto50sq)
summary(finalModelHPP)

vifModel<- lm(data=performanceandSalary,W.L.~playersto50+payrollMil+adjServiceTime)
vif(vifModel)

ols_step_forward_p(predictionModelPayroll, prem = 0.05, details = TRUE)
ols_step_backward_p(predictionModelPayroll, prem = 0.05, details = TRUE)

both <- step(intercept_only, direction='both', scope=formula(all), trace=0)


predictionModelSmall <- lm(data=smallMarketData,W.L.~(payrollMil)+playersto50+tenmilplayers+adjServiceTime+playersto50sq+adjServiceTimeSq)
finalModelSmall <- lm(data=smallMarketData,W.L.~payrollMil+playersto50+tenmilplayers+playersto50sq)

# Check Linearity - Overall Model

# Payroll - Significant
payrollModel <- lm(data=performanceandSalary, W.L.~Payroll)
summary(payrollModel)
# Players to 50 - Significant
playersto50Model <- lm(data=performanceandSalary, W.L.~playersto50)
summary(playersto50Model) 
# Service Time - Significant
serviceTimeModel <- lm(data=performanceandSalary, W.L.~adjServiceTime)
summary(serviceTimeModel)
# Highly Paid Players - Significant
hppModel <- lm(data=performanceandSalary, W.L.~tenmilplayers)
summary(hppModel)

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
ggplot(data=performanceandSalary,aes(x=playersto50,y=W.L.))+geom_point()+
  geom_smooth(method='lm', formula= y~x)+ ggtitle("Winning Percentage vs. Players to 50 (2012-2019)")+xlab("Players to 50")
# Service Time
ggplot(data=performanceandSalary,aes(x=adjServiceTime,y=W.L.))+geom_point()+
  geom_smooth(method='lm', formula= y~x)+ ggtitle("Winning Percentage vs. Adjusted Service Time (2012-2019)")+xlab("Service Time")
# Highly Paid Players
ggplot(data=performanceandSalary,aes(x=tenmilplayers,y=W.L.))+geom_point()+
  geom_smooth(method='lm', formula= y~x)+ ggtitle("Winning Percentage vs. Highly Paid Players (2012-2019)")+xlab("Highly Paid Players")

# Payroll is not statistically significant when limited to small markets
ggplot(data=smallMarketData,aes(x=Payroll/1000000,y=W.L.))+geom_point()+
  geom_smooth(method='lm', formula= y~x)+ ggtitle("Winning Percentage vs. Team Payroll in Small Markets (2012-2019)")+
  xlab("Payroll (in millions)")
payrollModelSmallMarket <- lm(data=smallMarketData, W.L.~Payroll)
summary(payrollModelSmallMarket)


# look up how to incorporate the probability with the market size
# pull individual salaries 
# this is specifically small markets, could do all
ggplot(data=smallMarketData,aes(x=Payroll/1000000,y=Playoffs))+geom_point()+
  geom_smooth(method='lm', formula= y~x)+ 
  ggtitle("Playoff vs. Team Payroll")
payrollModelLogistic <- glm(data=performanceandSalary, Playoffs~Payroll, family=binomial)
summary(payrollModelLogistic)

# number of high payed players per team - 10 mil threshold (what should the threshold be?)
# all teams
ggplot(data=performanceandSalary,aes(x=tenmilplayers,y=W.L.))+geom_point(aes(color=MarketSize))+
  geom_smooth(method='lm', formula= y~x, aes(color=MarketSize,fill=MarketSize))+ 
  facet_grid(MarketSize~.)+
  ggtitle("Win Percentage vs. Number of High Paid Players")
highPaidPlayers <- lm(data=performanceandSalary, Playoffs~Payroll)
summary(highPaidPlayers)

ggplot(data=performanceandSalary,aes(x=playersto50,y=W.L.))+geom_point()+
  geom_smooth(method='lm', formula= y~x)+ 
  ggtitle("Win Percentage vs. Players to 50% of Salary")
ggplot(data=performanceandSalary,aes(x=playersto50,y=W.L.))+geom_point(aes(color=MarketSize))+
  geom_smooth(method='lm', formula= y~x, aes(color=MarketSize,fill=MarketSize))+ 
  facet_grid(MarketSize~.)+
  ggtitle("Win Percentage vs. Players to 50% of Salary")
playerstoHalf <- lm(data=performanceandSalary,W.L.~playersto50)
summary(playerstoHalf)

ggplot(data=performanceandSalary,aes(x=playersto50,y=W.L.))+geom_point(aes(color=MarketSize))+
  geom_smooth(method='lm', formula= y~x, aes(color=MarketSize,fill=MarketSize))+ 
  facet_grid(MarketSize~.)+
  ggtitle("Win Percentage vs. Players to 50% of Salary")

smallMarketData <- filter(performanceandSalary, (MarketSize) == "Small")

summary(finalModel)
summary(predictionModel)
summary(highplayerModel)

tenmilplayers <- performanceandSalary %>%
  group_by(year) %>%
  summarize(
    count = sum(tenmilplayers))

ggplot(data=smallMarketData,aes(x=playersto50,y=W.L.))+geom_point()+
  geom_smooth(method='lm', formula= y~x)+ ggtitle("Winning Percentage vs. Players to 50")+xlab("Players to 50")

ggplot(data=smallMarketData,aes(x=adjServiceTime,y=W.L.))+geom_point()+
  geom_smooth(method='lm', formula= y~x)+ ggtitle("Winning Percentage vs. Service Time")+xlab("Adj. Service Time")

ggplot(data=smallMarketData,aes(x=tenmilplayers,y=W.L.))+geom_point()+
  geom_smooth(method='lm', formula= y~x)+ 
  ggtitle("Winning Percentage vs. Highly Paid Players (2012-2019)")

# Issues - difficult to develop, incorrect team codes 

# Next step - break down by league, break down by position
# color = league() division()
# denote pre arb?
# 

