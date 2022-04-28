require(baseballr)
require(dplyr)
require(readr)

library(ggplot2)

performanceandSalary <- read.csv("~/Sports Analytics Projects/Thesis Research/performanceandSalary.csv")
rosterwSalary <- read.csv("~/Sports Analytics Projects/Thesis Research/rosterwSalary.csv")

# Take payroll and divide by 1 million
ggplot(data=performanceandSalary,aes(x=Payroll/1000000,y=W.L.))+geom_point()+
  geom_smooth(method='lm', formula= y~x)+ ggtitle("Winning Percentage vs. Team Payroll")+xlab("Payroll (in millions)")
payrollModel <- lm(data=performanceandSalary, W.L.~Payroll)
summary(payrollModel)

# Market Size grouped by Chicago Tribune team valuations

smallMarketData <- filter(performanceandSalary, (MarketSize) == "Small")


# Payroll is not statistically significant when limited to small markets
ggplot(data=smallMarketData,aes(x=Payroll/1000000,y=W.L.))+geom_point()+
  geom_smooth(method='lm', formula= y~x)+ ggtitle("Winning Percentage vs. Team Payroll in Small Markets")+
  xlab("Payroll (in millions)")
payrollModelSmallMarket <- lm(data=smallMarketData, W.L.~Payroll)
summary(payrollModelSmallMarket)


# look up how to incorporate the probability with the market size
# pull individual salaries 
# this is specifically small markets, could do all
ggplot(data=smallMarketData,aes(x=Payroll/1000000,y=Playoffs))+geom_point()+
  geom_smooth(method='lm', formula= y~x)+ 
  ggtitle("Playoff vs. Team Payroll")
payrollModelLogistic <- lm(data=smallMarketData, Playoffs~Payroll)
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
playerstoHalf <- lm(data=performanceandSalary,W.L.~playersto50)
summary(playerstoHalf)

ggplot(data=performanceandSalary,aes(x=playersto50,y=W.L.))+geom_point(aes(color=MarketSize))+
  geom_smooth(method='lm', formula= y~x, aes(color=MarketSize,fill=MarketSize))+ 
  facet_grid(MarketSize~.)+
  ggtitle("Win Percentage vs. Players to 50% of Salary")

# Issues - difficult to develop, incorrect team codes 

# Next step - break down by league, break down by position
# color = league() division()
# denote pre arb?