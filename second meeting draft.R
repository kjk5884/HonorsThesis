require(baseballr)
require(dplyr)
require(readr)

library(Lahman)
library(ggplot2)

teamCodes <- read.csv("~/Sports Analytics Projects/Thesis Research/team codes.csv", fileEncoding = 'UTF-8-BOM')

rosterwSalary <- read.csv("~/Sports Analytics Projects/Thesis Research/rosterwSalary.csv")
performanceandSalary <- read.csv("~/Sports Analytics Projects/Thesis Research/performanceandSalary.csv")

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
ggplot(data=performanceandSalary,aes(x=Payroll,y=Playoffs))+geom_point()+
  geom_smooth(method='lm', formula= y~x)+ 
  ggtitle("Playoff vs. Team Payroll")
payrollModelLogistic <- lm(data=performanceandSalary, Playoffs~Payroll+MarketSize)
summary(payrollModelLogistic)

# group by year, team, rank the salary within team/year, then for rank <= 25

a <- rosterwSalary %>% 
  group_by(team)%>%
  group_by(year)%>%
 # how to incorporate rank function into this?

write.csv(rosterwSalary,"~/Sports Analytics Projects/Thesis Research/rosterwSalary.csv")
write.csv(performanceandSalary,"~/Sports Analytics Projects/Thesis Research/performanceandSalary.csv")
