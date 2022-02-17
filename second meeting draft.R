require(baseballr)
require(dplyr)
require(readr)

library(Lahman)
library(ggplot2)

# Download Rosters, do not need to do again
#get_retrosheet_data(path_to_directory = "~/Sports Analytics Projects/Thesis Research/Retrosheet/2012", 
                    #years_to_acquire = 2012)
#get_retrosheet_data(path_to_directory = "~/Sports Analytics Projects/Thesis Research/Retrosheet/2013", 
                    #years_to_acquire = 2013)
#get_retrosheet_data(path_to_directory = "~/Sports Analytics Projects/Thesis Research/Retrosheet/2014", 
                    #years_to_acquire = 2014)
#get_retrosheet_data(path_to_directory = "~/Sports Analytics Projects/Thesis Research/Retrosheet/2015", 
                    #years_to_acquire = 2015)
#get_retrosheet_data(path_to_directory = "~/Sports Analytics Projects/Thesis Research/Retrosheet/2016", 
                    #years_to_acquire = 2016)
#get_retrosheet_data(path_to_directory = "~/Sports Analytics Projects/Thesis Research/Retrosheet/2017", 
                    #years_to_acquire = 2017)
#get_retrosheet_data(path_to_directory = "~/Sports Analytics Projects/Thesis Research/Retrosheet/2018", 
                    #years_to_acquire = 2018)
#get_retrosheet_data(path_to_directory = "~/Sports Analytics Projects/Thesis Research/Retrosheet/2019", 
                    #years_to_acquire = 2019)

standings <- read.csv("~/Sports Analytics Projects/Thesis Research/standings.csv", fileEncoding = 'UTF-8-BOM')
roster2012 <- read_csv("~/Sports Analytics Projects/Thesis Research/Retrosheet/2012/download.folder/unzipped/roster2012.csv")
roster2013 <- read_csv("~/Sports Analytics Projects/Thesis Research/Retrosheet/2013/download.folder/unzipped/roster2013.csv")
roster2014 <- read_csv("~/Sports Analytics Projects/Thesis Research/Retrosheet/2014/download.folder/unzipped/roster2014.csv")
roster2015 <- read_csv("~/Sports Analytics Projects/Thesis Research/Retrosheet/2015/download.folder/unzipped/roster2015.csv")
roster2016 <- read_csv("~/Sports Analytics Projects/Thesis Research/Retrosheet/2016/download.folder/unzipped/roster2016.csv")
roster2017 <- read_csv("~/Sports Analytics Projects/Thesis Research/Retrosheet/2017/download.folder/unzipped/roster2017.csv")
roster2018 <- read_csv("~/Sports Analytics Projects/Thesis Research/Retrosheet/2018/download.folder/unzipped/roster2018.csv")
roster2019 <- read_csv("~/Sports Analytics Projects/Thesis Research/Retrosheet/2019/download.folder/unzipped/roster2019.csv")
teamCodes <- read.csv("~/Sports Analytics Projects/Thesis Research/team codes.csv", fileEncoding = 'UTF-8-BOM')

People$name <- paste(People$nameFirst, People$nameLast, sep=" ")
rosters12to19 <- rbind(roster2012,roster2013,roster2014,roster2015,roster2016,roster2017,roster2018,roster2019)
rosters12to19 <- merge(rosters12to19,People, by.x = "player_id", by.y = "retroID", all.x=TRUE)
rosters12to19 <- rosters12to19[-c(2:4,17:22,33)]
rosters12to19$age <- with(rosters12to19, year-birthYear)
rosters12to19 <- rosters12to19[c(25,6,4,5,26,24,8,2,3,9:22,1,23)]
serviceTime <- read.csv("~/Sports Analytics Projects/Thesis Research/MLB-Salaries 2012-19.csv", fileEncoding = 'UTF-8-BOM')
rosterwSalary <-merge(rosters12to19,serviceTime, by.x = c("nameFirst","nameLast","year"), by.y = c("nameFirst","nameLast","Year"), all.x=TRUE)
rosterwSalary$Service.Time[is.na(rosterwSalary$Service.Time)] <- 0
rosterwSalary$Salary[is.na(rosterwSalary$Salary)] <- 500000

roster25salary <- rosterwSalary %>% group_by(team,year) %>% summarize(Payroll = sum(Salary))
# would like to normalize by 25 highest salaries

roster25salary <- merge(roster25salary,teamCodes,by.x="team", by.y="Abb2")
roster25salary <- roster25salary[-c(1,5)]
performanceandSalary <- merge(roster25salary,standings, by.x=c("Team","year"),by.y=c("Tm","Year"))

ggplot(data=performanceandSalary,aes(x=Payroll,y=W.L.))+geom_point()+
  geom_smooth(method='lm', formula= y~x)+ ggtitle("Winning Percentage vs. Team Payroll")
payrollModel <- lm(data=performanceandSalary, W.L.~Payroll)
summary(payrollModel)

# Market Size grouped by Chicago Tribune team valuations
marketSize <- read_csv("~/Sports Analytics Projects/Thesis Research/Market Size.csv")
performanceandSalary <- merge(performanceandSalary,marketSize,by="Team")
smallMarketData <- filter(performanceandSalary, (MarketSize) == "Small")


# Payroll is not statistically significant when limited to small markets
ggplot(data=smallMarketData,aes(x=Payroll,y=W.L.))+geom_point()+
  geom_smooth(method='lm', formula= y~x)+ ggtitle("Winning Percentage vs. Team Payroll in Small Markets")
payrollModelSmallMarket <- lm(data=smallMarketData, W.L.~Payroll)
summary(payrollModelSmallMarket)

