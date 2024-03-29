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
require(baseballr)
require(readr)
require(dplyr)
library(Lahman)
library(ggplot2)
library(readr::read_csv)
library(tidyr)

teamCodes <- read.csv("~/Thesis Research/team codes.csv", fileEncoding = 'UTF-8-BOM')

standings <- read.csv("~/Thesis Research/standings.csv", fileEncoding = 'UTF-8-BOM')
roster2012 <- read.csv("~/Thesis Research/Retrosheet/2012/download.folder/unzipped/roster2012.csv")
roster2013 <- read.csv("~/Sports Analytics Projects/Thesis Research/Retrosheet/2013/download.folder/unzipped/roster2013.csv")
roster2014 <- read_csv("~/Sports Analytics Projects/Thesis Research/Retrosheet/2014/download.folder/unzipped/roster2014.csv")
roster2015 <- read_csv("~/Sports Analytics Projects/Thesis Research/Retrosheet/2015/download.folder/unzipped/roster2015.csv")
roster2016 <- read_csv("~/Sports Analytics Projects/Thesis Research/Retrosheet/2016/download.folder/unzipped/roster2016.csv")
roster2017 <- read_csv("~/Sports Analytics Projects/Thesis Research/Retrosheet/2017/download.folder/unzipped/roster2017.csv")
roster2018 <- read_csv("~/Sports Analytics Projects/Thesis Research/Retrosheet/2018/download.folder/unzipped/roster2018.csv")
roster2019 <- read_csv("~/Sports Analytics Projects/Thesis Research/Retrosheet/2019/download.folder/unzipped/roster2019.csv")

performanceandSalary <- merge(performanceandSalary,standings, by.x = c("Team","year"), by.y = c("Tm","Year"), all.x=TRUE)
performanceandSalary <- performanceandSalary[-c(15:21)]
performanceandSalary$W.L..x <- performanceandSalary$W.L.
colnames(performanceandSalary)[6] <- "W.L."

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

rosterwSalary <- rosterwSalary %>% 
  group_by(year,team)%>%
  mutate(rank=rank(desc(Salary),ties.method="min"))

roster25salary <- rosterwSalary %>% filter(rank <= 25) %>% group_by(team,year) %>% summarize(Payroll = sum(Salary))

highDollarTeams <- rosterwSalary %>% filter(Salary >=10000000) %>% group_by(team,year) %>% summarize(tenmilplayers = n())
roster25salary <- merge(roster25salary,highDollarTeams,by=c("team","year"),all.x=TRUE)

# would like to normalize by 25 highest salaries

roster25salary <- merge(roster25salary,teamCodes,by.x="team", by.y="Abb2")
roster25salary <- roster25salary[-c(6)]
performanceandSalary <- merge(roster25salary,standings, by.x=c("Team","year"),by.y=c("Tm","Year"))

marketSize <- read_csv("~/Sports Analytics Projects/Thesis Research/Market Size.csv")
performanceandSalary <- merge(performanceandSalary,marketSize,by="Team")
performanceandSalary[is.na(performanceandSalary)] <- 0

proportions <- merge(rosterwSalary2,performanceandSalary,by=c("team","year"),all.x = TRUE)
proportions$payrollProp <- proportions$Salary / proportions$Payroll


# created proportion of payroll field for what is taken up by each player - next step
# create dataframe with number of players that make up half of roster

# need to group by team/year, group by proportion col in descending order, tell r to start
# summing until sum exceeds 0.5 (or remains less than), count how many rows it took


proportions <- proportions[order(proportions$team,proportions$year,-proportions$payrollProp),]

years <- c(2012:2019)
runningTotal = 0
count = 0
playersto50 <- data.frame(matrix(ncol = 3, nrow = 0))

for (abb in teamCodes$Abb2){ # for each team in the league
  for (yearNum in years){ # for each year
    tempFile <- filter(proportions, team==abb,year==yearNum) # filter for the team and year
    for (val in tempFile$payrollProp){ # take a running total of the payroll percentage
      if (runningTotal<0.5){ # until it reaches 50%
        runningTotal = runningTotal + val
        count = count + 1
      } 
    }
    #print(c(abb,yearNum,count))
    playersto50 <- rbind(playersto50,c(abb,yearNum,count))
    count=0
    runningTotal=0
    rank=0
  }
}
adjServiceTime <- data.frame(matrix(ncol = 3, nrow = 0))
proportions2 <- filter(proportions, rank <=25)
yearsTotal=0
daysTotal=0
for (abb in teamCodes$Abb2){ # for each team in the league
  for (yearNum in years){ # for each year
    tempFile <- filter(proportions2, team==abb,year==yearNum) # filter for the team and year
    for (val in tempFile$st.years){ # sum up service time
       yearsTotal = yearsTotal + val
       
    } 
    for (val in tempFile$st.days){ # sum up service time
      daysTotal = daysTotal + val
      
    } 
    print(c(abb,yearNum,yearsTotal,daysTotal))
    runningTotal <- yearsTotal+(daysTotal/172)
    adjServiceTime <- rbind(adjServiceTime, c(abb,yearNum,as.numeric(runningTotal/25)))
    count = 0
    yearsTotal=0
    daysTotal=0
    }
    
    #playersto50 <- rbind(playersto50,c(abb,yearNum,count))
    
}
    
colnames(adjServiceTime) <- c('team', 'year', 'adjServiceTime')
performanceandSalary <- merge(performanceandSalary,adjServiceTime,by=c("team","year"))
performanceandSalary$adjServiceTime=as.numeric(performanceandSalary$adjServiceTime)

colnames(playersto50) <- c('team', 'year', 'playersto50')
performanceandSalary <- merge(performanceandSalary,playersto50,by=c("team","year"))

rosterwSalary2 <- separate(data = rosterwSalary, col = Service.Time, into = c("st.years", "st.days"),sep="\\.")
rosterwSalary2[is.na(rosterwSalary2)] <- 0
rosterwSalary2$st.years=as.numeric(rosterwSalary2$st.years)
rosterwSalary2$st.days=as.numeric(rosterwSalary2$st.days)

rosterwSalary <-rosterwSalary[-(6:25)]
rosterwSalary <-rosterwSalary[-(1:2)]

performanceandSalary$payrollMil <- performanceandSalary$Payroll/1000000
performanceandSalary$adjServiceTimeSq <-performanceandSalary$adjServiceTime^2
performanceandSalary$playersto50sq <-performanceandSalary$playersto50^2
performanceandSalary$payrollsq <-(performanceandSalary$payrollMil)^2
performanceandSalary$tenmilplayerssq <- performanceandSalary$tenmilplayers^2
performanceandSalary <- performanceandSalary[c(1:5,9,14:21)]

write.csv(rosterwSalary,"~/Thesis Research/rosterwSalary.csv",row.names = FALSE)
write.csv(performanceandSalary,"~/Thesis Research/performanceandSalary.csv", row.names = FALSE)
write.csv(performanceandSalary,"~/Thesis Research/performanceandSalary - Save.csv", row.names = FALSE)

performanceandSalary <- performanceandSalary[1:15]
