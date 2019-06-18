install.packages('haven')
install.packages("plyr")
install.packages("tidyr")
install.packages("lubridate")
install.packages("dplyr")
install.packages("tidyverse")
install.packages("shiny")

#Librarys used

library('haven')
library("plyr")
library("tidyr")
library("lubridate")
library("dplyr")
library(tidyverse)
library("shiny")
library(readxl)



#Reading raw data sets provided
RawDataIDemographics<-read_sas("C:/Users/dtuiran/Documents/GitHub/group-assignment-open-source-programming-bwin-group-7/Group Assignment/RawDataIDemographics.sas7bdat")
dailyaggregation <- read_sas('C:/Users/dtuiran/Documents/GitHub/group-assignment-open-source-programming-bwin-group-7/Group Assignment/RawDataIIUserDailyAggregation.sas7bdat')
RawPoker <- read_sas('C:/Users/dtuiran/Documents/GitHub/group-assignment-open-source-programming-bwin-group-7/Group Assignment/RawDataIIIPokerChipConversions.sas7bdat')
IG <- read_sas('C:/Users/dtuiran/Documents/GitHub/group-assignment-open-source-programming-bwin-group-7/Group Assignment/AnalyticDataInternetGambling.sas7bdat')


Country_codes<- read.table("C:/Users/dtuiran/Documents/GitHub/group-assignment-open-source-programming-bwin-group-7/Group Assignment/Country Code.csv",sep=",",header = TRUE,stringsAsFactors = FALSE)
Application_ID<-read.table("C:/Users/dtuiran/Documents/GitHub/group-assignment-open-source-programming-bwin-group-7/Group Assignment/ApplicationID.csv",sep=",",header = TRUE,stringsAsFactors = FALSE)
Language<-read.table("C:/Users/dtuiran/Documents/GitHub/group-assignment-open-source-programming-bwin-group-7/Group Assignment/Language.csv",sep=",",header = TRUE,stringsAsFactors = FALSE)
continent<-read_excel("C:/Users/dtuiran/Documents/GitHub/group-assignment-open-source-programming-bwin-group-7/Group Assignment/Continent.xlsx")





############################
#CLEANING DEMOGRAPHICS DATA.
############################


RawDataIDemographics$RegDate <- as.Date(RawDataIDemographics$RegDate,'%Y-%m-%d')
RawDataIDemographics$FirstPay <- as.Date(RawDataIDemographics$FirstPay,'%Y%m%d')
RawDataIDemographics$FirstAct <- as.Date(RawDataIDemographics$FirstAct,'%Y%m%d')
RawDataIDemographics$FirstSp <- as.Date(RawDataIDemographics$FirstSp,'%Y%m%d')
RawDataIDemographics$FirstCa <- as.Date(RawDataIDemographics$FirstCa,'%Y%m%d')
RawDataIDemographics$FirstGa <- as.Date(RawDataIDemographics$FirstGa,'%Y%m%d')
RawDataIDemographics$FirstPo <- as.Date(RawDataIDemographics$FirstPo,'%Y%m%d')

#Changing gender type to F and M
RawDataIDemographics$Gender=ifelse(RawDataIDemographics$Gender==1,"M",ifelse(RawDataIDemographics$Gender==0,"F","F"))

#Adding descriptions for country codes, application, langugae and contient
RawDataDemographics_1<- merge(x=RawDataIDemographics,y=Country_codes,by="Country",all.x = TRUE)
RawDataDemographics_2<- merge(x=RawDataDemographics_1,y=Application_ID,by="ApplicationID",all.x = TRUE)
RawDataDemographics_3<- merge(x=RawDataDemographics_2,y=Language,by="Language",all.x = TRUE)
RawDataDemographics_4<- merge(x=RawDataDemographics_3,y=continent,by="Country.Name",all.x = TRUE)



#############################
#CLEANING ANALYTICAL DATA SET
#############################


IG <- IG[, c('USERID', 'AGE')]
colnames(IG)[1] <- "UserID"

#Adding age column 
DemographicsFinal<- merge(x = RawDataDemographics_4, y = IG, by = "UserID", all.x =  TRUE)



################################
#CLEANING DAILY AGGREGATION DATA 
###############################


#Accounting corrections using negative numbers transformed to cero values.
dailyaggregation$Stakes[dailyaggregation$Stakes < 0] <- 0
dailyaggregation$Winnings[dailyaggregation$Winnings < 0] <- 0
dailyaggregation$Bets[dailyaggregation$Bets < 0] <- 0


#Creating a new Product Column Type to group products by Sports Book, Poker, Casino and Games
dailyaggregation$productType <-ifelse(dailyaggregation$ProductID %in% c(1,2),"SportsBook",
                                       ifelse(dailyaggregation$ProductID == 3,"Poker",
                                              ifelse(dailyaggregation$ProductID %in% c(4,5,8),"Casino",
                                                     ifelse(dailyaggregation$ProductID %in% c(6,7),"Games",0))))

#grouping by UserID and product Type to calculate some variables of interest.
dailyaggregation2<-ddply(dailyaggregation,.(UserID,productType),summarize,first_active_date = min(Date),last_active_date = max(Date), 
                         sum_stakes=sum(Stakes),sum_winnings=sum(Winnings),sum_bets=sum(Bets),
                         avg_bets=mean(Bets),avg_stakes=mean(Stakes),avg_winnings=mean(Winnings),.drop = TRUE)

#spreading each variable per product Type 
dailyaggregation2_spread <- dailyaggregation2 %>%
  gather(variable, value, -(UserID:productType)) %>%
  unite(temp, variable, productType) %>%
  spread(temp, value)

#Converting to dates format
dailyaggregation2_spread$first_active_date_Casino<- as.Date(dailyaggregation2_spread$first_active_date_Casino, format = ("%Y%m%d"),origin="19700101")     
dailyaggregation2_spread$first_active_date_Games<- as.Date(dailyaggregation2_spread$first_active_date_Games, format = ("%Y%m%d"),origin="19700101")      
dailyaggregation2_spread$first_active_date_SportsBook<- as.Date(dailyaggregation2_spread$first_active_date_SportsBook, format = ("%Y%m%d"),origin="19700101")
dailyaggregation2_spread$last_active_date_Casino<- as.Date(dailyaggregation2_spread$last_active_date_Casino, format = ("%Y%m%d"),origin="19700101")   
dailyaggregation2_spread$last_active_date_Games<- as.Date(dailyaggregation2_spread$last_active_date_Games, format = ("%Y%m%d"),origin="19700101")       
dailyaggregation2_spread$last_active_date_SportsBook<- as.Date(dailyaggregation2_spread$last_active_date_SportsBook, format = ("%Y%m%d"),origin="19700101")


#Merging demographics with daily aggreagtion transformed and in consumer level
merge1<- merge(x = DemographicsFinal, y = dailyaggregation2_spread, by = "UserID", all.x =  TRUE)

#Fixing variable types 
merge1$sum_stakes_Casino<-as.numeric(merge1$sum_stakes_Casino)
merge1$sum_stakes_Games <-as.numeric(merge1$sum_stakes_Games)
merge1$sum_stakes_SportsBook <-as.numeric(merge1$sum_stakes_SportsBook)

merge1$sum_winnings_Casino <-as.numeric(merge1$sum_winnings_Casino)
merge1$sum_winnings_Games <-as.numeric(merge1$sum_winnings_Games)
merge1$sum_winnings_SportsBook <-as.numeric(merge1$sum_winnings_SportsBook)

merge1$avg_bets_Casino <-as.numeric(merge1$avg_bets_Casino)
merge1$avg_bets_Games <-as.numeric(merge1$avg_bets_Games)
merge1$avg_bets_SportsBook <-as.numeric(merge1$avg_bets_SportsBook)

merge1$avg_stakes_Casino <-as.numeric(merge1$avg_stakes_Casino)
merge1$avg_stakes_Games <-as.numeric(merge1$avg_stakes_Games)
merge1$avg_stakes_SportsBook <-as.numeric(merge1$avg_stakes_SportsBook)

merge1$avg_winnings_Casino <-as.numeric(merge1$avg_winnings_Casino)
merge1$avg_winnings_Games <-as.numeric(merge1$avg_winnings_Games)
merge1$avg_winnings_SportsBook <-as.numeric(merge1$avg_winnings_SportsBook)

merge1$sum_bets_Casino <-as.numeric(merge1$sum_bets_Casino)
merge1$sum_bets_Games <-as.numeric(merge1$sum_bets_Games)
merge1$sum_bets_SportsBook <-as.numeric(merge1$sum_bets_SportsBook)


#DELETING COLUMNS
RawDataDemographics_4$Language<-NULL
RawDataDemographics_4$ApplicationID<-NULL
RawDataDemographics_4$Country<-NULL

merge1$FirstSp<-NULL
merge1$FirstCa<-NULL
merge1$FirstGa<-NULL
merge1$FirstAct<-NULL


################################
#CLEANING POKER CHIP CONVERSIONS 
################################


#Adding transaction type description
RawPoker$TransType <- ifelse(RawPoker$TransType == 124, 'Buy', 'Sell')


poker2<-ddply(RawPoker,.(UserID,TransType),summarize,first_active_date_poker = min(TransDateTime),last_active_date_poker = max(TransDateTime), 
                         sum_Amount=sum(TransAmount),
                         .drop = TRUE)

#spreading each variable per product Type 
pokerunique<- poker2 %>%
  gather(variable, value, -(UserID:TransType)) %>%
  unite(temp, variable, TransType) %>%
  spread(temp, value)

basetable<- merge(x = merge1, y = pokerunique, by = "UserID", all.x =  TRUE)

basetable$first_active_date_poker_Buy <-NULL
basetable$last_active_date_poker_Buy <-NULL


#???basetable$first_active_date_poker_Sell <-as.numeric(basetable$first_active_date_poker)
#basetable$last_active_date_poker_Sell <-as.numeric(basetable$last_active_date_poker)

basetable$sum_Amount_Buy <-as.numeric(basetable$sum_Amount_Buy)
basetable$sum_Amount_Sell <-as.numeric(basetable$sum_Amount_Sell)
basetable$first_active_date_poker_Sell <- as.Date(basetable$first_active_date_poker_Sell,format = ("%Y-%m-%d"),origin="1970-01-01")
basetable$last_active_date_poker_Sell <- as.Date(basetable$last_active_date_poker_Sell,format = ("%Y-%m-%d"),origin="1970-01-01")

basetable$AGE <-as.factor(basetable$AGE)


#######################
#CREATING NEW VARIABLES
#######################

#Gross Gaming Revenue (GGR) is the amount wagered minus the winnings returned to players, 
#a true measure of the economic value of gambling.
basetable$GGR_casino <- (basetable$sum_stakes_Casino - basetable$sum_winnings_Casino)
basetable$GGR_SportsBook  <- (basetable$sum_stakes_SportsBook - basetable$sum_winnings_SportsBook)
basetable$GGR_Games <- (basetable$sum_stakes_Games - basetable$sum_winnings_Games)
basetable$GGR_Total <- rowSums(basetable[,c("GGR_casino","GGR_SportsBook","GGR_Games")], na.rm=TRUE) 

basetable$GGR_Games <-as.numeric(basetable$GGR_Games)
basetable$GGR_Total <-as.numeric(basetable$GGR_Total)


# TOTAL ACTIVE DAYS : Difference between the last active date of play minus the first active date of Play
basetable$DaysActive_casino <- as.numeric((basetable$last_active_date_Casino - basetable$first_active_date_Casino))
basetable$DaysActive_games <- as.numeric((basetable$last_active_date_Games - basetable$first_active_date_Games ))
basetable$DaysActive_sportsBook <- as.numeric((basetable$last_active_date_SportsBook - basetable$first_active_date_SportsBook))
 
#TOTALS FOR EACH OPERATION
basetable$total_stakes <- rowSums(basetable[,c("sum_stakes_Casino","sum_stakes_Games","sum_stakes_SportsBook")],na.rm=TRUE)
basetable$total_bets <- rowSums(basetable[,c("sum_bets_Casino","sum_bets_Games","sum_bets_SportsBook")],na.rm=TRUE)
basetable$total_winnings <- rowSums(basetable[,c("sum_winnings_Casino","sum_winnings_Games","sum_winnings_SportsBook")],na.rm=TRUE)
basetable$DaysActive_Total <- rowSums(basetable[,c("DaysActive_casino","DaysActive_games","DaysActive_sportsBook")],na.rm=TRUE) 

basetable$Country.Name <- as.factor(basetable$Country.Name)
basetable$Gender <- as.factor(basetable$Gender)
basetable$Continents <- as.factor(basetable$Continents)



#Number of produts per customer 
basetable$numberofProducts<-ifelse(((!is.na((basetable$first_active_date_Casino) == TRUE) & (!is.na(basetable$first_active_date_Games) == TRUE) & (!is.na(basetable$first_active_date_SportsBook) == TRUE) & (!is.na(basetable$first_active_date_poker_Sell) == TRUE))),4,
                               
                         ifelse(((is.na(merge1$first_active_date_Casino) == TRUE) & (is.na(merge1$first_active_date_Games) == FALSE) & (is.na(merge1$first_active_date_SportsBook) == FALSE))& (is.na(basetable$first_active_date_poker_Sell) == FALSE)|
                                ((is.na(merge1$first_active_date_Casino) == FALSE) & (is.na(merge1$first_active_date_Games) == TRUE) & (is.na(merge1$first_active_date_SportsBook) == FALSE))& (is.na(basetable$first_active_date_poker_Sell) == FALSE)|
                                ((is.na(merge1$first_active_date_Casino) == FALSE) & (is.na(merge1$first_active_date_Games) == FALSE) & (is.na(merge1$first_active_date_SportsBook) == TRUE))& (is.na(basetable$first_active_date_poker_Sell) == FALSE)|
                                ((is.na(merge1$first_active_date_Casino) == FALSE) & (is.na(merge1$first_active_date_Games) == FALSE) & (is.na(merge1$first_active_date_SportsBook) == FALSE))& (is.na(basetable$first_active_date_poker_Sell) == TRUE),3,
                          
                         
                         ifelse(((is.na(merge1$first_active_date_Casino) == TRUE) & (is.na(merge1$first_active_date_Games) == TRUE) & (is.na(merge1$first_active_date_SportsBook) == FALSE))& (is.na(basetable$first_active_date_poker_Sell) == FALSE)|
                                ((is.na(merge1$first_active_date_Casino) == TRUE) & (is.na(merge1$first_active_date_Games) == FALSE) & (is.na(merge1$first_active_date_SportsBook) == FALSE))& (is.na(basetable$first_active_date_poker_Sell) == TRUE)|
                                ((is.na(merge1$first_active_date_Casino) == FALSE) & (is.na(merge1$first_active_date_Games) == TRUE) & (is.na(merge1$first_active_date_SportsBook) == TRUE))& (is.na(basetable$first_active_date_poker_Sell) == FALSE)|
                                ((is.na(merge1$first_active_date_Casino) == FALSE) & (is.na(merge1$first_active_date_Games) == FALSE) & (is.na(merge1$first_active_date_SportsBook) == TRUE)& (is.na(basetable$first_active_date_poker_Sell) == TRUE)),2,1)))

                               
EndDate <- as.Date('2005-10-01')



#Recency
basetable$Recency_Casino = as.numeric(difftime(EndDate, basetable$last_active_date_Casino, units = 'days'))
basetable$Recency_games = as.numeric(difftime(EndDate, basetable$last_active_date_Games, units = 'days'))
basetable$Recency_SportsBook = as.numeric(difftime(EndDate, basetable$last_active_date_SportsBook, units = 'days'))
basetable$Recency_poker = as.numeric(difftime(EndDate, basetable$last_active_date_poker, units = 'days'))

#Winning recency                      
basetable$WinRecency_Casino = ifelse(basetable$sum_winnings_Casino > 0,(as.numeric(difftime(EndDate, basetable$last_active_date_Casino), units = 'days')),0)
basetable$WinRecency_games = ifelse(basetable$sum_winnings_Games > 0,(as.numeric(difftime(EndDate, basetable$last_active_date_Games), units = 'days')),0)
basetable$WinRecency_SportsBook = ifelse(basetable$sum_winnings_SportsBook > 0,(as.numeric(difftime(EndDate, basetable$last_active_date_SportsBook), units = 'days')),0)

#Losing recency 
basetable$LossRecency_Casino = ifelse(basetable$sum_winnings_Casino == 0,(as.numeric(difftime(EndDate, basetable$last_active_date_Casino), units = 'days')),0)
basetable$LossRecency_games = ifelse(basetable$sum_winnings_Games == 0,(as.numeric(difftime(EndDate, basetable$last_active_date_Games), units = 'days')),0)
basetable$LossRecency_SportsBook = ifelse(basetable$sum_winnings_SportsBook == 0,(as.numeric(difftime(EndDate, basetable$last_active_date_SportsBook), units = 'days')),0)


#LENGHT OF SUBSCRIPTION (Since registration date)
basetable$LOS_casino = as.numeric(difftime(basetable$last_active_date_Casino,basetable$RegDate, units = 'days'))
basetable$LOS_Games = as.numeric(difftime(basetable$last_active_date_Games,basetable$RegDate, units = 'days'))
basetable$LOS_SportsBook = as.numeric(difftime(basetable$last_active_date_SportsBook,basetable$RegDate, units = 'days'))

#Winning bets 
basetable$WinningBets_casino = ifelse((basetable$sum_winnings_Casino > 0),(basetable$sum_bets_Casino),0)
basetable$WinningBets_Games = ifelse((basetable$sum_winnings_Games > 0),(basetable$sum_bets_Games),0)
basetable$WinningBets_SportsBook = ifelse((basetable$sum_winnings_SportsBook > 0),(basetable$sum_bets_SportsBook),0)

#Losing bets
basetable$LosingBets_casino = ifelse((basetable$sum_winnings_Casino == 0),(basetable$sum_bets_Casino),0)
basetable$LosingBets_Games = ifelse((basetable$sum_winnings_Games == 0),(basetable$sum_bets_Games),0)
basetable$LosingBets_SportsBook = ifelse((basetable$sum_winnings_SportsBook == 0),(basetable$sum_bets_SportsBook),0)

#Monetary value by LOS
basetable$MValLOS_casino = (basetable$GGR_casino/basetable$LOS_casino)
basetable$MValLOS_Games = (basetable$GGR_Games/basetable$LOS_Games)
basetable$MValLOS_SportsBook = (basetable$GGR_SportsBook/basetable$LOS_SportsBook)

#Monetary value of winning by LOS
basetable$MVWinnigsLOS_casino = as.numeric(basetable$sum_winnings_Casino/basetable$LOS_casino)
basetable$MVWinnigsLOS_Games = as.numeric(basetable$sum_winnings_Games/basetable$LOS_Games)
basetable$MVWinnigsLOS_SportsBook = as.numeric(basetable$sum_winnings_SportsBook/basetable$LOS_SportsBook)

#Monetary value stakes 
basetable$MVStakesLOS_casino = as.numeric(basetable$sum_stakes_Casino/basetable$LOS_casino)
basetable$MVStakesLOS_Games = as.numeric(basetable$sum_stakes_Games/basetable$LOS_Games)
basetable$MVStakesLOS_SportsBook = as.numeric(basetable$sum_stakes_SportsBook/basetable$LOS_SportsBook)


basetable$MValProfit_Poker =  basetable$sum_Amount_Sell - basetable$sum_Amount_Buy
basetable$LOS_Poker = as.numeric(difftime(EndDate, basetable$first_active_date_poker_Sell, units = 'days'))
basetable$ProfitByLOS_Poker = (basetable$sum_Amount_Sell - basetable$sum_Amount_Buy)/basetable$LOS_Poker


basetable$Language<-NULL
basetable$ApplicationID<-NULL
basetable$Country<-NULL

#Resolving Infinite Values
basetable$MValLOS_casino[is.infinite(basetable$MValLOS_casino)] <- NA
basetable$MValLOS_SportsBook[is.infinite(basetable$MValLOS_SportsBook)] <- NA
basetable$MValLOS_Games[is.infinite(basetable$MValLOS_Games)] <- NA

basetable$Recency_Casino[is.infinite(basetable$Recency_Casino)] <- NA
basetable$Recency_SportsBook[is.infinite(basetable$Recency_SportsBook)] <- NA
basetable$Recency_poker[is.infinite(basetable$Recency_poker)] <- NA
basetable$Recency_games[is.infinite(basetable$Recency_games)] <- NA

basetable$WinRecency_Casino[is.infinite(basetable$WinRecency_Casino)] <- NA
basetable$WinRecency_SportsBook[is.infinite(basetable$WinRecency_SportsBook)] <- NA
basetable$WinRecency_games[is.infinite(basetable$WinRecency_games)] <- NA


basetable$MVStakesLOS_casino[is.infinite(basetable$MVStakesLOS_casino)] <- NA
basetable$MVStakesLOS_Games[is.infinite(basetable$MVStakesLOS_Games)] <- NA
basetable$MVStakesLOS_SportsBook[is.infinite(basetable$MVStakesLOS_SportsBook)] <- NA


write.csv(basetable, file= 'C:/Users/dtuiran/Documents/GitHub/group-assignment-open-source-programming-bwin-group-7/Group Assignment/datamart.csv', row.names = FALSE)

