
# Assignment --------------------------------------------------------------
# Final Assignment (Individual) - English Premier League Soccer Standings
# Author - Abhishek Sharma

# Function --------------------------------------------------------------------
EPL_Standings <- function(dt,season){
  
# Libraries ---------------------------------------------------------------
  library(tidyverse)
  library(scales)
  library(lubridate)
  library(dplyr)
  library(readr)

# Reading Data ------------------------------------------------------------
  
  epl1 <- read_csv(url("http://www.football-data.co.uk/mmz4281/1718/E0.csv"))
  epl2 <- read_csv(url("http://www.football-data.co.uk/mmz4281/1819/E0.csv"))
  epl3 <- read_csv(url("http://www.football-data.co.uk/mmz4281/1920/E0.csv"))
  
# Function Code -------------------------------------------------------------------
 
#Check to identify the Season being referred in order to retrieve the Season's dataset
  Season <-substr(season,6,7)
  if (Season =='18'){
    s<-epl1
  } else if(Season =='19'){
    s<-epl2
  } else {
    s<-epl3
  }
  # Converting the Date(1st argument) in YMD format
  # in order to use the same to filter the season dataset
  dty <- substr(dt,7,10)
  dtd <- substr(dt,4,5)
  dtm <- substr(dt,1,2)
  dt_inp <- stringr::str_c(dty,dtm,dtd)
 
# Converting the Date column of season dataframe to YMD format
s$Date<- dmy((s$Date))

# Check to validate if the input date is a valid date i.e. 
# lies with in the range of season start and end date.  

if (ymd(dt_inp) %in% (s$Date)){
  
# Creating a subset of season dataframe as per input sent in the function
s <- s %>% filter(Date <= ymd(dt_inp))
  

# Retrieving Home games data from the parent Dataframe
HomeData <- select(s,Teamname = HomeTeam,Date,FTHG,FTAG, FTR)
HomeData <- mutate(HomeData,Win =(if_else(FTR=='H',1,0)))
HomeData <- mutate(HomeData,Loss=(if_else(FTR=='A',1,0))) 
HomeData <- mutate(HomeData,Draw= (if_else(FTR=='D',1,0)))

#Copying for streak calculation on Home Data
streakh <- select(HomeData,Teamname,Date,FTR) 

HomeData <- select(HomeData,Teamname,FTHG,FTAG, Date,Win,Loss,Draw) 

#Select Last10 records for each team where they've played games at Home
HL10 <- select(HomeData,Teamname,Date,Win,Loss,Draw) 
HL10 <- do.call("rbind", lapply(split(HL10, HL10$Teamname), tail,  10))

#Create consolidated Dataframe for Home Games
  HomeData2 <- HomeData %>% 
  group_by(Teamname) %>% 
  summarise(HomeRec=sum(Win-Loss-Draw),MatchesPlayed=(count=n()),
            Points=sum(Win*3,Draw*1),PPM=(Points/MatchesPlayed),
            PtPct=percent(Points/(3*MatchesPlayed)),
            GS=sum(FTHG),
            GSM=(GS/MatchesPlayed),
            GA=sum(FTAG),
            GAM=(GA/MatchesPlayed),Win=sum(Win))

  # Retrieving Away games data from the parent Dataframe
  
  AwayData <- select(s,Teamname = AwayTeam, Date,FTHG,FTAG, FTR)
  AwayData <- mutate(AwayData, Win = (if_else(FTR=='A',1,0)))
  AwayData<- mutate(AwayData,Loss=(if_else(FTR=='H',1,0)))
  AwayData <- mutate(AwayData,Draw= (if_else(FTR=='D',1,0)))
  
  #Copying for streak calculation on Away Data
  streaka <- select(AwayData,Teamname,Date,FTR) 
  
  AwayData <- select(AwayData,Teamname,FTHG,FTAG, Date,Win,Loss,Draw)
  
  #Select Last10 records for each team where they've played games Away
  AL10 <- select(AwayData,Teamname,Date, Win,Loss,Draw) 
  AL10 <- do.call("rbind", lapply(split(AL10, AL10$Teamname), tail,  10))

  #Create consolidated Dataframe for Away Games
  
  AwayData2 <- AwayData %>% 
    group_by(Teamname) %>% 
    summarise(AwayRec=sum(Win-Loss-Draw),MatchesPlayed=(count=n()),
              Points=sum(Win*3,Draw*1),PPM=(Points/MatchesPlayed),
              PtPct=percent(Points/(3*MatchesPlayed)),
              GS=sum(FTAG),
              GSM=(GS/MatchesPlayed),
              GA=sum(FTHG),
              GAM=(GA/MatchesPlayed),Win=sum(Win))
  
  #Merging Home and Away game Dataframe
  MergeData <- merge(HomeData2, AwayData2, all=TRUE)
  
  
  #Calculating Last10 on merged data 
  Last10 <- merge(HL10, AL10, all=TRUE)
  Team <- unique(Last10$Teamname)
  datalist2=list()
  for (i in 1:length(Team)){
    i< i +1
    tmp2 <- Last10[Last10$Teamname == Team[i], ] 
    #tmp2$Date <- dmy(tmp2$Date)
    tmp2<- tmp2[order(as.Date(tmp2$Date,format="%d/%m/%Y")),]
    datalist2[[i]] <- tmp2
  }
  Last10 = do.call(rbind, datalist2)
  Last10 <- do.call("rbind", lapply(split(Last10,Last10$Teamname), tail,  10))
  Last10<- select(Last10,Teamname,Win,Loss,Draw)
  L10 <- Last10 %>% 
    group_by(Teamname) %>% 
    summarise(Last10=sum(Win-Loss-Draw))
  
  
  #Replacing 0 with NA in HomeRec and AwayRec columns of Merged Data
  df2 <- MergeData %>% 
    group_by(Teamname) %>% 
    mutate(HomeRec = (if_else(is.na(HomeRec),0,HomeRec))) %>% 
    mutate(AwayRec= (ifelse(is.na(AwayRec), 0, AwayRec))) %>% 
    mutate(Record = sum(HomeRec,AwayRec)) 
  
 
  #Creating Final Dataframe 
  df2<- df2 %>% 
    group_by(Teamname) %>% 
    summarise(Record=sum(HomeRec,AwayRec),HomeRec=sum(HomeRec),AwayRec=sum(AwayRec),
          MatchesPlayed=sum(MatchesPlayed),Points=sum(Points),
          PPM=(Points/MatchesPlayed),PtPct=percent(Points/(3*MatchesPlayed)),
          GS=sum(GS),
          GSM=(GS/MatchesPlayed),
          GA=sum(GA),
          GAM=(GA/MatchesPlayed),Win=sum(Win))
  
  #Adding Last10 to Final Dataframe
  
  df2<-  mutate(df2,Last10=L10$Last10)
  
  
  #Streak Calculation from Home Data
  streakh <- do.call("rbind", lapply(split(streakh, streakh$Teamname), tail,  10))
  streakh <- mutate(streakh,W_L_D= (if_else(FTR=='H',"W",
                                            if_else(FTR=='A',"L","D"))))

  #Streak Calculation from Away Data
  streaka <- do.call("rbind", lapply(split(streaka, streaka$Teamname), tail,  10))
  streaka <- mutate(streaka,W_L_D= (if_else(FTR=='A',"W",
                                            if_else(FTR=='H',"L","D"))))
  
  #Streak Calculation on merge of Home and Away Data-
  streakm <- merge(streakh, streaka, all=TRUE)
  
  #Getting unique team names to use against in a loop to calculate streak
  Tname <- unique(streakm$Teamname)
  
  #Loop to sort dates per individual teams and populate a Dataframe
  i <- 0
  datalist1=list()
  for (i in 1:length(Tname)){
    tmp1 <- streakm[streakm$Teamname == Tname[i], ] 
    tmp1<- tmp1[order(as.Date(tmp1$Date,format="%d/%m/%Y")),]
    datalist1[[i]] <- tmp1
    i= i +1
  }
    streakm = do.call(rbind, datalist1)
    streakm <- do.call("rbind", lapply(split(streakm, streakm$Teamname), tail,  10))

    #Loop to calculate streak from dataframe populated above
  i <- 0
  datalist = list()
  for (i in 1:length(Tname)){
    tmp <- streakm[streakm$Teamname == Tname[i], ]
    #tmp <- streakm[streakm$Teamname == 'Brighton', ]
    rle_w <-rle(as.character(tmp$W_L_D == "W"))
    valuew <-rle_w$values
    W<- tail(valuew, n = 1)
    lengthw <- rle_w$lengths
    numw <-tail(lengthw, n = 1) 
    
    rle_l <-rle(as.character(tmp$W_L_D == "L"))
    valuel <-as.character(rle_l$values)
    L<- tail(valuel, n = 1)
    lengthl <- rle_l$lengths
    numl <-tail(lengthl, n = 1) 
    
    rle_d <-rle(as.character(tmp$W_L_D == "D"))
    valuet <-rle_d$values
    t<- tail(valuet, n = 1)
    lengtht <- rle_d$lengths
    numd <-tail(lengtht, n = 1) 
    
   if(W =='TRUE'){
     Res<- paste(c('W',numw), collapse='')
     datalist[[i]] <- Res
   }else if(L =='TRUE'){
     Res<- paste(c('L',numl), collapse='')
     datalist[[i]] <- Res
   }else if (t =='TRUE'){
     Res<-(paste(c('T',numd), collapse=''))
     datalist[[i]] <- Res
   } 
    i= i +1
    
  }
  
  #Adding Streak row to the dataframe
  streak_data = do.call(rbind, datalist)
  
  #Final Dataframe is below
  df3 <- mutate(df2,Streak=streak_data) 
  
  #Sorting Data as per PPM
  df4 <- df3[with(df3,order(-PPM)),]
  df5 <- select(df4,Teamname,Win,Record,HomeRec,AwayRec,MatchesPlayed
                ,Points,PPM,PtPct,GS,GSM,GA,GAM,Last10,Streak)
  
  #Check for getting rows with same PPM
  df6 <-  df5 %>% group_by(PPM) %>% filter(n()>1) 
  
  # Ordering the rows with same PPM with Wins(dsc),GSM(dsc) and GAM(asc)
  df6 <- df6[with(df6,order(-Win,-GSM,GAM)),]
  
  # Replacing the Ordered rows in the Original Dataframe
  df5[df5$Teamname %in% df6$Teamname, ] <- df6
  
  # Creating Final dataframe excluding Win column to pass on back to the function call
  df7 <- select(df5,Teamname,Record,HomeRec,AwayRec,MatchesPlayed
               ,Points,PPM,PtPct,GS,GSM,GA,GAM,Last10,Streak)
  
  #Returning the final dataframe to the main function call
  
  
  } else{
    print("Please use date range which lies in between the start and end date of season used in the 2nd argument of the function call")
 }
return(df7)
}


# Function Call -----------------------------------------------------------

# Please send Date(First Argument in MM/DD/YYY format).
# Please use only the dates which are part of the particular season.
# Season name should be in YYYY/YY format.

Final_Dataframe <-EPL_Standings("12/31/2017","2017/18")
