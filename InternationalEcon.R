################################
#######Packages#################
################################

if (!require(data.table)) install.packages('data.table') + library(data.table)

# Hall liebe Leute

################################
########Data import#############
################################


#############HS classification#####################

#set working directory to folder, with individual csv files (use STcsv and HScsv folders saved in Teams)
setwd("~/WU/MASTER/WS2122/R&P International/Environment/RTA_environment/HScsv")

#list all files that are saved in the directory folder
temp = list.files(pattern="*.csv")
HSdat.list <- lapply(temp,function(x){
  read.csv(file = x,
           sep =",",
           fill = TRUE,
           #quote="", 
           header = T 
  )
}
)

#bind whole list to one dataframe
HSdat<-data.table::rbindlist(HSdat.list)


##############SITC classfication###################

#set working directory
setwd("/Users/annevalder/Desktop/Uni/WU WIEN/WS21_22_Oslo/International_Seminar/data/STcsv")

#list all files that are saved in the directory folder
temp1 = list.files(pattern="*.csv")
STdat.list <- lapply(temp1,function(x){
  read.csv(file = x,
           sep =",",
           fill = TRUE,
           #quote="", 
           header = T 
  )
}
)

#bind whole list to one dataframe
STdat<-data.table::rbindlist(STdat.list)
summary(STdat)

##############################Get data via API#############################################

library(rjson)
string <- "http://comtrade.un.org/data/cache/partnerAreas.json"
reporters <- fromJSON(file=string)
reporters <- as.data.frame(t(sapply(reporters$results,rbind)))

get.Comtrade <- function(url="http://comtrade.un.org/api/get?"
                         ,maxrec=5000000
                         ,type="C" # commodities
                         ,freq="A" # anually
                         ,px="HS" # chnage here for other classification into "ST"
                         ,ps="now"
                         ,r
                         ,p
                         ,rg="all"
                         ,cc="TOTAL"
                         ,fmt="json"){
  string<- paste(url
                 ,"max=",maxrec,"&" #maximum no. of records returned
                 ,"type=",type,"&" #type of trade (c=commodities)
                 ,"freq=",freq,"&" #frequency
                 ,"px=",px,"&" #classification
                 ,"ps=",ps,"&" #time period
                 ,"r=",r,"&" #reporting area
                 ,"p=",p,"&" #partner country
                 ,"rg=",rg,"&" #trade flow
                 ,"cc=",cc,"&" #classification code
                 ,"fmt=",fmt        #Format
                 ,sep = "")
  
  if(fmt == "csv") {
    raw.data<- read.csv(string,header=TRUE)
    return(list(validation=NULL, data=raw.data))
  } else {
    if(fmt == "json" ) {
      raw.data<- fromJSON(file=string)
      data<- raw.data$dataset
      validation<- unlist(raw.data$validation, recursive=TRUE)
      ndata<- NULL
      if(length(data)> 0) {
        var.names<- names(data[[1]])
        data<- as.data.frame(t( sapply(data,rbind)))
        ndata<- NULL
        for(i in 1:ncol(data)){
          data[sapply(data[,i],is.null),i]<- NA
          ndata<- cbind(ndata, unlist(data[,i]))
        }
        ndata<- as.data.frame(ndata)
        colnames(ndata)<- var.names
      }
      return(list(validation=validation,data =ndata))
    }
  }
}

############### Extract HS data for 1984-2016 ####################

s1_HS <- get.Comtrade(r="all", p="all", rg = 2,fmt="csv", ps="2009,2010,2011,2012,2013,2014,2015,2016",px="H1")
s1_HS <- s1_HS$data
summary(s1_HS)

s2_HS <- get.Comtrade(r="all", p="all", rg = 2,fmt="csv", ps="2001,2002,2003,2004,2005,2006,2007,2008",px="H1")
s2_HS <- s2_HS$data
summary(s2_HS)

s3_HS <- get.Comtrade(r="all", p="all", rg = 2,fmt="csv", ps="1993,1994,1995,1996,1997,1998,1999,2000",px="H1")
s3_HS <- s3_HS$data
summary(s3_HS)

s4_HS <- get.Comtrade(r="all", p="all", rg = 2,fmt="csv", ps="1984,1985,1986,1987,1988,1989,1990,1991,1992",px="H1") #  I think there is no data down from 1988
s4_HS <- s4_HS$data
summary(s4_HS$Year)

data_all_HS <- rbind(s4_HS,s3_HS,s2_HS,s1_HS)
data_all_clean_HS <- data_all_HS[,c(1,2,7,10,11,13,14,32)]
summary(data_all_clean_HS)
# drop World observations

data_all_clean_HS <- data_all_clean_HS %>% filter(Partner !="World") # Reporter cannot be World right? if I filter years 1984-1988 drop out!

############### Extract ST data for 1984-2016 ####################

s1_ST <- get.Comtrade(r="all", p="all", rg = 2,fmt="csv", ps="2010,2011,2012,2013,2014,2015,2016",px="S1")
s1_ST <- s1_ST$data
summary(s1_ST$Year)

s2_ST <- get.Comtrade(r="all", p="all", rg = 2,fmt="csv", ps="2001,2002,2003,2004,2005,2006,2007,2008",px="S1")
s2_ST <- s2_ST$data
summary(s2_ST)
summary(s2_ST$Year)

s3_ST <- get.Comtrade(r="all", p="all", rg = 2,fmt="csv", ps="1993,1994,1995,1996,1997,1998,1999,2000",px="S1")
s3_ST <- s3_ST$data
summary(s3_ST)
summary(s3_ST$Year)

s4_ST <- get.Comtrade(r="all", p="all", rg = 2,fmt="csv", ps="1984,1985,1986,1987,1988,1989,1990,1991,1992",px="S1") #  I think there is no data down from 1988
s4_ST <- s4_ST$data
summary(s4_ST)
summary(s4_ST$Year)

data_all_ST <- rbind(s4_ST,s3_ST,s2_ST,s1_ST)
data_all_clean_ST <- data_all_ST[,c(1,2,7,10,11,13,14,32)]
summary(data_all_clean_ST)

# drop World observations

data_all_clean_ST <- data_all_clean_ST %>% filter(Partner !="World") # Reporter cannot be World right? 
#summary(data_all_clean_ST)

#####################Combine HS and ST #####################

Comtrade <- merge(data_all_clean_HS,data_all_clean_ST,by=c("Reporter","Partner","Year"))

############# Read in TREND data set #############
library(readxl)
library(dplyr)
library(tidyverse)
setwd("/Users/annevalder/Desktop/Uni/WU WIEN/WS21_22_Oslo/International_Seminar/data/")
data_path = "."
TREND <- read_xlsx(file.path(data_path, "trenddyadic.xlsx"),sheet = ) # we have 295 PEs here not 286 as they say in the paper!
TREND <- drop_na(TREND)
TREND <- TREND %>% filter(TREND$Year == c(1984:2016)) # select only relevant years
TREND$PE_sum <- rowSums(TREND[,-c(1:4)]) # delete not needed columns

#####INCOME data##########
GNI <- read_csv(file.path(data_path, "GNI_data.csv"))
GNI <- GNI[1:217,c(3,5)] # select onyl countries and relevant columns! 
GNI <- drop_na(GNI)

#########  Different approach first align GNI to TREND and then create dummy for either country!
#### Have 2 seperate columns with income which I then both use to create the dummy on determining developing countries in PTAs

colnames(GNI)[1] <- "country"
colnames(TREND)[2] <- "country"
TREND_new <- left_join(TREND,GNI,by="country") 

colnames(GNI)[1] <- "country2"
TREND_new <- left_join(TREND_new,GNI,by="country2") 

TREND_new$`2016 [YR2016].x` <- as.integer(TREND_new$`2016 [YR2016].x`)
TREND_new$`2016 [YR2016].y` <- as.integer(TREND_new$`2016 [YR2016].y`)


TREND_new$DevelpC <- ifelse(TREND_new$`2016 [YR2016].x` < 9265 | TREND_new$`2016 [YR2016].y` < 9265 , 1, 0) # 2000 Value 
TREND_new <- filter(TREND_new,DevelpC == 1) ## also higher income values because only 1 needs to be developing !!! 
mean(TREND_new$PE_sum) # average number of PEs in PTAs! 26.26
max(TREND_new$PE_sum)# 134
###TO DO: COMPARE TO LIST OF DEVELOPING COUNTRIES IN PAPER APPENDIX 

### Relate Comtrade and TREND data ####

colnames(TREND_new)[2] <- "Reporter"
colnames(TREND_new)[3] <- "Partner"

# 1) Combine Comtrade (ALL trades) with modified TREND Data (includes all Trades with PTAs (including PEs))

TREND_new_small <- TREND_new[,c(1,2,3,4,300,301,302,303)] # delete not needed columns for now .. 
Data_final <- left_join(Comtrade,TREND_new_small,by=c("Partner","Reporter")) # JOIN COMTRADE AND TREND 

#### Keep only those which have PTA: 
###leads to only about 300...

Data_final_1 <- filter(Data_final, Data_final$Trade.Agreement != "")
#Data_final_2 <- Data_final %>% filter(!is.na(Trade.Agreement))

#### TEST just with 1 reporter country

### here it works with the smaller sample

Comtrade_AT <- filter(Comtrade,Reporter == "Austria")
TREND_new_small_AT <- filter(TREND_new_small, Reporter == "Austria")
Data_final_AT <- left_join(Comtrade_AT,TREND_new_small_AT,by=c("Partner","Reporter"))
Data_final_AT_1 <- filter(Data_final_AT, Data_final_AT$Trade.Agreement != "")






