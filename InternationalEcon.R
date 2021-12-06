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
setwd("/Users/annevalder/Desktop/LJKHCprediction_ols, test$MedHousePrice/WU WIEN/WS21_22_Oslo/International_Seminar/data/STcsv")

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

s1_HS <- get.Comtrade(r="all", p="all", rg = 2,fmt="csv", ps="2009,2010,2011,2012,2013,2014,2015,2016",px="HS")
s1_HS <- s1_HS$data
summary(s1_HS)

s2_HS <- get.Comtrade(r="all", p="all", rg = 2,fmt="csv", ps="2001,2002,2003,2004,2005,2006,2007,2008",px="HS")
s2_HS <- s2_HS$data
summary(s2_HS)

s3_HS <- get.Comtrade(r="all", p="all", rg = 2,fmt="csv", ps="1993,1994,1995,1996,1997,1998,1999,2000",px="HS")
s3_HS <- s3_HS$data
summary(s3_HS)

s4_HS <- get.Comtrade(r="all", p="all", rg = 2,fmt="csv", ps="1984,1985,1986,1987,1988,1989,1990,1991,1992",px="HS") #  I think there is no data down from 1988
s4_HS <- s4_HS$data
summary(s4_HS)

data_all_HS <- rbind(s4_HS,s3_HS,s2_HS,s1_HS)
data_all_clean_HS <- data_all_HS[,c(1,2,7,10,11,13,14,32)]

############### Extract ST data for 1984-2016 ####################

s1_ST <- get.Comtrade(r="all", p="all", rg = 2,fmt="csv", ps="2009,2010,2011,2012,2013,2014,2015,2016",px="ST")
s1_ST <- s1_ST$data
summary(s1_ST)

s2_ST <- get.Comtrade(r="all", p="all", rg = 2,fmt="csv", ps="2001,2002,2003,2004,2005,2006,2007,2008",px="ST")
s2_ST <- s2_ST$data
summary(s2_ST)

s3_ST <- get.Comtrade(r="all", p="all", rg = 2,fmt="csv", ps="1993,1994,1995,1996,1997,1998,1999,2000",px="ST")
s3_ST <- s3_ST$data
summary(s3_ST)

s4_ST <- get.Comtrade(r="all", p="all", rg = 2,fmt="csv", ps="1984,1985,1986,1987,1988,1989,1990,1991,1992",px="ST") #  I think there is no data down from 1988
s4_ST <- s4_ST$data
summary(s4_ST)

data_all_ST <- rbind(s4_ST,s3_ST,s2_ST,s1_ST)
data_all_clean_ST <- data_all_ST[,c(1,2,7,10,11,13,14,32)]

############# Read in TREND data set #############
library(readxl)
library(dplyr)
library(tidyverse)
setwd("/Users/annevalder/Desktop/LJKHCprediction_ols, test$MedHousePrice/WU WIEN/WS21_22_Oslo/International_Seminar/data")
data_path = "."
TREND <- read_xlsx(file.path(data_path, "trenddyadic.xlsx"),sheet = )
TREND_clean <- drop_na(TREND)
TREND_clean <- TREND %>% filter(TREND$Year == c(1984:2016))
colnames(TREND_clean)[2] <- "country"
#####GNI data##########
GNI <- read_csv(file.path(data_path, "GNI_data.csv"))
GNI <- GNI[,c(3:5)]

DATA <- left_join(TREND_clean,GNI,by="country")

#colnames(GNI)[1] <- "country"

##### Create dummy 
GNI$`2016 [YR2016]` <- as.integer(GNI$`2016 [YR2016]`)
GNI <- drop_na(GNI)

GNI$DevelpC <- ifelse(GNI$`2016 [YR2016]` < 12476, 1, 0)

test <- left_join(TREND_clean,GNI,by="country")
test <- test %>% filter(DevelpC == 1)

test2 <- left_join(TREND_clean,GNI,by="country2")
test2 <- test2 %>% filter(DevelpC == 1)



