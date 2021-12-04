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
           quote="", 
           header = T 
  )
}
)

#bind whole list to one dataframe
HSdat<-data.table::rbindlist(HSdat.list)


##############SITC classfication###################

#set working directory
setwd("~/WU/MASTER/WS2122/R&P International/Environment/RTA_environment/STcsv")

#list all files that are saved in the directory folder
temp1 = list.files(pattern="*.csv")
STdat.list <- lapply(temp1,function(x){
  read.csv(file = x,
           sep =",",
           fill = TRUE,
           quote="", 
           header = T 
  )
}
)

#bind whole list to one dataframe
STdat<-data.table::rbindlist(STdat.list)