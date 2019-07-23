library(httr)
library(XML)
library(dplyr)
library(tidyr)
library(DBI)
library(RSQLite)

setwd("/Users/juliesmith26/Desktop/Summer2019")

#################################
## A - Data scraping functions ##
#################################

url <- "http://wslwebservices.leg.wa.gov/" ## URL prefix for all GET requests on the wsl service.

## Function: getLegislation
## Arguments: biennium - a character string of the format "XXXX-YY" representing the two years of the particular congress
##            billNumber - a character string of the format "XXXX" for the number of the bill to scrape
##
## Returns: a dataframe of legislative summary information including billId, introduction date, bill titles/descriptions

getLegislation <- function(biennium, billNumber){
  
  path <- paste("legislationservice.asmx/GetLegislation?biennium=", 
                gsub(" ", "%20", biennium), "&billNumber=", gsub(" ", "%20", billNumber), sep="")
  
  return(xmlToDataFrame(xmlParse(paste(url, path, sep = "")),
                        stringsAsFactors = FALSE))
}

## Function: getLegislationByYear
## Arguments: year - a character string of the format "XXXX"
##
## Returns: a dataframe containing a list of all of the bills introduced during the year

getLegislationByYear <- function(year){
  
  path <- paste("legislationservice.asmx/GetLegislationByYear?year=", gsub(" ", "%20", year), sep="")
  
  return(xmlToDataFrame(xmlParse(paste(url, path, sep = "")),
                        stringsAsFactors = FALSE))
}

## Function: getCurrentStatus
## Arguments: biennium - a character string of the format "XXXX-YY" representing the two years of the particular congress
##            billNumber - a character string of the format "XXXX" for the number of the bill to scrape
##
## Returns: a dataframe with the bill's current status
##          **NOTE** THIS IS THE BILL'S STATUS AS OF TODAY. IF A BILL WAS NEVER PASSED, IT LISTS THE MOST RECENT STATUS

getCurrentStatus <- function(biennium, billNumber){
  path <- paste("legislationservice.asmx/GetCurrentStatus?biennium=", 
                gsub(" ", "%20", biennium), "&billNumber=", gsub(" ", "%20", billNumber), sep="")
  
  return(xmlToDataFrame(xmlParse(paste(url, path, sep = "")),
                        stringsAsFactors = FALSE))
}

## Function: getStatusChanges
## Arguments: biennium - a character string of the format "XXXX-YY" representing the two years of the particular congress
##            billNumber - a character string of the format "XXXX" for the number of the bill to scrape
##            beginDate - a character string of the format "YYYY-MM-DD"
##            endDate - a character string of the format "YYYY-MM-DD"
##
## Returns: a dataframee of all status changes occurring on the bill between the dates listed

getStatusChanges <- function(biennium, billNumber, beginDate, endDate){
  path <- paste("legislationservice.asmx/GetLegislativeStatusChangesByBillNumber?biennium=", 
                gsub(" ", "%20", biennium), "&billNumber=", gsub(" ", "%20", billNumber), 
                "&beginDate=", beginDate, "&endDate=", endDate, sep="")
  
  return(xmlToDataFrame(xmlParse(paste(url, path, sep = "")),
                        stringsAsFactors = FALSE))
}

## Function: getAmendment
## Arguments: biennium - a character string of the format "XXXX-YY" representing the two years of the particular congress
##            billNumber - a character string of the format "XXXX" for the number of the bill to scrape
##
## Returns: a dataframe of all amendment actions (accepted and rejected) on the particular bill, including URL to the amendment text

getAmendments <- function(biennium, billNumber){
  path <- paste("legislationservice.asmx/GetAmendmentsForBiennium?biennium=", 
                biennium, "&billNumber=", billNumber, sep = "")
  
  return(xmlToDataFrame(xmlParse(paste(url, path, sep = "")),
                        stringsAsFactors = FALSE))
  
}

## Function: getHearings
## Arguments: biennium - a character string of the format "XXXX-YY" representing the two years of the particular congress
##            billNumber - a character string of the format "XXXX" for the number of the bill to scrape
##
## Returns: a dataframe of dates, locations, and descriptions of committee hearings on the particular bill

getHearings <- function(biennium, billNumber){
  path <- paste("legislationservice.asmx/GetHearings?biennium=",
                biennium, "&billNumber=", billNumber, sep = "")
  
  return(xmlToDataFrame(xmlParse(paste(url, path, sep = "")),
                        stringsAsFactors = FALSE))
}

## Function: getRollCalls
## Arguments: biennium - a character string of the format "XXXX-YY" representing the two years of the particular congress
##            billNumber - a character string of the format "XXXX" for the number of the bill to scrape
##
## Returns: a dataframe of roll call information on the particular bill
##          **NOTE** BECAUSE XML ALLOWS NESTED LISTS, THE XML PACKAGE CANNOT COMPLETELY PARSE SOME PORTIONS INTO A COMPREHENSIBLE DATA FRAME
##          **NOTE** BECAUSE OF THIS, THE CURRENT IMPLEMENTATION OF THIS FUNCTION IS INCOMPLETE

getRollCalls <- function(biennium, billNumber){
  path <- paste("legislationservice.asmx/GetRollCalls?biennium=",
                biennium, "&billNumber=", billNumber, sep = "")
  
  return(xmlToDataFrame(xmlParse(paste(url, path, sep = "")),
                        stringsAsFactors = FALSE))
}

## Function: getSponsors
## Arguments: biennium - a character string of the format "XXXX-YY" representing the two years of the particular congress
##
## Returns: a dataframe of all of the sponsors (all congressmembers) for that biennium

getSponsors <- function(biennium){
  path <- paste("sponsorservice.asmx/GetSponsors?biennium=",
                biennium, sep = "")
  
  return(xmlToDataFrame(xmlParse(paste(url, path, sep = "")),
                        stringsAsFactors = FALSE))
}

## Function: getBillSponsors
## Arguments: biennium - a character string of the format "XXXX-YY" representing the two years of the particular congress
##            billId - a character string of the format "XX YYYY" where XX is the prefix (HB, SB, etc.) and YYYY is the bill number
##
## Returns: a dataframe of all sponsors on the bill, including co-sponsors

getBillSponsors <- function(biennium, billId){
  path <- paste("legislationservice.asmx/GetSponsors?biennium=",
                biennium, "&billId=", billId, sep = "")
  
  return(xmlToDataFrame(xmlParse(paste(url, path, sep = "")),
                        stringsAsFactors = FALSE))
}

## Function: getLegislationSigned
## Arguments: biennium - a character string of the format "XXXX-YY" representing the two years of the particular congress
##            agency - either "House" or "Senate"
##
## Returns: a dataframe containing all of the bills that originated in the specified chamber and were eventually signed into law

getLegislationSigned <- function(biennium, agency){
  path <- paste("legislationservice.asmx/GetLegislationGovernorSigned?biennium=", 
                biennium, "&agency=", agency, sep = "")
  
  return(xmlToDataFrame(xmlParse(paste(url, path, sep = "")),
                        stringsAsFactors = FALSE))
}

## Function: getCommittees
## Arguments: biennium - a character string of the format "XXXX-YY" representing the two years of the particular congress
##
## Returns: a dataframe of all of the committees and their corresponding committee code that were active in that biennium

getCommittees <- function(biennium){
  path <- paste("CommitteeService.asmx/GetCommittees?biennium=",
                biennium, sep = "")
  
  return(xmlToDataFrame(xmlParse(paste(url, path, sep = "")),
                        stringsAsFactors = FALSE))
}

#######################
## Placeholder title ##
#######################

## Generate list of biennia

biennium_list = c()
counterb = 1
for (year in seq(2001, 2019, 2)){
  biennium_list[counterb] = paste(year, "-", substr(year+1,3,4), sep="")
  counterb = counterb + 1
}

## Generate master committee table

master_comms <- data.frame()

for(biennium in biennium_list){
  year_comm <- getCommittees(biennium)
  
  year_comm <- year_comm %>% mutate(Biennium = biennium, Id = strtoi(Id))
  
  master_comms <- bind_rows(master_comms, year_comm)
}

master_comms$Acronym <- trimws(master_comms$Acronym)

master_comms$CommitteeId <- NA
commCounter <- 0

for(counter in 1:nrow(master_comms)){
  master_comms$CommitteeId[counter] <- (strtoi(substr(master_comms$Biennium[counter], 1, 4)) - 1991) / 2 + 65
  
  master_comms$CommitteeId[counter] <- intToUtf8(master_comms$CommitteeId[counter])
  
  if(counter == 1){
    master_comms$CommitteeId[counter] <- paste(master_comms$CommitteeId[counter], as.character(commCounter), sep = "")
  }
  else if(master_comms$Biennium[counter] == master_comms$Biennium[counter-1]){
    commCounter <- commCounter + 1
    
    master_comms$CommitteeId[counter] <- paste(master_comms$CommitteeId[counter], as.character(commCounter), sep = "")
  }
  else{
    commCounter <- 0
    
    master_comms$CommitteeId[counter] <- paste(master_comms$CommitteeId[counter], as.character(commCounter), sep = "")
  }
}

## Generate master sponsors table

master_sponsors <- data.frame()

for(biennium in biennium_list){
  year_spons <- getSponsors(biennium)
  
  year_spons <- year_spons %>% mutate(Biennium = biennium, repId = strtoi(Id), type = ifelse(Agency == "House", "rep", "sen"))
  year_spons <- year_spons %>% mutate(cong = length(seq(1889,substr(biennium,1,4),2)))
  
  master_sponsors <- bind_rows(master_sponsors, year_spons)
}

master_sponsors <- master_sponsors %>% arrange(LastName) %>% mutate(Id = seq(1,nrow(master_sponsors))) %>% select(Id, repId, cong, FirstName, LastName, type, District, Party, Biennium)

## Generate legislation table

raw_bills <- data.frame()
master_bills <- data.frame()

for(biennium in biennium_list){
  year1 <- strtoi(substr(biennium,1,4))
  year2 <- year1 + 1
  
  year1_bills <- getLegislationByYear(as.character(year1))
  year2_bills <- getLegislationByYear(as.character(year2))
  
  biennium_bills <- bind_rows(year1_bills, year2_bills)
  
  raw_bills <- bind_rows(raw_bills, biennium_bills) %>% select(Biennium, BillId, BillNumber)
}

sampleSize <- 5

for(biennium in biennium_list){
  year_bills <- raw_bills[which(raw_bills$Biennium == biennium) ,]
  
  year_sample <- sample_n(year_bills, sampleSize, replace = FALSE)
  
  for(bill in 1:nrow(year_sample)){
    current_bill <- getLegislation(year_sample$Biennium[bill], year_sample$BillNumber[bill])
    
    master_bills <- bind_rows(master_bills, current_bill)
  }
}

## Generate action table

raw_actions <- data.frame()

for(bill in 1:nrow(master_bills)){
  current_action <- getStatusChanges(master_bills$Biennium[bill], master_bills$BillNumber[bill],
                               paste("01-01-", substr(master_bills$Biennium[bill],1,4), sep = ""),
                               paste("12-31-", as.character(strtoi(substr(master_bills$Biennium[bill],1,4)) + 1), sep = ""))
  
  current_action <- current_action %>% mutate(Biennium = master_bills$Biennium[bill])
  
  raw_actions <- bind_rows(raw_actions, current_action)
}

##
## Placeholder title
##

actionTableCopy <- raw_actions
actionTableCopy$Status <- sapply(actionTableCopy$Status, trimws)

len = nrow(actionTableCopy)
actionTableCopy$Action = rep(NA, len) #holds actions

actionTableFinal = data.frame() #the actionTable that all biennium-based actionTables are put into

for (biennium in biennium_list){
  counter = 1
  
  subset_bi_df = master_comms[which(master_comms$Biennium==biennium),] #committees in that biennium
  committee_names = subset_bi_df$Name #all the committees by name in  biennium
  committee_acronyms = subset_bi_df$Acronym #all coms by acronym in biennium
  
  actionTableBiennium = actionTableCopy[actionTableCopy$Biennium==biennium,] #actionTable lines that happen in biennium given
  
  descriptions = actionTableBiennium[,2] #the descriptions of bill's progress to iterate through
  
  location_flag = ""
  current_bill = actionTableBiennium$BillId[counter] #sets starting bill for biennium
  
  #sets initial location of bill
  if (grepl("HB", actionTableBiennium$BillId[counter])){
    location_flag = "House"
  }
  if (grepl("SB", actionTableBiennium$BillId[counter])){
    location_flag = "Senate"
  }
  
  for (d in descriptions){ #d is a single line of an event in the bill's history
    
    if (actionTableBiennium$BillId[counter] == current_bill){ #if d is in the same bill as previous line d-1
      
      if (grepl("eturned to Senate", d)){
        #this bill has been passed to Senate
        location_flag = "Senate" }
      
      if (grepl("eturned to House", d)){
        #this bill has been passed to House
        location_flag = "House" }
      
    }
    
    else if (actionTableBiennium$BillId[counter] != current_bill){ #we are now in a new bill
      current_bill = actionTableBiennium$BillId[counter] #update current bill
      
      #sets starting location
      if (grepl("HB", actionTableBiennium$BillId[counter])){
        location_flag = "House"
      }
      if (grepl("SB", actionTableBiennium$BillId[counter])){
        location_flag = "Senate"
      }
    }
    
    for (acronym in committee_acronyms){ 
      if (grepl(acronym, d)){
        temp4 = master_comms[which(master_comms$Acronym==acronym & master_comms$Biennium==biennium),]
        actionTableBiennium$Action[counter] = temp4$CommitteeId 
      }
      
      for (committee in committee_names){
        
        if (grepl(paste("eferred to", committee),d)){
          #if true, then d contains bill was "refereed to committee" after this action
          temp1 = master_comms[which(master_comms$Name==committee & master_comms$Biennium==biennium & master_comms$Acronym==acronym),]
          if(length(temp1$Name) > 0){
            actionTableBiennium$Action[counter] = temp1$CommitteeId 
          }
        }
        
        #House/Senate Rule Committee
        if (grepl(paste("eturned to", paste(location_flag, "Rules Committee")), d)){ #looks for "returned to House/Senate Rules Committee"
          actionTableBiennium$Action[counter] = subset_bi_df$CommitteeId[which(subset_bi_df$LongName==paste(location_flag, "Committee on Rules", sep = " "))] }
        
        if (grepl(paste("eturned to", paste(location_flag, "Rules \"X\" file")), d)){ #looks for "House/Senate Rules "X" File" 
          actionTableBiennium$Action[counter] = subset_bi_df$CommitteeId[which(subset_bi_df$LongName==paste(location_flag, "Committee on Rules", sep = " "))]  }
        
        if (grepl("by Rules Committee", d)){ #looks for "by Rules Committee"
          actionTableBiennium$Action[counter] = subset_bi_df$CommitteeId[which(subset_bi_df$LongName==paste(location_flag, "Committee on Rules", sep = " "))]  }
        
        if (grepl("passed to Rules|Passed to Rules", d)){ #looks for "passed to Rules"
          actionTableBiennium$Action[counter] = subset_bi_df$CommitteeId[which(subset_bi_df$LongName==paste(location_flag, "Committee on Rules", sep = " "))]  }
        ##########
        
        if (location_flag == "House"){
          #if true, then this description is in the house
          
          if (grepl("Third reading|Third Reading", d)){
            actionTableBiennium$Action[counter] = "House Floor" }
          
          if (grepl("Ways & Means", d)){
            temp7 = master_comms[which(master_comms$Name==committee & master_comms$Biennium==biennium & master_comms$Acronym=="WAYS"),]
            if (length(temp7$Name)>0){
              actionTableBiennium$Action[counter] = temp7$CommitteeId }}
          
          if (grepl("Higher Education", d)){
            temp10 = master_comms[which(master_comms$Name==committee & master_comms$Biennium==biennium & master_comms$Acronym=="HE"),]
            if (length(temp10$Name)>0){
              actionTableBiennium$Action[counter] = temp10$Name #temp10$CommitteeId
            }}
        }
        
        if (location_flag == "Senate"){
          #if true, then this description is in the senate
          
          if (grepl("hird reading|Third Reading", d)){
            actionTableBiennium$Action[counter] = "Senate Floor" }
          
          if (grepl("Ways & Means", d)){
            temp8 = master_comms[which(master_comms$Name==committee & master_comms$Biennium==biennium & master_comms$Acronym=="WM"),]
            if (length(temp8$Name)>0){
              actionTableBiennium$Action[counter] = temp8$CommitteeId }}
          
          if (grepl("Higher Education", d)){
            temp11 = master_comms[which(master_comms$Name==committee & master_comms$Biennium==biennium & master_comms$Acronym=="HIE"),]
            if (length(temp11$Name)>0){
              actionTableBiennium$Action[counter] = temp11$CommitteeId }}
        }
        
      }
      
    }
    
    if (grepl("Delivered to Governor", d)){
      #if true, then bill was delivered to governor
      actionTableBiennium$Action[counter] = "Governor's Desk" }
    
    if (grepl("Governor signed", d)){
      #if true, then bill was signed
      actionTableBiennium$Action[counter] = "Sign" }
    
    if (grepl("Speaker signed", d)){
      #if true, then house speaker signed bill
      actionTableBiennium$Action[counter] = "House Floor" }
    
    if (grepl("President signed", d)){
      #if true, then senate president signed bill
      actionTableBiennium$Action[counter] = "Senate Floor" }
    
    if (grepl("onference", d)){
      actionTableBiennium$Action[counter] = "Conference" }
    
    if (grepl("Senate Rules", d)){
      actionTableBiennium$Action[counter] = subset_bi_df$CommitteeId[which(subset_bi_df$LongName==paste(location_flag, "Committee on Rules", sep = " "))]  }
    
    if (grepl("ffective date", d)){
      actionTableBiennium$Action[counter] = "Became law" }
    
    if (length((d == d[grep("Third reading, passed", d)])) >0){
      if (d == d[grep("Third reading, passed", d)]){
        #this bill has been passed to other chamber, so location_flag needs to change
        if (location_flag == "House"){
          location_flag = "Senate" }
        else if (location_flag == "Senate"){
          location_flag = "House" }
      }} 
    
    counter = counter + 1
    
    
  }
  actionTableFinal = rbind(actionTableFinal, actionTableBiennium)
  
}

master_actions <- actionTableFinal[complete.cases(actionTableFinal[, 10]) ,]

######################
## SQLite Packaging ##
######################

WashEx <- dbConnect(RSQLite::SQLite(), dbname = "WashEx-db.sqlite")

dbWriteTable(WashEx, "Legislation", master_bills, overwrite = TRUE)
dbWriteTable(WashEx, "Sponsors", master_sponsors, overwrite = TRUE)
dbWriteTable(WashEx, "Committees", master_comms, overwrite = TRUE)
dbWriteTable(WashEx, "Actions", master_actions, overwrite = TRUE)

dbDisconnect(WashEx)

toc()