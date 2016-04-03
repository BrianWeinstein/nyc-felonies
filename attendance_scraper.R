
# load packages
library(XML)
library(httr)
library(dplyr)
library(data.table)

# set working directory
setwd("~/Documents/nyc-felonies")

# create date list
date_list <- gsub("-", "", seq(from = as.Date("2015-01-01"), to = as.Date("2016-03-11"), by = 1))

# initalize an empty list to store results
storage <- list() ; startTime <- Sys.time()

for(i in 1:length(date_list)){
  
  message(paste0(i, ": ", date_list[i]))
  
  # get xml for day i
  response <- GET(paste0("https://raw.githubusercontent.com/ajschumacher/NYCattends/master/xml/", date_list[i], ".xml"))
  
  if(response$status_code == 200){
    
    # convert to list
    xml_doc <- xmlToList(xmlParse(rawToChar(response$content)))
    
    # extract the daily citywide total
    daily_pct <- as.data.frame(t(matrix(unlist(xml_doc[length(xml_doc)]))),
                               stringsAsFactors = FALSE) %>%
      select(date = V2, attn_pct = V3) %>%
      mutate(date=as.character(as.Date(date, format = "%Y%m%d")),
             attn_pct=as.numeric(attn_pct))
    
    rm(xml_doc)
    
    # store daily citywide total
    storage[[i]] <- daily_pct
    
    # append to csv in case of error
    suppressWarnings(write.table(daily_pct, file = "datasets/raw/school_attendance.csv", append = TRUE,
                                 col.names = ifelse(i == 1, TRUE, FALSE),
                                 row.names = FALSE, sep = ","))
    
  } else{
    
    # return null
    storage[[i]] <- NULL
    
  }
}
endTime <- Sys.time()
endTime - startTime

storagedf <- rbindlist(storage)

