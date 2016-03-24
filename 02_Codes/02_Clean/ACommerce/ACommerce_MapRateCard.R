MapRateCard <- function(mergedOMSData, rateCardFilePath, postalCodePath) {
  suppressMessages({
    require(dplyr)
    require(tools)
    require(magrittr)
    require(methods)
    require(futile.logger)
    require(XLConnect)
  })
  
  functionName <- "MapRateCard"
  flog.info(paste("Function", functionName, "started"), name = reportName)
  
  output <- tryCatch({
    setClass("myNumeric")
    setAs("character","myNumeric", function(from) as.numeric(gsub('[^0-9\\.]','',from)))
    
#     wb <- loadWorkbook(rateCardFilePath)  
#     rateCard <- readWorksheet(object = wb, sheet = 1, colTypes = c(XLC$DATA_TYPE.STRING, XLC$DATA_TYPE.NUMERIC, XLC$DATA_TYPE.NUMERIC,
                                                                   # XLC$DATA_TYPE.NUMERIC))
    rateCard <- read.csv(rateCardFilePath, quote = '"', sep=",", row.names = NULL,
                         col.names = c("Zone", "Min", "Max", "Rates" ), 
                         colClasses = c("character","myNumeric", "myNumeric", "myNumeric"))
#     wb <- loadWorkbook(postalCodePath)
    # postalCode <- readWorksheet(object = wb, sheet = 1, colTypes = c(XLC$DATA_TYPE.STRING, XLC$DATA_TYPE.STRING))
    postalCode <- read.csv(postalCodePath, quote = '"', sep=",", row.names = NULL,
                           col.names = c("postal_code","area"), 
                           colClasses = c("character", "character"))
    
#     mergedOMSData_rev <- left_join(mergedOMSData, 
#                                    postalCode ,
#                                    by = c("origin_branch" = "postal_code"))
#     mergedOMSData_rev %<>% mutate(origin_area = area) %>%
#       select(-c(area))
    
    mergedOMSData %<>% 
      mutate(is_OMSPostcode = ifelse(is.na(postcode), 0, 1)) %>%
      mutate(postcode = ifelse(is.na(postcode), destination_branch, postcode)) %>%
      mutate(postcode = gsub(".$", "0", gsub('[^0-9]', '', postcode))) 
    
    mergedOMSData_rev <- left_join(mergedOMSData, 
                                   postalCode ,
                                   by = c("postcode" = "postal_code"))
    mergedOMSData_rev %<>% mutate(dest_area =  area) %>%
      select(-c(area))
#     
#     mergedOMSData_rev %<>% mutate(area_revised = ifelse(origin_area == "Greater Bangkok" & dest_area == "Greater Bangkok", "Greater Bangkok", 
#                                                         ifelse(origin_area == "Remote area" | dest_area == "Remote area", "Remote area", 
#                                                                ifelse(origin_area == "Upcountry" | dest_area == "Upcountry", "Upcountry", NA))))
    
#     mergedOMSData_rev %<>%
#       # mutate(area_revised = ifelse(is.na(area_revised), "Zone_B", area_revised)) %>%
#       mutate(area_revised = ifelse(existence_flag == "NOT_OKAY", NA, area_revised))
    
    mergedOMSData_rev %<>%
      mutate(Min = ifelse(calculatedWeight <= 1, 0,
                                 ifelse(calculatedWeight <= 2, 1.1,  
                                        ifelse(calculatedWeight <= 5, 2.1,
                                               ifelse(calculatedWeight <= 10, 5.1,
                                                      ifelse(calculatedWeight <= 15, 10.1,
                                                             ifelse(calculatedWeight <= 20, 15.1, 20.1))))))) %>%
      mutate(Max = ifelse(calculatedWeight <= 1, 1,
                          ifelse(calculatedWeight <= 2, 2,  
                                 ifelse(calculatedWeight <= 5, 5,
                                        ifelse(calculatedWeight <= 10, 10,
                                               ifelse(calculatedWeight <= 15, 15,
                                                      ifelse(calculatedWeight <= 20, 20, 99)))))))
    
    mergedOMSData_rev <- left_join(mergedOMSData_rev, rateCard,
                                   by = c("dest_area" = "Zone",
                                          "Min", "Max"))
    
    mergedOMSData_rev %<>%
      mutate(RateCardMappedFlag = ifelse(is.na(Rates), "NOT_OKAY","OKAY"))
    
    mergedOMSData_rev
    
  }, error = function(err) {
    flog.error(paste(functionName, err, sep = " - "), name = reportName)
  }, finally = {
    flog.info(paste("Function", functionName, "ended"), name = reportName)
  })
  
  output
}