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
                         col.names = c("Zone", "weightCategory", "Rates" ), 
                         colClasses = c("character","character", "myNumeric"))
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
      mutate(weightCategory = as.character(ifelse(calculatedWeight <= 1, "w00-01",
                          ifelse(calculatedWeight <= 3, "w01-03",
                                 ifelse(calculatedWeight <= 5, "w03-05",
                                        ifelse(calculatedWeight <= 10, "w05-10",
                                               ifelse(calculatedWeight <= 15, "w10-15", 
                                                      ifelse(calculatedWeight <= 20, "w15-20", "w20-99"))))))))
    
    mergedOMSData_rev <- left_join(mergedOMSData_rev, rateCard,
                                   by = c("dest_area" = "Zone",
                                          "weightCategory"))
    
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