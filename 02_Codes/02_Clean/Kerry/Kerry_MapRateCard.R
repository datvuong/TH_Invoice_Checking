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
    
    wb <- loadWorkbook(rateCardFilePath)  
    rateCard <- readWorksheet(object = wb, sheet = 1, colTypes = c(XLC$DATA_TYPE.STRING, XLC$DATA_TYPE.NUMERIC, XLC$DATA_TYPE.NUMERIC,
                                                                   XLC$DATA_TYPE.NUMERIC))
    wb <- loadWorkbook(postalCodePath)
    postalCode <- readWorksheet(object = wb, sheet = 1, colTypes = c(XLC$DATA_TYPE.STRING, XLC$DATA_TYPE.STRING))
    
    postalCode %<>%
      mutate(area_revised = ifelse(area == "Greater Bangkok", "Zone_A",
                                   ifelse(area == "Remote Are", "Remote_area", "Zone_B")))
    
    mergedOMSData_rev <- left_join(mergedOMSData, 
                                   postalCode %>%
                                     select(postal_code, area_revised),
                                   by = c("level_7_name" = "postal_code"))
    mergedOMSData_rev %<>%
      mutate(area_revised = ifelse(is.na(area_revised), "Zone_B", area_revised)) %>%
      mutate(area_revised = ifelse(existence_flag == "NOT_OKAY", NA, area_revised))
    
    mergedOMSData_rev %<>%
      mutate(Min = ifelse(package_weight <= 1, 0.1,
                          ifelse(package_weight <= 3, 1.1,
                                 ifelse(package_weight <= 5, 3.1,
                                        ifelse(package_weight <= 10, 5.1,
                                               ifelse(package_weight <= 15, 10.1, 15.1)))))) %>%
      mutate(Max = ifelse(package_weight <= 1, 1,
                          ifelse(package_weight <= 3, 3,
                                 ifelse(package_weight <= 5, 5,
                                        ifelse(package_weight <= 10, 10,
                                               ifelse(package_weight <= 15, 15, 20))))))
    
    mergedOMSData_rev <- left_join(mergedOMSData_rev, rateCard,
                                   by = c("area_revised" = "Zone",
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