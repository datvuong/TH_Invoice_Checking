MapRateCard <- function(mergedOMSData, rateCardFilePath) {
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
    rateCard <- readWorksheet(object = wb, sheet = 1, colTypes = c(XLC$DATA_TYPE.STRING, XLC$DATA_TYPE.STRING, XLC$DATA_TYPE.STRING,
                                                                   XLC$DATA_TYPE.STRING, XLC$DATA_TYPE.STRING, XLC$DATA_TYPE.STRING,
                                                                   XLC$DATA_TYPE.NUMERIC, XLC$DATA_TYPE.NUMERIC, XLC$DATA_TYPE.NUMERIC,
                                                                   XLC$DATA_TYPE.NUMERIC, XLC$DATA_TYPE.STRING))
    
    
    
    rateCardRev <- rateCard %>%
      select(Region_name, City_name, District_name, first_1kg, add_1kg, Insurance_Charge, COD_Fee) %>%
      mutate(Region_name = gsub("[^a-zA-Z0-9]", "", toupper(Region_name))) %>%
      mutate(City_name = gsub("[^a-zA-Z0-9]", "", toupper(City_name))) %>%
      mutate(District_name = gsub("[^a-zA-Z0-9]", "", toupper(District_name))) %>%
      mutate(Region_name = gsub("^(KAB|KOTA)", "", toupper(Region_name))) %>%
      mutate(City_name = gsub("^(KAB|KOTA)", "", toupper(City_name))) %>%
      mutate(District_name = gsub("^(KAB|KOTA)", "", toupper(District_name))) %>%
      mutate(Region_name = gsub("(KAB|KOTA)$", "", toupper(Region_name))) %>%
      mutate(City_name = gsub("(KAB|KOTA)$", "", toupper(City_name))) %>%
      mutate(District_name = gsub("(KAB|KOTA)$", "", toupper(District_name))) %>%
      mutate(mappingCode = paste0(Region_name, City_name, District_name)) %>%
      arrange(mappingCode, desc(first_1kg)) %>%
      filter(!duplicated(mappingCode)) %>%
      select(-c(mappingCode))
    
    mergedOMSDataRev <- mergedOMSData %>%
      mutate(level_2_name = gsub("[^a-zA-Z0-9]", "", toupper(level_2_name))) %>%
      mutate(level_3_name = gsub("[^a-zA-Z0-9]", "", toupper(level_3_name))) %>%
      mutate(level_4_name = gsub("[^a-zA-Z0-9]", "", toupper(level_4_name))) %>%
      mutate(level_2_name = gsub("^(KAB|KOTA)", "", toupper(level_2_name))) %>%
      mutate(level_3_name = gsub("^(KAB|KOTA)", "", toupper(level_3_name))) %>%
      mutate(level_4_name = gsub("^(KAB|KOTA)", "", toupper(level_4_name))) %>%
      mutate(level_2_name = gsub("(KAB|KOTA)$", "", toupper(level_2_name))) %>%
      mutate(level_3_name = gsub("(KAB|KOTA)$", "", toupper(level_3_name))) %>%
      mutate(level_4_name = gsub("^(KAB|KOTA)", "", toupper(level_4_name))) %>%
      mutate(level_4_name = gsub("(KAB|KOTA)$", "", toupper(level_4_name)))
    
    mappedRateCard <- left_join(mergedOMSDataRev, 
                                rateCardRev,
                                by = c("level_2_name" = "Region_name",
                                       "level_3_name" = "City_name",
                                       "level_4_name" = "District_name"))
    
    mappedRateCard %<>%
      mutate(RateCardMappedFlag = ifelse(is.na(first_kg), "NOT_OKAY","OKAY"))
    
    mappedRateCard
    
  }, error = function(err) {
    flog.error(paste(functionName, err, sep = " - "), name = reportName)
  }, finally = {
    flog.info(paste("Function", functionName, "ended"), name = reportName)
  })
  
  output
}