MapRateCard <- function(mergedOMSData, rateCardFilePath, postalCodePath) {
  suppressMessages({
    require(dplyr)
    require(tools)
    require(magrittr)
    require(methods)
    require(futile.logger)
    require(XLConnect)
    require(reshape2)
  })
  
  functionName <- "MapRateCard"
  flog.info(paste("Function", functionName, "started"), name = reportName)
  
  output <- tryCatch({
    setClass("myNumeric")
    setAs("character","myNumeric", function(from) as.numeric(gsub('[^0-9\\.]','',from)))
    
    rateCard <- read.csv(rateCardFilePath, quote = '"', sep=",", header = TRUE, colClasses = c("min" = "character", "max" = "character"))
    postalCode <- read.csv(postalCodePath, quote = '"', sep=",", header = TRUE,
                           colClasses = c( "postal_code" = "character"))
    rateCard <- rateCard[,c("min","max",paste0(id3PL, "_A"),paste0(id3PL, "_B"),paste0(id3PL, "_C"))]
    postalCode <- postalCode[,c("postal_code", id3PL)]
    
    mergedOMSData %<>% 
      mutate(is_OMSPostcode = ifelse(is.na(postcode), 0, 1)) %>%
      mutate(postcode = ifelse(is.na(postcode), destination_branch, postcode)) %>%
      mutate(postcode = gsub(".$", "0", gsub('[^0-9]', '', postcode))) %>%
      mutate(maxWeight = ifelse(round(calculatedWeight,2) <= 0, 0, 
                                ifelse(round(calculatedWeight,2) <= 2, ceiling(calculatedWeight/0.1)*0.1,
                                      ifelse(round(calculatedWeight,2) <= 5, ceiling(calculatedWeight/0.5)*0.5,
                                             ifelse(round(calculatedWeight,2) <= 20,ceiling(calculatedWeight/5)*5, ceiling(calculatedWeight) ))))) %>% 
      mutate(minWeight = ifelse(round(calculatedWeight,2) <= 0, 0, 
                                ifelse(round(calculatedWeight,2) <= 2, maxWeight - 0.1,
                                       ifelse(round(calculatedWeight,2) <= 5, maxWeight - 0.5,
                                              ifelse(round(calculatedWeight,2) <= 20,maxWeight - 5,maxWeight-1 ))))) %>%
      mutate(maxWeight = as.character(maxWeight)) %>%
      mutate(minWeight = as.character(minWeight))
    
    mergedOMSData_rev <- left_join(mergedOMSData, 
                                   postalCode ,
                                   by = c("postcode" = "postal_code"))
    names(mergedOMSData_rev) <- gsub(id3PL, "dest_area", names(mergedOMSData_rev))
    
    mergedOMSData_rev <- left_join(mergedOMSData_rev, rateCard,
                                   by = c("minWeight" = "min", "maxWeight" = "max"))
    
    mergedOMSData_rev %<>%
      mutate(Rates = ifelse(dest_area == "Greater Bangkok", mergedOMSData_rev[,paste0(id3PL, "_A")],
                            ifelse(dest_area == "Upcountry", mergedOMSData_rev[,paste0(id3PL, "_B")],
                                   ifelse(dest_area == "Remote area", mergedOMSData_rev[,paste0(id3PL, "_C")],NA)))) %>%
      mutate(RateCardMappedFlag = ifelse(is.na(Rates), "NOT_OKAY","OKAY"))
    # mergedOMSData_rev <- mergedOMSData_rev[,- c(paste0(id3PL, "_A"),paste0(id3PL, "_B"),paste0(id3PL, "_C"))]
    # Rate Calculation 
    mergedOMSData_rev[,c("paidPrice", "shippingFee", "shippingSurcharge")][is.na(mergedOMSData_rev[,c("paidPrice", "shippingFee", "shippingSurcharge")])] <- 0
    mergedOMSData_rev %<>%
      mutate(carrying_fee_laz = Rates) %>%
      mutate(return_fee_laz = ifelse(delivery_status == deliveryStatus2, Rates, 0)) %>%
      mutate(cod_fee_laz = round(ifelse(payment_method == "CashOnDelivery" & !is.na(delivered), ifelse((paidPrice + shippingFee + shippingSurcharge) <= CODRate1stUpperBound, CODRate1st,(paidPrice + shippingFee + shippingSurcharge) * CODRate), NA), 2)) %>%
      mutate(cod_fee_fin = round(cash * CODRate, 2)) %>%
      mutate(insurance_fee_laz = round(paidPrice * insuranceFeeRate,2))
    
    mergedOMSData_rev
    
  }, error = function(err) {
    flog.error(paste(functionName, err, sep = " - "), name = reportName)
  }, finally = {
    flog.info(paste("Function", functionName, "ended"), name = reportName)
  })
  
  output
}