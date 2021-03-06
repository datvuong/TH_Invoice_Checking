source("02_Codes/00_init.R")

tryCatch({
  
  flog.info("Initial Setup", name = reportName)
  #loading data 
  source("02_Codes/01_Load/Load_Invoice_Data.R")
  
  load("01_Input/RData/packageDataBased.RData")
  invoiceData <- LoadInvoiceData("01_Input/CJ/01_Invoice")
  invoiceData %<>% 
    mutate(tracking_number = ifelse(substr(tracking_number, 1, 1) == "6", tracking_number_rts, tracking_number)) 
  #mapping invoice data and oms data
  flog.info("Mapping invoice data and OMS data", name = reportName)
  mergedOMSData <- left_join(invoiceData,
                             packageDataBased,
                             by = "tracking_number")
  rm(packageDataBased)
  gc()
  mergedOMSData %<>%
    mutate(package_number = ifelse(is.na(package_number.y), package_number.x,
                                   package_number.y)) %>%
    select(-c(package_number.x, package_number.y)) %>%
    replace_na(list(carrying_fee=0, redelivery_fee=0, rejection_fee=0,
                    cod_fee=0, special_area_fee=0, special_handling_fee=0,
                    insurance_fee=0))
  
  #load historical sku weight data
  setClass("myNumeric")
  setAs("character","myNumeric", function(from) as.numeric(gsub('[^0-9\\.]','',from)))
  
  skusActualWeight <- read.csv(paste0("01_Input/", "skus_actual_weight.csv"), quote = '"', sep=",", row.names = NULL,
                               col.names = c("sku","sum_of_TN","minWeight","maxWeight","medWeight","meanWeight"), 
                               colClasses = c("character", "myNumeric", "myNumeric", "myNumeric", "myNumeric", "myNumeric"))
  source("02_Codes/01_Load/loadCommonVariables_CJ.R")
  loadCommonVariables(paste0("01_Input/CJ/", "commonVariables.csv"))
  
  mergedOMSData %<>% mutate(sku = substr(skus, 1, 16))
  mergedOMSData <- left_join(mergedOMSData, skusActualWeight %>% select(sku, medWeight), by = c("sku" = "sku"))
  mergedOMSData %<>% mutate(is_medWeight = ifelse(itemsCount == 1 & !is.na(medWeight), 1, 0)) %>%
    mutate(calculatedWeight = ifelse(itemsCount == 1 & !is.na(medWeight) & ((package_chargeable_weight <= weightFirstUpperBound & (package_chargeable_weight - medWeight) > weightFirstThreshold * package_chargeable_weight) |
                                                                              (package_chargeable_weight > weightFirstUpperBound & package_chargeable_weight <= weightSecondUpperBound & (package_chargeable_weight - medWeight) > weightSecondThreshold * package_chargeable_weight) | 
                                                                              (package_chargeable_weight > weightSecondUpperBound & (package_chargeable_weight - medWeight) > weightThirdThreshold)), medWeight, package_chargeable_weight))
  # Existence Flag
  mergedOMSData %<>%
    mutate(existence_flag = ifelse(!is.na(order_nr), "OKAY", "NOT_OKAY"))
  
  # Map Rate Card
  source("02_Codes/02_Clean/CJ/CJ_MapRateCard.R")
  mergedOMSData_rate <- MapRateCard(mergedOMSData, 
                                    rateCardFilePath =  "01_Input/CJ/05_Ratecards/CJ_rates.csv",
                                    postalCodePath =  "01_Input/CJ/04_Postalcode/CJ_postalcode.csv")
  
  # Rate Calculation 
  codFinData <- read.csv(paste0("01_Input/CJ/02_COD/", "COD_FinData.csv"), quote = '"', sep=",", row.names = NULL,
                         col.names = c("tracking_number", "tracking_number_ref", "pickupDate", "destination", 
                                       "cash", "cod_surcharge", "bach_date", "type", "quarter"),
                         colClasses = c("character", "character", "character", "character",
                                        "myNumeric", "myNumeric", "character", "character", "character"))
  
  codFinData %<>% 
    mutate(tracking_number = ifelse(substr(tracking_number, 1, 1) == "6", tracking_number_rts, tracking_number)) %>%
    mutate(tracking_number = toupper(tracking_number)) %>%
    group_by(tracking_number) %>%
    summarise(cash = sum(cash))
  
  mergedOMSData_rate <- left_join(mergedOMSData_rate, codFinData, by = c("tracking_number" = "tracking_number"))
  mergedOMSData_rate[,c("paidPrice", "shippingFee", "shippingSurcharge")][is.na(mergedOMSData_rate[,c("paidPrice", "shippingFee", "shippingSurcharge")])] <- 0
  mergedOMSData_rate %<>%
    mutate(carrying_fee_laz = ifelse(weightCategory == "w20-99", (ceiling(calculatedWeight) - 20) * carryingFeeOver20 + Rates, Rates)) %>%
    mutate(redelivery_fee_laz = ifelse(dest_area == "Remote area", carrying_fee_laz -150, carrying_fee_laz) * (number_packages - 1)) %>%  # number of packages is storing number of attempts
    mutate(overWeight_fee_laz = ifelse(ceiling(calculatedWeight) - 20 > 10, carryingFeeOver30, 0)) %>%
    mutate(return_fee_laz = ifelse(delivery_status == "Failed delivery", returnRate, 0)) %>%
    mutate(cod_fee_laz = round(ifelse(payment_method == "CashOnDelivery" & !is.na(delivered), CODRate, NA), 2)) %>%
    mutate(cod_fee_fin = round(ifelse(cash > 0, CODRate,NA), 2)) %>%
    mutate(insurance_fee_laz = round(paidPrice * insuranceFeeRate,2))
  
  mergedOMSData_rate %<>%
    mutate(carrying_fee_flag = ifelse(carrying_fee_laz >= carrying_fee + special_area_fee , "OKAY", "NOT_OKAY")) %>%
    mutate(redelivery_fee_flag = ifelse(redelivery_fee_laz >= redelivery_fee, "OKAY", "NOT_OKAY" )) %>%
    mutate(overWeight_fee_flag = ifelse(overWeight_fee_laz >= special_handling_fee, "OKAY", "NOT_OKAY")) %>%
    mutate(return_fee_flag = ifelse(return_fee_laz >= carrying_fee + special_area_fee , "OKAY", "NOT_OKAY")) %>%
    mutate(cod_fee_flag = ifelse(round(cod_fee - cod_fee_laz,2) <= CODThreshold & !is.na(delivered), "OKAY", "NOT_OKAY")) %>%
    mutate(cod_fee_fin_flag = ifelse(round(cod_fee - cod_fee_fin,2) <= CODThreshold, "OKAY", "NOT_OKAY" )) %>%
    mutate(insurance_fee_flag = ifelse(round(insurance_fee - insurance_fee_laz,2) <= insuranceFeeThreshold , "OKAY", "NOT_OKAY" ))
  
  mergedOMSData_rate %<>%
    mutate(status_flag = ifelse(delivery_status == "Delivery" & !is.na(cancelled) & (shipped >= cancelled | is.na(shipped)), "Delivery_Cancelled", 
                                ifelse(delivery_status == "Failed delivery" & !is.na(delivered), "FailedDelivery_Delivered", "OKAY")))
  
  paidInvoiceData <- LoadInvoiceData("01_Input/CJ/03_Paid_Invoice")
  paidInvoice <- NULL
  paidInvoiceList <- NULL
  if (!is.null(paidInvoiceData)) {
    paidInvoiceData %<>%
      mutate(tracking_number = ifelse(substr(tracking_number, 1, 1) == "6", tracking_number_rts, tracking_number)) 	
    paidInvoice <- paidInvoiceData$tracking_number
    paidInvoiceList <- select(paidInvoiceData, tracking_number, rawFile)
    paidInvoiceList <- paidInvoiceList %>%
      filter(!duplicated(tracking_number))
    row.names(paidInvoiceList) <- paidInvoiceList$tracking_number
  }
  
  mergedOMSData_rate %<>%
    mutate(Duplication_Flag=ifelse(duplicated(paste0(toupper(tracking_number), toupper(tracking_number_rts), toupper(delivery_status))),"Duplicated",
                                   ifelse(tracking_number %in% paidInvoice,
                                          "Duplicated","Not_Duplicated"))) %>%
    mutate(DuplicationSource=ifelse(duplicated(paste0(toupper(tracking_number), toupper(tracking_number_rts), toupper(delivery_status))),"Self_Duplicated",
                                    ifelse(tracking_number %in% paidInvoice,
                                           paidInvoiceList[tracking_number,]$InvoiceFile,"")))
  
  mergedOMSData_final <- mergedOMSData_rate %>%
    select(line_id,X3pl_name, package_pickup_date,package_pod_date,invoice_number,tracking_number,tracking_number_rts,order_number,package_volume,package_height,package_width,package_length,package_weight.x,package_chargeable_weight,carrying_fee,redelivery_fee,rejection_fee,cod_fee,special_area_fee,special_handling_fee,insurance_fee,vat,origin_branch,destination_branch,delivery_zone_zip_code,rate_type,delivery_status,number_packages,project_type,
           order_nr, unit_price,itemsCount,paidPrice,shippingFee,shippingSurcharge,sku,skus,volumetricDimension,actualWeight,payment_method,package_number,shipped,cancelled,delivered,being_returned,rts,Seller_Code,Seller,tax_class,shipment_provider_name,postcode,seller_postcode,origineName,
           medWeight,is_medWeight,calculatedWeight,dest_area,is_OMSPostcode,weightCategory,Rates,cash,
           carrying_fee_laz,overWeight_fee_laz,return_fee_laz,cod_fee_laz,insurance_fee,
           existence_flag,RateCardMappedFlag,carrying_fee_flag,overWeight_fee_flag,return_fee_flag,cod_fee_flag,cod_fee_fin_flag, insurance_fee_flag,status_flag,Duplication_Flag,DuplicationSource
    )
  
  #   source("02_Codes/01_Load/Load_Invoice_Data.R")
  #   CODData <- LoadInvoiceData("01_Input/CJ/02_COD")
  
  flog.info("Writing Result to csv format!!!", name = reportName)
  # source("02_Codes/04_Reports/SummaryReport.R")
  source("02_Codes/04_Reports/OutputRawData.R")
  
  #   exceedThresholdTrackingNumber <- finalOutput %>%
  #     filter(manualCheck == "EXCEED_THRESHOLD") %>%
  #     select(deliveryCompany, trackingNumber, packageChargeableWeight, packageChargeableWeight, carryingFee,
  #            lazadaWeight, lazadaDimWeight, lazadaCalFee)
  #   
  #   notFoundTrackingNumber <- finalOutput %>%
  #     filter(manualCheck == "NOT_FOUND") %>%
  #     select(deliveryCompany, trackingNumber, Seller_Code)
  
  OutputRawData(mergedOMSData_final, paste0("05_Output/CJ/checkedInvoice_",dateReport,".csv"))
  #   OutputRawData(exceedThresholdTrackingNumber, paste0("2_Output/gdex/exceedThresholdTrackingNumber_",dateReport,".csv"))
  #   OutputRawData(notFoundTrackingNumber, paste0("2_Output/gdex/notFoundTrackingNumber_",dateReport,".csv"))
  # SummaryReport(mergedOMSData_final, paste0("05_Output/CJ/summaryReport_",dateReport,".csv"))
  
  
  #   invoiceFiles <- unique(mergedOMSData_final$rawFile)
  #   for (iFile in invoiceFiles) {
  #     fileName <- gsub(".xls.*$", "_checked.csv", iFile)
  #     fileData <-  as.data.frame(mergedOMSData_final %>% filter(rawFile == iFile))
  #     write.csv(fileData, file.path("05_Output/CJ", fileName),
  #                row.names = FALSE)
  #   }
  
  flog.info("Done", name = reportName)
  
},error = function(err){
  flog.error(err, name = reportName)
  flog.error("PLease send 3_Script/Log folder to Regional OPS BI for additional support",
             name = reportName)
})
