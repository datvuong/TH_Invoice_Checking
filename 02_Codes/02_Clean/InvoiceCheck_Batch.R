source("02_Codes/00_init.R")

setClass("myNumeric")
setAs("character","myNumeric", function(from) as.numeric(gsub('[^0-9\\.]','',from)))
args <- commandArgs(trailingOnly = TRUE)
id3PL <- NULL
id3PL <- as.character(args[1])

 # Source and path for checking
source("02_Codes/01_Load/fn_Load_Data.R")
source(paste0("02_Codes/01_Load/loadCommonVariables_", id3PL, ".R"))
source(paste0("02_Codes/02_Clean/", id3PL, "/", id3PL, "_MapRateCard.R"))
source("02_Codes/04_Reports/OutputRawData.R")
RDataPath = "01_Input/RData/packageDataBased.RData"
invoiceDataPath = paste0("01_Input/",id3PL, "/01_Invoice")
invoicePaidDataPath = paste0("01_Input/",id3PL, "/03_Paid_Invoice") 
finCODPath = paste0("01_Input/", id3PL, "/02_COD")
skuWeightPath = "01_Input/skus_actual_weight.csv"
rateCardFilePath = "01_Input/3PL_ratecards.csv"
postalCodePath = "01_Input/3PL_postalCodes.csv"
commonVariablesPath = paste0("01_Input/", id3PL, "/commonVariables.csv")
outputPath = paste0("05_Output/", id3PL,"/checkedInvoice_",dateReport,".csv")

# fixing tracking number logic for each 3PL
fn_dataFix <- function(input_data, id3PL) {
  require(dplyr,quietly = TRUE)
  require(tools,quietly = TRUE)
  require(magrittr,quietly = TRUE)
  require(methods, quietly= TRUE)
  functionName <- "fn_dataFix"
  flog.info(paste("Function", functionName, "started"), name = reportName)

  output <- tryCatch({
    if (id3PL == "kerry") {
      input_data %<>% 
        mutate(tracking_number = ifelse(substr(tracking_number, 1, 1) == "1", tracking_number_rts, tracking_number)) %>%
        mutate(tracking_number = toupper(tracking_number))
    } 
    input_data %<>%
      mutate(tracking_number = toupper(tracking_number))
  }, error = function(err) {
    flog.error(paste(functionName, err, sep = " - "), name = reportName)
  }, finally = {
    flog.info(paste(functionName, "ended"), name = reportName)
  })
  
  output
}

tryCatch({
  
  flog.info("Initial Setup", name = reportName)
# Load Rdata and Invoice data for mapping.  
  flog.info("Loading OMS RData package", name = reportName)
  load(RDataPath)
  invoiceData <- LoadInvoiceData(invoiceDataPath)
  invoiceData <- fn_dataFix(invoiceData, id3PL)
  mergedOMSData <- left_join(invoiceData,
                             packageDataBased,
                             by = "tracking_number")
  rm(packageDataBased)
  gc()
  mergedOMSData %<>%
    mutate(package_number = ifelse(is.na(package_number.y), package_number.x, package_number.y)) %>%
    select(-c(package_number.x, package_number.y)) %>%
    mutate(sku = substr(skus, 1, 16))
  # SKU historical weight
  flog.info("Loading SKU historical data and common variables", name = reportName)
  skusActualWeight <- LoadSKUWeightData(skuWeightPath)
  # Common Variables --> source loadCommonVariables Function
  loadCommonVariables(commonVariablesPath)
  
  mergedOMSData <- left_join(mergedOMSData, skusActualWeight %>% select(sku, medWeight), by = c("sku" = "sku"))
  mergedOMSData %<>% 
    mutate(is_medWeight = ifelse(itemsCount == 1 & !is.na(medWeight), 1, 0)) %>%
    mutate(calculatedWeight = ifelse(is_medWeight == 1 & ((package_chargeable_weight <= weightFirstUpperBound & (package_chargeable_weight - medWeight) > weightFirstThreshold * package_chargeable_weight) |
                                                          (package_chargeable_weight > weightFirstUpperBound & package_chargeable_weight <= weightSecondUpperBound & (package_chargeable_weight - medWeight) > weightSecondThreshold * package_chargeable_weight) | 
                                                          (package_chargeable_weight > weightSecondUpperBound & (package_chargeable_weight - medWeight) > weightThirdThreshold)), medWeight, package_chargeable_weight))
  # Existence Flag
  mergedOMSData %<>%
    mutate(existence_flag = ifelse(!is.na(order_nr), "OKAY", "NOT_OKAY"))
  
  #Fin COD data
  codFinData <- LoadFinCODData(finCODPath)
  codFinData <- fn_dataFix(codFinData, id3PL)
  codFinData %<>% 
    group_by(tracking_number) %>%
    summarise(cash = sum(cash))
  
  mergedOMSData <- left_join(mergedOMSData, codFinData, by = c("tracking_number" = "tracking_number"))
  
  #Paid invoice
  paidInvoiceData <- LoadInvoiceData(invoicePaidDataPath)
  
  paidInvoice <- NULL
  paidInvoiceList <- NULL
  
  if (!is.null(paidInvoiceData)) {
    paidInvoice <- paidInvoiceData$tracking_number
    paidInvoiceList <- select(paidInvoiceData, tracking_number, rawFile)
    paidInvoiceList <- paidInvoiceList %>%
      filter(!duplicated(tracking_number))
    row.names(paidInvoiceList) <- paidInvoiceList$tracking_number
  }
  
  # Map Rate Card --> source MapRateCard function
  mergedOMSData_rate <- MapRateCard(mergedOMSData, rateCardFilePath, postalCodePath) #, CODRate, insuranceFeeRate, deliveryStatus1, deliveryStatus2)
  mergedOMSData_rate %<>%
    mutate(weightCategory = paste0("w", minWeight, "-", maxWeight)) %>%
    select(-c(minWeight, maxWeight))
  
  mergedOMSData_rate %<>%
    mutate(carrying_fee_flag = ifelse(carrying_fee_laz >= carrying_fee, "OKAY", "NOT_OKAY")) %>%
    mutate(return_fee_flag = ifelse(redelivery_fee == 0, "NA", ifelse(return_fee_laz >= redelivery_fee, "OKAY", "NOT_OKAY"))) %>%
    mutate(cod_fee_flag = ifelse(round(cod_fee - cod_fee_laz,2) <= CODThreshold , "OKAY", "NOT_OKAY")) %>%
    mutate(cod_fee_fin_flag = ifelse(round(cod_fee - cod_fee_fin,2) <= CODThreshold, "OKAY", "NOT_OKAY" )) %>%
    mutate(insurance_fee_flag = ifelse(round(insurance_fee - insurance_fee_laz,2) <= insuranceFeeThreshold , "OKAY", "NOT_OKAY" )) %>%
    mutate(status_flag = ifelse(delivery_status == deliveryStatus1 & !is.na(cancelled) & (shipped >= cancelled | is.na(shipped)), "Delivery_Cancelled", 
                                ifelse(delivery_status == deliveryStatus2 & !is.na(delivered), "FailedDelivery_Delivered", "OKAY"))) %>%
    mutate(Duplication_Flag=ifelse(duplicated(paste0(toupper(tracking_number), toupper(tracking_number_rts), toupper(delivery_status))),"Duplicated",
                                   ifelse(tracking_number %in% paidInvoice, "Duplicated","Not_Duplicated"))) %>%
    mutate(DuplicationSource=ifelse(duplicated(paste0(toupper(tracking_number), toupper(tracking_number_rts), toupper(delivery_status))),"Self_Duplicated",
                                    ifelse(tracking_number %in% paidInvoice, paidInvoiceList[tracking_number,]$InvoiceFile,"")))
  
  mergedOMSData_final <- mergedOMSData_rate %>%
    select(line_id,X3pl_name, package_pickup_date,package_pod_date,invoice_number,tracking_number,tracking_number_rts,order_number,package_volume,package_height,package_width,package_length,package_weight.x,package_chargeable_weight,carrying_fee,redelivery_fee,rejection_fee,cod_fee,special_area_fee,special_handling_fee,insurance_fee,vat,origin_branch,destination_branch,delivery_zone_zip_code,rate_type,delivery_status,number_packages,project_type,
           order_nr, unit_price,itemsCount,paidPrice,shippingFee,shippingSurcharge,sku,skus,volumetricDimension,actualWeight,payment_method,package_number,shipped,cancelled,delivered,being_returned,rts,Seller_Code,Seller,tax_class,shipment_provider_name,postcode,seller_postcode,origineName,
           medWeight,is_medWeight,calculatedWeight,dest_area,is_OMSPostcode,weightCategory,Rates,cash,carrying_fee_laz, return_fee_laz,cod_fee_laz,cod_fee_fin,insurance_fee_laz,existence_flag,RateCardMappedFlag,carrying_fee_flag,return_fee_flag, cod_fee_flag,cod_fee_fin_flag,insurance_fee_flag,status_flag,Duplication_Flag,DuplicationSource
    )
  
  flog.info("Writing Result to csv format!!!", name = reportName)
  # source("02_Codes/04_Reports/SummaryReport.R")

  
#   exceedThresholdTrackingNumber <- finalOutput %>%
#     filter(manualCheck == "EXCEED_THRESHOLD") %>%
#     select(deliveryCompany, trackingNumber, packageChargeableWeight, packageChargeableWeight, carryingFee,
#            lazadaWeight, lazadaDimWeight, lazadaCalFee)
#   
#   notFoundTrackingNumber <- finalOutput %>%
#     filter(manualCheck == "NOT_FOUND") %>%
#     select(deliveryCompany, trackingNumber, Seller_Code)
  
  OutputRawData(mergedOMSData_final, outputPath)
#   OutputRawData(exceedThresholdTrackingNumber, paste0("2_Output/gdex/exceedThresholdTrackingNumber_",dateReport,".csv"))
#   OutputRawData(notFoundTrackingNumber, paste0("2_Output/gdex/notFoundTrackingNumber_",dateReport,".csv"))
  # SummaryReport(mergedOMSData_final, paste0("05_Output/LEX/summaryReport_",dateReport,".csv"))
  
  
#   invoiceFiles <- unique(mergedOMSData_final$rawFile)
#   for (iFile in invoiceFiles) {
#     fileName <- gsub(".xls.*$", "_checked.csv", iFile)
#     fileData <-  as.data.frame(mergedOMSData_final %>% filter(rawFile == iFile))
#     write.csv(fileData, file.path("05_Output/LEX", fileName),
#                row.names = FALSE)
#   }
  
  flog.info("Done", name = reportName)
  
},error = function(err){
  flog.error(err, name = reportName)
  flog.error("PLease send 3_Script/Log folder to Regional OPS BI for additional support",
             name = reportName)
})
