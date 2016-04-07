source("02_Codes/00_init.R")

tryCatch({
  
  flog.info("Initial Setup", name = reportName)
  
  source("02_Codes/01_Load/Load_Seller_Data.R")
  
  load("01_Input/RData/packageDataBased.RData")
  invoiceData <- LoadSellerData("01_Input/seller_mapping")
  
  flog.info("Mapping with OMS data", name = reportName)
  mergedOMSData <- left_join(invoiceData, packageDataBased, by = "tracking_number")
  rm(packageDataBased)
  gc()
   # Existence Flag
  mergedOMSData %<>%
    mutate(existence_flag = ifelse(!is.na(order_nr), "OKAY", "NOT_OKAY"))
  
#   mergedOMSData_package <- mergedOMSData %>%
#     select(tracking_number, package_number)
#   mergedOMSData_package <- unique(mergedOMSData_package)
#   mergedOMSData_package <- left_join(mergedOMSData_package, as.data.frame(table(mergedOMSData_package$package_number)), by = c("package_number" = "Var1"))
  
#   mergedOMSData_rate %<>%
#     mutate(Duplication_Flag=ifelse(duplicated(paste0(toupper(tracking_number), toupper(tracking_number_rts), toupper(delivery_status))),"Duplicated",
#                                    ifelse(tracking_number %in% paidInvoice,
#                                           "Duplicated","Not_Duplicated"))) %>%
#     mutate(DuplicationSource=ifelse(duplicated(paste0(toupper(tracking_number), toupper(tracking_number_rts), toupper(delivery_status))),"Self_Duplicated",
#                                     ifelse(tracking_number %in% paidInvoice,
#                                            paidInvoiceList[tracking_number,]$InvoiceFile,"")))
  
  mergedOMSData_final <- mergedOMSData %>%
    select(line_id, X3pl_name, package_pickup_date, seller_name, zipcode, number_packages, tracking_number, reference_number, pickup_charge,
           order_nr, Seller_Code,Seller,seller_postcode,shipment_provider_name, shipped, existence_flag
          )

#   source("02_Codes/01_Load/Load_Invoice_Data.R")
#   CODData <- LoadInvoiceData("01_Input/Kerry/02_COD")
  
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
  
  OutputRawData(mergedOMSData_final, paste0("05_Output/seller_mapping/output_",dateReport,".csv"))
#   OutputRawData(exceedThresholdTrackingNumber, paste0("2_Output/gdex/exceedThresholdTrackingNumber_",dateReport,".csv"))
#   OutputRawData(notFoundTrackingNumber, paste0("2_Output/gdex/notFoundTrackingNumber_",dateReport,".csv"))
  # SummaryReport(mergedOMSData_final, paste0("05_Output/Kerry/summaryReport_",dateReport,".csv"))
  
  
#   invoiceFiles <- unique(mergedOMSData_final$rawFile)
#   for (iFile in invoiceFiles) {
#     fileName <- gsub(".xls.*$", "_checked.csv", iFile)
#     fileData <-  as.data.frame(mergedOMSData_final %>% filter(rawFile == iFile))
#     write.csv(fileData, file.path("05_Output/Kerry", fileName),
#                row.names = FALSE)
#   }
  
  flog.info("Done", name = reportName)
  
},error = function(err){
  flog.error(err, name = reportName)
  flog.error("PLease send 03_Script/Log folder to Regional OPS BI for additional support",
             name = reportName)
})
