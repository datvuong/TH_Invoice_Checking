source("02_Codes/00_init.R")

tryCatch({
  
  flog.info("Initial Setup", name = reportName)
  
  source("02_Codes/01_Load/Load_Invoice_Data.R")
  
  load("01_Input/RData/packageDataBased.RData")
  invoiceData <- LoadInvoiceData("01_Input/Kerry/01_Invoice")
  
  mergedOMSData <- left_join(invoiceData,
                             packageDataBased,
                             by = "tracking_number")
  rm(packageDataBased)
  gc()
  mergedOMSData %<>%
    mutate(package_number = ifelse(is.na(package_number.y), package_number.x,
                                   package_number.y)) %>%
    select(-c(package_number.x, package_number.y))
  
  # Existence Flag
  mergedOMSData %<>%
    mutate(existence_flag = ifelse(!is.na(rts), "OKAY", "NOT_OKAY"))
  
  # Map Rate Card
  source("02_Codes/02_Clean/Kerry/Kerry_MapRateCard.R")
  mergedOMSData_rate <- MapRateCard(mergedOMSData, 
                                    rateCardFilePath =  "01_Input/Kerry/02_Ratecards/Kerry_rates.xlsx",
                                    postalCodePath =  "01_Input/Kerry/04_Postalcode/Kerry_postalcode.xlsx")
  
  # Rate Calculation 
  mergedOMSData_rate %<>%
    mutate(carrying_fee_laz = Rates) %>%
    mutate(cod_fee_laz = round(ifelse(payment_method == "CashOnDelivery",
                                (paidPrice + shippingFee + shippingSurcharge) * 0.028, 0),
                               2))
  mergedOMSData_rate %<>%
    mutate(carrying_fee_flag = ifelse(carrying_fee_laz >= carrying_fee, "OKAY", "NOT_OKAY")) %>%
    mutate(cod_fee_flag = ifelse(cod_fee_laz >= cod_fee, "OKAY", "NOT_OKAY"))
  
  paidInvoiceData <- LoadInvoiceData("01_Input/Kerry/03_Paid_Invoice")
  
  paidInvoice <- NULL
  paidInvoiceList <- NULL
  
  if (!is.null(paidInvoiceData)) {
    paidInvoice <- paidInvoiceData$tracking_number
    paidInvoiceList <- select(paidInvoiceData, tracking_number, rawFile)
    paidInvoiceList <- paidInvoiceList %>%
      filter(!duplicated(tracking_number))
    row.names(paidInvoiceList) <- paidInvoiceList$tracking_number
  }
  
  mergedOMSData_rate %<>%
    mutate(Duplication_Flag=ifelse(duplicated(tracking_number),"Duplicated",
                                   ifelse(tracking_number %in% paidInvoice,
                                          "Duplicated","Not_Duplicated"))) %>%
    mutate(DuplicationSource=ifelse(duplicated(tracking_number),"Self_Duplicated",
                                    ifelse(tracking_number %in% paidInvoice,
                                           paidInvoiceList[tracking_number,]$InvoiceFile,"")))
  
  mergedOMSData_final <- mergedOMSData_rate %>%
    select(-c(level_7_code, level_7_customer_address_region_type, level_7_fk_customer_address_region,
              level_6_code, level_6_customer_address_region_type, level_6_fk_customer_address_region,
              level_5_code, level_5_customer_address_region_type, level_5_fk_customer_address_region))
  
  
  flog.info("Writing Result to csv format!!!", name = reportName)
  
  invoiceFiles <- unique(mergedOMSData_final$rawFile)
  for (iFile in invoiceFiles) {
    fileName <- gsub(".xls.*$", "_checked.csv", iFile)
    fileData <-  as.data.frame(mergedOMSData_final %>% filter(rawFile == iFile))
    write.csv(fileData, file.path("05_Output/Kerry", fileName),
               row.names = FALSE)
  }
  
  flog.info("Done", name = reportName)
  
},error = function(err){
  flog.error(err, name = reportName)
  flog.error("PLease send 3_Script/Log folder to Regional OPS BI for additional support",
             name = reportName)
})
