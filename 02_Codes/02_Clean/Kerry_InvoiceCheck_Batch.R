source("3_Script/1_Code/00_init.R")

tryCatch({
  
  flog.info("Initial Setup", name = reportName)
  
  source("3_Script/1_Code/01_Loading/Load_Invoice_Data.R")
  
  load("1_Input/RData/packageDataBased.RData")
  invoiceData <- LoadInvoiceData("1_Input/LEX/01_Invoice")
  
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
    mutate(existence_flag = ifelse(!is.na(RTS_Date), "OKAY", "NOT_OKAY"))
  
  # Map Rate Card
  source("3_Script/1_Code/03_Processing/LEX/LEX_MapRateCard.R")
  mergedOMSData_rate <- MapRateCard(mergedOMSData, "1_Input/LEX/02_Ratecards/LEX Rate Card-Final to Lzd.xlsx")
  
  # Rate Calculation 
  mergedOMSData_rate %<>%
    mutate(carrying_fee_laz = Initial.1st.Kg + (package_chargeable_weight - 1) * Next.Kg) %>%
    mutate(insurance_fee_laz = ifelse((paidPrice + shippingFee + shippingSurcharge) < 1000000, 2500,
                                      (paidPrice + shippingFee + shippingSurcharge) * Insurance.Charge)) %>%
    mutate(cod_fee_laz = ifelse(payment_method == "CashOnDelivery",
                                (paidPrice + shippingFee + shippingSurcharge) * COD.Fee,
                                0))
  mergedOMSData_rate %<>%
    mutate(carrying_fee_flag = ifelse(carrying_fee_laz >= carrying_fee, "OKAY", "NOT_OKAY")) %>%
    mutate(insurance_fee_flag = ifelse(insurance_fee_laz >= insurance_fee, "OKAY", "NOT_OKAY")) %>%
    mutate(cod_fee_flag = ifelse(cod_fee_laz >= cod_fee, "OKAY", "NOT_OKAY"))
  
  paidInvoiceData <- LoadInvoiceData("1_Input/LEX/03_Paid_Invoice/")
  
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
    select(-c(level_4_code, level_4_customer_address_region_type, level_4_fk_customer_address_region,
              level_3_code, level_3_customer_address_region_type, level_3_fk_customer_address_region,
              level_2_code, level_2_customer_address_region_type, level_2_fk_customer_address_region))
  
  
  flog.info("Writing Result to csv format!!!", name = reportName)
  
  invoiceFiles <- unique(mergedOMSData$rawFile)
  for (iFile in invoiceFiles) {
    fileName <- gsub(".xls.*$", "_checked.csv", iFile)
    fileData <-  as.data.frame(mergedOMSData %>% filter(rawFile == iFile))
    write.csv2(fileData, file.path("2_Output/LEX", fileName),
               row.names = FALSE)
  }
  
  flog.info("Done", name = reportName)
  
},error = function(err){
  flog.error(err, name = reportName)
  flog.error("PLease send 3_Script/Log folder to Regional OPS BI for additional support",
             name = reportName)
})
