LoadInvoiceData <- function(invoicePath) {
  suppressMessages({
    require(readr)
    require(dplyr)
    require(tools)
    require(magrittr)
    require(methods)
    require(XLConnect)
    require(futile.logger)
  })
  
  functionName <- "LoadInvoiceData"
  flog.info(paste("Function", functionName, invoicePath, "started"), name = reportName)
  
  output <- tryCatch({
    
    setClass("myDate")
    setAs("character","myDate", function(from) as.POSIXct(substr(gsub('"','',from), 1, 10), format="%m/%d/%Y"))
    setClass("myNumeric")
    setAs("character","myNumeric", function(from) as.numeric(gsub('[^0-9\\.]','',from)))
    
    fullData <- NULL
    
    subData <- NULL
    excelFiles <- list.files(invoicePath)
    excelFiles <- excelFiles[grepl("^[^~\\$].*\\.(xls|xlsx|csv)$", excelFiles)]
    colNames <- c("line_id", "3pl_name", "package_pickup_date",
                  "package_pod_date", "invoice_number", "package_number",
                  "tracking_number", "tracking_number_rts", "order_number", 
                  "package_volume", "package_height", "package_width",
                  "package_length", "package_weight", "package_chargeable_weight",
                  "carrying_fee", "redelivery_fee", "rejection_fee",
                  "cod_fee", "special_area_fee", "special_handling_fee",
                  "insurance_fee", "vat", "origin_branch",
                  "destination_branch", "delivery_zone_zip_code", "rate_type", 
                  "delivery_status", "number_packages", "project_type")
    
    for (ifile in excelFiles) {
      subData <- read.csv(file.path(invoicePath, ifile), quote = '"', sep=",", row.names = NULL,
                                    col.names = colNames,
                                    colClasses = c("character", "character", "myDate",
                                                   "myDate", "character", "character",
                                                   "character", "character","character", 
                                                   "myNumeric", "myNumeric", "myNumeric",
                                                   "myNumeric", "myNumeric", "myNumeric",
                                                   "myNumeric", "myNumeric", "myNumeric",
                                                   "myNumeric", "myNumeric", "myNumeric",
                                                   "myNumeric", "myNumeric", "character",
                                                   "character", "character", "character",
                                                   "character", "myNumeric", "character"))
      
      subData %<>%
        mutate(package_chargeable_weight = package_weight) %>%
        mutate(tracking_number = toupper(tracking_number)) %>%
        mutate(rawFile = ifile)
      
      if (is.null(fullData)) {
        fullData <- subData
      } else {
        fullData <- rbind_list(fullData, subData)
      }
      gc()
    }
    
    fullData
    
  }, error = function(err) {
    flog.error(paste(functionName, err, sep = " - "), name = reportName)
  }, finally = {
    flog.info(paste(functionName, "ended"), name = reportName)
  })
  
  output
}

### Loading FIN COD Data

LoadFinCODData <- function(FinCODPath) {
  suppressMessages({
    require(readr)
    require(dplyr)
    require(tools)
    require(magrittr)
    require(methods)
    require(XLConnect)
    require(futile.logger)
  })
  
  functionName <- "LoadFinCODData"
  flog.info(paste("Function", functionName, "started"), name = reportName)
  
  output <- tryCatch({
    
    setClass("myDate")
    setAs("character","myDate", function(from) as.POSIXct(substr(gsub('"','',from), 1, 10), format="%m/%d/%Y"))
    setClass("myNumeric")
    setAs("character","myNumeric", function(from) as.numeric(gsub('[^0-9\\.]','',from)))
    
    fullData <- NULL
    
    subData <- NULL
    excelFiles <- list.files(FinCODPath, pattern = "*.csv")
    colNames <- c("tracking_number", "tracking_number_rts", "pickupDate", "destination", 
                  "cash", "cod_surcharge", "bach_date", "type", "quarter")
    for (ifile in excelFiles) {
      subData <- read.csv(file.path(FinCODPath, ifile), quote = '"', sep=",", row.names = NULL,
                                  col.names = colNames,
                                  colClasses = c("character", "character", "character", "character",
                                                 "myNumeric", "myNumeric", "character", "character", "character"))
      
      subData %<>%
        mutate(tracking_number = toupper(tracking_number))
      
      if (is.null(fullData)) {
        fullData <- subData
      } else {
        fullData <- rbind_list(fullData, subData)
      }
      gc()
    }
    
    fullData
    
  }, error = function(err) {
    flog.error(paste(functionName, err, sep = " - "), name = reportName)
  }, finally = {
    flog.info(paste(functionName, "ended"), name = reportName)
  })
  
  output
}

### Loading CommonVariable 


LoadSKUWeightData <- function(SKUWeightPath) {
  suppressMessages({
    require(readr)
    require(dplyr)
    require(tools)
    require(magrittr)
    require(methods)
    require(XLConnect)
    require(futile.logger)
  })
  
  functionName <- "LoadSKUWeightData"
  flog.info(paste("Function", functionName, "started"), name = reportName)
  
  output <- tryCatch({
    
    setClass("myDate")
    setAs("character","myDate", function(from) as.POSIXct(substr(gsub('"','',from), 1, 10), format="%m/%d/%Y"))
    setClass("myNumeric")
    setAs("character","myNumeric", function(from) as.numeric(gsub('[^0-9\\.]','',from)))
    
    fullData <- NULL
    
    subData <- NULL
    # excelFiles <- list.files(SKUWeightPath, pattern = "*.csv")
    colNames <- c("sku","sum_of_TN","minWeight","maxWeight","medWeight","meanWeight")
    # for (ifile in excelFiles) {
      subData <- read.csv(SKUWeightPath, quote = '"', sep=",", row.names = NULL,
                                  col.names = colNames,
                                  colClasses = c("character", "myNumeric", "myNumeric", "myNumeric", "myNumeric", "myNumeric"))
      
      subData %<>%
        mutate(sku = toupper(sku))
#       
#       if (is.null(fullData)) {
        fullData <- subData
#       } else {
#         fullData <- rbind_list(fullData, subData)
#       }
#       gc()
#     }
    
    fullData
    
  }, error = function(err) {
    flog.error(paste(functionName, err, sep = " - "), name = reportName)
  }, finally = {
    flog.info(paste(functionName, "ended"), name = reportName)
  })
  
  output
}




