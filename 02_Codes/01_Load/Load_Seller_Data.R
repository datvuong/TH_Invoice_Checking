LoadSellerData <- function(invoicePath) {
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
  flog.info(paste("Function", functionName, "started"), name = reportName)
  
  output <- tryCatch({
    
    setClass("myDate")
    setAs("character","myDate", function(from) as.POSIXct(substr(gsub('"','',from), 1, 10),
                                                          format="%d/%m/%Y"))
    setClass("myNumeric")
    setAs("character","myNumeric", function(from) as.numeric(gsub('[^0-9\\.]','',from)))
    
    excelFiles <- list.files(invoicePath, pattern = ".csv")
    # excelFiles <- excelFiles[grepl("^[^~\\$].*\\.(xls|xlsx|csv)$", excelFiles)]
    invoiceData <- NULL
    colNames <- c("line_id", "3pl_name", "package_pickup_date",
                  "seller_name", "zipcode", "number_packages",
                  "tracking_number", "reference_number", "pickup_charge")
    fullData <- NULL
    for (ifile in excelFiles) {
        invoiceFileData <- read.csv(file.path(invoicePath, ifile), quote = '"', sep=",", row.names = NULL,
                                    col.names = colNames,
                                    colClasses = c("character", "character", "myDate",
                                                   "character", "character", "myNumeric", 
                                                   "character", "character", "myNumeric"))
      
      invoiceFileData %<>%
        mutate(tracking_number = toupper(tracking_number))
      
      if (is.null(fullData)) {
        fullData <- invoiceFileData
      } else {
        fullData <- rbind_list(fullData, invoiceFileData)
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


