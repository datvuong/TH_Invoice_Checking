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
    setAs("character","myDate", function(from) as.POSIXct(substr(gsub('"','',from), 1, 10),
                                                          format="%m/%d/%Y"))
    setClass("myNumeric")
    setAs("character","myNumeric", function(from) as.numeric(gsub('[^0-9\\.]','',from)))
    
    excelFiles <- list.files(FinCODPath, pattern = "*.csv")
    #excelFiles <- excelFiles[grepl("^[^~\\$].*\\.(xls|xlsx|csv)$", excelFiles)]
    invoiceData <- NULL
    colNames <- c("tracking_number", "tracking_number_ref", "pickupDate", "destination", 
                  "cash", "cod_surcharge", "bach_date", "type", "quarter")
    fullData <- NULL
    for (ifile in excelFiles) {
        invoiceFileData <- read.csv(file.path(FinCODPath, ifile), quote = '"', sep=",", row.names = NULL,
                                    col.names = colNames,
                                    colClasses = c("character", "character", "character", "character",
                                                   "myNumeric", "myNumeric", "character", "character", "character"))
      
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


