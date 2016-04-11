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
  flog.info(paste("Function", functionName, "started"), name = reportName)
  
  output <- tryCatch({
    
    setClass("myDate")
    setAs("character","myDate", function(from) as.POSIXct(substr(gsub('"','',from), 1, 10),
                                                          format="%Y-%m-%d"))
    setClass("myNumeric")
    setAs("character","myNumeric", function(from) as.numeric(gsub('[^0-9\\.]','',from)))
    
    excelFiles <- list.files(invoicePath)
    excelFiles <- excelFiles[grepl("^[^~\\$].*\\.(xls|xlsx|csv)$", excelFiles)]
    invoiceData <- NULL
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
    fullData <- NULL
    for (ifile in excelFiles) {
#       if (file_ext(ifile) %in% c("xls", "xlsx")) {
#         wb <- loadWorkbook(file.path(invoicePath, ifile))
#         invoiceFileData <- readWorksheet(wb, 1, colTypes = c(XLC$DATA_TYPE.STRING, XLC$DATA_TYPE.STRING, XLC$DATA_TYPE.STRING,
#                                                              XLC$DATA_TYPE.STRING, XLC$DATA_TYPE.STRING, XLC$DATA_TYPE.STRING,
#                                                              XLC$DATA_TYPE.STRING, XLC$DATA_TYPE.STRING, XLC$DATA_TYPE.STRING,
#                                                              XLC$DATA_TYPE.NUMERIC, XLC$DATA_TYPE.NUMERIC, XLC$DATA_TYPE.NUMERIC,
#                                                              XLC$DATA_TYPE.NUMERIC, XLC$DATA_TYPE.NUMERIC, XLC$DATA_TYPE.NUMERIC,
#                                                              XLC$DATA_TYPE.NUMERIC, XLC$DATA_TYPE.NUMERIC, XLC$DATA_TYPE.NUMERIC,
#                                                              XLC$DATA_TYPE.NUMERIC, XLC$DATA_TYPE.NUMERIC, XLC$DATA_TYPE.NUMERIC,
#                                                              XLC$DATA_TYPE.NUMERIC, XLC$DATA_TYPE.NUMERIC, XLC$DATA_TYPE.STRING,
#                                                              XLC$DATA_TYPE.STRING, XLC$DATA_TYPE.STRING, XLC$DATA_TYPE.STRING,  XLC$DATA_TYPE.STRING))
#         names(invoiceFileData) <- colNames
#       } else {
        invoiceFileData <- read.csv(file.path(invoicePath, ifile), quote = '"', sep=",", row.names = NULL,
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
        # }
      
      invoiceFileData %<>%
        mutate(package_chargeable_weight = package_weight) %>%
        mutate(tracking_number = ifelse(substr(tracking_number, 1, 1) == "1", tracking_number_rts, tracking_number)) %>%
        mutate(tracking_number = toupper(tracking_number)) %>%
        mutate(rawFile = ifile)
      
      if (is.null(fullData)) {
        fullData <- invoiceFileData
      } else {
        fullData <- rbind_list(fullData, invoiceFileData)
      }
      
      wb <- NULL
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


