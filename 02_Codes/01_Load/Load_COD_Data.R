LoadCODInvoice <- function(CODPath) {
  suppressMessages({
    require(readr)
    require(dplyr)
    require(tools)
    require(magrittr)
    require(methods)
    require(futile.logger)
  })
  
  functionName <- "LoadCODInvoice"
  flog.info(paste("Function", functionName, "started"), name = reportName)
  
  output <- tryCatch({
    
    setClass("myNumeric")
    setAs("character","myNumeric", function(from) as.numeric(gsub('[^0-9\\.]','',from)))
    
    excelFiles <- list.files(path = CODPath, pattern = "*.csv") 
    # excelFiles <- excelFiles[grepl("^[^~\\$].*\\.(xls|xlsx|csv)$", excelFiles)]
    CODData <- NULL
    colNames <- c("tracking_number", "order_number", "destination", "cash", 
                  "cod_fee", "tracking_number_KE", "report")
    fullData <- NULL
    for (ifile in excelFiles) {
#       if (file_ext(ifile) %in% c("xls", "xlsx")) {
#         wb <- loadWorkbook(file.path(invoicePath, ifile))
#         invoiceFileData <- readWorksheet(wb, 1, colTypes = c(XLC$DATA_TYPE.STRING, XLC$DATA_TYPE.STRING,
#                                                              XLC$DATA_TYPE.STRING, XLC$DATA_TYPE.STRING, XLC$DATA_TYPE.STRING,
#                                                              XLC$DATA_TYPE.STRING, XLC$DATA_TYPE.STRING, XLC$DATA_TYPE.STRING,
#                                                              XLC$DATA_TYPE.NUMERIC, XLC$DATA_TYPE.NUMERIC, XLC$DATA_TYPE.NUMERIC,
#                                                              XLC$DATA_TYPE.NUMERIC, XLC$DATA_TYPE.NUMERIC, XLC$DATA_TYPE.NUMERIC,
#                                                              XLC$DATA_TYPE.NUMERIC, XLC$DATA_TYPE.NUMERIC, XLC$DATA_TYPE.NUMERIC,
#                                                              XLC$DATA_TYPE.NUMERIC, XLC$DATA_TYPE.NUMERIC, XLC$DATA_TYPE.NUMERIC,
#                                                              XLC$DATA_TYPE.NUMERIC, XLC$DATA_TYPE.NUMERIC, XLC$DATA_TYPE.STRING,
#                                                              XLC$DATA_TYPE.STRING, XLC$DATA_TYPE.STRING, XLC$DATA_TYPE.STRING))
#         names(invoiceFileData) <- colNames
#       } else {
        CODFileData <- read_csv(file.path(CODPath, ifile), skip = 1,
                                    col_names = colNames,
                                    col_types = cols(col_character(), col_character(), col_character(), col_double(),
                                                  col_double(), col_character(), col_character()))
      # }
      
      CODFileData %<>%
        mutate(package_chargeable_weight = package_weight) %>%
        mutate(rawFile = ifile)
      
      if (is.null(fullData)) {
        fullData <- CODFileData
      } else {
        fullData <- rbind_list(fullData, CODFileData)
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


