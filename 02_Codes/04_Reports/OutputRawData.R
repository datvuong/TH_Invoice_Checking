OutputRawData <- function(checkedInvoiceData, outputFile) {
suppressMessages({
    require(dplyr)
    require(tools)
    require(magrittr)
    require(methods)
    require(futile.logger)
  })
  
  functionName <- "OutputRawData"
  flog.info(paste("Function", functionName, "started"), name = reportName)
  
  output <- tryCatch({
    
    write.csv(checkedInvoiceData, outputFile, row.names = FALSE)
    
    for (iWarn in warnings()){
      flog.warn(paste(functionName, iWarn), logger = reportName)
    }
    
  }, error = function(err) {
    flog.error(paste(functionName, err, sep = " - "), name = reportName)
  }, finally = {
    flog.info(paste(functionName, "ended"), name = reportName)
  })
  
  output
}

