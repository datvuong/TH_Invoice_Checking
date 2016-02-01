OutputRawData <- function(checkedInvoiceData, outputFile) {
suppressMessages({
    require(dplyr)
    require(tools)
    require(magrittr)
    require(methods)
    require(logging)
  })
  
  functionName <- "OutputRawData"
  loginfo(paste("Function", functionName, "started"), logger = reportName)
  
  tryCatch({
    
    write.csv(checkedInvoiceData, outputFile, row.names = FALSE)
    
    for (iWarn in warnings()){
      logwarn(paste(functionName, iWarn), logger = reportName)
    }
    
  }, error = function(err) {
    logerror(paste(functionName, err, sep = " - "), logger = consoleLog)
  }, finally = {
    loginfo(paste(functionName, "ended"), logger = reportName)
  })
}