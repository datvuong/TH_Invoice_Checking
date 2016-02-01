SummaryReport <- function(checkedInvoice, outputFile) {
  suppressMessages({
    require(dplyr)
    require(tools)
    require(magrittr)
    require(methods)
    require(logging)
  })
  
  functionName <- "SummaryReport"
  loginfo(paste("Function", functionName, "started"), logger = reportName)
  
  tryCatch({
    
    summary <- checkedInvoice %>%
      group_by(manualCheck) %>%
      summarize(txnCount = n(),
                invoiceFee = sum(carryingFee),
                lazSuggested = sum(feeSuggested))
    
    write.csv(summary, outputFile, row.names = FALSE)
    
    for (iWarn in warnings()){
      logwarn(paste(functionName, iWarn), logger = reportName)
    }
    
  }, error = function(err) {
    logerror(paste(functionName, err, sep = " - "), logger = consoleLog)
  }, finally = {
    loginfo(paste(functionName, "ended"), logger = reportName)
  })
  
}