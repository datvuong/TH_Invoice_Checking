ExistenceChecking <- function(invoiceData) {
  suppressMessages({
    require(dplyr)
    require(tools)
    require(magrittr)
    require(methods)
    require(futile.logger)
  })
  
  functionName <- "ExistenceChecking"
  flog.info(paste("Function", functionName, "started"), name = reportName)
  
  output <- tryCatch({
    
    checkedInvoice <- invoiceData %>%
      mutate(ExistenceCheck = ifelse(!is.na(order_nr), "OKAY", "NOT_FOUND"))
    
    checkedInvoice
    
  }, error = function(err) {
    flog.error(paste(functionName, err, sep = " - "), name = reportName)
  }, finally = {
    flog.info(paste("Function", functionName, "ended"), name = reportName)
  })
  
  output
}