loadCommonVariables <- function(variablesFilePath) {
suppressMessages({
    require(dplyr)
    require(tools)
    require(magrittr)
    require(methods)
  require(futile.logger)
  })
  
  functionName <- "loadCommonVariables"
  flog.info(paste("Function", functionName, "started"), name = reportName)
  
  commonVarialbes <- tryCatch({
    
    commonVarialbes <- read.csv(variablesFilePath, stringsAsFactors = FALSE)
    
    weightFirstThreshold <<- commonVarialbes$value[1]
    weightSecondThreshold <<- commonVarialbes$value[2]
    weightThirdThreshold <<- commonVarialbes$value[3]
    weightFirstUpperBound <<- commonVarialbes$upper_bound[1]
    weightSecondUpperBound <<- commonVarialbes$upper_bound[2]
    weightThirdUpperBound <<- commonVarialbes$upper_bound[3]
    carryingFeeOver20 <<- commonVarialbes$value[4]
    CODRate1st <<- commonVarialbes$value[5]
    CODRate2nd <<- commonVarialbes$value[6]
    CODRate1stBound <<- commonVarialbes$upper_bound[5]
    CODThreshold <<- commonVarialbes$value[7]
    insuranceFeeRate <<- commonVarialbes$value[8]
    insuranceFeeThreshold <<- commonVarialbes$value[9]
    returnRate <<- commonVarialbes$value[10]
    
    for (iWarn in warnings()){
      flog.warn(paste(functionName, iWarn), name = reportName)
    }
    
    commonVarialbes
    
  }, error = function(err) {
    flog.error(paste(functionName, err, sep = " - "), name = consoleLog)
  }, finally = {
    flog.info(paste(functionName, "ended"), name = reportName)
  })
  
  commonVarialbes
}