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
    CODRate3rd <<- commonVarialbes$value[7]
    CODRate4th <<- commonVarialbes$value[8]
    CODRate5th <<- commonVarialbes$value[9]
    CODRate6th <<- commonVarialbes$value[10]
    CODRate7th <<- commonVarialbes$value[11]
    CODRate1stBound <<- commonVarialbes$upper_bound[5]
    CODRate2ndBound <<- commonVarialbes$upper_bound[6]
    CODRate3rdBound <<- commonVarialbes$upper_bound[7]
    CODRate4thBound <<- commonVarialbes$upper_bound[8]
    CODRate5thBound <<- commonVarialbes$upper_bound[9]
    CODRate6thBound <<- commonVarialbes$upper_bound[10]
    CODRate7thBound <<- commonVarialbes$upper_bound[11]
    CODThreshold <<- commonVarialbes$value[12]
    insuranceFeeRate <<- commonVarialbes$value[13]
    insuranceFeeThreshold <<- commonVarialbes$value[14]
    returnRate <<- commonVarialbes$value[15]
    
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