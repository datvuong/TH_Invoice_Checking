loadCommonVariables <- function(variablesFilePath) {
suppressMessages({
    require(dplyr)
    require(tools)
    require(magrittr)
    require(methods)
    # require(logging)
  require(futile.logger)
  })
  
  functionName <- "loadCommonVariables"
  # loginfo(paste("Function", functionName, "started"), logger = reportName)
  flog.info(paste("Function", functionName, "started"), name = reportName)
  
  commonVarialbes <- tryCatch({
    
    commonVarialbes <- read.csv(variablesFilePath, stringsAsFactors = FALSE)
    
    codThreshold <<- commonVarialbes$value[1]
    weightFirstThreshold <<- commonVarialbes$value[2]
    weightSecondThreshold <<- commonVarialbes$value[3]
    weightThirdThreshold <<- commonVarialbes$value[4]
    weightFirstUpperBound <<- commonVarialbes$upper_bound[2]
    weightSecondUpperBound <<- commonVarialbes$upper_bound[3]
    weightThirdUpperBound <<- commonVarialbes$upper_bound[4]
    
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