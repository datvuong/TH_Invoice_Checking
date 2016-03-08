dateReport <- format(Sys.time(), "%Y%m%d")
timeReport <- format(Sys.time(), "%Y%m%d%H%M")
suppressMessages({
  options( java.parameters = "-Xmx8g" ) # Set heap memory for Java upto 4GB
#   if (!("readr" %in% rownames(installed.packages()))) {
#     install.packages("readr", repos = "http://cran.rstudio.com", quiet = TRUE)
#   }
  library(readr)
  library(dplyr)
  library(tidyr)
  library(magrittr)
  library(methods)
  library(lubridate)
  library(futile.logger)
  library(XLConnect)
})

reportName <- paste0("THInvoiceCheck")
warningLog <- paste0("THInvoiceCheck", "warning")
flog.appender(appender.tee(file.path("06_Log",
                                      paste0("TH_InvoiceChecking",dateReport,".csv"))),
              name = reportName)

layout <- layout.format(paste0(timeReport,'|[~l]|[~t]|[~n.~f]|~m'))
flog.layout(layout, name=reportName)