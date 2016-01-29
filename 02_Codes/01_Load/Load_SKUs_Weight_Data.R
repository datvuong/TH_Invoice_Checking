LoadSKUsWeight <- function(SKUsPath) {
  suppressMessages({
    require(readr)
    require(dplyr)
    require(tools)
    require(magrittr)
    require(methods)
    require(futile.logger)
  })
  
  functionName <- "LoadSKUsWeight"
  flog.info(paste("Function", functionName, "started"), name = reportName)
  
  output <- tryCatch({
    
    setClass("myNumeric")
    setAs("character","myNumeric", function(from) as.numeric(gsub('[^0-9\\.]','',from)))
    
    excelFiles <- list.files(path = folder, pattern = "*.csv") list.files(invoicePath)
    excelFiles <- excelFiles[grepl("^[^~\\$].*\\.(xls|xlsx|csv)$", excelFiles)]
    invoiceData <- NULL
    colNames <- c("3pl_name", "package_pickup_date",
                  "package_pod_date", "invoice_number", "package_number",
                  "tracking_number", "tracking_number_rts", "order_number",
                  "package_volume", "package_height", "package_width",
                  "package_length", "package_weight", "package_chargeable_weight",
                  "carrying_fee", "redelivery_fee", "rejection_fee",
                  "cod_fee", "special_area_fee", "special_handling_fee",
                  "insurance_fee", "vat", "origin_branch",
                  "destination_branch", "delivery_zone_zip_code", "rate_type")
    fullData <- NULL
    for (ifile in excelFiles) {
      if (file_ext(ifile) %in% c("xls", "xlsx")) {
        wb <- loadWorkbook(file.path(invoicePath, ifile))
        invoiceFileData <- readWorksheet(wb, 1, colTypes = c(XLC$DATA_TYPE.STRING, XLC$DATA_TYPE.STRING,
                                                             XLC$DATA_TYPE.STRING, XLC$DATA_TYPE.STRING, XLC$DATA_TYPE.STRING,
                                                             XLC$DATA_TYPE.STRING, XLC$DATA_TYPE.STRING, XLC$DATA_TYPE.STRING,
                                                             XLC$DATA_TYPE.NUMERIC, XLC$DATA_TYPE.NUMERIC, XLC$DATA_TYPE.NUMERIC,
                                                             XLC$DATA_TYPE.NUMERIC, XLC$DATA_TYPE.NUMERIC, XLC$DATA_TYPE.NUMERIC,
                                                             XLC$DATA_TYPE.NUMERIC, XLC$DATA_TYPE.NUMERIC, XLC$DATA_TYPE.NUMERIC,
                                                             XLC$DATA_TYPE.NUMERIC, XLC$DATA_TYPE.NUMERIC, XLC$DATA_TYPE.NUMERIC,
                                                             XLC$DATA_TYPE.NUMERIC, XLC$DATA_TYPE.NUMERIC, XLC$DATA_TYPE.STRING,
                                                             XLC$DATA_TYPE.STRING, XLC$DATA_TYPE.STRING, XLC$DATA_TYPE.STRING))
        names(invoiceFileData) <- colNames
      } else {
        invoiceFileData <- read_csv(file.path(invoicePath, ifile), skip = 1,
                                    col_names = colNames,
                                    col_types = cols(col_character(), col_character(),
                                                  col_character(), col_character(), col_character(),
                                                  col_character(), col_character(), col_character(),
                                                  col_double(), col_double(), col_double(),
                                                  col_double(), col_double(), col_double(),
                                                  col_double(), col_double(), col_double(),
                                                  col_double(), col_double(), col_double(),
                                                  col_double(), col_double(), col_character(),
                                                  col_character(), col_character(), col_character()))
      }
      
      invoiceFileData %<>%
        mutate(package_chargeable_weight = package_weight) %>%
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


