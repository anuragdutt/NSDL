## st.date <- "1980-01-01"             ##
## en.date <- as.character(Sys.Date()) ##
## comp.code <- 258146                  ##
## bhav <- 1                           ##

##' Extract the 
##'
##' .
##' @title 
##' @param comp.code 
##' @param bhav 
##' @return 
##' @author 
mcapCal <- function(comp.code, bhav){


## library("data.table") ##
## library("zoo")      ##
## library("RMySQL")   ##



  path.bhav <- "/database/SECMKT/BHAVCOPY/CM"

  mcap.query   <- paste0("SELECT co_stkdate, nse_market_cap FROM prowess_sa_daily_stocks
                        WHERE co_code = ", comp.code, ";")
  symbol.query <- paste0("SELECT nse_symbol FROM identity
                        WHERE finance1_cocode = ", comp.code, ";")
  
  con <- dbConnect(RMySQL::MySQL(), user="fld", password="fld", dbname="fld")
  dbSendQuery(con, "start transaction")
  mcap.data <- dbGetQuery(con, mcap.query)
  symbol    <- dbGetQuery(con, symbol.query)
  dbSendQuery(con, "commit")

  mcap.data$co_stkdate <- as.Date(as.character(
                                       mcap.data$co_stkdate), "%Y-%m-%d")
  mcap.data <- data.frame(mcap.data, stringsAsFactors=FALSE)
  colnames(mcap.data) <- c("Date", "market_cap")

  mcap.data <- mcap.data[order(mcap.data[["Date"]],
                               mcap.data[["market_cap"]]), ]
  
  all.cons <- dbListConnections(MySQL())
  for(con in all.cons)
    {  dbDisconnect(con)
     }
  bhav.data <- NULL

  if(bhav == 1) {
    date.today <- as.Date(as.character(Sys.Date()))
    date.last  <- as.Date(max(mcap.data$Date))
    if(as.numeric(difftime(date.today, date.last, units = "days")) > 0) {
      date.seq   <- seq(from = date.last, to = date.today, by = "days")
      date.day   <- as.character(format(date.seq, format = "%d"))
      date.month <- toupper(as.character(format(date.seq, format = "%b")))
      date.year  <- as.character(format(date.seq, format = "%Y"))
      file.names <- paste0("/database/SECMKT/BHAVCOPY/CM/",
                           date.year,"/",date.month,"/cm",
                           date.day, date.month, date.year,"bhav.csv")
      all.files  <- list.files(path = "/database/SECMKT/BHAVCOPY/CM",
                               recursive = TRUE, full.names = TRUE)
      files.av   <- all.files[which(all.files %in% file.names)]
      
      if(length(files.av) > 0){ 

#        cat(paste0("loaded datatable for company ", comp.code, "\n"))
        dates.bhav <- lapply(files.av, function(path) {
#          print(path)
          date.files <- tryCatch(fread(path, sep = ",", verbose = FALSE),
                                 error=function(e) NULL)
 #         colnames(date.files) <- date.files[1,]
          if(!is.null(date.files)) {
            date.files <- date.files[, which(colnames(date.files) %in% c("SYMBOL", "SERIES",
                                                                    "CLOSE", "TIMESTAMP")),
                                     with = FALSE]
          }
          return(date.files)
        })          
        bhav.data <- rbindlist(dates.bhav)
        bhav.data <- as.data.frame(bhav.data)
 #       detach("package:data.table", unload=TRUE)

        
        if(nrow(bhav.data) > 0) {
          colnames(bhav.data) <- as.character(c("SYMBOL", "SERIES", "CLOSE", "TIMESTAMP"))
          bhav.data <- as.data.frame.matrix(bhav.data)
          bhav.data <- bhav.data[which(bhav.data$SERIES %in% c("EQ", "BE")),
                                 c("SYMBOL", "SERIES", "TIMESTAMP", "CLOSE")]
          bhav.data$TIMESTAMP <- as.Date(tolower(bhav.data$TIMESTAMP),format = "%d-%b-%Y")
          bhav.data <- bhav.data[order(bhav.data[["TIMESTAMP"]],
                                       bhav.data[["SYMBOL"]],
                                       bhav.data[["SERIES"]]), ]
          bhav.data <- bhav.data[which(bhav.data$SYMBOL == as.character(symbol)), ]
          bhav.data <- bhav.data[!duplicated(bhav.data$TIMESTAMP, fromLast = TRUE), ]
          bhav.data$SERIES <- NULL
          bhav.data$SYMBOL <- NULL
          colnames(bhav.data) <- c("Date", "Closing_Price")
          nos <- as.numeric(tail(mcap.data$market_cap, 1))/
            as.numeric(bhav.data$Closing_Price[which(bhav.data$Date == date.last)])
          bhav.data$market_cap <- as.numeric(bhav.data$Closing_Price)*as.numeric(nos)
          bhav.data <- bhav.data[-which(bhav.data$Date == date.last), ]
          bhav.data <- bhav.data[order(bhav.data[["Date"]],
                                       bhav.data[["Closing_Price"]]), ]
          bhav.data$Closing_Price <- NULL
 #         cat(paste0("detached datatable for company ", comp.code, "\n"))
          
        } else {
          cat(paste0("No Bhavcopy data found for company", symbol, "\n"))
          bhav.data <- NA
        }
 
      } else {
        cat(paste0("No Bhavcopy files found, for remaing dates for company", symbol, "\n"))
      }
    } else {
        cat(paste0("All dates covered in fld for company", symbol, "\n"))
      }
  }
  
if(!is.null(bhav.data) && !is.na(bhav.data) && nrow(bhav.data) > 0) {
    mcap.data <- as.data.frame(rbind(mcap.data, bhav.data),
                               stringsAsFactors = FALSE)
  }
  return(mcap.data)

}










