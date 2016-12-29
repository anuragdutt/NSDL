rm(list = ls())
##' Extract the closing price for a nse listed isin
##'
##' The time-series starting and end can be provided as argument
##' @title getBhavcopyData
##' @param st.dt start date of the time series
##' @param en.dt End date of the time series
##' @param isin Isin for which data is to be extracted
##' @param col CLOSE, OPEN, HIGH, LOW
##' @param zoo return the result as a zoo object
##' @return returns the timeseries of closing prices for the isin required
##' @author Anurag Dutt
getBhavcopyData <- function(st.dt, en.dt, isin, col, zoo = TRUE) {
  library(zoo)
  library(data.table)
  col <- toupper(col) 
  bhav.path <- "/database/SECMKT/BHAVCOPY/CM/"
  date.seq <- seq(from = as.Date(st.dt),
                  to = as.Date(en.dt),
                  by = "day")
  date.day <- format(date.seq, "%a")
  date.seq <- date.seq[which(date.day != "Sun")]
  
  retval <- lapply(date.seq, function(dt) {
    path <- paste0(bhav.path,
                   format(dt , "%Y"),
                   sep = "/",
                   toupper(format(dt , "%b")),
                   sep = "/",
                   paste0(sep = "cm",
                          toupper(format(dt, "%d%b%Y")),
                          sep ="bhav.csv")
                   )
    
    dat <- NULL
    if(file.exists(path)) {
      dat <- fread(path)
      if(isin != "all") {
        dat <- subset(dat[, c("SYMBOL", col, "ISIN"),
                          with = FALSE],
                      ISIN == isin)
        if(nrow(dat) > 1) {
          dat <- dat[!duplicated(dat$SYMBOL, fromLast = TRUE), ]
        }
        dat$DATE <- dt
      } else {
        dat <- dat[, c("SYMBOL", col, "ISIN"),
                   with = FALSE]
        if(nrow(dat) > 1) {
          dat <- dat[!duplicated(dat$SYMBOL, fromLast = TRUE), ]
        }
      }
      dat$DATE <- dt
    } else {
      dat <- NULL
    }
      return(dat)
      
    }
                   )
    
    
    isin.dat <- rbindlist(retval)
  if(zoo == TRUE) {
    isin.dat <- read.zoo(isin.dat[, c("DATE", col), with = FALSE])
  }
  return(isin.dat)
}
