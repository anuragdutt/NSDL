###################################################################
## load("/home/nsdl/CONSTANTS/mcap_nse_universe.Rdata")          ##
## load("/home/nsdl/CONSTANTS/close_price_all_stocks.Rdata")     ##
## source("/home/nsdl/LIB/stock_returns.R")                      ##
## library(data.table)                                           ##
## st.dt = "2015-05-01"                                          ##
## en.dt = "2015-05-31"                                          ##
## mcap <- mcap.dat                                              ##
## close <- isin.dat                                             ##
###################################################################
##' Gets the market returns for the entire universe of nifty stocks
## for given dates weighted by market cap
##'
##' Takes in start date and end date as input and calculates the
##'     market cap weighted returns for entire universe of nifty
##'     stocks 
##' @title getMarketReturns
##' @param st.dt starting date of the data
##' @param en.dt end date of the data
##' @param mode use all stocks of nifty stocks
##' @param roll.per rolling window period
##' @param on Calculatio of returns to be done on simple or rolling
##'     window returns
##' @param mcap Market Capitalization for the given universe of nifty stocks
##' @param close Closing prices from Bhavcopy
##' @return Market cap weighted returns for the entire universe of
##'     nifty stocks
##' @author Anurag Dutt
getMarketReturns <- function(st.dt, en.dt,
                             mode = "all",
                             roll.per = 0,
                             on = "simple",
                             mcap,
                             close) {

    source("/home/nsdl/LIB/stock_returns.R")
    st.dt <- as.Date(st.dt)
    en.dt <- as.Date(en.dt)
    mcap.dat <- mcap
    isin.dat <- close
    colnames(isin.dat) <- tolower(colnames(isin.dat))
    isin.dat <- isin.dat[isin.dat$date >= st.dt &
                         isin.dat$date <= en.dt, ]
    isin.dat <- isin.dat[order(isin.dat[["symbol"]],
                               isin.dat[["date"]]), ]
    colnames(mcap.dat) <- c("date", "mcap", "symbol")
    if(mode == "all") {
        mcap.req <- mcap.dat[mcap.dat$date >= st.dt &
                             mcap.dat$date <= en.dt, ]
        if(nrow(mcap.req) == 0) {
            stop("No dates found for calculation of marketcap")
        }
        stocks.req <- unique(mcap.req$symbol)
        ret.dat    <- stockSimpleReturns(isin.dat, roll.per)
        if(on == "simple") {
            ret.dat <- ret.dat[!is.na(ret.dat$simple_returns), ]
        } else {
            ret.dat <- ret.dat[!is.na(ret.dat$rolling_returns), ]
        }
        ret.dat <- ret.dat[ret.dat$date >= st.dt &
                           ret.dat$date <= en.dt, ]
        
        ret.dat <- ret.dat[order(ret.dat[["symbol"]],
                                 ret.dat[["date"]]), ]
        mcap.returns <- merge(ret.dat, mcap.req,
                              by = c("symbol", "date"),
                              all.x = TRUE)
        mcap.returns <- mcap.returns[!is.na(mcap.returns$mcap), ]

        mcap.returns <- mcap.returns[order(mcap.returns[["date"]],
                                           mcap.returns[["symbol"]])]
        dt.st <-
            seq_along(mcap.returns$date)[!duplicated(mcap.returns$date)]
        dt.en <- c((dt.st-1)[-1], length(mcap.returns$date))
        dt.in <- paste0(sep = "c(", dt.st, sep = ":",
                        dt.en, sep = ")")
        
        market.ret <- lapply(dt.in, function(inx) {
            dat <- mcap.returns[eval(parse(text=inx)), ]
            dat <- dat[!is.na(dat$mcap), ]
            if(on == "simple") {
                wm <- weighted.mean(dat$log_returns, dat$mcap)
                
            } else {
                wm <- weighted.mean(dat$rolling_log_returns, dat$mcap)
            }
            retval <- data.table(as.character(unique(dat$date)), wm)
            
        }
        )
        
        market.ret <- rbindlist(market.ret)
        colnames(market.ret) <- tolower(c("DATE", "MARKET_RETURNS"))
        market.ret$date <- as.Date(market.ret$date)

    }
    return(market.ret)

}
