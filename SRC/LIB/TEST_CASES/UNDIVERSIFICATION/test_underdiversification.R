##########################################
## load("demo_portfolio_returns.Rdata") ##
## load("demo_market_returns.Rdata")    ##
##########################################

library(data.table)
load("../../../CONSTANTS/mcap_nse_universe.Rdata")
load("../../../CONSTANTS/close_price_all_stocks.Rdata")
source("../../read_holdings_file.R")
options(scipen = 999, digits = 10)
##' Function to calculate simple and rolling window returns for a set
##' of stocks
##' The function takes in closing price timeseries of a set of stocks
##' as input and returns the log and simple returns with or without a
##' rolling window analysis
##' @title stockSimpleReturns 
##' @param price.data closing price data time series for a group of stocks
##' @param roll.per rolling window period if any
##' @param log.ret logical; TRUE if log returns are to be calculated
##' @return returns time series of a group of stocks based on closing prices
##' @author Anurag Dutt
stockSimpleReturns <- function(price.data,
                               roll.per = 0,
                               log.ret = TRUE) {
    library(zoo)
    library(data.table)

    ## set column names
    
    colnames(price.data) <- c("SYMBOL", "PRICE", "ISIN", "DATE")
    price.data$DATE <- as.Date(price.data$DATE)
    ## get the list for stocks for which returns are to be calculated
    
    all.stocks <- unique(price.data$SYMBOL)
    all.ret <- do.call('rbind',lapply(all.stocks, function(stk) {
#        print(stk)
        isin.ind <- subset(price.data, SYMBOL == stk)
        isin.ind <- isin.ind[order(isin.ind[["DATE"]]), ]
        ## check if data is available; proceed if true
        
        if(nrow(isin.ind) > roll.per) {

            ## simple returns
            
            simple.returns <-
                diff(isin.ind$PRICE)/isin.ind$PRICE[-length(isin.ind$PRICE)]
            isin.ind$SIMPLE_RETURNS <- c(NA, simple.returns)

            ## simple rolling window returns
            
            if(roll.per >= 1) {
                rolling.returns <- rollapply(simple.returns,
                                             roll.per,
                                             mean)
                isin.ind$ROLLING_RETURNS <- c(rep(NA, roll.per),
                                              rolling.returns)
            }

            ## log of simple returns
            
            if(log.ret == TRUE) {
                log.returns <- diff(log(isin.ind$PRICE))
                isin.ind$LOG_RETURNS <- c(NA, log.returns)

                ## log of rolling window returns
                
                if(roll.per >= 1) {
                    rolling.log <- rollapply(log.returns,
                                             roll.per,
                                                 mean)
                    isin.ind$ROLLING_LOG_RETURNS <- c(rep(NA, roll.per), rolling.log)
                    
                }
            }
            return(isin.ind)
        } else {
            return(NULL)
        }
    }
    )
    )

    colnames(all.ret) <- tolower(colnames(all.ret))
    
    return(all.ret)
}



##' Gets the market returns for the entire universe of nifty stocks
## for given dates weighted by market cap
##'
##' Takes in start date and end date as input and calculates the
##'     market cap weighted returns for entire universe of nifty
##'     stocks 
##' @title getMarketReturns
##' @param st.dt starting date of the data
##' @param en.dt end date of the data
##' @param mode use all stocks or nifty stocks
##' @param roll.per rolling window period
##' @param on Calculation of returns to be done on simple or rolling
##'     window returns
##' @param mcap Market Capitalization for the given universe of nifty stocks
##' @param close Closing prices from Bhavcopy
##' @return Market cap weighted returns for the entire universe of
##'     nifty stocks
##' @author Anurag Dutt
getMarketReturns <- function(st.dt, en.dt,
                             mode = "all",
                             nifty = NA,
                             roll.per = 0,
                             on = "simple",
                             mcap,
                             close) {


    ## convert start and end to date format
    
    st.dt <- as.Date(st.dt)
    en.dt <- as.Date(en.dt)

    ## order the data.frame and store the market cap values
    
    mcap.dat <- mcap
    isin.dat <- close
    colnames(isin.dat) <- tolower(colnames(isin.dat))
    isin.dat <- isin.dat[isin.dat$date >= st.dt &
                         isin.dat$date <= en.dt, ]
    isin.dat <- isin.dat[order(isin.dat[["symbol"]],
                               isin.dat[["date"]]), ]
    colnames(mcap.dat) <- c("date", "mcap", "symbol")

    ## if market comprises of entire universe of nse stocks
    
    if(mode == "all") {

        ## pick out the dates between start and end dates
        
        mcap.req <- mcap.dat[mcap.dat$date >= st.dt &
                             mcap.dat$date <= en.dt, ]
        if(nrow(mcap.req) == 0) {
            stop("No dates found for calculation of marketcap")
        }

        ## pick all the stocks for which market cap data is available
        
        stocks.req <- unique(mcap.req$symbol)

        ## Calculate the returns of the entire nse universe of stocks
        ## for which closing prices are available
        
        ret.dat    <- stockSimpleReturns(isin.dat, roll.per)
        if(on == "simple") {
            ret.dat <- ret.dat[!is.na(ret.dat$log_returns), ]
        } else {
            ret.dat <- ret.dat[!is.na(ret.dat$rolling_log_returns), ]
        }

        ## pick out the returns between the start and end dates
        
        ret.dat <- ret.dat[ret.dat$date >= st.dt &
                           ret.dat$date <= en.dt, ]
        
        ret.dat <- ret.dat[order(ret.dat[["symbol"]],
                                 ret.dat[["date"]]), ]
        ## merge returns with the market cap data
        
        mcap.returns <- merge(ret.dat, mcap.req,
                              by = c("symbol", "date"),
                              all.x = TRUE)
        ## remove the dates for which market cap data is not available
        
        mcap.returns <- mcap.returns[!is.na(mcap.returns$mcap), ]

        mcap.returns <- mcap.returns[order(mcap.returns[["date"]],
                                           mcap.returns[["symbol"]])]
        dt.st <-
            seq_along(mcap.returns$date)[!duplicated(mcap.returns$date)]
        dt.en <- c((dt.st-1)[-1], length(mcap.returns$date))
        dt.in <- paste0(sep = "c(", dt.st, sep = ":",
                        dt.en, sep = ")")

        ## calculate market returns for all dates for all the stocks
        ## weighted by market cap
        
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

    } else if(mode == "nifty") {
        if(is.na(nifty)) {
            stop("Provide a list of nifty isins")
        }

        ## sort out the nifty 50 stocks
        mcap.dat <- mcap.dat[which(mcap.dat$symbol %in% nifty), ]

        ## pick out the dates between start and end dates

        mcap.req <- mcap.dat[mcap.dat$date >= st.dt &
                             mcap.dat$date <= en.dt, ]
        if(nrow(mcap.req) == 0) {
            stop("No dates found for calculation of marketcap")
        }

        ## pick all the stocks for which market cap data is available

        stocks.req <- unique(mcap.req$symbol)

        ## Calculate the returns of the cnx nifty 50 stocks
        ## for which closing prices are available
        
        isin.dat   <- isin.dat[which(isin.dat$symbol %in% nifty), ]
        ret.dat    <- stockSimpleReturns(isin.dat, roll.per)
        if(on == "simple") {
            ret.dat <- ret.dat[!is.na(ret.dat$log_returns), ]
        } else {
            ret.dat <- ret.dat[!is.na(ret.dat$rolling_log_returns), ]
        }
        ret.dat <- ret.dat[ret.dat$date >= st.dt &
                           ret.dat$date <= en.dt, ]
        
        ret.dat <- ret.dat[order(ret.dat[["symbol"]],
                                 ret.dat[["date"]]), ]

        ## merge returns with the market cap data
        
        mcap.returns <- merge(ret.dat, mcap.req,
                              by = c("symbol", "date"),
                              all.x = TRUE)

        ## remove the dates for which market cap data is not available

        mcap.returns <- mcap.returns[!is.na(mcap.returns$mcap), ]

        mcap.returns <- mcap.returns[order(mcap.returns[["date"]],
                                           mcap.returns[["symbol"]])]
        dt.st <-
            seq_along(mcap.returns$date)[!duplicated(mcap.returns$date)]
        dt.en <- c((dt.st-1)[-1], length(mcap.returns$date))
        dt.in <- paste0(sep = "c(", dt.st, sep = ":",
                        dt.en, sep = ")")

        ## calculate market returns for all dates for all the nifty 50
        ## weighted by market cap
        
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

getPortfolioReturns <- function(holdings,
                                roll.per = 0,
                                on = "simple",
                                wt = "qty",
                                close) {
    source("/home/nsdl/LIB/stock_returns.R")
    holdings <- data.table(holdings)
    isin.dat <- close
    ret.dt   <- as.Date(unique(holdings$date))
    isin.dat <- isin.dat[isin.dat$DATE <= ret.dt, ]
    isin.dat <- isin.dat[order(isin.dat[["SYMBOL"]],
                               isin.dat[["DATE"]]), ]
    colnames(isin.dat) <- tolower(colnames(isin.dat))
    
    ret.dat <- stockSimpleReturns(isin.dat, roll.per)
    holdings$date <- as.Date(holdings$date)

    for(i in c(1:10)) {
        if(length(which(ret.dt %in% unique(ret.dat$date))) == 0) {
            ret.dt = ret.dt - 1
        }
    }

    ret.dat <- ret.dat[ret.dat$date == ret.dt, ]
    if(on == "simple") {
        ret.dat <- ret.dat[!is.na(ret.dat$simple_returns), ]
                 
    } else {
        ret.dat <- ret.dat[!is.na(ret.dat$rolling_returns), ]
                     
    }
    ret.dat$date <- as.Date(unique(holdings$date))

    return.portfolio <- merge(holdings, ret.dat,
                              by = c("date", "isin"),
                              all.x = TRUE)
    return.portfolio <- return.portfolio[!is.na(return.portfolio$log_returns)]
    return.portfolio <-
        return.portfolio[order(return.portfolio[["client_id"]],
                               return.portfolio[["isin"]])]

    return.portfolio$val <-
        return.portfolio$qty*return.portfolio$price

    c.st <-
        seq_along(return.portfolio$client_id)[!duplicated(return.portfolio$client_id)]
    c.en <- c((c.st-1)[-1], length(return.portfolio$lient_id))
    c.in <- paste0(sep = "c(", c.st,
                   sep = ":",
                   c.en,
                   sep = ")")
    
        portfolio.ret <- lapply(c.in, function(inx){
            dat <- return.portfolio[eval(parse(text=inx)), ]
            if(wt == "qty") {
                wm <- weighted.mean(dat$log_returns, dat$qty)
            } else {
                wm <- weighted.mean(dat$log_returns, dat$val)
            }
            retval <- data.table(unique(dat$client_id), wm, sum(dat$val))
            return(retval)
        })

    portfolio.ret <- rbindlist(portfolio.ret)
    colnames(portfolio.ret) <- tolower(c("CLIENT_ID", "PORTFOLIO_RETURNS", "VAL"))
    

    return(portfolio.ret)

}

prepareReturnSeries <- function(close,
                                roll.ud = 250,
                                ret.dt,
                                roll.per = 0,
                                on = "simple") {
    library(data.table)
    library(zoo)
    isin.dat <- close
    ret.dt   <- as.Date(ret.dt)
    isin.dat <- isin.dat[isin.dat$DATE <= ret.dt, ]
    isin.dat <- isin.dat[order(isin.dat[["SYMBOL"]],
                               isin.dat[["DATE"]]), ]
    colnames(isin.dat) <- tolower(colnames(isin.dat))
    dt.unique <- sort(unique(isin.dat$date))
    ret.dat   <- stockSimpleReturns(isin.dat, roll.per)
    
    for(i in c(1:10)) {
        if(length(which(ret.dt %in% unique(ret.dat$date))) == 0) {
            ret.dt = ret.dt - 1
            
        }
    }

    dt.inx  <- which(dt.unique == ret.dt)
    inx.req <- (dt.inx - roll.ud):dt.inx
    dt.req  <- dt.unique[inx.req]

    if(on == "simple") {
        ret.dat <- ret.dat[!is.na(ret.dat$log_returns), ]
        col <- "log_returns"
    } else {
        ret.dat <- ret.dat[!is.na(ret.dat$rolling_log_returns), ]
        col <- "rolling_log_returns"
               
    }

    symbols <- unique(ret.dat$symbol[which(ret.dat$date == ret.dt)])
    ret.dat <- ret.dat[which(ret.dat$symbol %in% symbols), ]
    ret.dat <- ret.dat[order(ret.dat[["symbol"]],
                             ret.dat[["date"]]), ]
    
    i.st <-
        seq_along(ret.dat$symbol)[!duplicated(ret.dat$symbol)]
    i.en <- c((i.st[-1] - 1), length(ret.dat$symbol))
    i.in <- paste0(sep = "c(", i.st, sep = ":", i.en, sep = ")")


    get.returns <- lapply(i.in,
                          function(inx) {
                              dat     <- ret.dat[eval(parse(text = inx)), ]
                              isin    <- unique(dat$isin) 
                              df.temp <- data.table(date = dt.req)
                              ret.req <- dat[order(dat[["date"]]),
                                             c("date",
                                               "simple_returns"),
                                             with = FALSE]
                              retval  <- merge(df.temp, ret.req,
                                               by = "date",
                                               all.x = TRUE)
                              if(length(which(
                                  is.na(retval$simple_returns))) > 0) {
                                  retval$simple_returns[
                                      which(is.na(retval$simple_returns))] <- 0
                              }
                              
                              isin.col <- tail(dat$isin, 1) 

                              colnames(retval) <- c("date", isin.col)
                              return(retval)
                              
                          })
    

    returns <- Reduce(function (x, y) merge(x, y, by = "date",
                                            allow.cartesian = TRUE),
                      get.returns)
   
    
    return(returns)
    
}

calUnderdiversification <- function(market.ret,
                                    portfolio.ret,
                                    r.squared = TRUE) {
    if(r.squared == FALSE) {
        mg <- lm(portfolio.ret ~ market.ret)
        ##  mg <- lm(ret.series ~ market.ret$market_returns)
        ##  varh <- var(market.ret$market_returns)
        beta <- summary(mg)$coefficients[2,1]
        varh <- var(market.ret)
        vare <- var(resid(mg))
        ud <- round(vare/((beta^2)*varh + vare),2)*100
        
    } else {
        mg <- lm(portfolio.ret ~ market.ret)
        ## mg <- lm(ret.series ~ market.ret$market_returns)
        r.squared <- summary(mg)$adj.r.squared
        div <- r.squared
        beta <- summary(mg)$coefficients[2,1]
        ud <- (1 - div)*100
    }
    return(c(ud, beta))
    
}

applyFunc <- function(expr){
    bquote(getUd(holdings[.(eval(expr)), ]))

    
}



getDummyPortfolioReturns <- function(holdings,
                                     returns,
                                     market.ret) {
    library(data.table)
    holdings <- data.table(holdings)

    c.st <-
        seq_along(holdings$client_id)[!duplicated(holdings$client_id)]
    c.en <- c((c.st[-1] - 1), length(holdings$client_id))
    c.in <- paste0(sep = "c(", c.st, sep = ":", c.en, sep = ")")

## c.dat <- holdings[eval(parse(text = c.in))]    
    
    getUd <- function(c.dat) {
        weight.vec  <- c.dat$qty
        names(weight.vec) <- c.dat$isin
        isin.unique <- unique(c.dat$isin)
        returns.col <- colnames(returns)[which(colnames(returns) %in%
                                               isin.unique)]
        portfolio.all <- returns[,
                                 c("date", returns.col),
                                 with = FALSE]
        portfolio.stk <- portfolio.all[,
                                       c(names(weight.vec)),
                                       with = FALSE]
        portfolio.ret <- data.table(t(t(portfolio.stk)*weight.vec))
        portfolio.ret <- portfolio.ret[, names(weight.vec),
                                       with = FALSE]
        portfolio.ret <- portfolio.ret[,
                                       sum := Reduce(`+`, .SD),
                                       .SDcols=colnames(portfolio.ret)][]
        ret.series    <- portfolio.ret$sum/sum(weight.vec)
        ud            <-
            calUnderdiversification(market.ret$market_returns, 
                                    ret.series)
        retval        <- c(unique(c.dat$client_id), ud)
        return(retval)
    }

    compile.func <- lapply(c.in, function(id) applyFunc(parse(text = id)))
    ud.holdings  <- do.call('rbind', compile.func)
    colnames(ud.holdings) <- c("client_id",
                               "underdiversification",
                               "portfolio_beta")

    return(ud.holdings)
}


dummy <- read.csv("dummy_portfolio.csv",
                  stringsAsFactor = FALSE)
colnames(dummy) <- c("company", "industry", "symbol", "series", "isin")
isin.all <- dummy$isin
symbol.all <- dummy$symbol                                             
nifty.cal <- getMarketReturns(st.dt = as.Date("2015-02-11"),          
                              en.dt = as.Date("2016-03-28"),          
                              nifty = symbol.all,                     
                              mode = "nifty",                         
                              roll.per = 0,                           
                              mcap = mcap.dat,                        
                              close = isin.dat)                       


holdings <-
    readFile("DUMMY_DATA/20160328/IGIDR_POSITION_28032016.CSV.gz",
             only.nse = FALSE)
returns <- prepareReturnSeries(isin.dat,
                               ret.dt = unique(holdings$date))
ud.holdings <- getDummyPortfolioReturns(holdings,
                                        returns,
                                        market.ret = nifty.cal)








####################################### comparison of a 4 stock portfolio

price.data <- isin.dat[which(isin.dat$symbol %in% symbol.all), ]

port.ret <- stockSimpleReturns(price.data
                               )
df <- NULL
df <- lapply(symbol.all, function(sym) {
    dat <- port.ret[which(port.ret$symbol == sym),
                    c("date", "log_returns"),
                    with = FALSE]
    colnames(dat) <- c("date", sym)
    return(dat)
}
)

ret.dat <- Reduce(function(x, y) merge(x, y, by = "date", all = TRUE), df)
ret.dat <- ret.dat[-1, ]
ret.dat[is.na(ret.dat)] <- 0
total.ret <- rowSums(ret.dat[, 2:ncol(ret.dat),
                             with = FALSE])

ret.eq <- data.frame(date = ret.dat$date,
                     equal_returns = total.ret/5)
all.ret <- merge(nifty.cal, ret.eq, by = "date", all.x = TRUE)

pdf("returns_comparison.pdf")
par(mar = c(2, 4, 2, 2))
plot(read.zoo(all.ret[, c("date", "market_returns"), with = FALSE]),
     ylim = range(c(all.ret$market_returns, all.ret$equal_returns)),
     lwd = 2,
     xlab = "dates",
     ylab = "returns",
     col = "red", type = 'l')
eq <- read.zoo(all.ret[, c("date", "equal_returns"), with = FALSE])
lines(eq,
      col = "blue",
      lwd = 2)
legend(x = "bottomleft", bty = "n", lwd = c(2,2,2,2,2), lty =
                                                            c(1,1,1,1,1),
       col= c("red", "blue"),
       legend = c("MARKET CAP WEIGHTED RETURNS",
                  "EQUALLY WEIGHTED RETURNS"))
grid()
                                        #boxplot(write)
dev.off()


##################################################comparing kospi mcap

all.files <- list.files("../KOSPI",
                        full.names = TRUE)
market.cap <- c(24, 20, 36, 40, 23, 18, 207, 14, 17)
names(market.cap) <- c("hyndai_mobis",
                       "hynix",
                       "hyundai",
                       "korea_electric",
                       "naver",
                       "posco",
                       "samsung",
                       "samsung_sds",
                       "sk_telecom"
                       )
holdings <- data.frame(dp_id = rep(1, length(market.cap)),
                       client_id = rep(1, length(market.cap)),
                       date = as.Date("2016-05-22"),
                       isin = names(market.cap),
                       qty = market.cap                       
                       )

price.dat <- do.call('rbind',
                     lapply(all.files[-grep("kospi", all.files)],
                            function(close){
                                dat <- read.csv(close)
                                colnames(dat) <- c("DATE", "CLOSE")
                                dat$SYMBOL <- unlist(strsplit(unlist(strsplit(close,
                                                                              ".csv")),
                                                              "/"))[3]
                                dat$ISIN <- dat$SYMBOL
                                return(dat)
                            })
                     )

price.dat <- price.dat[, c("SYMBOL", "CLOSE", "ISIN", "DATE")]

price.dat$DATE <- as.Date(price.dat$DATE, format = "%d-%m-%Y")
price.dat <- data.table(price.dat)
returns <- prepareReturnSeries(price.dat,
                               ret.dt = unique(holdings$date)
                               )

kospi.dat <- read.csv("../KOSPI/kospi_daily_values.csv",
                      stringsAsFactor = FALSE)
kospi.dat <- kospi.dat[1:342, c("Exchange.Date", "Close")]
colnames(kospi.dat) <- c("date", "close")
kospi.dat$date <- as.Date(kospi.dat$date, format = "%d-%b-%Y")
kospi.dat$symbol <- "kospi"
kospi.dat$isin <- "kospi"
kospi.dat <- kospi.dat[, c("symbol", "close", "isin", "date")]
kospi.dat$close <- as.numeric(gsub(",", "", kospi.dat$close))
colnames(kospi.dat) <- toupper(c("symbol", "close", "isin", "date")) 
market.ret <- stockSimpleReturns(kospi.dat)
market.ret <- market.ret[which(market.ret$date >=
                               as.Date("2015-05-15")), ]
market.ret <- market.ret[, c("date", "log_returns")]
colnames(market.ret) <- c("date", "market_returns")
market.ret <- data.table(market.ret)
get.ud <- getDummyPortfolioReturns(holdings,
                                   returns,
                                   market.ret)



##################################################comparing kospi
##################################################equal wt

all.files <- list.files("../KOSPI",
                        full.names = TRUE)
market.cap <- rep(1, 9)
names(market.cap) <- c("hyndai_mobis",
                       "hynix",
                       "hyundai",
                       "korea_electric",
                       "naver",
                       "posco",
                       "samsung",
                       "samsung_sds",
                       "sk_telecom"
                       )
holdings <- data.frame(dp_id = rep(1, length(market.cap)),
                       client_id = rep(1, length(market.cap)),
                       date = as.Date("2016-05-22"),
                       isin = names(market.cap),
                       qty = market.cap                       
                       )

price.dat <- do.call('rbind',
                     lapply(all.files[-grep("kospi", all.files)],
                            function(close){
                                dat <- read.csv(close)
                                colnames(dat) <- c("DATE", "CLOSE")
                                dat$SYMBOL <- unlist(strsplit(unlist(strsplit(close,
                                                                              ".csv")),
                                                              "/"))[3]
                                dat$ISIN <- dat$SYMBOL
                                return(dat)
                            })
                     )

price.dat <- price.dat[, c("SYMBOL", "CLOSE", "ISIN", "DATE")]

price.dat$DATE <- as.Date(price.dat$DATE, format = "%d-%m-%Y")
price.dat <- data.table(price.dat)
returns <- prepareReturnSeries(price.dat,
                               ret.dt = unique(holdings$date)
                               )

kospi.dat <- read.csv("../KOSPI/kospi_daily_values.csv",
                      stringsAsFactor = FALSE)
kospi.dat <- kospi.dat[1:342, c("Exchange.Date", "Close")]
colnames(kospi.dat) <- c("date", "close")
kospi.dat$date <- as.Date(kospi.dat$date, format = "%d-%b-%Y")
kospi.dat$symbol <- "kospi"
kospi.dat$isin <- "kospi"
kospi.dat <- kospi.dat[, c("symbol", "close", "isin", "date")]
kospi.dat$close <- as.numeric(gsub(",", "", kospi.dat$close))
colnames(kospi.dat) <- toupper(c("symbol", "close", "isin", "date")) 
market.ret <- stockSimpleReturns(kospi.dat)
market.ret <- market.ret[which(market.ret$date >=
                               as.Date("2015-05-15")), ]
market.ret <- market.ret[, c("date", "log_returns")]
colnames(market.ret) <- c("date", "market_returns")
market.ret <- data.table(market.ret)
get.ud <- getDummyPortfolioReturns(holdings,
                                   returns,
                                   market.ret)

############################################## Analysis Cospi by
############################################## market cap
load("../../../CONSTANTS/close_price_all_stocks.Rdata")
cospi.comp <- read.csv("../COSPI/cospi_companies.csv",
                       stringsAsFactor = FALSE)
colnames(cospi.comp) <- c("company_name", "nse_symbol")
cospi.dat  <- read.csv("../COSPI/cospi_data.csv",
                       stringsAsFactor = FALSE)
colnames(cospi.dat) <- c("date", "close", "daily_returns",
                         "number_of_companies", "beta", "diversification")
cospi.dat$date <- as.Date(cospi.dat$date, format = "%d-%b-%y")
cospi.dat$isin <- "cospi"
cospi.dat$symbol <- "cospi"

cospi.ser  <- cospi.dat[, c("symbol", "close", "isin", "date")]
market.ret <- stockSimpleReturns(cospi.ser)

comp.select <- cospi.comp[which(cospi.comp$nse_symbol %in%
                                c("ITC", "CIPLA", "DRREDDY",
                                  "LUPIN", "SUNPHARMA",
                                  "INFY", "TCS", "ACC",
                                  "AUROPHARMA", "HCLTECH",
                                  "WIPRO", "TECHM",
                                  "TATAMOTORS", "MARUTI",
                                  "HDFCBANK", "ICICIBANK",
                                  "COALINDIA",  "ONGC",
                                  "RELIANCE", "BHARTIARTL",
                                  "HINDUNILVR",
                                  "LT", "BHEL", "ZEEMEDIA")), ]
colnames(comp.select) <- c("company", "symbol")
isin.dat <- isin.dat[which(isin.dat$SYMBOL %in%
                           unique(comp.select$symbol)), ]
colnames(isin.dat) <- tolower(colnames(isin.dat))
isins <- isin.dat[, c("symbol", "isin"), with = FALSE]
comp.select <- merge(comp.select, isins, by = "symbol", all.x = TRUE)
comp.select <- comp.select[!duplicated(comp.select$symbol), ]
comp.select$qty <- c(282, 434, 1400, 314, 376, 1780, 526, 1050,
                     2980, 1840, 1390, 2820, 2920, 661,
                     1240, 1180, 1830, 3077,  1890,
                     1290, 5040, 526, 1350, 10)

holdings <- do.call('rbind', lapply(1:10, function(cl) {
    len.pos  <- 1:nrow(comp.select)
    port.inx <- sample(len.pos, 5)
    dat <- comp.select[port.inx, c("isin", "qty")]
    dat$client_id <- cl
    dat$date <- as.Date("2016-03-31")
    dat$dp_id <- 1
    dat <- dat[, c("dp_id", "client_id", "date", "isin", "qty")]
}))
colnames(isin.dat) <- toupper(colnames(isin.dat))
returns <- prepareReturnSeries(isin.dat,
                               ret.dt = unique(holdings$date)
                               )


market.ret <- market.ret[, c("date", "log_returns")]
colnames(market.ret) <- c("date", "market_returns")

all.ret <- merge(returns, market.ret, by = "date", all.x = TRUE)

returns <- all.ret
returns$market_returns <- NULL
market.ret <- all.ret[, c("date", "market_returns"), with = FALSE]

get.ud <- getDummyPortfolioReturns(holdings,
                                   returns,
                                   market.ret)


############################################## Analysis Cospi by
############################################## equal weight
holdings$qty <- 1
load("../../../CONSTANTS/close_price_all_stocks.Rdata")
cospi.comp <- read.csv("../COSPI/cospi_companies.csv",
                       stringsAsFactor = FALSE)
colnames(cospi.comp) <- c("company_name", "nse_symbol")
cospi.dat  <- read.csv("../COSPI/cospi_data.csv",
                       stringsAsFactor = FALSE)
colnames(cospi.dat) <- c("date", "close", "daily_returns",
                         "number_of_companies", "beta", "diversification")
cospi.dat$date <- as.Date(cospi.dat$date, format = "%d-%b-%y")
cospi.dat$isin <- "cospi"
cospi.dat$symbol <- "cospi"

cospi.ser  <- cospi.dat[, c("symbol", "close", "isin", "date")]
market.ret <- stockSimpleReturns(cospi.ser)

comp.select <- cospi.comp[which(cospi.comp$nse_symbol %in%
                                c("ITC", "CIPLA", "DRREDDY",
                                  "LUPIN", "SUNPHARMA",
                                  "INFY", "TCS", "ACC",
                                  "AUROPHARMA", "HCLTECH",
                                  "WIPRO", "TECHM",
                                  "TATAMOTORS", "MARUTI",
                                  "HDFCBANK", "ICICIBANK",
                                  "COALINDIA",  "ONGC",
                                  "RELIANCE", "BHARTIARTL",
                                  "HINDUNILVR",
                                  "LT", "BHEL", "ZEEMEDIA")), ]
colnames(comp.select) <- c("company", "symbol")
isin.dat <- isin.dat[which(isin.dat$SYMBOL %in%
                           unique(comp.select$symbol)), ]
colnames(isin.dat) <- tolower(colnames(isin.dat))
comp.select <- merge(comp.select, isins, by = "symbol", all.x = TRUE)
isins <- isin.dat[, c("symbol", "isin"), with = FALSE]
comp.select <- comp.select[!duplicated(comp.select$symbol), ]
comp.select$qty <- rep(1, nrow(comp.select))
colnames(isin.dat) <- toupper(colnames(isin.dat))
returns <- prepareReturnSeries(isin.dat,
                               ret.dt = unique(holdings$date)
                               )


market.ret <- market.ret[, c("date", "log_returns")]
colnames(market.ret) <- c("date", "market_returns")

all.ret <- merge(returns, market.ret, by = "date", all.x = TRUE)

returns <- all.ret
returns$market_returns <- NULL
market.ret <- all.ret[, c("date", "market_returns"), with = FALSE]

get.ud.eq <- getDummyPortfolioReturns(holdings,
                                      returns,
                                      market.ret)


holdings.all <- merge(holdings, isins, by = "isin", all.x = TRUE)
holdings.all$client_isin <- paste0(holdings.all$client_id, holdings.all$isin)
holdings.all <- holdings.all[!duplicated(holdings.all$client_isin), ]
holdings.all <- holdings.all[order(holdings.all$client_id, holdings.all$isin), ]
holdings.all$qty <- NULL
holdings.all$client_isin <- NULL
holding.all <- holdings.all[, c("client_id", "date", "symbol", "isin")]

###########################################################################
## cnx <- read.csv("nifty_list.csv",                                     ##
##                 header = TRUE,                                        ##
##                 stringsAsFactor = FALSE)                              ##
## colnames(cnx) <- c("company", "industry", "symbol", "series", "isin") ##
## isin.all <- cnx$isin                                                  ##
## symbol.all <- cnx$symbol                                              ##
## nifty.cal <- getMarketReturns(st.dt = as.Date("2015-05-01"),          ##
##                               en.dt = as.Date("2015-05-31"),          ##
##                               nifty = symbol.all,                     ##
##                               mode = "nifty",                         ##
##                               roll.per = 0,                           ##
##                               mcap = mcap.dat,                        ##
##                               close = isin.dat)                       ##
##                                                                       ##
##                                                                       ##
## nifty.index <- read.csv("nifty_prices.csv",                           ##
##                      header = TRUE,                                   ##
##                      stringsAsFactor = FALSE)                         ##
## colnames(nifty.index) <- c("date",                                    ##
##                            "high",                                    ##
##                            "low",                                     ##
##                            "close",                                   ##
##                            "shares_traded",                           ##
##                            "turnover")                                ##
## nifty.index$symbol <- "nifty"                                         ##
## nifty.index$isin   <- "nifty"                                         ##
## nifty.index$date   <- as.Date(nifty.index$date,                       ##
##                               format = "%d-%b-%Y")                    ##
## nifty.index        <- nifty.index[,c("symbol",                        ##
##                                      "close",                         ##
##                                      "isin",                          ##
##                                      "date")]                         ##
##                                                                       ##
## index.dat    <- nifty.index[which(nifty.index$date %in%               ##
##                                 unique(nifty.cal$date)), ]            ##
## nifty.returns <- stockSimpleReturns(index.dat,                        ##
##                                     roll.per = 0,                     ##
##                                     log.ret = TRUE)                   ##
##                                                                       ##
##                                                                       ##
## nifty.dat <- merge(nifty.cal, nifty.returns, by = "date",             ##
##                    all.x = TRUE)                                      ##
## market.ret <- nifty.dat$market_returns[-1]                            ##
## portfolio.ret <- nifty.dat$log_returns[-1]                            ##
###########################################################################


ud <- calUnderdiversification(market.ret = all.ret$market_returns,
                              portfolio.ret = all.ret$equal_returns)



