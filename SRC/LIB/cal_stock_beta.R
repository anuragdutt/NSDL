##' Calculates stock beta on a given rolling window
##'
##' Takes in market returns and stock returns and calculates the stock
##'     beta for a given rolling window
##' @title calStockBeta 
##' @param market.ret Market returns for a given series of dates
##' @param stock.ret Stock returns for a given series of dates
##' @return stock beta for the given series of dates
##' @author Anurag Dutt


########################################################################################
### options(scipen = 999, digits = 7)                                                ###
### library(data.table)                                                              ###
### load("/home/nsdl/CONSTANTS/close_price_all_stocks.Rdata")                        ###
### close <- isin.dat                                                                ###
### source("/home/nsdl/LIB/read_holdings_file.R")                                    ###
### holdings <-                                                                      ###
###     readFile("/home/nsdl/DATAMART1/20150502/IGIDR_POSITION_02052015.CSV.gz",  ## ###
###              only.nse = TRUE)                                                    ###
### source("/home/nsdl/LIB/create_market_portfolio.R")                               ###
### load("/home/nsdl/CONSTANTS/mcap_nse_universe.Rdata")                             ###
### market.ret <- getMarketReturns(st.dt = min(dt.req) - 1,                          ###
###                                en.dt = max(dt.req),                              ###
###                                mcap = mcap.dat,                                  ###
###                                close = isin.dat)                                 ###
### ret.dt <- as.Date(unique(holdings$date))                                         ###
########################################################################################
                               

applyFunc <- function(expr){
    bquote(getSb(holdings[.(eval(expr)), ]))
}

calStockBeta <- function(market.ret, stock.ret) {
    mg <- lm(stock.ret ~ market.ret)
    stock.beta <- summary(mg)$coefficients[2, 4]
    return(stock.beta)
}

prepareBetaSeries <- function(close,
                              roll.sb = 250,
                              ret.dt,
                              market.ret,
                              roll.per = 0,
                              on = "simple") {
    source("/home/nsdl/LIB/stock_returns.R")
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
    inx.req <- (dt.inx - roll.sb):dt.inx
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


    get.beta <- lapply(i.in,
                       function(inx, market.ret) {
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
                           
                           stk.beta <-
                               calStockBeta(market.ret$market_returns,
                                            retval$simple_returns)
                           
                           beta     <- c(tail(dat$isin, 1), stk.beta) 
                           return(beta)
                           
                       },
                       market.ret = market.ret
                       )
    
    
    beta <- data.table(do.call('rbind', get.beta))
    colnames(beta) <- c("isin", "beta")
    
    return(returns)
    
}

getPortfolioBeta <- function(holdings,
                             beta,
                             market.ret) {
    library(data.table)
    holdings <- data.table(holdings)
    holdings <- merge(holdings, beta,
                      by = "isin",
                      all.x = TRUE)
    holdings             <- holdings[!is.na(holdings$beta), ]
    holdings$beta        <- as.numeric(holdings$beta) 
    holdings$weighted    <- holdings$beta*holdings$qty
    holdings             <- holdings[, c("dp_id",
                                         "client_id",
                                         "date",
                                         "isin",
                                         "qty",
                                         "beta",
                                         "weighted"
                                         ), with = FALSE] 
    
    pb.weighted <- aggregate(weighted ~ client_id,
                             data = holdings,
                             FUN = sum)
    pb.weights  <- aggregate(qty ~ client_id,
                             data = holdings,
                             FUN = sum)

    pb          <- merge(pb.weighted, pb.weights,
                         by = "client_id",
                         all.x = TRUE)

    pb          <- data.table(pb[!is.na(pb$qty), ]) 

    pb$portfolio_beta <- pb$weighted/pb$qty

    pb <- pb[,
             c("client_id", "portfolio_beta"),
             with = FALSE]

    return(pb)
}

