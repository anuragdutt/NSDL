## options(scipen = 999, digits = 10)
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

    ## get the list for stocks for which returns are to be calculated
    
    all.stocks <- unique(price.data$SYMBOL)
    all.ret <- do.call('rbind',lapply(all.stocks, function(stk) {
#        print(stk)
        isin.ind <- subset(price.data, SYMBOL == stk)

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



