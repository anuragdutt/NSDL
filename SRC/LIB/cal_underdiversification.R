##' Calculate and aggregate underdiversification according to given
##' rules 
##'
##' Takes in market returns and portfolio returns of a given
##'     timeseries as input
##' @title calUnderdiversification
##' @param market.ret time series of given market returns
##' @param portfolio.ret time series of portfolio returns
##' @return underdiversification values of given time series period
##' @author Anurag Dutt




## options(scipen = 999, digits = 7)                                                 
## library(data.table)                                                               
## load("/home/nsdl/CONSTANTS/close_price_all_stocks.Rdata")                         
## close <- isin.dat                                                                 
## source("/home/nsdl/LIB/read_holdings_file.R")                                     
## load("/home/nsdl/DATAMART1/client_master_non_aggregated.Rdata")                      
## cli.data <- data.table(cli.data)                                                     
## pan.data <- cli.data[, c("client_id", "pan_1"),                                      
##                      with = FALSE]                                                   
## holdings <-                                                                       
##     readFile("/home/nsdl/DATAMART1/20150831/IGIDR_POSITION_31082015.CSV.gz",      
##              only.nse = TRUE,                                                        
##              aggregate = TRUE,                                                       
##              pan.data = pan.data)                                                 
## ret.dt <- as.Date(unique(holdings$date))                                             


## load("demo_holdings.Rdata")
## load("demo_market_returns.Rdata")
## library(parallel)

applyFunc <- function(expr){
    bquote(getUd(holdings[.(eval(expr)), ]))
    
}



calUnderdiversification <- function(market.ret,
                                    portfolio.ret,
                                    r.squared = TRUE) {
    if(r.squared == FALSE) {
        mg <- lm(portfolio.ret ~ market.ret)
        ##  mg <- lm(ret.series ~ market.ret$market_returns)
        ##  varh <- var(market.ret$market_returns)
        varh <- var(market.ret)
        vare <- var(resid(mg))
        beta <- summary(mg)$coefficients[2,1]
        ud <- round(vare/((beta^2)*varh + vare), 2)*100
        
    } else {
        mg <- lm(portfolio.ret ~ market.ret)
        ## mg <- lm(ret.series ~ market.ret$market_returns)
        beta <- summary(mg)$coefficients[2,1]
        r.squared <- summary(mg)$r.squared
        div <- r.squared
        ud <- (1 - div)*100
        
    }
    return(c(ud, beta))
    
}

getDateSeries <- function(dt.unique,
                          roll.ud = 250,
                          ret.dt,
                          close,
                          roll.per = 0,
                          ret.dat = NULL) {
    ## source("/home/nsdl/LIB/stock_returns.R")
    ret.dt    <- as.Date(ret.dt)
    dt.unique <- as.Date(dt.unique)
    close <- data.frame(close)
    close <- close[which(close$date <= ret.dt), ]
    close <- close[order(close[["symbol"]],
                         close[["date"]]), ]
    colnames(close) <- tolower(colnames(close))

    if(is.null(ret.dat)) {
        ret.dat   <- stockSimpleReturns(close, roll.per)
    }
    for(i in c(1:10)) {
        if(length(which(ret.dt %in% unique(ret.dat$date))) == 0) {
            ret.dt = ret.dt - 1
            
        }
    }

    dt.inx  <- which(dt.unique == ret.dt)
    inx.req <- (dt.inx - roll.ud):dt.inx
    dt.req  <- dt.unique[inx.req]
    return(dt.req)

}

prepareReturnSeries <- function(close,
                                roll.ud = 250,
                                ret.dt,
                                roll.per = 0,
                                on = "simple",
                                cores = 8,
                                ret.dat = NULL) {
#    source("/home/nsdl/LIB/stock_returns.R")
#    library(data.table)
#    library(zoo)
    isin.dat <- close
    colnames(isin.dat) <- tolower(colnames(isin.dat))
    ret.dt   <- as.Date(ret.dt)
    isin.dat <- isin.dat[which(isin.dat$date <= ret.dt), ]
    isin.dat <- isin.dat[order(isin.dat[["symbol"]],
                               isin.dat[["date"]]), ]
    dt.unique <- sort(unique(isin.dat$date))
    if(is.null(ret.dat)) {
        ret.dat   <- stockSimpleReturns(isin.dat, roll.per)
    }
    
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


    get.returns <- mclapply(i.in,
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
                              
                          },
                          mc.cores = cores)
    

    returns <- Reduce(merge, get.returns)
   
    
    return(returns)
    
}



## source("/home/nsdl/LIB/create_market_portfolio.R")                                
## load("/home/nsdl/CONSTANTS/mcap_nse_universe.Rdata")                              
## market.ret <- getMarketReturns(st.dt = min(dt.req) - 1,                           
##                                en.dt = max(dt.req),                               
##                                mcap = mcap.dat,                                   
##                                close = isin.dat)                                  


getUdBeta <- function(holdings,
                                     returns,
                                     market.ret) {
##    library(data.table)
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




getUdBetaParallelised <- function(holdings,                          
                                  returns,                           
                                  market.ret,
                                  save.do = FALSE,
                                  ip.master = "10.150.4.68",
                                  ip.slave =
                                      c("10.150.4.69",
                                        "10.150.4.70"),
                                  cores = 7,
                                  rshcmd = "ssh",
                                  user = "nsdl",
                                  save.path = NULL) {                      
    library(data.table)                                                             
    library(parallel)
        
    if(!is.null("save.path") && save.do == TRUE) {
        save.path <- "/home/nsdl/RESULTS/ud_beta.Rdata"
    }

    holdings <- data.table(holdings)                                                
                                                                                    
    c.st <-                                                                         
        seq_along(holdings$client_id)[!duplicated(holdings$client_id)]              
    c.en <- c((c.st[-1] - 1), length(holdings$client_id))                           
    c.in <- paste0(sep = "c(", c.st, sep = ":", c.en, sep = ")")                    
                                                                                    
                                                                                    
    getUd <- function(inx) {                                                        
        library(data.table)                                                         
        c.dat       <- holdings[eval(parse(text = inx)), ]                          
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

    hosts <- c(rep(paste0(user, "@", ip.master), each = cores),
               as.character(sapply(ip.slave, function(ip) {
                   return(rep(paste0(user, "@", ip),
                              each = cores))
               }))
               )

    
    my.cluster <- makePSOCKcluster(hosts,
                                   master = ip.master,
                                   rshcmd = rshcmd,
                                   outfile = "",
                                   homogenous = TRUE
                                   )

    clusterExport(cl=my.cluster,                                                    
                  varlist = c("c.in",                                               
                              "holdings",                                           
                              "returns",                                            
                              "market.ret",                                         
                              "calUnderdiversification"),
                  envir = environment()
                  )                                                                 
                                                                                    
    ud.list <- parLapplyLB(cl = my.cluster,                                         
                           X = c.in,                                                
                           fun = getUd                                              
                           )                                                        
                                                                                    
    stopCluster(my.cluster)                                                         
                                                                                    
    ud.holdings <- do.call('rbind', ud.list)
    colnames(ud.holdings) <- c("client_id",
                               "underdiversification",
                               "beta")
    ud.holdings <- data.frame(ud.holdings,
                              stringsAsFactors = FALSE)
    ud.holdings$underdiversification <-
        as.numeric(ud.holdings$underdiversification)

    ud.holdings$beta <- as.numeric(ud.holdings$beta)
         
   if(save.do == TRUE) {
       save(ud.holdings,
            file = save.path)
   }

    return(ud.holdings)                                                             
}                                                                                   



getUdBetaParallelisedMc <- function(holdings,                          
                                    returns,                           
                                    market.ret,
                                    save.do = FALSE,
                                    cores = 7,
                                    save.path = NULL) {                      

    library(data.table)                                                             
    library(parallel)
        
    if(!is.null("save.path") && save.do == TRUE) {
        save.path <- "/home/nsdl/RESULTS/ud_beta.Rdata"
    }

    holdings <- data.table(holdings)                                                
                                                                                    
    c.st <-                                                                         
        seq_along(holdings$client_id)[!duplicated(holdings$client_id)]              
    c.en <- c((c.st[-1] - 1), length(holdings$client_id))                           
    c.in <- paste0(sep = "c(", c.st, sep = ":", c.en, sep = ")")                    
                                                                                    
                                                                                    
    getUd <- function(inx) {                                                        
        ##  library(data.table)
        ## print(inx)
        c.dat       <- holdings[eval(parse(text = inx)), ]                          
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
        retval        <- data.table(client_id =
                                        unique(c.dat$client_id),
                                    underdiversification = ud[1],
                                    beta = ud[2])                             
        return(retval)                                                              
    }                                                                               

                                                                                    
    ud.list <- mclapply(c.in,
                      function(inx) getUd(inx),
                      mc.cores = cores
                      )                                                        
                                                                                    
                                                                                   
    ud.holdings <- rbindlist(ud.list)
    colnames(ud.holdings) <- c("client_id",
                               "underdiversification",
                               "beta")
    ud.holdings <- data.frame(ud.holdings,
                              stringsAsFactors = FALSE)
    ud.holdings$underdiversification <-
        as.numeric(ud.holdings$underdiversification)

    ud.holdings$beta <- as.numeric(ud.holdings$beta)
         
   if(save.do == TRUE) {
       save(ud.holdings,
            file = save.path)
   }

    return(ud.holdings)                                                             
}                                                                                   

## port.dat <- getDummyPortfolioReturnsParallelised(holdings = holdings,
##                                                  close = isin.dat)
