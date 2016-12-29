## library(data.table)                                                                 
## load("/home/nsdl/CONSTANTS/close_price_all_stocks.Rdata")                           
## close <- isin.dat                                                                   
## source("/home/nsdl/LIB/read_holdings_file.R")
## load("/home/nsdl/DATAMART1/client_master_non_aggregated.Rdata")
## cli.data <- data.table(cli.data)
## pan.data <- cli.data[, c("client_id", "pan_1"),
##                      with = FALSE]
## load("/home/nsdl/DATAMART1/holdings_aggregated.Rdata")
## holdings <-
##     readFile("/home/nsdl/DATAMART1/20150831/IGIDR_POSITION_31082015.CSV.gz",
##              only.nse = TRUE,
##              aggregate = TRUE,
##              pan.data = pan.data)
## load("/home/nsdl/DATAMART1/client_master_aggregated.Rdata")

##' Returns the portfolio returns for a day's holdings file
##'
##' returns calculated on a given day's holdings
##' @title getPortfolioReturns
##' @param holdings holdings file for a given date
##' @param roll.per rollling window period if any
##' @param on returns to be simple or rolling window
##' @param wt Should the returns be value weighted or quantity weighted 
##' @param close close price data for all stocks
##' @return dataframe of portfolio value and portfolio returns for all households
##' @author Anurag Dutt
getPortfolioReturns <- function(holdings,
                                roll.per = 0,
                                on = "simple",
                                wt = "qty",
                                close,
                                ret.dat = NULL) {
    
#    source("/home/nsdl/LIB/stock_returns.R")
    isin.dat <- close
    colnames(isin.dat) <- tolower(colnames(isin.dat))
    holdings <- data.table(holdings)
    holdings$date <- as.Date(holdings$date)
    ret.dt   <- as.Date(unique(holdings$date))
    ret.dt   <- ret.dt + 1
    isin.dat <- isin.dat[isin.dat$date <= ret.dt, ]
    isin.dat <- isin.dat[order(isin.dat[["symbol"]],
                               isin.dat[["date"]]), ]

    if(is.null(ret.dat)) {
        ret.dat <- stockSimpleReturns(isin.dat, roll.per)
        ## save(ret.dat, file = "/home/nsdl/CONSTANTS/returns_data.Rdata")
    } else {
        ret.dat <- ret.dat
    }
    
    for(i in c(1:10)) {
        if(length(which(ret.dt %in% unique(ret.dat$date))) == 0) {
            ret.dt = ret.dt - 1
        }
    }

    ret.dat <- ret.dat[ret.dat$date == ret.dt, ]
    if(on == "simple") {
        ret.dat <- ret.dat[!is.na(ret.dat$log_returns), ]
                 
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
    c.en <- c((c.st-1)[-1], length(return.portfolio$client_id))
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
            retval <- data.table(unique(dat$client_id),
                                 wm,
                                 sum(dat$val),
                                 ret.dt)
            return(retval)
        })

    portfolio.ret <- rbindlist(portfolio.ret)
    colnames(portfolio.ret) <- tolower(c("CLIENT_ID",
                                         "PORTFOLIO_RETURNS",
                                         "VAL",
                                         "DATE"))
    

    return(portfolio.ret)

}



getPortfolioReturnsParallelised <- function(holdings,
                                            roll.per = 0,
                                            on = "simple",
                                            wt = "qty",
                                            close,
                                            save.do = FALSE,
                                            ret.dat = NULL,
                                            ip.master = "10.150.4.68",
                                            ip.slave =
                                                c("10.150.4.69",
                                                  "10.150.4.70"),
                                            cores = 7,
                                            rshcmd = "ssh",
                                            user = "nsdl",
                                            save.path = NULL) {
    ## library(parallel)
    ## source("/home/nsdl/LIB/stock_returns.R")
    
    
    if(!is.null("save.path") && save.do == TRUE) {
        save.path <- "/home/nsdl/RESULTS/portfolio_returns.Rdata"
    }

    holdings      <- data.table(holdings)
    holdings$date <- as.Date(holdings$date)
    ret.dt        <- as.Date(unique(holdings$date))
    ret.dt        <- ret.dt + 1
    isin.dat      <- close
    colnames(isin.dat) <- tolower(colnames(isin.dat))
    isin.dat <- isin.dat[which(isin.dat$date <= ret.dt), ]
    isin.dat <- isin.dat[order(isin.dat[["symbol"]],
                               isin.dat[["date"]]), ]

    
    if(is.null(ret.dat)) {
        ret.dat <- stockSimpleReturns(isin.dat, roll.per)
        ## save(ret.dat, file = "/home/nsdl/CONSTANTS/returns_data.Rdata")
    } else {
        ret.dat <- ret.dat
    }

    for(i in c(1:10)) {
        if(length(which(ret.dt %in% unique(ret.dat$date))) == 0) {
            ret.dt = ret.dt - 1
        }
    }

    ret.dat <- ret.dat[ret.dat$date == ret.dt, ]
    if(on == "simple") {
        ret.dat <- ret.dat[!is.na(ret.dat$log_returns), ]
                 
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
    c.en <- c((c.st-1)[-1], length(return.portfolio$client_id))
    c.in <- paste0(sep = "c(", c.st,
                   sep = ":",
                   c.en,
                   sep = ")")
    
    getRet <- function(inx,
                       wt){
        library(data.table)
        ## return.portfolio <- tmp
        dat <- return.portfolio[eval(parse(text=inx)), ]
        if(wt == "qty") {
            wm <- weighted.mean(dat$log_returns, dat$qty)
        } else {
            wm <- weighted.mean(dat$log_returns, dat$val)
        }
        retval <- data.table(unique(dat$client_id), wm, sum(dat$val))
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
                              ##                              "tmp",
                              "return.portfolio",
                              "wt"),
                              envir = environment()
                  )
    
    
    portfolio.ret <- parLapplyLB(cl = my.cluster,
                                 X = c.in,
                                 fun = getRet,
                                 wt = wt
                                 )

    stopCluster(my.cluster)
    
    portfolio.ret <- rbindlist(portfolio.ret)
    colnames(portfolio.ret) <- tolower(c("CLIENT_ID",
                                         "PORTFOLIO_RETURNS",
                                         "VAL"))

   if(save.do == TRUE) {
       save(portfolio.ret,
            file = save.path)
   }
    
    return(portfolio.ret)


}



getPortfolioReturnsMc <- function(holdings,
                                  roll.per = 0,
                                  on = "simple",
                                  wt = "qty",
                                  close,
                                  save.do = FALSE,
                                  ret.dat = NULL,
                                  save.path = NULL,
                                  cores = 7) {
    ## library(parallel)
    ## source("/home/nsdl/LIB/stock_returns.R")
    
    
    if(!is.null("save.path") && save.do == TRUE) {
        save.path <- "/home/nsdl/RESULTS/portfolio_returns.Rdata"
    }

    holdings      <- data.table(holdings)
    holdings$date <- as.Date(holdings$date)
    ret.dt        <- as.Date(unique(holdings$date))
    ret.dt        <- ret.dt + 1
    isin.dat      <- close
    colnames(isin.dat) <- tolower(colnames(isin.dat))
    isin.dat <- isin.dat[which(isin.dat$date <= ret.dt), ]
    isin.dat <- isin.dat[order(isin.dat[["symbol"]],
                               isin.dat[["date"]]), ]

    
    if(is.null(ret.dat)) {
        ret.dat <- stockSimpleReturns(isin.dat, roll.per)
        ## save(ret.dat, file = "/home/nsdl/CONSTANTS/returns_data.Rdata")
    } else {
        ret.dat <- ret.dat
    }

    for(i in c(1:10)) {
        if(length(which(ret.dt %in% unique(ret.dat$date))) == 0) {
            ret.dt = ret.dt - 1
        }
    }

    ret.dat <- ret.dat[ret.dat$date == ret.dt, ]
    if(on == "simple") {
        ret.dat <- ret.dat[!is.na(ret.dat$log_returns), ]
                 
    } else {
        ret.dat <- ret.dat[!is.na(ret.dat$rolling_returns), ]
                     
    }
    ret.dat$date <- as.Date(unique(holdings$date))

    return.portfolio <- merge(holdings, ret.dat,
                              by = c("date", "isin"),
                              all.x = TRUE)
    return.portfolio <- return.portfolio[!is.na(return.portfolio$log_returns), ]
    return.portfolio <-
        return.portfolio[order(return.portfolio[["client_id"]],
                               return.portfolio[["isin"]])]

    return.portfolio$val <-
        return.portfolio$qty*return.portfolio$price

    c.st <-
        seq_along(return.portfolio$client_id)[!duplicated(return.portfolio$client_id)]
    c.en <- c((c.st-1)[-1], length(return.portfolio$client_id))
    c.in <- paste0(sep = "c(", c.st,
                   sep = ":",
                   c.en,
                   sep = ")")
##    count <- 0
    getRet <- function(inx,
                       wt){
        library(data.table)
##        count <<- count + 1
##        print(count)
        ## return.portfolio <- tmp
        dat <- return.portfolio[eval(parse(text=inx)), ]
        if(wt == "qty") {
            wm <- weighted.mean(dat$log_returns, dat$qty)
        } else {
            wm <- weighted.mean(dat$log_returns, dat$val)
        }
        retval <- data.table(unique(dat$client_id), wm, sum(dat$val))
##         colnames(retval) <- c("client_id",
##                               "ret",
##                               "val")
        return(retval)
    }
    
    
    portfolio.ret <- mclapply(c.in, function(inx, wt) getRet(inx, wt),
                              wt = wt,
                              mc.cores = cores
                              )
    
##     portfolio.ret <- lapply(c.in, function(inx, wt) getRet(inx, wt),
##                             wt = wt
##                             ##mc.cores = cores
##                             )
    
    portfolio.ret <- rbindlist(portfolio.ret)
    colnames(portfolio.ret) <- tolower(c("CLIENT_ID",
                                         "PORTFOLIO_RETURNS",
                                         "VAL"))

   if(save.do == TRUE) {
       save(portfolio.ret,
            file = save.path)
   }
    
    return(portfolio.ret)


}



## port.dat <- getPortfolioReturnsParallelised(holdings = holdings,
##                                             close = isin.dat)
## client.type <- pan.data[, c("client_id", "client_type")]
## client.port <- merge(port.dat, client.type, by = "client_id", all.x = TRUE)




