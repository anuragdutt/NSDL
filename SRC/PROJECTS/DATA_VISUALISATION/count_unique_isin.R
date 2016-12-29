## Sourcing the read and search modules
source("/home/nsdl/LIB/read_client.R")
source("/home/nsdl/LIB/read_holdings_file.R")
source("/home/nsdl/LIB/retrieve_file.R")
source("/home/nsdl/LIB/cal_portfolio_returns.R")
source("/home/nsdl/LIB/isin_trades.R")
source("/home/nsdl/LIB/stock_returns.R")

library(data.table)
library(parallel)

load("/home/nsdl/CONSTANTS/close_price_all_stocks.Rdata")

date.seq <- as.Date("2015-09-01")

##' Input a date sequence and extract the equity holdings of every
##' client for the given dates separately and save it as a .Rdata file
##'
##' The function inputs a date sequence, sources in read and search
##'     module and finds out the path of the file in the entire
##'     DATAMART. Then reads the file, counts the number of unique
##'     isins, calculates the number of nifty and nifty junior stocks
##'     in the holdings, portfolio returns and Net asset value and merges the client
##'     demographics to the above result. It then saves the file at
##'     desired location.  
##'     
##' @title getUniqueIsinCount
##' @param date.seq A vector of desired dates
##' @param save.path Desired location for saving the data
##' @return A data.frame/data.table containing :
##' \itemize{
##'  \item Number of unique isins for each account
##'  \item Net Asset Value
##'  \item Percentage of nifty and nifty junior stocks in the
##'     portfolio
##'  \item Client dempgraphics from client master
##'  \item partial: whether it was a partial execution (0 or 1)
##' }                                                                           
##' @author Anurag Dutt

getUniqueIsinCount <- function(date.seq,
                               save.path = "/home/nsdl/RESULTS",
                               client.path = "/home/nsdl/DATAMART/CLIENT_MASTER_FullDump_06102015.CSV") {

    date.seq <- as.Date(date.seq)
                                        # search for files of a given date
    path.files <- retrieveFile(date.seq, "holdings")
                                        # read client data
    cli.data   <- readClientBeforeOct2010(client.path)
    
    lapply(path.files, function(path) {
        
        dt <- as.Date(tail(unlist(strsplit(unlist(strsplit(path,
                                                           ".CSV.gz"))[1],
                                           "_"))
                         , 1), format = "%d%m%Y")
                                        # read holdings for a given date
        holdings <- readFile(path)
        nifty.dat <- isinTrade(holdings)
                                        # serialize by client ID
        c.st <-
            seq_along(holdings$client_id)[!duplicated(holdings$client_id)]
        c.en <- c((c.st-1)[-1], length(holdings$client_id))
        c.in <- paste0(sep = "c(", c.st,
                       sep = ":",
                       c.en,
                       sep = ")")
                                        # count number of isins
        isin.count <- mclapply(c.in, function(inx){
            dat    <- holdings[eval(parse(text=inx)), ]
            retval <- data.table(cbind(unique(dat$client_id), nrow(dat)))
            colnames(retval) <- c("client_id", "isin")
            return(retval)
        },
        mc.cores = 3
        )
                                        # merge with client, nifty and nifty junior data 
        isin.count <- rbindlist(isin.count)
        colnames(isin.count) <- c("client_id", "isin")
        ret.dat <- getPortfolioReturns(holdings)
        colnames(ret.dat) <- tolower(colnames(ret.dat))
        ret.dat <- merge(ret.dat, isin.count, by = "client_id",
                         all.x = TRUE)
        all.dat <- merge(ret.dat, nifty.dat,
                         by = "client_id",
                         all.x = TRUE)
                                        # save at the desired location
        save(all.dat, file =
                          paste0("/home/nsdl/RESULTS/DEMO_SUMSTATS/returns_value",
                                 dt, ".Rdata"))
        
    })
    
}


