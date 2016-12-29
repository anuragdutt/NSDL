## Sourcing LIB files
rm(list=ls())

source("/home/nsdl/LIB/isin_trades.R")
source("/home/nsdl/LIB/retrieve_file.R")
source("/home/nsdl/LIB/read_holdings_file.R")
source("/home/nsdl/LIB/read_client.R")
source("/home/nsdl/LIB/write_to_log.R")
source("/home/nsdl/LIB/account_age.R")
source("/home/nsdl/LIB/change_in_holdings.R")
source("/home/nsdl/LIB/nifty_percentage.R")
source("/home/nsdl/LIB/portfolio_share.R")

library("parallel")
library("data.table")

                                        # creating log files and setting global variables
time.nw     <- paste0(unlist(strsplit(as.character(Sys.time()), " ")), collapse="_")
log.file    <- paste0("nestle-holdings-chk_log-", time.nw, ".log")
path.log    <- "/home/nsdl/LOGS/NESTLE_HOLDINGS"
createLogs(path.log, log.file)

save.path <- "/home/nsdl/RESULTS/NESTLE_HOLDINGS"
client.path <- "/home/nsdl/DATAMART/CLIENT_MASTER_FullDump_06102015.CSV"
isin <- "INE239A01016"




################################################################
## dates <- c(as.Date("2014-10-01"), as.Date("2014-10-02"),   ##
##            as.Date("2014-10-03"), as.Date("2014-10-04"),   ##
##            as.Date("2014-09-15"), as.Date("2014-09-16"))   ##
#################################################################



dates <- seq(from=as.Date("2015-05-01", format="%Y-%m-%d"),
             to=as.Date("2015-07-31", format="%Y-%m-%d"), by="day")

msg         <- "Searching the requisite files in DATAMART"
status      <- toupper("update")
append.mode <- TRUE
appendLogs(msg, path.log, log.file, append.mode, status) 


f.path <- retrieveFile(dates, "holdings")
                                        # get the path of all the holdings file to be read
getISINHoldings <- function(f.path, isin) {

    all.client <- NULL
    mclapply(f.path, function(path) {
        h.data <- readFile(path)
        client <- unique(h.data$client_id[which(h.data$isin == isin)])

        msg         <-
            paste0("Aggregating clients with given isin for date: ",
                   unique(h.data$date)[1]
                   )
        status      <- toupper("update")
        append.mode <- TRUE
        appendLogs(msg, path.log, log.file, append.mode, status) 
        
        h.data <- h.data[which(h.data$client_id%in% client), ]
        all.client <<- c(all.client, unique(h.data$client)) 
        
    }, mc.cores = 1
    )

return(all.client)
}
                                        # log the process and dates of all the holdings files to be read
msg         <- "Finding out all the clients with holdings for given isin"
status      <- toupper("update")
append.mode <- TRUE
appendLogs(msg, path.log, log.file, append.mode, status) 
client.all <-  getISINHoldings(f.path, isin)
client.all <- unique(client.all)
 
getAllHoldings <- function(f.path, save.path, client, isin) {
                                        # Read the client file
    cli.data <- readClientBeforeOct2010(client.path)
    cli.data$dp_id <- NULL
    count <- 0
    tmp   <- NULL
    lapply(f.path, function(path) {
                                        # Read the holdings file
        h.data <- readFile(path) 
        client.data <- h.data[which(h.data$client_id %in%
                                    client), ]
        msg         <-
            paste0("Aggregating holdings for all clients in the given isin: ",
                   unique(client.data$date)[1]
                   )
        count.data <- countIsin(client.data, isin)
                                        # Calculate the difference between counts of subsequent dates
        if(count == 0) {
            tmp <<- count.data
            colnames(count.data) <- c("client_id", "recent_count")
#            colnames(tmp) <- c("client_id", "prev_count")
            colnames(tmp) <<- c("client_id", "prev_count")
            diff.data <- cbind(count.data$client_id, NA)
        } else {
            colnames(count.data) <- c("client_id", "recent_count")
            change.data   <- merge(count.data, tmp, by = "client_id")
            change.data   <- data.frame(change.data)
            change.data$diff_count <- change.data$recent_count - change.data$prev_count
            diff.data     <- change.data[, c("client_id",
                                             "diff_count")]
            colnames(count.data) <- c("client_id", "prev_count")
            tmp <<- count.data
            
        }
        head(diff.data)
        colnames(diff.data) <- c("client_id", "difference")
                                        # Get age data and nifty and
                                        # nifty junior share
        dt <- as.Date(unlist(strsplit(path, "/"))[5], format = "%Y%m%d")        
        age.data   <- ageAccount(cli.data, dt)
        get.dist   <- isinTrade(client.data)
        share.calc <- merge(client.data, get.dist, by = "client_id",
                            all.x = TRUE)
        per.data   <- niftysharesPortfolio(share.calc, "all")
        n.shares   <- portfolioIsinCount(client.data)
        
        count <<- count + 1
                                        # Merge individual datasets
                                        # on CLIENT IDs
        derieved   <- merge(diff.data, age.data, by = "client_id",
                            all.x = TRUE)
        derieved   <- merge(derieved, get.dist, by = "client_id",
                            all.x = TRUE)
        derieved   <- merge(derieved, per.data, by = "client_id",
                            all.x = TRUE)
        derieved   <- merge(derieved, n.shares, by = "client_id",
                            all.x = TRUE)
        head(derieved)
        
        res.cli    <- merge(client.data, cli.data, by = "client_id",
                            all.x = TRUE)
        result     <- merge(res.cli, derieved, by = "client_id")
        path.to.save <- paste0(save.path, "/",
                               "nestle_client_holdings_",
                               dt, ".Rdata")
        head(result)
        save(result, file = path.to.save)
    }
    )
}

getAllHoldings(f.path, save.path, client.all, isin)
