## Progressive levels of state ban
rm(list=ls())
## setwd("../.")
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


time.nw     <- paste0(unlist(strsplit(as.character(Sys.time()), " ")), collapse="_")
log.file    <- paste0("nestle-holdings-chk_log-", time.nw, ".log")
path.log    <- "/home/nsdl/LOGS/DEMO_SUMSTATS"
createLogs(path.log, log.file)

save.path <- "/home/nsdl/RESULTS/DEMO_SUMSTATS"
client.path <- "/home/nsdl/DATAMART/CLIENT_MASTER_FullDump_06102015.CSV"
# isin <- "INE239A01016"




################################################################
## dates <- c(as.Date("2014-10-01"), as.Date("2014-10-02"),   ##
##            as.Date("2014-10-03"), as.Date("2014-10-04"),   ##
##            as.Date("2014-09-15"), as.Date("2014-09-16"))   ##
#################################################################



dates <- seq(from=as.Date("2015-05-01", format="%Y-%m-%d"),
             to=as.Date("2015-05-10", format="%Y-%m-%d"), by="day")

msg         <- "Searching the requisite files in DATAMART"
status      <- toupper("update")
append.mode <- TRUE
appendLogs(msg, path.log, log.file, append.mode, status) 


f.path <- retrieveFile(dates, "holdings")

 
getAllHoldings <- function(f.path, save.path) {
    cli.data <- readClientBeforeOct2010(client.path)
    total.clients <- length(unique(cli.data$client_id))
    save(total.clients, file = paste0(save.path, "/",
                                      "total_accounts.Rdata"))
    total.dps <- length(unique(cli.data$dp_id))
    save(total.dps, file = paste0(save.path, "/",
                                      "total_depositories.Rdata"))
    total.pans <- length(unique(cli.data$pan_1))
    save(total.pans, file = paste0(save.path, "/",
                                   "total_pan_ids.Rdata"))

    occ.breakup <- tapply(cli.data$client_id, cli.data$occupation,
                          length)
    save(occ.breakup, file = paste0(save.path, "/",
                                      "occ_breakup.Rdata"))

    acc.category <- tapply(cli.data$client_id, cli.data$ac_category,
                          length)
    save(occ.breakup, file = paste0(save.path, "/",
                                      "acc_category.Rdata"))

    dis.breakup <- tapply(cli.data$client_id, cli.data$district_code,
                          length)
    save(occ.breakup, file = paste0(save.path, "/",
                                    "dis_breakup.Rdata"))

    type.breakup <- tapply(cli.data$client_id, cli.data$client_type,
                          length)
    save(occ.breakup, file = paste0(save.path, "/",
                                      "occ_breakup.Rdata"))
    cli.data$dp_id <- NULL
    tmp   <- NULL
    lapply(f.path, function(path) {
        client.data <- readFile(path)
        dt <- as.Date(unlist(strsplit(path, "/"))[5], format = "%Y%m%d")        
        age.data   <- ageAccount(cli.data, dt)
        get.dist   <- isinTrade(client.data)
        share.calc <- merge(client.data, get.dist, by = "client_id",
                            all.x = TRUE)
        per.data   <- niftysharesPortfolio(share.calc, "all")
        n.shares   <- portfolioIsinCount(client.data)
        
        
        derieved   <- merge(age.data, get.dist, by = "client_id",
                            all.x = TRUE)
        derieved   <- merge(derieved, per.data, by = "client_id",
                            all.x = TRUE)
        derieved   <- merge(derieved, n.shares, by = "client_id",
                            all.x = TRUE)
        
        res.cli    <- merge(client.data, cli.data, by = "client_id",
                            all.x = TRUE)
        result     <- merge(res.cli, derieved, by = "client_id")
        path.to.save <- paste0(save.path, "/",
                               "Client_holdings_",
                               dt, ".Rdata")
         save(result, file = path.to.save)
    }
    )
}

getAllHoldings(f.path, save.path)
