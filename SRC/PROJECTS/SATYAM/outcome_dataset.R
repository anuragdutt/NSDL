rm(list = ls())

#############################
## loading global datasets ##
#############################
                                        # closing prices datset
load("/home/nsdl/CONSTANTS/close_price_all_stocks.Rdata")
colnames(isin.dat) <- tolower(colnames(isin.dat))
                                        # load matching dataset
load("/home/nsdl/CONSTANTS/sample_clients.Rdata")
                                        # loadings returns
load("/home/nsdl/CONSTANTS/returns_data.Rdata")
                                        # loading matched sample
load("/home/nsdl/CONSTANTS/matched_sample.Rdata")
############################
## initiate library files ##
############################
    
source("/home/nsdl/LIB/read_holdings_file.R")
source("/home/nsdl/LIB/read_client.R")
source("/home/nsdl/LIB/cal_turnover.R")
source("/home/nsdl/LIB/stock_returns.R")
source("/home/nsdl/LIB/aggregate_pan_client_master.R")
source("/home/nsdl/LIB/retrieve_file.R")
source("/home/nsdl/LIB/cal_portfolio_returns.R")
source("/home/nsdl/LIB/write_to_log.R")

options(scipen = 999, digits = 5)

#########################
## intiate R libraries ##
#########################

library(data.table)
library(zoo)
library(parallel)

###############################
## initiate global variables ##
###############################
date.event   <- as.Date("2009-01-06")
isin.satyam  <- "INE275A01028"
close.satyam <- as.numeric(isin.dat[which(isin.dat$isin == isin.satyam &
                                          isin.dat$date ==
                                          date.event),
                                    c("close"),
                                    with = FALSE])
cli.path <- "/home/nsdl/DATAMART1/CLIENT_MASTER_FullDump_06102015.CSV"
length.out <- 200
path.to.log <- "/home/nsdl/LOGS/SATYAM/OUTCOMES"
path.to.res <- "/home/nsdl/RESULTS/SATYAM/DATA"
dfile.name   <- "satyam_dataset"
st.dt <- as.Date("2008-06-10")
en.dt <- as.Date("2009-06-30")

########################
## Generic Workhorses ##
########################
time.nw     <- paste0(unlist(strsplit(as.character(Sys.time()), " ")),
                      collapse="_")
log.file <- paste0("outcome_log_",
                   time.nw,
                   ".log"
                   )
createLogs(path.to.log, log.file)

appendLogs(msg = "Calculating dates",
           path.to.log,
           log.file,
           append.mode = TRUE,
           status = "UPDATE"
           )

dates.before <- sort(as.Date(seq(from = date.event,
                                length.out = length.out,
                                by = "-1 day")))
dates.after  <- sort(as.Date(seq(from = date.event,
                                 length.out = length.out,
                                 by = "day")))
dates.all    <- c(dates.before, dates.after)

if(!is.na(st.dt) && !is.na(en.dt)) {
    dates.all <- seq(from = as.Date(st.dt),
                     to = as.Date(en.dt),
                     by = "day")
}

## holdings <- readFile(path.all[1],
##                      only.nse = TRUE,
##                      aggregate = TRUE,
##                      pan.data = pan.data)
## client.match <- unique(holdings$client_id)


#lapply(dates.all, function(dt){

getOutcomes <- function(dt,
                        match.cl,
                        isin.dat,
                        ret.dat) {

    file.path <- paste0(path.to.res,
                        "/",
                        dfile.name,
                        "_",
                        dt,
                        ".Rdata")


    if(dt != dates.all[1]) {
    dt.prev <- dt - 1
    file.prev <- NULL
    for(i in 1:20) {
        file.chk <- paste0(path.to.res,
                           "/",
                           dfile.name,
                           "_",
                           dt.prev,
                           ".Rdata")

        if(file.exists(file.chk)) {
            file.prev <- file.chk
        } else {
            dt.prev <- dt.prev - i
        }
    }
    }
    library("parallel")
    library("data.table")
    library("zoo")

    source("/home/nsdl/LIB/cal_turnover.R")
    source("/home/nsdl/LIB/cal_portfolio_returns.R")

    ncores <- detectCores()
    ncores <- ncores - 1
    if(ncores > 6) {
        ncores <- 6
    }
    if(file.exists(file.path)) {
        
        
        if(dt == dates.all[1]) {


            turnover <- data.frame(client_id = match.cl,
                                   net_turnover = NA,
                                   gross_turnover = NA)
            ret.data <- data.frame(client_id = match.cl,
                                   portfolio_returns = NA,
                                   val = NA)
            
        } else {
            
            
##             appendLogs(msg = "Getting portfolio value and returns",
##                        path.to.log,
##                        log.file,
##                        append.mode = TRUE,
##                        status = "UPDATE"
            ##                        )
            load(file.prev)
            holdings.prev <- holdings
            load(file.path)
            holdings.prev <- holdings.prev[which(holdings.prev$client_id %in% match.cl), ]
            holdings      <- holdings[which(holdings$client_id %in% match.cl), ]
            
            
            ret.data <- getPortfolioReturnsMc(holdings.prev,
                                              roll.per = 0,
                                              on = "simple",
                                              wt = "qty",
                                              close = isin.dat,
                                              save.do = FALSE,
                                              ret.dat = ret.dat,
                                          cores = ncores
                                          )
    
        
##             appendLogs(msg = "Getting net and gross turnover",
##                        path.to.log,
##                        log.file,
##                        append.mode = TRUE,
##                        status = "UPDATE"
##                        )
            
            turnover <- getTurnover(holdings,
                                    holdings.prev = holdings.prev,
                                    close = isin.dat
                                    )
            
            turnover <- turnover[!is.na(turnover$net_turnover), ]
            turnover <- turnover[!is.na(turnover$gross_turnover), ]
        }
        
        
##         appendLogs(msg = "Merging turnover and returns by client id",
##                    path.to.log,
##                    log.file,
##                    append.mode = TRUE,
##                    status = "UPDATE"
##                    )
        
        outcome <- merge(turnover, ret.data,
                         by = "client_id",
                         all = TRUE)
        
##         appendLogs(msg = "Saving the outcome data",
##                    path.to.log,
##                    log.file,
##                    append.mode = TRUE,
##                    status = "UPDATE"
##                    )
        
        file.name <-
            paste0("/home/nsdl/RESULTS/SATYAM/OUTCOMES/outcome_satyam_dataset_",
                   dt,
                   ".Rdata")
        save(outcome, file = file.name)
    }

}


hosts <- paste0("nsdl@", c("192.9.11.55",
                           "192.9.11.57",
                           "192.9.11.59",
                           "192.9.11.61"
                           )
               )

my.cluster <- makePSOCKcluster(hosts,
                               master = "192.9.11.50",
                               rshcmd = "ssh -i $HOME/.ssh/igidr-ws",
                               outfile = "",
                               homogenous = TRUE
                               )

clusterExport(cl = my.cluster,
              varlist = c("dates.all",
                          "match.cl",
                          "getOutcomes",
                          "path.to.res",
                          "dfile.name",
                          "detectCores"
                          ),
              envir = environment()
              )

outcomes <- parLapplyLB(cl=my.cluster,
                        X = dates.all,
                        fun = getOutcomes,
                        match.cl = match.cl,
                        isin.dat = isin.dat,
                        ret.dat = ret.dat
                        )

stopCluster(my.cluster)
