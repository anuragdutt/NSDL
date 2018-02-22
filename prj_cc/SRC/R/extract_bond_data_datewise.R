rm(list = ls())

#############################
## loading global datasets ##
#############################
                                        # closing prices datset
load("/home/nsdl/CONSTANTS/close_price_all_stocks.Rdata")
colnames(isin.dat) <- tolower(colnames(isin.dat))
                                        # long term bonds
load("/home/nsdl/CONSTANTS/longTermCoBo_isins.rda")
                                        # short term bonds
load("/home/nsdl/CONSTANTS/shortTermCoBo_isins.rda")
                                        # Commercial papers bonds
load("/home/nsdl/CONSTANTS/cp_isins.rda")

############################
## initiate library files ##
############################
    
source("/home/nsdl/LIB/read_holdings_file.R")
source("/home/nsdl/LIB/read_client.R")
source("/home/nsdl/LIB/aggregate_pan_client_master.R")
source("/home/nsdl/LIB/retrieve_file.R")
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
cli.path <- "/home/nsdl/DATAMART1/CLIENT_MASTER_FullDump_06102015.CSV"
length.out <- 200
path.to.log <- "/home/nsdl/LOGS/CAPITAL_CONTROLS"
ncores <- 3
st.dt <- as.Date("2014-01-01")
en.dt <- as.Date("2016-03-31")

########################
## Generic Workhorses ##
########################

time.nw     <- paste0(unlist(strsplit(as.character(Sys.time()), " ")),
                      collapse="_")
log.file <- paste0("data_log_",
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
    dates.all <- as.Date(seq(from = as.Date(st.dt),
                             to = as.Date(en.dt),
                             by = "day"))
}


appendLogs(msg = "Reading client master",
           path.to.log,
           log.file,
           append.mode = TRUE,
           status = "UPDATE"
           )

cli.data <- readClientBeforeOct2015(cli.path,
                                    on.ctype = NULL)

pan.mapping <- cli.data[, c("client_id", "pan_1"), with = FALSE]


appendLogs(msg = "Retrieving paths",
           path.to.log,
           log.file,
           append.mode = TRUE,
           status = "UPDATE"
           )

path.all    <- retrieveFile(dates.all)

count <- 0
tmp   <- NULL

all.cbs <- c(as.character(tCobo_isins),
             as.character(stCobo_isins),
             as.character(cp_isins)
             )
all.cbs <- all.cbs[!is.na(all.cbs)]

mclapply(path.all, function(fpath,
                            pan.data){
    
    dt <- as.Date(unlist(strsplit(
        unlist(strsplit(fpath, "_"))[3], ".CSV.gz"))[1],
        format = "%d%m%Y")
    
    appendLogs(msg = paste0("Retrieving holdings for date ", dt),
               path.to.log,
               log.file,
               append.mode = TRUE,
               status = "UPDATE"
               )

    holdings <- readFile(fpath,
                         only.nse = FALSE,
                         aggregate = TRUE,
                         pan.data = pan.mapping
                         )
    holdings <- data.table(holdings)

    client.cb <- holdings$client_id[which(holdings$isin %in% all.cbs)]

    holdings <- holdings[which(holdings$client_id %in% client.cb), ]

    file.name <-
        paste0("/home/nsdl/RESULTS/CAPITAL_CONTROLS/ALL_BONDS/cb_dataset_",
               dt,
               ".Rdata")
    save(holdings, file = file.name)

},
mc.cores = ncores,
)
