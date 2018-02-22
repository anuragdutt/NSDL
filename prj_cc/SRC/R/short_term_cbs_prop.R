rm(list = ls())
options(scipen = 999, digits = 3)
#########################
## intiate R libraries ##
#########################

library(data.table)
library(zoo)
library(parallel)
library(boot)

#############################
## loading global datasets ##
#############################
                                        # bhavcopy closing prices
load("/home/nsdl/CONSTANTS/close_price_all_stocks.Rdata")
colnames(isin.dat) <- tolower(colnames(isin.dat))
isin.dat <- data.table(isin.dat)
                                        # load long corporate term bond
load("/home/nsdl/CONSTANTS/longTermCoBo_isins.rda")
                                        # load short term corporate bond 
load("/home/nsdl/CONSTANTS/shortTermCoBo_isins.rda")
                                        # load commercial papers
load("/home/nsdl/CONSTANTS/cp_isins.rda")
                                        # load entire universe of bonds
load("/home/nsdl/CONSTANTS/FPI_debtSecTradingData_201601.rda")
                                        # load foreign clients
load("/home/nsdl/DATAMART1/foreign_demographics.Rdata")
                                        # load non bond holders
load("/home/nsdl/RESULTS/CAPITAL_CONTROLS/non_cb_holders.Rdata")
###############################
## initiate global variables ##
###############################

path.holdings <- "/home/nsdl/RESULTS/CAPITAL_CONTROLS/DATA"
path.total    <- "/home/nsdl/RESULTS/CAPITAL_CONTROLS/CB_MARKET_CAP"
file.name  <- "fpi_dataset_"
file.total <- "cb_mcap_"
save.dir <- "/home/nsdl/RESULTS/CAPITAL_CONTROLS"
client_path <- "/home/nsdl/DATAMART1/CLIENT_MASTER_FullDump_06102015.CSV"
per.person <- FALSE
cores <- 3
boot.run <- 1000
ci <- c(0.025, 0.975)

############################
## initiate library files ##
############################

source("/home/nsdl/LIB/read_client.R")
source("/home/nsdl/LIB/read_holdings_file.R")
source("/home/nsdl/LIB/aggregate_pan_client_master.R")
source("/home/nsdl/LIB/isin_composition.R")
source("/home/nsdl/LIB/total_quantity.R")

#####################
## processing data ##
#####################


dates.seq <- seq(from = as.Date("2014-01-01"),
                 to = as.Date("2016-03-31"),
                 by = "day")


## cli.data <- readClientBeforeOct2015(c.path = client_path,
##                                     on.ctype = NULL)
## fpi.pan  <- pan.data$client_id[which(pan.data$client_type %in% c(3, 13, 14))] 
## cli.data <- cli.data[which(cli.data$client_type %in% c(3, 13, 14)), ]
## save(cli.data, file = "foreign_demographics.Rdata")

pan.data <- aggregatePan(cli.data)
fpi.pan  <- pan.data$client_id[which(pan.data$client_type %in% c(3, 13, 14))]
fpi.pan  <- fpi.pan[!is.na(fpi.pan)]

all.cbs  <- unique(c(as.character(stCobo_isins)
                     )
                   )
all.cbs  <- as.character(all.cbs[!is.na(all.cbs)])

sampleSum <- function(x, d) {
    return(sum(x[d]))
}

fpi.cat <- do.call('rbind',
                   mclapply(dates.seq, function(dt) {
                       
                       fp.fpi <- paste0(sep = path.holdings,
                                        sep = "/",
                                        sep = file.name,
                                        dt,
                                        sep = ".Rdata")
                       
                       fp.total <- paste0(sep = path.total,
                                          sep = "/",
                                          sep = file.total,
                                          dt,
                                          sep = ".Rdata")
                       if(!file.exists(fp.fpi)) {
                           dt <- dt - 1
                           fp.fpi <- paste0(sep = path.holdings,
                                            sep = "/",
                                            sep = file.name,
                                            dt,
                                            sep = ".Rdata")
                           
                       }

                       if(!file.exists(fp.fpi)) {
                           dt <- dt - 1
                           fp.fpi <- paste0(sep = path.holdings,
                                            sep = "/",
                                            sep = file.name,
                                            dt,
                                            sep = ".Rdata")
                           
                       }

                       if(!file.exists(fp.fpi)) {
                           dt <- dt - 1
                           fp.fpi <- paste0(sep = path.holdings,
                                            sep = "/",
                                            sep = file.name,
                                            dt,
                                            sep = ".Rdata")
                           
                       }
                       
                       
                       if(file.exists(fp.fpi) && file.exists(fp.total)) {
                           load(fp.fpi)
                           load(fp.total)
                           holdings <-
                               holdings[which(holdings$client_id %in% fpi.pan), ]
                           holdings <-
                               holdings[which(holdings$client_id %in%
                                              fpi.pan), ]
                           holdings <-
                               holdings[!holdings$client_id %in%
                                        non.holders.common, ]
                           cb.count <- getIsinQuantity(holdings,
                                                       all.cbs)
                           
                           sum.count <- sum(cb.count$isin_qty)
                           lt.total <- as.numeric(retval[3])
                           dt.ret <- as.Date(retval[1])
                           prop     <- sum.count/lt.total
                           
                           return(c(as.character(dt.ret),
                                    prop
                                    )
                                  )
                           }
                   }, mc.cores = cores)
            )



colnames(fpi.cat) <- c("date",
                       "proportion"
                       )

fpi.cat <- data.table(fpi.cat)


save(fpi.cat, file = "/home/nsdl/RESULTS/CAPITAL_CONTROLS/fpi_sum_short_term_prop.Rdata")
