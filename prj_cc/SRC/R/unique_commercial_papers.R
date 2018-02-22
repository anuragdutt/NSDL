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
                                        # load all bonds data
load("/home/nsdl/CONSTANTS/cobo_FPIinvestible_data.rda")
                                        # Corporate bonds less than
                                        # one year
load("/home/nsdl/CONSTANTS/bonds_mat_less_than_one.rda")
###############################
## initiate global variables ##
###############################

path.holdings <- "/home/nsdl/RESULTS/CAPITAL_CONTROLS/DATA"
file.name <- "fpi_dataset_"
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

#####################
## processing data ##
#####################
dates.seq <- seq(from = as.Date("2015-01-01"),
                 to = as.Date("2016-03-31"),
                 by = "day")

file.path <- paste0(sep = path.holdings,
                    sep = "/",
                    sep = file.name,
                    dates.seq,
                    sep = ".Rdata")

## cli.data <- readClientBeforeOct2015(c.path = client_path,
##                                     on.ctype = NULL)
## fpi.pan  <- pan.data$client_id[which(pan.data$client_type %in% c(3, 13, 14))] 
## cli.data <- cli.data[which(cli.data$client_type %in% c(3, 13, 14)), ]
## save(cli.data, file = "foreign_demographics.Rdata")

pan.data <- aggregatePan(cli.data)
fpi.pan  <- pan.data$client_id[which(pan.data$client_type %in% c(3, 13, 14))]
fpi.pan  <- fpi.pan[!is.na(fpi.pan)]

all.cbs  <- unique(c(as.character(mat_less_one),
                     as.character(cp_isins)
                     )
                   )
                     
all.cbs  <- as.character(all.cbs[!is.na(all.cbs)])

sampleMedian <- function(x, d) {
    return(median(x[d]))
}

fpi.cat <- do.call('rbind',
                   mclapply(file.path, function(fp) {
                       if(file.exists(fp)) {
                           load(fp)
                           holdings <-
                               holdings[which(holdings$client_id %in% fpi.pan), ]
                           holdings <-
                               holdings[which(holdings$client_id %in%
                                              fpi.pan), ]
                           holdings <-
                               holdings[!holdings$client_id %in%
                                        non.holders.common, ]
                           cb.count <- getIsinComposition(holdings,
                                                          all.cbs)
                           cb.notnull <- cb.count[cb.count$isin_count
                                                  != 0, ]
                           
                           if(nrow(cb.notnull) == 0){
                               cb.notnull <- cb.count
                           }
                           b.count <- boot(cb.count$isin_count,
                                           sampleMedian,
                                           R = boot.run)

                           b.notnull <- boot(cb.notnull$isin_count,
                                           sampleMedian,
                                           R = boot.run)
                           ci.count <- quantile(b.count$t, ci)
                           ci.notnull <- quantile(b.notnull$t, ci)
                           median.count <- median(cb.count$isin_count)
                           median.notnull <-
                               median(cb.notnull$isin_count)

                           return(c(as.character(unique(holdings$date)),
                                    median.count, median.notnull,
                                    ci.count, ci.notnull
                                    )
                                  )
                           }
                   }, mc.cores = cores)
            )



colnames(fpi.cat) <- c("date",
                       "median_all",
                       "median_holders",
                       "all_25",
                       "all_975",
                       "notnull_25",
                       "notnull_975")

fpi.cat <- data.table(fpi.cat)

fpi.cat$median_all     <- as.numeric(fpi.cat$median_all)
fpi.cat$median_holders <- as.numeric(fpi.cat$median_holders)
fpi.cat$all_25 <- as.numeric(fpi.cat$all_25)
fpi.cat$all_975 <- as.numeric(fpi.cat$all_975)
fpi.cat$notnull_25 <- as.numeric(fpi.cat$notnull_25)
fpi.cat$notnull_975 <- as.numeric(fpi.cat$notnull_975)


save(fpi.cat, file = "/home/nsdl/RESULTS/CAPITAL_CONTROLS/fpi_median_one_yr_cbs.Rdata")
