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
source("/home/nsdl/LIB/total_quantity.R")

#####################
## processing data ##
#####################

nse.isin <- unique(isin.dat$isin)
odi <- unique(debtTrades_FPI$ISIN[-which(debtTrades_FPI$RFDD_INSTR_TYPE
                                  %in% c("REG_DL_INSTR_EQ",
                                         "REG_DL_INSTR_EU",
                                         "REG_DL_INSTR_OE",
                                         "REG_DL_INSTR_PS"
                                         ))])
odi <- odi[-which(odi %in% nse.isin)]

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

all.cbs  <- unique(c(as.character(ltCobo_isins)
                     )
                   )
all.cbs  <- as.character(all.cbs[!is.na(all.cbs)])
all.cbs  <- all.cbs[-which(all.cbs == "INE733E07JP6")]

sampleSum <- function(x, d) {
    return(sum(x[d]))
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
                           cb.count <- getIsinQuantity(holdings,
                                                       all.cbs)
                           
                           b.count <- boot(cb.count$isin_qty,
                                           sampleSum,
                                           R = boot.run)
                           ci.count <- quantile(b.count$t, ci)
                           sum.count <- sum(cb.count$isin_qty)

                           return(c(as.character(unique(holdings$date)),
                                    sum.count,
                                    ci.count
                                    )
                                  )
                           }
                   }, mc.cores = cores)
            )



colnames(fpi.cat) <- c("date",
                       "median_all",
                       "all_25",
                       "all_975"
                       )

fpi.cat <- data.table(fpi.cat)

fpi.cat$median_all     <- as.numeric(fpi.cat$median_all)
fpi.cat$all_25 <- as.numeric(fpi.cat$all_25)
fpi.cat$all_975 <- as.numeric(fpi.cat$all_975)

fpi.cat$median_all <- fpi.cat$median_all/1000000
fpi.cat$all_25 <- fpi.cat$all_25/1000000
fpi.cat$all_975 <- fpi.cat$all_975/1000000


save(fpi.cat, file = "/home/nsdl/RESULTS/CAPITAL_CONTROLS/fpi_sum_long_term_minus_ntpc_cbs.Rdata")
