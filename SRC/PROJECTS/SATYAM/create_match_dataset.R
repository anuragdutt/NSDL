rm(list = ls())
###############################
## initiate global variables ##
###############################
date.event   <- as.Date("2009-01-06")
isin.satyam  <- 
close.satyam <- 

############################
## initiate library files ##
############################
    
source("../../LIB/read_holdings_file.R")
source("../../LIB/read_client.R")
source("../../LIB/account_age.R")
source("../../LIB/get_state.R")
source("../../LIB/cal_underdiversification.R")
source("../../LIB/create_market_portfolio.R")
source("../../LIB/cal_turnover.R")
source("../../LIB/isin_presence.R")
source("../../LIB/stock_returns.R")
source("../../LIB/aggregate_pan_client_master.R")

options(scipen = 999, digits = 5)

#########################
## intiate R libraries ##
#########################

library(data.table)
library(zoo)
library(survival)
library(matchIt)

#############################
## loading global datasets ##
#############################

                                        # bhavcopy closing prices
load("/home/nsdl/CONSTANTS/close_price_all_stocks.Rdata")
colnames(isin.dat) <- tolower(colnames(isin.dat))
                                        # market cap from fld
load("/home/nsdl/CONSTANTS/mcap_nse_universe.Rdata")
                                        # mapping nsdl district codes
                                        # to district names
load("/home/nsdl/CONSTANTS/state_district_mapping.Rdata")

c.path   <- "/home/nsdl/DATAMART1/CLIENT_MASTER_FullDump_06102015.CSV"

########################
## Generic Workhorses ##
########################

cli.data <- readClientBeforeOct2015(c.path,
                                    on.ctype = NULL)
                                        # read client master
pan.data <- cli.data[, c("client_id",
                         "pan_1")]
                                        # aggregate by PAN
cli.data <- aggregatePan(cli.data)
cli.data$account_opening_date <-
    as.Date(cli.data$account_opening_date)
cli.data <- cli.data[which(cli.data$account_opening_date <=
                           date.event),
                     ]
cli.data <- cli.data[, c("client_id",
                         "ac_category",
                         "occupation",
                         "district_code"
                         "account_opening_date",
                         "account_closing_date"
                         )]

cli.data <- cli.data[!is.na(cli.data$district_code), ]
cli.data <- merge(cli.data, state.data,
                  by = "district_code",
                  all.x = TRUE)
cli.data <- cli.data[!is.na(cli.data$district), ]

                                        # Retrieve event date

f.path   <- retrieveFile(date.event, ftype = "holdings")
holdings <- readFile(f.path,
                     only.nse = TRUE,
                     aggregate = TRUE,
                     pan.data = pan.data
                     )
                                        # Calculate account age

age.data <- ageAccount(cli.data, date.event)
                                        # Get portfolio returns
ret.data <- getPortfolioReturnsParallelised(holdings,
                                            roll.per = 0,
                                            on = "simple",
                                            wt = "qty",
                                            close = isin.dat)
val.data <- ret.data[, c("client_id", "val")]

                                        # Check if the client has satyam
satyam.presence <- isinPresence(holdings,
                                val = val.data,
                                isin = isin.satyam,
                                close = close.satyam)

ret.dt  <- as.Date(unique(holdings$date))
                                        # Get returns of all the nse stocks
returns <- prepareReturnSeries(close = isin.dat,
                               roll.ud = 250,
                               ret.dt = ret.dt,
                               roll.per = 0,
                               on = "simple")



dt.req <- getDateSeries(dt.unique = sort(unique(isin.dat$DATE)),
                         roll.ud = roll.ud,
                        ret.dt = ret.dt)

                                        # Get market returns for
                                        # all nse stocks
market.ret <- getMarketReturns(st.dt = min(dt.req) - 1,
                               en.dt = max(dt.req),
                               mcap = mcap.dat,
                               close = isin.dat)

                                        # Get underdiversification and
                                        # portfolio beta
ud.beta <- getDummyPortfolioReturnsParallelised(holdings,
                                                returns,
                                                market.ret)
