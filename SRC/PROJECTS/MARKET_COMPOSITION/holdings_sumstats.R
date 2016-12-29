rm(list = ls())
options(scipen = 999, digits = 6)
source("/home/nsdl/LIB/account_age.R")
source("/home/nsdl/LIB/retrieve_file.R")
source("/home/nsdl/LIB/read_client.R")
source("/home/nsdl/LIB/read_holdings_file.R")
source("/home/nsdl/LIB/stock_returns.R")
source("/home/nsdl/LIB/create_market_portfolio.R")
source("/home/nsdl/LIB/cal_stock_beta.R")
source("/home/nsdl/LIB/cal_portfolio_returns.R")

load("/home/nsdl/CONSTANTS/state_district_mapping.Rdata")
load("/home/nsdl/CONSTANTS/close_price_all_stocks.Rdata")
load("/home/nsdl/CONSTANTS/mcap_nse_universe.Rdata")

library(data.table)

date.seq <- seq(as.Date("2015-05-01"),
                as.Date("2015-07-31"),
                by = "day")

st.dt <- min(date.seq)
en.dt <- max(date.seq)
market.ret <- getMarketReturns(st.dt,
                               en.dt,
                               mcap = mcap.dat,
                               close = isin.dat)

holdingSumstats <- function(date.seq,
                            market.ret,
                            save.path = "/home/nsdl/RESULTS/MARKET_COMPOSITION/RETURNS/",
                            client.path = "/home/nsdl/DATAMART/CLIENT_MASTER_FullDump_06102015.CSV") {

    path.files <- retrieveFile(date.seq, "holdings")
    cli.data   <- readClientBeforeOct2010(client.path)
    
    lapply(path.files, function(path){
        dt <-
            as.Date(tail(unlist(strsplit(unlist(strsplit(path,
                                                         ".CSV.gz"))[1],
                                         "_"))
                       , 1), format = "%d%m%Y")
        cli.tmp <- cli.data
        cli.age <- ageAccount(cli.tmp, dt)
        colnames(state.data) <- c("district", "district_code",
                                  "state")

        holdings   <- readFile(path)
        stock.ret  <- stockSimpleReturns(isin.dat)
        inv.ret <- getPortfolioReturns(holdings,
                                       close = isin.dat)
        inv.age <- merge(inv.ret, cli.age,
                         by = "client_id",
                         all.x = TRUE)
        inv.dem <- merge(inv.age, cli.tmp,
                         by = "client_id")
        h.sumstats <- merge(inv.dem, state.data,
                            by = "district_code")

        print(paste0("saving for date: ", dt))
        save(h.sumstats, file = paste0(save.path,
                                       "portfolio_returns_",
                                       dt,
                                       ".Rdata"))

                    
    })
}

holdingSumstats(date.seq, market.ret)
