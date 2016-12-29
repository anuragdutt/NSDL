## source("/FRG/db_nsdl/SRC/LIB/read_holdings_file.R")
## holdings.prev <- readFile("DATA/IGIDR_POSITION_20052016.CSV.gz",
##                           only.nse = FALSE)
## holdings      <- readFile("DATA/IGIDR_POSITION_21052016.CSV.gz",
##                           only.nse = FALSE)
## holdings.prev <- holdings.prev[-which(holdings.prev$qty == 0), ]
## holdings      <- holdings[-which(holdings == 0), ]
## holdings$date <- as.Date(holdings$date)
## holdings.prev$date <- as.Date(holdings.prev$date)
## load("close_price_all_stocks.Rdata")
## close <- isin.dat

getTurnover <- function(holdings,
                        holdings.prev,
                        close) {

    ## library(data.table)
    colnames(close) <- tolower(colnames(close))
    close <- data.frame(close)
    close <- close[which(close$date <=
                         as.Date(unique(holdings$date))), ]
    close <- data.table(close)
    isin.dat <- close[, c("isin", "close", "date"), with = FALSE]
    holdings$client_isin <- paste0(holdings$client_id, sep = "-", holdings$isin)
    holdings$date <- as.Date(holdings$date)
    holdings.prev$date <- as.Date(holdings.prev$date)
    holdings <- merge(holdings, isin.dat,
                      by = c("isin", "date"),
                      all.x = TRUE)
    holdings$val   <- holdings$qty*holdings$close

    holdings.prev$client_isin <- paste0(holdings.prev$client_id,
                                        sep = "-",
                                        holdings.prev$isin)
    holdings.prev <- merge(holdings.prev, isin.dat,
                      by = c("isin", "date"),
                      all.x = TRUE)
    holdings.prev$val   <- holdings.prev$qty*holdings.prev$close

    holdings.prev$client_isin <- paste0(holdings.prev$client_id,
                                        sep = "-",
                                        holdings.prev$isin)
    
    isin.entry <- holdings$client_isin[-which(holdings$client_isin
                                              %in%
                                              holdings.prev$client_isin)]
    isin.exit  <- holdings.prev$client_isin[-which(holdings.prev$client_isin
                                                   %in%
                                                   holdings$client_isin)]

    qty.prev   <- c(holdings.prev$val, rep(0, length(isin.entry)))
    qty.next   <- c(holdings$val, rep(0, length(isin.exit)))
    names(qty.prev) <- c(holdings.prev$client_isin, isin.entry)
    names(qty.next) <- c(holdings$client_isin, isin.exit)

    qty.prev <- qty.prev[order(names(qty.prev))]
    qty.next <- qty.next[order(names(qty.next))]

    net_turnover   <- qty.next - qty.prev
    gross_turnover <- abs(net_turnover)

    odd.series  <- seq(from = 1, to = 2*length(qty.prev), by = 2)
    even.series <- seq(from = 2, to = 2*length(qty.prev), by = 2)
    ret.df <- data.table(client_id = unlist(strsplit(names(qty.prev),
                                              "-"))[odd.series],
                         isin = unlist(strsplit(names(qty.prev),
                                         "-"))[even.series],
                         net_turnover   = net_turnover,
                         gross_turnover = gross_turnover,
                         key = "client_id")

    ret.df <- ret.df[order(ret.df[["client_id"]],
                           ret.df[["isin"]]), ]
    net.turnover <- ret.df[, net_t:=sum(net_turnover),
                           by=list(client_id)][, c("client_id",
                                                   "net_t"),
                                               with = FALSE]

    gross.turnover <- ret.df[, gross_t:=sum(gross_turnover),
                             by=list(client_id)][, c("client_id",
                                                       "gross_t"),
                                                 with = FALSE]
    
    net.turnover <- net.turnover[!duplicated(net.turnover$client_id), ]
    gross.turnover <- gross.turnover[!duplicated(gross.turnover$client_id), ]

    turnover <- merge(net.turnover, gross.turnover, all.x = TRUE)
    colnames(turnover) <- c("client_id",
                            "net_turnover",
                            "gross_turnover")
    return(turnover)
    
}
