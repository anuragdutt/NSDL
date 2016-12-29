rm(list = ls())
library(data.table)
load("/home/nsdl/CONSTANTS/close_price_all_stocks.Rdata")
load("/home/nsdl/CONSTANTS/stock_returns_all.Rdata")
ret.dat <- stock.ret
ret.dat$symbol <- NULL
close <- isin.dat
colnames(close) <- tolower(colnames(close))

load("/home/nsdl/RESULTS/SATYAM/DATA/satyam_dataset_2008-06-02.Rdata")
holdings$client_isin <- paste0(holdings$client_id,
                               sep = "_",
                               holdings$isin)
discard <- holdings
load("/home/nsdl/RESULTS/SATYAM/DATA/satyam_dataset_2008-06-03.Rdata")
holdings$client_isin <- paste0(holdings$client_id,
                               sep = "_",
                               holdings$isin)
## Getting bought
bought <- holdings[!holdings$client_isin %in%
                   discard$client_isin, ]
## Getting Sold
sold     <- discard[!discard$client_isin %in% holdings$client_isin, ]
## Adjusting discard
discard  <- discard[!discard$client_isin %in% sold$client_isin]

## load("/home/nsdl/RESULTS/SATYAM/DATA/satyam_dataset_2008-06-04.Rdata")

getDisposition <- function(holdings,
                           discard,
                           bought,
                           ret.dat) {

    ## library(data.table)
    holdings <- data.table(holdings)
    discard  <- data.table(discard)
    bought   <- data.table(bought)

    dt.today <- holdings$date[1]
    
    holdings$client_isin <- paste0(holdings$client_id,
                               sep = "_",
                               holdings$isin)
    ## Removing discarded
    holdings <- holdings[!holdings$client_isin %in%
                         discard$client_isin, ]
    ## Getting Bought
    bought.new <- holdings[!holdings$client_isin %in%
                           bought$client_isin, ]
    ## Getting Sold
    sold.new <- bought[!bought$client_isin %in% holdings$client_isin,
                       ]
    ## Adjusting discard
    discard.adj <- discard[!discard$client_isin %in%
                           sold.new$client_isin, ]
    ## Adjusting bought
    bought.adj <- rbindlist(list(bought, bought.new))

    ## Getting realized profits and losses
    sold.new <- merge(sold.new, ret.dat,
                      by = c("date", "isin"),
                      all.x = TRUE)
    sold.new <- sold.new[!is.na(sold.new$price), ]
    sold.new <- sold.new[!is.na(sold.new$log_returns), ]
    colnames(sold.new)[which(colnames(sold.new) %in%
                                      c("price",
                                        "date",
                                        "simple_returns",
                                        "log_returns"))] <-
        paste0(c("date", "price", "simple_returns", "log_returns"),
               sep = "_prev")
    sold.new$date <- as.Date(dt.today)
       sold.new <- merge(sold.new, ret.dat,
                      by = c("date", "isin"),
                      all.x = TRUE)
    sold.new <- sold.new[!is.na(sold.new$price), ]
    sold.new <- sold.new[!is.na(sold.new$log_returns), ]
    colnames(sold.new)[which(colnames(sold.new) %in%
                                      c("price",
                                        "date",
                                        "simple_returns",
                                        "log_returns"))] <-
        paste0(c("date", "price", "simple_returns", "log_returns"),
               sep = "_next")
    sold.new$returns_re <- sold.new$log_returns_next - sold.new$log_returns_prev
    sold.new$price_re  <- log(sold.new$price_next) - log(sold.new$price_prev)
    

}
