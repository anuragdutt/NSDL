##' Calculate the percentage of nifty, nifty junior and psu as a total
##' percentage of the portfolio.
##'
##'As of now the measures are calculated only on quantity and not on
##'     the net asset value of the portfolio
##' @title niftySharesPosrtfolio
##' @param dataset dataset released from isinTrades.R with total
##'     stocks, and quantity of nifty, nifty junor and psu stocks
##' @param measure percentage of which of the three mentioned or all
##'     are to be calculated
##' @return percentage of nifty, nifty junior and psu in total
##'     portfolio for all the accounts
##' @author Anurag Dutt
niftysharesPortfolio <- function(dataset,
                                 measure = "all") {
    dataset <- dataset[order(dataset[["client_id"]]), ]
    c.s <-
        seq_along(dataset$client_id)[!duplicated(dataset$client_id)]
    c.e <- c((c.s-1)[-1], length(dataset$client_id))
    c.i <- paste0(sep = "c(", c.s, sep = ":", c.e, sep = ")")

    res <- do.call('rbind', lapply(c.i, function(inx) {
        dat <- dataset[eval(parse(text=inx)), ]
        dat$total   <- nrow(dat) 
        
        if(measure == "nifty") {
            dat$nifty_per <- dat$nifty*100/dat$total
            retval <- dat[, c("client_id", "nifty_per")]
            retval <- retval[!duplicated(retval$client_id), ]
        }
        if(measure == "niftyjr") {
            dat$niftyjr_per <- dat$niftyjr*100/dat$total
            retval <- dat[, c("client_id", "niftyjr_per")]
            retval <- retval[!duplicated(retval$client_id), ]
        }
        if(measure == "psu") {
            dat$psu_per <- dat$psu*100/dat$total
            retval <- dat[, c("client_id", "psu_per")]
            retval <- retval[!duplicated(retval$client_id), ]
        }
        if(measure == "all") {
            dat$nifty_per <- dat$nifty*100/dat$total
            dat$niftyjr_per <- dat$niftyjr*100/dat$total
            dat$psu_per <- dat$psu*100/dat$total
            retval <- dat[, c("client_id", "nifty_per",
                              "niftyjr_per", "psu_per")]
            retval <- retval[!duplicated(retval$client_id), ]
        }
        
        return(retval)
    }
    )
    )
    res <- data.frame(res)

    return(res)
}


