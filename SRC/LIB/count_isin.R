##' Count the quantity of a given stock for a day's holdings
##'
##' Takes in holdings and isin as arguments and count the isin
##'     holdings of those specific people with the given isin
##' @title countIsin
##' @param dataset 
##' @param stock 
##' @return dataset with client and holdings of the particular isin in
##'     the question
##' @author Anurag Dutt
countIsin <- function(dataset, stock) {
    dataset <- dataset[order(dataset[["client_id"]]), ]
    c.s <-
        seq_along(dataset$client_id)[!duplicated(dataset$client_id)]
    c.e <- c((c.s-1)[-1], length(dataset$client_id))
    c.i <- paste0(sep = "c(", c.s, sep = ":", c.e, sep = ")") 
    result   <- do.call('rbind', lapply(c.i, 
                                        function(inx) {
                                            count <- 0
                                            dat   <- dataset[eval(parse(text=inx)), ]
                                            rel <- dat[which(dat$isin == stock), ]
                                            if(nrow(rel) == 0) {
                                                count <- 0
                                            } else {
                                                count <- dat$qty[which(dat$isin ==
                                                                       stock)]
                                            }
                                            retval <-
                                                cbind(unique(dat$client_id), count)
                                            retval <- data.frame(retval)
                                            return(retval)
                                        }
                                        )
                          )
    return(result)
}



