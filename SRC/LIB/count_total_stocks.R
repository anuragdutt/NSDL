##' Count the total number of unique isins in a given portfolio
##'
##' Calculated for all the clients for a given day's holdings
##' @title portfolioIsinCount
##' @param dataset Holdings for a given date
##' @return Number of unique isin for a given day's holdings file
##' @author Anurag Dutt
portfolioIsinCount <- function(dataset) {
    dataset <- dataset[order(dataset[["client_id"]]), ]
    c.s <-
        seq_along(dataset$client_id)[!duplicated(dataset$client_id)]
    c.e <- c((c.s-1)[-1], length(dataset$client_id))
    n.share  <- c.e - c.s + 1
    client.d <- dataset$client_id[c.s]
    retval   <- cbind(client.d, n.share)
    colnames(retval) <- c("client_id", "n_share")
    retval   <- data.frame(retval) 
    return(retval)
}
