getNetTrades <- function(holdings,
                         holdings.prev,
                         close) {
    colnames(close) <- tolower(colnames(close))
    price.dat <- close[, c("isin", "price")]
    holdings  <- merge(holdings,
                       price.dat,
                       by = "isin",
                       all.x = TRUE)
    holdings <- holdings[, c("client_id", "isin", "qty")]
    holdings.prev  <- merge(holdings.prev
                            price.dat,
                            by = "isin",
                            all.x = TRUE)
    holdings.prev <- holdings.prev[, c("client_id", "isin", "qty")]

    chk.holdings <- merge(holdings, holdings.prev,
                      by = c("client_id", "isin"),
                      all.x = TRUE)


}
