getIsinComposition <- function(holdings, isin.vec) {
    library(data.table)
    holdings <- data.table(holdings)
    sum.isin <- holdings[, .N,
                         by = list(client_id)]
    colnames(sum.isin) <- c("client_id",
                            "total_isin")
    setkey(holdings, isin)
    only.isins <- holdings[J(isin.vec)]
    isin.count <- only.isins[, .N,
                         by = list(client_id)]
    colnames(isin.count) <- c("client_id",
                            "isin_count")

    isin.merge <- merge(sum.isin,
                        isin.count,
                        by = "client_id",
                        all.x = TRUE)
    isin.merge <- isin.merge[!is.na(isin.merge$client_id), ]
    isin.merge <- isin.merge[!is.na(isin.merge$total_isin), ]
    isin.merge[is.na(isin.merge)] <- 0
    isin.merge$per_comp <- isin.merge$isin_count*100/isin.merge$total_isin
    return(isin.merge)
}
