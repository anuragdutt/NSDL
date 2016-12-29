getIsinQuantity <- function(holdings, isin.vec) {
    library(data.table)
    holdings <- data.table(holdings)
    sum.isin <- holdings[, total_qty:=sum(qty),
                         by = list(client_id)][, c("client_id",
                                                   "total_qty"), with = FALSE]
    colnames(sum.isin) <- c("client_id",
                            "total_qty")
    setkey(holdings, isin)
    only.isins <- holdings[J(isin.vec)]
    isin.count <- only.isins[, total_qty:=sum(qty),
                             by = list(client_id)][, c("client_id",
                                                       "total_qty"),
                                                   with = FALSE]
    colnames(isin.count) <- c("client_id",
                              "isin_qty")

    sum.isin   <- sum.isin[!duplicated(sum.isin$client_id), ]
    isin.count <- isin.count[!duplicated(isin.count$client_id), ]

    isin.merge <- merge(sum.isin,
                        isin.count,
                        by = "client_id",
                        all.x = TRUE)
    isin.merge <- isin.merge[!is.na(isin.merge$client_id), ]
    isin.merge <- isin.merge[!is.na(isin.merge$total_qty), ]
    isin.merge[is.na(isin.merge)] <- 0
    isin.merge$per_comp <- isin.merge$isin_qty*100/isin.merge$total_qty
    return(isin.merge)
}
