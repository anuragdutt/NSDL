isinPresence <- function(holdings,
                         val,
                         isin,
                         close) {

    ## library(data.table)
    if(length(isin) != length(close)){
        stop("Missing closing prices or isins. Check you dumbass.")
    }
                                        # filtering ids with given isins
    c.id <- unique(holdings$client_id[which(holdings$isin %in% isin)])
                                        #  removing unecessary columns
    holdings$date   <- NULL
    holdings$dp_id  <- NULL
                                        # checking if the
                                        # list of isins
                                        # are present in
                                        # the holdings
    holdings$status <- 0
    holdings$status[which(holdings$client_id %in% c.id)] <- 1
                                        # get portfolio composition of requisite isin
    isin.list <- lapply(1:length(isin),
                        function(num)
                        {
                            ine   <- isin[num]
                            cp <- close[num]
                            h.isin   <- holdings[which(holdings$isin %in% ine), ]
                            h.isin$isin_value <- as.numeric(h.isin$qty)*as.numeric(cp)
                            return(h.isin)
                        }
                        )
                                        # merging isin values
    isin.p   <- rbindlist(isin.list)
    isin.p   <- isin.p[order(isin.p[["client_id"]],
                             isin.p[["isin"]]), ]
    isin.val <- aggregate(isin_value ~ client_id,
                          data = isin.p,
                          FUN  = sum)
    isin.p$isin_value <- NULL
    isin.p   <- merge(isin.p,
                      isin.val,
                      by = "client_id",
                      all.x = TRUE
                      )
    isin.p   <- isin.p[!duplicated(isin.p$client_id)] 
    h.nisin  <- holdings[which(holdings$status == 0), ]
    h.nisin  <- h.nisin[!duplicated(h.nisin$client_id), ]
    h.nisin$isin_value <- 0
    holdings <- rbind(isin.p, h.nisin)
    holdings <- merge(holdings, val,
                      by = "client_id",
                      all.x = TRUE)
    holdings <- holdings[!is.na(holdings$val), ]
    holdings <- holdings[!is.na(holdings$client_id), ]
    holdings <- holdings[!duplicated(holdings$client_id), ]
    holdings$composition <- holdings$isin_value*100/holdings$val
    retval <- holdings[, c("client_id", "status", "isin_value"),
                       with = FALSE]
    colnames(retval) <- c("client_id", "isin_presence", "isin_value")
    return(retval)

}
