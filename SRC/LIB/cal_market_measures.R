# options(warn = -1)
getRealisedVolDaily <- function(sym, dates) {
    library(frgdatalib)
    library(xts)
    library(data.table)
    spot.data <- iddb(date = dates,
                      symbol = sym,
                      segment = "CASH",
                      data.type = "TAO",
                      measures = "rawnse",
                      sub.type = "CASH_Trades",
                      rawtime = FALSE,
                      retclass = "data.table",
                      fields = c("trade_time",
                                 "trade_price"
                                 )
                 )

    get.rvol <- lapply(spot.data, function(spot) {

        if(nrow(spot) > 30) {
            dt <- as.Date(unique(substr(spot$trade_time, 1,
                                        10))[1])
            
            timeline <- seq(from = as.POSIXct(paste(dt, "09:16:00")),
                            to = as.POSIXct(paste(dt, "15:30:00")),
                            by = "sec")
            
            spot$trade_time <- as.POSIXct(as.character(spot$trade_time))
            spot$trade_price <- spot$trade_price/100
            
            spot <- spot[spot$trade_time >= as.POSIXct(paste(dt,
                                                             "09:15:00")),
                         ]
            
            spot <- spot[spot$trade_time <= as.POSIXct(paste(dt,
                                                             "15:30:00")), ]
            spot <- xts(spot[ , c("trade_price"),
                             with = FALSE],
                        order.by = spot$trade_time)
            
            spot <- aggregate(spot$trade_price,
                              time(spot) - as.numeric(time(spot)) %% 1,
                              tail, 1)
            spot <- data.table(trade_time = index(spot),
                               trade_price = coredata(spot))
            colnames(spot) <- c("trade_time", "trade_price")
            
            time.ser <- data.table(trade_time = timeline)
            spot <- merge(time.ser, spot, by = "trade_time", all.x = TRUE)
            
            spot <- xts(spot[ , c("trade_price"),
                             with = FALSE],
                        order.by = spot$trade_time)
            
            agg <- aggregate(spot$trade_price,
                             time(spot) - as.numeric(time(spot)) %% 300,
                             tail, 1)
            agg <- agg[!is.na(coredata(agg))]
            agg <- xts(agg)
            colnames(agg) <- "trade_prices"

            rvol <- sd(diff(log(coredata(agg))))
            
            retval <- data.table(date = dt,
                                 nse_symbol = sym,
                                 r_vol <- rvol
                                 )
        } else {
            retval <- NULL
        }
            }
        )

    rvol <- rbindlist(get.rvol)
    colnames(rvol) <- c("date", "nse_symbol", "r_vol")
    return(rvol)

}
