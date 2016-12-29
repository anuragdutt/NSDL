options(digits.secs=6)
library(frgdatalib)
## st.dt <- "2015-06-01"
## en.dt <- "2015-06-05"
## sym <- "NESTLEIND"

getTradeIntensity <- function(symbol,
                              st.dt,
                              en.dt,
                              avg.freq = "hourly") {

  st.dt <- as.Date(st.dt)
  en.dt <- as.Date(en.dt)
  dat <- lapply(symbol, function(sym) {
    dates <- seq.Date(from = as.Date(st.dt),
                      to = as.Date(en.dt),
                      by = "days")
    trades.dat <- iddb(date = dates,
                       symbol = sym,
                       segment = "CASH",
                       measures = "activepassive",
                       fields = c("tradeTime",
                         "tradePrice",
                         "tradeQuantity",
                         "activeSide",
                         "activeClientIdentityFlag",
                         "passiveClietIdentityFlag",
                         "buyClientIdentityFlag",
                         "sellClientIdentityFlag"),
                       retclass = "data.table",
                       generate = TRUE,
                       wait = TRUE
                       )

    colnames(trades.dat) <- c("trade_time",
                              "trade_price",
                              "trade_quantity",
                              "active_side",
                              "active_client_identity_flag",
                              "passive_client_identity_flag",
                              "buy_client_identity_flag",
                              "sell_client_identity_flag")

    lapply(dates, function(dt, sym), sym = sym {
      
      each.day[, trade_value := (trade_price * trade_quantity)]
      each.day[,
               trade_hour := format(trade_time, "%Y-%m-%d %H")]
      each.day[,
               trade_value_sum:= sum(trade_value),
               by = trade_hour]
      each.day[,
               buy_:= sum(buy_algo_indicator),
               by = trade_hour]
      each.day[,
               sell_algo_indicator_sum:= sum(sell_algo_indicator),
               by = trade_hour]
      

    return(trades.dat)
  }
                )
  names(dat) <- symbol
       






