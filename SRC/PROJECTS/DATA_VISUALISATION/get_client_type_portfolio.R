load("/home/nsdl/RESULTS/DATA_VISUALISATION/portfolio_returns_aggregated.Rdata")
load("/home/nsdl/DATAMART1/client_master_aggregated.Rdata")
#client.type <- pan.data[, c("client_id", "client_type")]
port.client <- merge(portfolio.ret, pan.data,
                     by = "client_id",
                     all.x = TRUE)
port.client <- port.client[1:100000, ]
save(port.client,
     file = "/home/nsdl/RESULTS/DATA_VISUALISATION/port_client_all.Rdata")
