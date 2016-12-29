library(xtable)
library(parallel)
load("/home/nsdl/DATAMART1/holdings_aggregated.Rdata")
load("/home/nsdl/DATAMART1/holdings_non_aggregated.Rdata")
load("/home/nsdl/DATAMART1/client_master_aggregated.Rdata")
load("/home/nsdl/DATAMART1/client_master_non_aggregated.Rdata")
cli.data <- cli.data[!is.na(cli.data$pan_1), ]
cli.data$account_closing_date <-
    as.Date(as.character(cli.data$account_closing_date),
            format = "%y-%m-%d")
client.type <- as.numeric(table(pan.data$client_type))
names(client.type) <- names(table(pan.data$client_type))
cli.data$opening_year <-
    format(as.Date(cli.data$account_opening_date),
           format = "%Y")
cli.data$opening_year <- as.numeric(cli.data$opening_year)
cli.data$closing_year <-
    format(as.Date(cli.data$account_closing_date),
           format = "%Y")
cli.data$closing_year <- as.numeric(cli.data$closing_year)
cli.data$closing_year[which(is.na(cli.data$closing_year))] <-
    2016

pan.mul <-
        unique(cli.data$pan_1[which(duplicated(cli.data$pan <-
                                                      1))])
mul.data <- cli.data[which(cli.data$pan_1 %in% pan.mul), ]

