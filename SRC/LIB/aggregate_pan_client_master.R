aggregatePan <- function(cli.data) {
    
    colnames(cli.data) <- c("dp_id", "client_id",
                            "ac_category", "pan_1",
                            "pan_2", "pan_3",
                            "occupation",
                            "district_code",
                            "client_type",
                            "account_opening_date",
                            "account_modification_date",
                            "account_status",
                            "account_closing_date"
                            )
    
    cli.data$status <- 0
    cli.data$status[
        which(is.na(cli.data$account_closing_date))] <- 1
    cli.data$account_opening_date <-
        as.Date(cli.data$account_opening_date)
    
    cli.data <- cli.data[order(cli.data[["client_id"]],
                               -cli.data[["status"]],
                               rev(cli.data[["account_opening_date"]])), ]
    cli.data <- cli.data[!duplicated(cli.data$pan_1), ]

    cli.data$status    <- NULL
    cli.data$client_id <- NULL
    cli.data$client_id <- cli.data$pan_1
    
    
    cli.data <- cli.data[, c("dp_id", "client_id",
                            "ac_category", "pan_1",
                            "pan_2", "pan_3",
                            "occupation",
                            "district_code",
                            "client_type",
                            "account_opening_date",
                            "account_modification_date",
                            "account_status",
                            "account_closing_date"
                            )]

    return(cli.data)
}


