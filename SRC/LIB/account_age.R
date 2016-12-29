ageAccount <- function(dataset, dt) {
    dt <- as.Date(dt)
    dataset$account_opening_date <- as.Date(dataset$account_opening_date)
    dataset <- dataset[order(dataset[["client_id"]]), ]
    dt.open <- dataset$account_opening_date
    dataset$age_account <- dt - dt.open
    retval  <- dataset[, c("client_id", "age_account")]  
    retval  <- data.frame(retval)
    return(retval)

}

ageClosedAccount <- function(dataset) {
    dataset$age_account <-
        as.Date(dataset$close_opening_date) - as.Date(dataset$account_opening_date)
    retval  <- dataset[, c("client_id", "age_account")]
    retval  <- data.frame(retval)
    return(retval)

}
