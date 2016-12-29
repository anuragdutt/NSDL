##' Read the client master file for a given date
##' @title readClientBeforeOct2010
##' @param c.path path to client master
##' @return data.table containg the entire client master details for a
##'     given date
##' @author Anurag Dutt
readClientBeforeOct2015 <- function(c.path,
                                    on.ctype = NULL) {
    library(data.table)
    library(bit64)
    
    f.d <- fread(c.path)
    f.d <- data.frame(f.d)

    colnames(f.d) <- c("dp_id", "client_id",
                       "ac_category", "pan_1",
                       "pan_2", "pan_3",
                       "occupation", "district_code",
                       "client_type",
                       "account_opening_date",
                       "account_modification_date",
                       "account_status",
                       "account_closing_date"
                       )

    f.d$pan_1 <- as.character(f.d$pan_1)
    f.d$pan_2 <- as.character(f.d$pan_2)
    f.d$pan_3 <- as.character(f.d$pan_3)

    if(length(duplicated(f.d$client_id)) > 0) {
        f.d <- f.d[!duplicated(f.d$client_id), ]
    }
    if(!is.na(as.Date(f.d$account_opening_date[!is.na(f.d$account_opening_date)][1], format = "%d-%b-%Y"))) {
        f.d$account_opening_date <- as.Date(tolower(f.d$account_opening_date),
                               format="%d-%b-%Y")
    }
    if(!is.na(as.Date(f.d$account_opening_date[!is.na(f.d$account_opening_date)][1], format = "%y-%m-%d"))) {
        f.d$account_opening_date <- as.Date(tolower(f.d$account_opening_date),
                               format="%d-%b-%Y")
    }

    if(!is.na(as.Date(f.d$account_modification_date[!is.na(f.d$account_modification_date)][1], format = "%d-%b-%Y"))) {
        f.d$account_modification_date <- as.Date(tolower(f.d$account_modification_date),
                               format="%d-%b-%Y")
    }
    
    if(!is.na(as.Date(f.d$account_modification_date[!is.na(f.d$account_modification_date)][1], format = "%y-%m-%d"))) {
        f.d$account_modification_date <- as.Date(tolower(f.d$account_modification_date),
                               format="%d-%b-%Y")
    }

    if(!is.na(as.Date(f.d$account_closing_date[!is.na(f.d$account_closing_date)][1], format = "%d-%b-%Y"))) {
        f.d$account_closing_date <- as.Date(tolower(f.d$account_closing_date),
                               format="%d-%b-%Y")
    }

    if(!is.na(as.Date(f.d$account_closing_date[!is.na(f.d$account_closing_date)][1], format = "%y-%m-%d"))) {
        f.d$account_closing_date <- as.Date(tolower(f.d$account_closing_date),
                               format="%d-%b-%Y")
    }

    
    if(!is.null(on.ctype)) {
        on.ctype <- as.numeric(on.ctype)
        f.d$client_type <- as.numeric(f.d$client_type)
        f.d <- f.d[which(f.d$client_type %in% on.ctype), ]
    }
    
    return(f.d)

}
