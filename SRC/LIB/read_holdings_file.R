##' Read te holdings file for a given date
##'
##' 
##' @title readFile 
##' @param fpath path to the holdings file
##' @param only.nse remove acounts with unknown isins
##' @param aggregate aggregate on PAN1 field, needs client data
##' @param cli.data client data needed for aggregation on PAN
##' @return the entire holdings file for a given date
##' @author Anurag Dutt
readFile <- function(fpath,
                     only.nse = TRUE,
                     aggregate = FALSE,
                     pan.data = NULL
                     ) {
    
    library(data.table)
    if(aggregate == TRUE && is.null(pan.data)) {
        stop("Can't proceed. Need client data to aggregate on PAN")
    }
    if(is.null(fpath) || is.na(fpath)) {
        stop("No file path found. Cowardly exiting the run.")
    }
    
    tmp              <- tempfile()
    sys.com          <- paste0("gzip -cd ", fpath, " > ", tmp)
    system(sys.com)
    f.data           <- fread(tmp)
##    f.data           <- data.frame(f.data)
    if(length(which(colnames(f.data) %in% Var.1)) > 0) {
        f.data$Var.1     <- NULL
    }
    if(length(which(colnames(f.data) %in% Var.1)) > 0) {
        f.data$V1     <- NULL
    }
##     colnames(f.data) <- NULL
##     rownames(f.data) <- NULL
    file.remove(tmp)
    colnames(f.data) <- c("dp_id",
                          "client_id",
                          "date",
                          "isin",
                          "qty")
    
    dt <- as.Date(unlist(strsplit(
        unlist(strsplit(fpath, ".CSV.gz"))[1],
        "_"))[3], format = "%d%m%Y")
    f.data$date <- dt
    f.data <- f.data[order(f.data[["client_id"]],
                           f.data[["isin"]]), ]

    if(only.nse == TRUE) {
        load("/home/nsdl/CONSTANTS/close_price_all_stocks.Rdata")
        colnames(isin.dat) <- tolower(colnames(isin.dat))
        dt.isin  <- as.Date(unique(f.data$date))
        for(i in c(1:10)) {
            if(length(which(dt.isin %in%
                            unique(isin.dat$date))) == 0) {
                dt.isin = dt.isin - 1
            }
        }
                
        
        isin.dat <- isin.dat[which(isin.dat$date == dt.isin), ]
        isins <- unique(isin.dat$isin)
        f.data <- f.data[which(f.data$isin %in% isins), ]
    }


    if(aggregate == TRUE) {
        colnames(pan.data) <- c("client_id", "pan_1")
        pan.data <- data.table(pan.data)
        f.data <- merge(f.data, pan.data,
                        by = "client_id",
                        all.x = TRUE)

        f.data <- f.data[!is.na(f.data$pan_1), ]
        f.data <- f.data[!is.na(f.data$isin), ]


        f.data$client_id <- NULL
        f.data$client_id <- f.data$pan_1
        f.data$pan_1 <- NULL

        f.data <- f.data[order(f.data[["client_id"]],
                               f.data[["isin"]]), ]
        f.data$client_isin <- paste0(f.data$client_id,
                                  sep = "-",
                                  f.data$isin)
        sum.isin <- f.data[, total:=sum(qty),
                           by = list(client_isin)][, c("client_isin",
                                                       "total"),
                                                   with = FALSE]
        colnames(sum.isin) <- c("client_isin", "qty")
        f.data$qty <- NULL
        holdings   <- merge(f.data, sum.isin,
                            by ="client_isin",
                            all.x = TRUE,
                            )
        
        holdings  <- holdings[, c("dp_id",
                                  "client_id",
                                  "date",
                                  "isin",
                                  "qty",
                                  "client_isin"), with = FALSE]

        holdings  <- holdings[order(holdings[["client_id"]],
                                    holdings[["isin"]]), ]
        holdings  <- holdings[!duplicated(holdings$client_isin), ]

        holdings$client_isin <- NULL
        
        colnames(holdings) <- c("dp_id",
                                "client_id",
                                "date",
                                "isin",
                                "qty")
        
    } else {
        holdings <- f.data
    }
                          
    return(holdings)

}
