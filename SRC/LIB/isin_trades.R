## This script is used to find the traded quantity for list of ISINs and
## list of dates sent to function


                                        # dummy vector with all available dates

getDistribution <- function(inx, dat, psu, nifty, niftyjr) {
    d <- dat[eval(parse(text = inx)), ]
    psu.count     <- length(which(d$isin %in% psu))
    nifty.count   <- length(which(d$isin %in% nifty))
    niftyjr.count <- length(which(d$isin %in% niftyjr))

    retval <- c(unique(d$client_id), psu.count, nifty.count, niftyjr.count)
    return(retval)
}
                                     
                                     
                                 

readConstants <- function(file.name) {
    res <- as.list(NULL)
    for(i in 1:length(file.name)) {
        f <- read.csv(file.name[i], stringsAsFactors = FALSE)
        res[[i]] <- f
        names(res)[i] <- unlist(strsplit(unlist(strsplit(file.name[i], ".CSV")[1]), "/"))[length(unlist(strsplit(unlist(strsplit(file.name[i], ".CSV")[1]), "/")))]
    }
    return(res)
}
    



isinTrade <- function(df) # take all if not given
    {
                           
        
        df <- data.frame(df, stringsAsFactors = FALSE)

        const.files <- list.files(path = "/home/nsdl/CONSTANTS",
                                  full.names = TRUE)
        const.files <- const.files[
            c(grep("niftyStocks.csv", const.files),
              grep("niftyJuniorStocks.csv", const.files),
              grep("psuStocks.csv", const.files)
              )]
        
        const.dat <- readConstants(const.files)
        
                                        # getting traded quantity for each isin
        
        h <- df
        h <- h[order(h[["client_id"]],
                     h[["isin"]]), ]
        s <- seq_along(h$client_id)[!duplicated(h$client_id)]
        e <- seq_along(h$client_id)[!duplicated(h$client_id, fromLast = TRUE)]
        i <- paste0(sep = "c(", s, sep = ":", e, sep = ")")
                             
        dis <- do.call('rbind', lapply(i,
                                       function(ix, dat, psu, nifty, niftyjr)
                                           getDistribution(ix, dat, psu, nifty, niftyjr),
                                       dat = h,
                                       psu = const.dat$psuStocks$ISIN.Code,
                                       nifty = const.dat$niftyStocks$ISIN.Code,
                                       niftyjr = const.dat$niftyJuniorStocks$ISIN.Code
                                       ))
                                        #result <- dis
                             colnames(dis) <- c("client_id", "psu", "nifty", "niftyjr")
                                        #   names(result) <- dis
                                        #save(result, file = paste0("../../RESULTS/market-composition-", d, ".Rdata"))
                             
        result <- data.frame(dis, stringsAsFactors = F)
        return(result)
        
        
    }


                   
#isinTrade(all.files, "all", as.Date("2014-10-01"))











    



           
