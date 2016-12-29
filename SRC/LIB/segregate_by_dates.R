segregateFiles <- function(files) {
    client.date   <- NULL
    tran.date     <- NULL
    holding.date  <- NULL
    lapply(files, function(path) {
               dt <- NULL
#               print(path)
               if(length(grep("IGIDR_CLIENT_MASTER", path, fixed = F)) > 0) {
                   dt <- unlist(strsplit(unlist(strsplit(path,
                                                         ".CSV"))[1],
                                         "IGIDR_CLIENT_MASTER_"))[2]
                   client.date <<- c(client.date, dt)
               }
               if(length(grep("IGIDR_TRANSACTION", path, fixed = F)) > 0) {
                   dt <- unlist(strsplit(unlist(strsplit(path,
                                                         ".CSV"))[1],
                                         "IGIDR_TRANSACTION_"))[2]
                   tran.date <<- c(tran.date, dt)
               }
               if(length(grep("IGIDR_POSITION", path, fixed = F)) > 0) {
                   dt <- unlist(strsplit(unlist(strsplit(path,
                                                         ".CSV"))[1],
                                         "IGIDR_POSITION_"))[2]
                   holding.date <<- c(holding.date, dt)
               }
               
           }
           )
    
    retval <- list(c = client.date,
                   t = tran.date,
                   h = holding.date
                   )

    return(retval)


}












    



           
