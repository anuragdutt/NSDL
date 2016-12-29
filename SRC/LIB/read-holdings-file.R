readFile <- function(fpath){
    library(data.table)
    tmp              <- tempfile()
    sys.com          <- paste0("gzip -cd ", fpath, " > ", tmp)
    system(sys.com)
    f.data           <- fread(tmp)
    f.data           <- data.frame(f.data)
    f.data$Var.1     <- NULL
    colnames(f.data) <- NULL
    rownames(f.data) <- NULL
    file.remove(tmp)
    colnames(f.data) <- c("dp_id",
                          "client_id",
                          "date",
                          "isin",
                          "qty")
    f.data$date <- as.Date(tolower(f.data$date), format="%d-%b-%Y")
                          
    return(f.data)

}
