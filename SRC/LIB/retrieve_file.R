##' Search the holdings file for a given date and locate the path of
##' the file in the DATAMART
##'
##' 
##' @title retrieveFile
##' @param dt date for which file is to be searched
##' @param ftype holdings or client
##' @return path of the file for the given date or date.seq
##' @author Anurag Dutt
retrieveFile <- function(dt,
                         ftype = "holdings") {
    dt <- as.Date(dt)
    dt <- format(dt, format="%Y%m%d")
    s.path1   <- "/home/nsdl/DATAMART1"
    s.path2   <- "/home/nsdl/DATAMART2"
    s.path3   <- "/home/nsdl/DATAMART3"

    all.dirs1 <- list.dirs(path = s.path1, recursive = F,
                           full.names = T)
    all.dirs2 <- list.dirs(path = s.path2, recursive = F,
                           full.names = T)
    all.dirs3 <- list.dirs(path = s.path3, recursive = F,
                           full.names = T)

    all.dirs  <- sort(unique(c(all.dirs1, all.dirs2, all.dirs3)))
    
    if(length(grep("lost", all.dirs)) > 0) {
        all.dirs <- all.dirs[-grep("lost", all.dirs)]
    }
    path.retrieve <- NULL
    for(i in dt) {
        if(length(grep(i, all.dirs)) > 0) {
            path <- all.dirs[grep(i, all.dirs)]
            path.retrieve <- c(path.retrieve, path)
        }
    }
    f.return <- NULL
    if(length(path.retrieve) > 0) {
        lapply(path.retrieve, function(p) {
            f.all <- list.files(p, full.names = FALSE)
            if(ftype == "holdings") {
                dt.f   <- as.Date(unlist(strsplit(p, "/"))[5],
                                  format = "%Y%m%d")
                f.name <- paste0("IGIDR_POSITION_",
                                 format(as.Date(dt.f, "%Y%m%d"),
                                        "%d%m%Y"),
                                 ".CSV.gz")
                if(length(grep(f.name, f.all)) > 0) {
                    f.path  <- paste0(p,
                                      "/",
                                      f.name)
                                     
                    f.return <<- c(f.return, f.path)
                }
            }
        }
        )
    }
    return(f.return)
}

