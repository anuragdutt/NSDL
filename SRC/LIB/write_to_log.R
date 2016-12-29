appendLogs <- function(msg, path.to.log, file.name, append.mode, status){

    file.path <- paste0(path.to.log, "/", file.name)
    cat(paste0(Sys.time(), " : ", status, " : ", msg, ";\n"),
        file = file.path,
        append = append.mode)

}

createLogs <- function(path.to.log, file.name) {
    file.path <- paste0(path.to.log, "/", file.name)
    file.create(file.path)
    
}
