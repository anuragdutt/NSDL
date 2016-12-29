#dir.path <- "/home/nsdl/RESULTS/NESTLE_HOLDINGS"
##' Takes in path as an argument and checks whether a directory exists
##'
##' Creates the directory doesn't exist
##' @title dirExist 
##' @param dir.path path of the directory 
##' @return NULL
##' @author Anurag Dutt
dirExist <- function(dir.path) {
    if(!dir.exists(dir.path)) {
        dir.create(dir.path)
    }
}

##' Takes in path as an argument and checks whether a file exists
##'
##' Creates the file doesn't exist
##' @title fileExist 
##' @param file.path path of the file
##' @return NULL
##' @author Anurag Dutt
fileExist <- function(file.path) {
    if(!file.exists(file.path)) {
        file.create(file.path)
    }
}
