readGlobalFile <- function(path.to.global) {
    global.variables <- scan(path.to.global, what = 'character',
                              allowEscapes = TRUE)
    for(i in 1:length(global.variables))
    {
        str <- unlist(strsplit(global.variables[i], '='))
        assign(str[1], str[2], envir = .GlobalEnv)
    }

}


