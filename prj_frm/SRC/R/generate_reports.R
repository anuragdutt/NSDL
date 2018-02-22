generateTables <- function(frm.dat,
                           subheadings) {

    mapply(function(ss, sh, lbl) {
        sh <- unlist(sh)
        area <- unlist(strsplit(lbl, "_"))[1]
        subarea <- unlist(strsplit(lbl, "_"))[2]
        if(area == "Legal foundations" &&
           subarea == "Ministry of finance restructuring"
           ) {
            event <- event.mof
        }
        if(area == "Legal foundations" &&
           subarea != "Ministry of finance restructuring"
           ) {
            event <- event.lf
        }
        if(area == "Markets") {
            event <- event.m
        }
        if(area == "Financial firms") {
            event <- event.ff
        }
        
        if(length(sh) != length(event)) {
            event <- event[-which(event %in% sh)]
        } else {
            event <- NULL
        }
        if(length(sh) > 0) {
            
            lapply(sh, function(sub.h) {
                str <- paste0("ss$", sub.h, "$events")
                dat <- eval(parse(text = str))
                if(!is.null(dat)) {
                    dat$url <- paste0("\\url{", dat$url, "}")
                    dat$score <- paste0("\\colorbox{lightgray}{", dat$score, "}")
                    dat$story <- gsub("%", "\\%", dat$story, fixed = TRUE)
                    filename <- paste0(gsub(" ", "_",
                                            tolower(paste(ss$area,
                                                          gsub(",",
                                                               "",ss$subarea), sub.h))), ".tex")
                    print(xtable(dat, digits = 2,
                             caption = NULL
                             ),
                      only.contents = TRUE,
                      hline.after = NULL,
                      include.rownames = FALSE,
                      include.colnames = FALSE,
                      type = "latex",
                      sanitize.text.function = function(x) x,
                      file = paste0("../../DOC/TABLES/", filename))
                } else {
                    dat <- data.table(date = "-",
                                      milestine = "-",
                                      url = "-",
                                      story = "-",
                                      score = "0.0")
                    dat$url <- paste0("\\url{", dat$url, "}")
                    dat$story <- gsub("%", "\\%", dat$story, fixed =
                                                                 TRUE)
                    dat$score <- paste0("\\colorbox{lightgray}{", dat$score, "}")
                    
                    filename <- paste0(gsub(" ", "_",
                                            tolower(paste(ss$area,
                                                          gsub(",",
                                                               "",ss$subarea), sub.h))), ".tex")
                    print(xtable(dat, digits = 2,
                                 caption = NULL
                                 ),
                          only.contents = TRUE,
                          hline.after = NULL,
                          include.rownames = FALSE,
                          include.colnames = FALSE,
                          type = "latex",
                          sanitize.text.function = function(x) x,
                          file = paste0("../../DOC/TABLES/", filename))
                    
                }
            })
        }
        if(length(event) > 0) {
            
            lapply(event, function(sub.h) {
                dat <- data.table(date = "-",
                                  milestine = "-",
                                  url = "-",
                                  story = "-",
                                  score = "0.0")
                    dat$url <- paste0("\\url{", dat$url, "}")
                    dat$story <- gsub("%", "\\%", dat$story, fixed =
                                                                 TRUE)
                    dat$score <- paste0("\\colorbox{lightgray}{", dat$score, "}")
                    
                    filename <- paste0(gsub(" ", "_",
                                            tolower(paste(ss$area,
                                                          gsub(",",
                                                               "",ss$subarea), sub.h))), ".tex")
                    print(xtable(dat, digits = 2,
                                 caption = NULL
                                 ),
                          only.contents = TRUE,
                          hline.after = NULL,
                          include.rownames = FALSE,
                          include.colnames = FALSE,
                          type = "latex",
                          sanitize.text.function = function(x) x,
                          file = paste0("../../DOC/TABLES/", filename))
            })
        }
    }, frm.dat, subheadings, names(subheadings), SIMPLIFY = FALSE)

}

    
