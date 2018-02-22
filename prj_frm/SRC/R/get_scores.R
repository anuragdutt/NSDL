getScore <- function(sh, inx) {
    score.strings <- paste0(sep = paste0("frm.dat$'",
                                                   inx, "'$"
                                                   ),
                                      sh,
                                      sep = "$events$score"
                             )

    date.strings <- paste0(sep = paste0("frm.dat$'",
                                                   inx, "'$"
                                                   ),
                                      sh,
                                      sep = "$events$date"
                             )
    score <- mapply(function(str.sc, str.dt) {
        scr  <- eval(parse(text = str.sc))
        date <- eval(parse(text = str.dt))
        if(is.null(scr)) {
            scr <- 0
        }
        names(scr) <- date
        return(scr)
    }, score.strings, date.strings, SIMPLIFY = FALSE)
    score
}


getWeight <- function(sh, inx) {
    weight.strings <- paste0(sep = paste0("frm.dat$'",
                                                   inx, "'$"
                                                   ),
                                      sh,
                                      sep = "$weight"
                             )
    
    weight <- sapply(weight.strings, function(str) {
        return(eval(parse(text = str)))
    })
    names(weight) <- NULL
    weight    
}



alignSectionScores <- function(ind.scores, ind.weights) {
    
    retval <- mapply(function(scr, wgt) {
        sections <- unlist(strsplit(names(scr), "\\$"))[seq(from = 3,
                                                            to = 5*length(scr),
                                                            by = 5
                                                            )]
        sections = tolower(sections)
        get.scores <- mapply(function(sc, wg, sec) {
            sc <- sc[!names(sc) == ""]
            sc <- sc[!is.null(names(sc))]
            sc <- sc[!is.na(names(sc))]
            if(length(sc) > 0) {
                    retval <- data.table(date = names(sc),
                                         score = sc,
                                         weight = wg,
                                         subsection = sec)
            } else {
                retval <- NULL
            }
            return(retval) 
            
            }, scr, wgt, sections, SIMPLIFY = FALSE, USE.NAMES = TRUE)
        
        scores <- do.call('rbind', get.scores)
        return(scores)
    }, ind.scores, ind.weights, SIMPLIFY = FALSE, USE.NAMES = TRUE)

    retval
}

getTotalScores <- function(section.scores,
                           denom,
                           multiplier = 100,
                           year = FALSE) {
    if(class(section.scores) == "list") {
        if(year){
            dat.tot <- do.call('rbind', section.scores)
            dat.tot$date <- as.Date(dat.tot$date)
            dat.tot <- dat.tot[order(dat.tot$date), ]
            dat.tot$year <- format(dat.tot$date, "%Y")
            dat.tot$wm <- dat.tot$score*dat.tot$weight
            year.scr <- dat.tot[, total_wm:=sum(wm),
                                by = list(year)][, c("year", "total_wm"),
                                                 with = FALSE]
            year.scr <- year.scr[!duplicated(year.scr$year), ]
            year.scr$index <- year.scr$total_wm/denom
#            year.scr$index <- cumsum(year.scr$index)
#            year.scr$index <- year.scr$index*multiplier
        } else {
            dat.tot <- do.call('rbind', section.scores)
            dat.tot$date <- as.Date(dat.tot$date)
            dat.tot <- dat.tot[order(dat.tot$date), ]
            dat.tot$wm <- dat.tot$score*dat.tot$weight
            year.scr <- dat.tot[, total_wm:=sum(wm),
                                by = list(date)][, c("date", "total_wm"),
                                                 with = FALSE]
            year.scr <- year.scr[!duplicated(year.scr$date), ]
            year.scr$index <- year.scr$total_wm/denom
#            year.scr$index <- cumsum(year.scr$index)
#            year.scr$index <- year.scr$index*multiplier

        }
    } else {
        if(year) {
            dat.tot <- section.scores
            dat.tot$date <- as.Date(dat.tot$date)
            dat.tot <- dat.tot[order(dat.tot$date), ]
            dat.tot$year <- format(dat.tot$date, "%Y")
            dat.tot$wm <- dat.tot$score*dat.tot$weight
            year.scr <- dat.tot[, total_wm:=sum(wm),
                                by = list(year)][, c("year", "total_wm"),
                                                 with = FALSE]
            year.scr <- year.scr[!duplicated(year.scr$year), ]
            year.scr$index <- year.scr$total_wm/denom
 #           year.scr$index <- cumsum(year.scr$index)
 #           year.scr$index <- year.scr$index*multiplier
        } else {
            dat.tot <- section.scores
            dat.tot$date <- as.Date(dat.tot$date)
            dat.tot <- dat.tot[order(dat.tot$date), ]
            dat.tot$wm <- dat.tot$score*dat.tot$weight
            year.scr <- dat.tot[, total_wm:=sum(wm),
                                by = list(date)][, c("date", "total_wm"),
                                                 with = FALSE]
            year.scr <- year.scr[!duplicated(year.scr$date), ]
            year.scr$index <- year.scr$total_wm/denom
#            year.scr$index <- cumsum(year.scr$index)
#            year.scr$index <- year.scr$index*multiplier
            
        }
    }
    year.scr
}


buildTimeSeries <- function(index,
                            start.yr,
                            end.yr,
                            year = FALSE) {
    if(year){
        start.yr <- format(start.yr, "%Y")
        start.yr <- as.numeric(start.yr)
        end.yr <- as.numeric(end.yr)
        yr.series <- start.yr:end.yr
        year.dat <- data.table(year = yr.series)
        index$year <- as.numeric(index$year)
        inx <- merge(year.dat, index, by = "year", all.x = TRUE)
        inx$score <- na.locf(inx$score, na.rm = FALSE)
        
        inx$score[is.na(inx$score)] <- 0
    } else {
        start.yr <- as.Date(start.yr)
        end.yr <- as.Date(end.yr)
        yr.series <- as.Date(seq(from = start.yr,
                                 to = end.yr,
                                 by = "day"))
        year.dat <- data.table(date = yr.series)
        index$date <- as.Date(index$date)
        inx <- merge(year.dat, index, by = "date", all.x = TRUE)
        inx$score <- na.locf(inx$score, na.rm = FALSE)
        inx$weight <- na.locf(inx$weight, na.rm = FALSE)
        inx$score[is.na(inx$score)] <- 0
        inx$weight[is.na(inx$weight)] <- 0
        inx <- inx[, c("date", "score", "weight"), with = FALSE]
    }
    inx
}

calDenom <- function(index.weights,
                     subsection = NA,
                     section = NA
                     ) {
    
    if(is.na(subsection) && is.na(section)) {
        denom <- sum(unlist(index.weights))
    }
    if(!is.na(subsection) & is.na(section)) {
        stop("please provide section")
    }
    if(is.na(subsection) && !is.na(section)) {
        n <- names(index.weights)
        sec <- unlist(strsplit(n, "_"))[seq(from = 1,
                                            to = 2*length(n),
                                            by = 2)]
        sec.name <- which(sec == section)
        index.weights <- index.weights[sec.name]
        denom <- sum(unlist(index.weights))
    }
    if(!is.na(subsection) && !is.na(section)) {
        n <- names(index.weights)
        sec <- unlist(strsplit(n, "_"))[seq(from = 1,
                                            to = 2*length(n),
                                            by = 2)]
        sub <- unlist(strsplit(n, "_"))[seq(from = 2,
                                            to = 2*length(n),
                                            by = 2)]
        sec.name <- which(sec == section)
        index.weights <- index.weights[sec.name]
        sub <- sub[sec.name]
        sub.name <- which(sub == subsection)
        if(length(sub.name) == 0) {
            stop("subsection not available")
        }
        index.weights <- index.weights[sub.name]
        denom <- sum(unlist(index.weights))
    } 
    denom
}


