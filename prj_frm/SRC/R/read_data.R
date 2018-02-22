###############################
## Fixing program parameters ##
###############################

rm(list = ls())
options(warn = -1, digits = 3, scipen = 999)


###############################
## Initiate global variables ##
###############################

file.path <- "../DATA/frm.json"
mandatory.keys <- c("area", "subarea", "explanation")
graph.path <- "../../DOC/GRAPHS/"
fn.constant <- "frm_index_"

event.lf <- c("Concepts", "DraftLaw", "LawEnacted", "LawImplemented")
event.m  <-
    c("MarketInfrastructure",
      "EntryBarriersIntoIntermediation",
      "SoundRegulatoryArrangement",
      "SoundRegulations",
      "EnforcementOfRegulations")
event.ff <- c("LicensingEntryBarriers",
              "SoundRegulatoryArrangement",
              "SoundRegulations",
              "EnforcementOfRegulations")
event.mof <- c("Concepts",
               "FormalDocumentsReadied",
               "Implemented")
multiplier <- 100
yr <- FALSE

########################
## Initiate libraries ##
########################
    
library(jsonlite)
library(data.table)
library(parallel)
library(zoo)
library(xtable)
library(optparse)

##########################
## source library files ##
##########################

source("get_scores.R")
source("plot_measures.R")
source("generate_reports.R")

##############################
## Declare global variables ##
##############################

start.yr <- as.Date("2005-01-01")
## end.yr <- as.numeric(format(as.Date(Sys.time()), "%Y"))
end.yr <- as.Date(Sys.time())
######################
## Read scores file ##
######################

frm.dat <- fromJSON(file.path)

#################
## Define keys ##
#################

keys <- unlist(lapply(frm.dat, function(x) {
    retval <- x$subarea
    retval
})
)

if(length(keys) != length(frm.dat)) {
    stop("Missing keys. Please readjust")
}

areas <- unlist(lapply(frm.dat, function(x) {
    retval <- x$area
    retval
})
)

subheadings <- lapply(frm.dat, function(obj) {
    sections <- names(obj)
    if(length(which(sections %in% mandatory.keys)) > 0) {
        sections <- sections[!sections %in% mandatory.keys]
    }
    sections
})

names(subheadings) <- paste0(areas, sep = "_", keys)

total.subheadings  <- 1:length(subheadings)


ind.weights <- mapply(getWeight, subheadings, total.subheadings,
                     SIMPLIFY = FALSE, USE.NAMES = TRUE)
                     

ind.scores <- mapply(getScore, subheadings, total.subheadings,
                     SIMPLIFY = FALSE, USE.NAMES = TRUE)


section.scores <- alignSectionScores(ind.scores = ind.scores,
                                     ind.weights = ind.weights)


denom <- calDenom(index.weights = ind.weights,
                  subsection = NA,
                  section = NA)

build.series <- mapply(function(ss, lbl) {
                                        #check event lf
    print(lbl)
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
    sec.t <- lapply(event, function(e) {
        e <- tolower(e)
        s <- ss[which(ss$subsection == e), ]
        if(!is.null(s)) {
            if(nrow(s) > 0) {
                time.series <- buildTimeSeries(index = s,
                                               start.yr = start.yr,
                                           end.yr = end.yr,
                                           year = yr)
            colnames(time.series) <- c("date", paste0("s_", e),
                                       paste0("w_", e))
            } else {
                yr.series <- as.Date(seq(from = start.yr,
                                         to = end.yr,
                                         by = "day"))
                time.series <- data.table(date = yr.series)
                time.series$score <- 0
                time.series$weight <- 0
                colnames(time.series) <- c("date", paste0("s_", e),
                                           paste0("w_", e))
            }
        } else {
            yr.series <- as.Date(seq(from = start.yr,
                                     to = end.yr,
                                     by = "day"))
            time.series <- data.table(date = yr.series)
            time.series$score <- 0
            time.series$weight <- 0
            colnames(time.series) <- c("date", paste0("s_", e),
                                       paste0("w_", e))          
        }
        return(time.series)
    })
    sec.a <- Reduce(function(x, y) merge(x, y, all = TRUE, by = "date"),
                    sec.t)
    sec.a
}, section.scores, names(subheadings), SIMPLIFY = FALSE, USE.NAMES = TRUE
)


dates.dat <- build.series[[1]]$date
sub <- lapply(1:length(build.series), function(s) {
    section <- unlist(strsplit(names(build.series)[[s]], "_"))[1]
    subsection <- unlist(strsplit(names(build.series)[[s]], "_"))[2]
    d <- calDenom(index.weights = ind.weights,
                  section = section,
                  subsection = subsection)
    tmp <- build.series[[s]]
    mul <- build.series[[s]]
    mul.w <- mul[, grep("w_", colnames(mul)), with = FALSE]
    mul.s <- mul[, grep("s_", colnames(mul)), with = FALSE]

    tm <- mul.w*mul.s

    sm <- rowSums(tm)
    wm <- sm*multiplier/d

    time.s <- data.table(date = tmp$date,
                         index = wm)

    retval <- data.table(section = section,
                         subsection = subsection,
                         index = tail(wm, 1))
    
    f.n <-paste0(gsub(" ", "_", tolower(section)), sep = "_",
                 gsub(" ", "_", tolower(subsection)))
    f.n <- gsub(",", "", f.n)
    plotIndexDaily(mdata = time.s,
                   y.lim = 100,
                   increment = 10,
                   filename = paste0(graph.path,
                                     fn.constant,
                                     f.n,
                                     ".png"),
                   round.vec = 0,
                   mai.vec = c(0.5, 4.5, 2, 1.8),
                   mgp.vec = c(14.5, 4.5 ,0),
                   mtext.line = 11.5,
                   legend.vec = c(paste0(section, sep = "-", subsection)),
                   col.vec = c("darkred"),
                   lty.vec = c(1),
                   lwd.vec = c(10),
                   ncol.vec = 1,
                   cex.vec = 9.5,
                   y.jump = 0
                   )
    ret <- list(retval, sm)
    return(ret)
})

subsec <- lapply(sub, function(x) {
    return(x[[1]])
})
sm <- lapply(sub, function(x) {
    return(x[[2]])
})

all.sum <- do.call('cbind', sm)
col <- names(subheadings)
all.sum <- data.table(all.sum)
colnames(all.sum) <- col
all.sum$date <- dates.dat

subsec <- rbindlist(subsec)

sec <- unlist(strsplit(names(section.scores), "_"))[seq(from = 1,
                                                        to = 2*length(subheadings),
                                                        by = 2)]
sec.u <- unique(sec)
names(sm) <- names(section.scores)

lapply(sec.u, function(s) {
    col.sec <- grep(s, colnames(all.sum))
    time.series <- all.sum[, col.sec, with = FALSE]
    sub   <- subsec[which(subsec$section == s), ]
    filename <- paste0("frm_index_", gsub(" ", "_", tolower(s)), ".tex")
#denom calculation
    
    den <- calDenom(index.weights = ind.weights,
                    section = s,
                    subsection = NA)
    print(xtable(sub, digits = 1,
                 caption = NULL
                 ),
          only.contents = TRUE,
          hline.after = NULL,
          include.rownames = FALSE,
          include.colnames = FALSE,
          type = "latex",
          sanitize.text.function = function(x) x,
          file = paste0("../../DOC/TABLES/", filename))
    
    
    wm <- rowSums(time.series)*100/den
    
    time.series <- data.table(date = dates.dat,
                              index = wm)
    
    f.n <- gsub(" ", "_", tolower(s))
    f.n <- gsub(",", "", f.n)
    plotIndexDaily(mdata = time.series,
                   y.lim = 100,
                   increment = 10,
                   filename = paste0(graph.path,
                                     fn.constant,
                                     f.n,
                                     ".png"),
                   round.vec = 0,
                   mai.vec = c(1, 4.5, 2, 1.8),
                   mgp.vec = c(14.5, 4.5 ,0),
                   mtext.line = 11.5,
                   legend.vec = c(paste0(s, " index")),
                   col.vec = c("darkred"),
                   lty.vec = c(1),
                   lwd.vec = c(10),
                   ncol.vec = 1,
                   cex.vec = 9.5,
                   y.jump = 0
                   )

})

mul <- all.sum
mul$date <- NULL
wm.t <- rowSums(mul)*100/denom

time.series <- data.table(date = dates.dat,
                          index = wm.t)


f.n <- "financial_reforms_index"
plotIndexDaily(mdata = time.series,
               y.lim = 100,
               increment = 10,
               filename = paste0(graph.path,
                                 fn.constant,
                                 f.n,
                                 ".png"),
               round.vec = 0,
               mai.vec = c(0.5, 4.5, 2, 1.8),
               mgp.vec = c(14.5, 4.5 ,0),
               mtext.line = 11.5,
               legend.vec = c("Overall financial reforms index"),
               col.vec = c("darkred"),
               lty.vec = c(1),
               lwd.vec = c(10),
               ncol.vec = 1,
               cex.vec = 9.5,
               y.jump = 0
               )
rm(f.n)

generateTables(frm.dat = frm.dat,
               subheadings = subheadings)

