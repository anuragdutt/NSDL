##################################################################################################
## The program reads FPI holdings file on the last date of every month
## for the years specified by the user. And returns a table showing
## composition of long term CoBos, short term CoBos, CP's, equities
## and others as a percentage of total FPI holdings for that day. 
###################################################################################################


rm(list = ls())
options(scipen = 999)

## Library
require(xtable)

## Load datasets
cp.isins <-load("../DATA/cp_isins.rda")
lt.cobo.isins <- load("../DATA/longTermCoBo_isins.rda")
st.cobo.isins <- load("../DATA/shortTermCoBo_isins.rda")
equity.isins <- load("../DATA/close_price_all_stocks.Rdata")
equity.isins <- unique(isin.dat$ISIN)

## FPI holdings files
read.path <- "../DATA/home/nsdl/RESULTS/CAPITAL_CONTROLS/DATA/"
all.files <-list.files(read.path)


## Function returning the list of FPI holdings file to be read for the
## last day of every month in the year specified by the user.
getFileForLastDateOfMonths <- function(year){
    req.files <- all.files[grep(year, all.files)]
    all.dates <- as.Date(substr(req.files, 13, 22))
    all.months <- format(all.dates, "%m")
    all.months <- all.months[!duplicated(all.months)]
    by.months <- do.call('rbind', lapply(all.months, function(x){
        print(x)
        month.files <- req.files[grep(paste0("-",x,"-"), req.files,
                                       fixed =TRUE)]
        last.date <- month.files[length(month.files)]
        ret.file <- last.date
        return(ret.file)
    })
    )
    return(by.months)
}

getFilesByYear <- function(start.yr, end.yr){
    years <- as.character(seq(start.yr, end.yr, 1))
    by.years <- do.call('rbind', lapply(years, function(y){
        print(y)
        ret.val <- getFileForLastDateOfMonths(y)
        return(ret.val)
    })
    )
    return(by.years) 
}
 
files.to.read <- getFilesByYear(start.yr = 2015,
                                end.yr = 2016)

read.files <- function(files){
    by.files <- do.call('rbind', lapply(files, function(x){
        load(paste0(read.path,x)) 
        
        return(holdings)
        
    })
    )
    return(by.files)
}
all.data <- read.files(files = files.to.read)

compute.percentage.comp <- function(files){
    by.files <- do.call('rbind', lapply(files, function(x){
        load(paste0(read.path,x)) 
        
        dt <- as.Date(substr(x, 13, 22))

        total.qty <- sum(holdings$qty)
        
        lt.cobos <- holdings[which(holdings$isin %in% ltCobo_isins), ]
        lt.cobos.qty <- sum(lt.cobos$qty)
        lt.cobos.pc <- (lt.cobos.qty/total.qty)*100

        st.cobos <- holdings[which(holdings$isin %in% stCobo_isins), ]
        st.cobos.qty <- sum(st.cobos$qty)
        st.cobos.pc <- (st.cobos.qty/total.qty)*100


        cps <- holdings[which(holdings$isin %in% cp_isins), ]
        cps.qty <- sum(cps$qty)
        cps.pc <- (cps.qty/total.qty)*100

        equity <- holdings[which(holdings$isin %in% equity.isins), ]
        equity.qty <- sum(equity$qty)
        equity.pc <- (equity.qty/total.qty)*100

        others <- holdings[-which(holdings$isin %in% c(ltCobo_isins,
                                                       stCobo_isins,
                                                       cp_isins,
                                                       equity.isins)),
                                                       ]
        others.qty <- sum(others$qty)
        others.pc <- (others.qty/total.qty)*100

        ret.val <- data.frame(lt.cobos.pc,
                              st.cobos.pc,
                              cps.pc,
                              equity.pc,
                              others.pc)
        rownames(ret.val) <- dt
        return(ret.val)
        
    })
    )
    return(by.files)
}


percentage.composition <- compute.percentage.comp(files = files.to.read)


print(xtable(percentage.composition), only.contents = FALSE, comment = FALSE,
      include.rownames = TRUE, digits = 6,
      hline.after = c(-1,0,nrow(percentage.composition)), include.colnames = TRUE,
      file = "../../DOC/TABLES/fpi_holdings_composition.tex") 




























