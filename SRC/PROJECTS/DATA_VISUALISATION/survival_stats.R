options(scipen = 999, digits = 5)
load("/home/nsdl/DATAMART1/client_master_aggregated.Rdata")
library(xtable)
library(survival)
pan.data$account_closing_date <-
    as.Date(as.character(pan.data$account_closing_date),
            format = "%y-%m-%d")
pan.data <- pan.data[-which(pan.data$account_closing_date <
                            pan.data$account_opening_date), ]
pan.data$year_open  <- as.numeric(format(as.Date(pan.data$account_opening_date),
                                         format = "%Y"))
pan.data$year_close <- as.numeric(format(as.Date(pan.data$account_closing_date),
                                         format = "%Y"))
pan.data$month_open  <- as.numeric(format(as.Date(pan.data$account_opening_date),
                                          format = "%m"))
pan.data$month_close <- as.numeric(format(as.Date(pan.data$account_closing_date),
                                          format = "%m"))


pan.data <- pan.data[-which(pan.data$account_opening_date ==
                            "9999-12-31"), ]

len        <- length(unique(as.numeric(pan.data$year_open)))
year.open  <- sort(unique(as.numeric(pan.data$year_open)))
year.close <- sort(unique(as.numeric(pan.data$year_close)))


close.col <- as.data.frame.matrix(table(pan.data$year_open, pan.data$year_close))
if(length(year.open) != length(year.close)) {
    eval(parse(text =
                   paste0("close.col$",
                          "temp",
                          " <- 0")))
    colnames(close.col)[which(colnames(close.col) == "temp")] <-
        year.open[-which(year.open %in% year.close)]
    close.col <- close.col[, sort(colnames(close.col))]
}

close.col[close.col == 0] <- NA
close.col[1, ][which(is.na(close.col[1, ]))] <- 0


open.dat <- table(pan.data$year_open)

open.df   <- do.call('rbind',
                     lapply(1:nrow(close.col),
                            function(yr) {
                                tmp <- open.dat[yr]
                                red.diff <- unlist(
                                    lapply(as.numeric(close.col[yr, ]),
                                           function(num) {
                                               dif <- tmp - num
                                               if(!is.na(num)) {
                                                   tmp <<- tmp - num    
                                               }
                                               
                                        return(dif)
                                    }
                                    )
                                    )
                            }
                            )
                     )
                               
                     


colnames(open.df) <- year.open
rownames(open.df) <- year.open

save(open.df,
     file= "/home/nsdl/RESULTS/DATA_VISUALISATION/survival_matrix.Rdata")

print(xtable(open.df, digits=1),
      hline.after=NULL, only.contents=TRUE,
      include.rownames = FALSE,
      include.colnames=FALSE,
      type="latex",
      file="/home/nsdl/RESULTS/DATA_VISUALISATION/survival_matrix.tex")

count <- 0
open.per <- do.call('rbind',
                    lapply(1:nrow(open.df),
                           function(rn) {
                               retval <-
                                   c(open.df[rn, ]*100/open.dat[rn])
                               count <<- count + 1
                               retval <- round(as.numeric(as.character(retval)), 2)

                               retval[count] <-
                                   paste0("\\colorbox{lightgray}{" ,
                                          retval[count],
                                         "}"
                                         )
                               if((count + 1) <= length(retval)) {
                                   retval[count + 1] <-
                                       paste0("\\colorbox{lightgray}{" ,
                                              retval[count + 1],
                                              "}"
                                              )
                               }
                               if((count + 7) <= length(retval)) {
                                   retval[count + 7] <-
                                       paste0("\\colorbox{lightgray}{" ,
                                              retval[count + 7],
                                              "}"
                                              )
                               }
                               return(retval)
                           }
                           )
                    )


colnames(open.per) <- year.open
rownames(open.per) <- year.open


save(open.per,
     file= "/home/nsdl/RESULTS/DATA_VISUALISATION/survival_percentages.Rdata")

print(xtable(open.per, digits = 2),
      hline.after=NULL, only.contents=TRUE,
      include.rownames = TRUE,
      include.colnames=FALSE,
      type="latex",
      sanitize.text.function = function(x) x,
      file="/home/nsdl/RESULTS/DATA_VISUALISATION/survival_percentages.tex")



library(survival)

dates.dat <- pan.data[, c("client_id",
                          "year_open",
                          "year_close",
                          "account_opening_date",
                          "account_closing_date")]
dates.dat$account_opening_date <- as.Date(dates.dat$account_opening_date)
dates.dat$account_closing_date <- as.Date(dates.dat$account_closing_date)

dates.dat$status <- NA
dates.dat$status[which(is.na(dates.dat$year_close))] <- 1
dates.dat$status[which(is.na(dates.dat$status))] <- 0
dates.dat$year_close[which(is.na(dates.dat$year_close))] <- 2015
dates.dat$account_closing_date[which(is.na(dates.dat$account_closing_date))] <- as.Date("2015-08-31")
dates.dat$year_diff <-
    round(as.numeric(dates.dat$account_closing_date - dates.dat$account_opening_date)/365)
dates.dat$year_diff[which(dates.dat$year_diff == 0)] <- 1

pdf("/home/nsdl/RESULTS/DATA_VISUALISATION/survival_plot_aggregated_accounts.pdf")
plot(survfit(Surv(dates.dat$year_diff, dates.dat$status)~1),
     xlab = "Age of accounts",
     ylab = "Proportion of accounts survived"
     )
dev.off()

dates.sub <- dates.dat[which(dates.dat$status == 0), ]
prop.surv <- 1 - cumsum(as.numeric(table(dates.sub$year_diff)))/nrow(dates.dat)
prop.surv <- c(1, prop.surv)
names(prop.surv) <- c(0, names(table(dates.sub$year_diff)))
pdf("/home/nsdl/RESULTS/DATA_VISUALISATION/survival_aggregated_accounts.pdf")
plot(names(prop.surv),
     prop.surv,
     type="s",
     xlab = "Survival time in years",
     ylab = "proportion of accounts surviving")
grid()
dev.off()


dates.dat <- pan.data[, c("client_id",
                          "year_open",
                          "year_close",
                          "account_opening_date",
                          "account_closing_date")]

dates.sub <- dates.dat[which(dates.dat$year_open %in% c(2007, 2008)), ]
dates.sub$account_opening_date <- as.Date(dates.sub$account_opening_date)
dates.sub$account_closing_date <- as.Date(dates.sub$account_closing_date)

dates.sub$status <- NA
dates.sub$status[which(is.na(dates.sub$account_closing_date))] <- 1
dates.sub$status[which(is.na(dates.sub$status))] <- 0
dates.sub$account_closing_date[which(is.na(dates.sub$account_closing_date))] <- as.Date("2015-08-31")
dates.sub$year_diff <-
    round(as.numeric(dates.sub$account_closing_date - dates.sub$account_opening_date)/365)
dates.sub$year_diff[which(dates.sub$year_diff == 0)] <- 1
#dates.sub$year_close[which(is.na(dates.sub$year_close))] <- 2015

#load("subset_data.Rdata")
#pdf("/home/nsdl/RESULTS/DATA_VISUALISATION/survival_plot_07-08_accounts.pdf")
dates.sub <- dates.sub[which(dates.sub$status == 1), ]
prop.surv <- 1 - cumsum(as.numeric(table(dates.sub$year_diff)))/nrow(dates.sub)
prop.surv <- c(1, prop.surv)
names(prop.surv) <- c(0, names(table(dates.sub$year_diff)))
pdf("/home/nsdl/RESULTS/DATA_VISUALISATION/survival_07-08_aggregated_accounts.pdf")
plot(names(prop.surv),
     prop.surv,
     type="s",
     xlab = "Survival time in years",
     ylab = "proportion surviving")
grid()
dev.off()

diff.mat <- do.call('rbind', lapply(sort(unique(dates.dat$year_open)), function(i) {
    print(i)
    sub   <- dates.dat[which(dates.dat$year_open == i), ]
    total <- nrow(sub)
    sub   <- sub[which(sub$status == 0), ]
    diff  <- as.numeric(table(sub$year_diff))
    df    <- data.frame(diff = diff,
                       gap = names(table(sub$year_diff)))
    bm <- data.frame(gap = 1:length(unique(dates.dat$year_open)))
    df <- merge(bm, df, by = "gap", all.x = TRUE)
    diff <- df$diff
    diff[is.na(diff)] <- 0
    names(diff) <- bm$gap
    diff <- 1 - (cumsum(diff))/total
    diff <- round(diff, 4)
    return(diff)
}
)
)

save(diff.mat, file = "/home/nsdl/RESULTS/DATA_VISUALISATION/survival_differences_percentages.Rdata")

surv.mat <- do.call('rbind', lapply(sort(unique(dates.dat$year_open)), function(i) {
    print(i)
    sub   <- dates.dat[which(dates.dat$year_open == i), ]
    total <- nrow(sub)
    sub   <- sub[which(sub$status == 0), ]
    diff  <- as.numeric(table(sub$year_diff))
    df    <- data.frame(diff = diff,
                       gap = names(table(sub$year_diff)))
    bm <- data.frame(gap = 1:length(unique(dates.dat$year_open)))
    df <- merge(bm, df, by = "gap", all.x = TRUE)
    diff <- df$diff
    diff[is.na(diff)] <- 0
    names(diff) <- bm$gap
    diff <- total - (cumsum(diff))
    diff <- round(diff, 4)
    return(diff)
}
)
)


save(surv.mat, file = "/home/nsdl/RESULTS/DATA_VISUALISATION/survival_differences.Rdata")


del.per <- do.call('cbind',
                   lapply(1:(ncol(diff.mat) - 1), function(i) {
                   return(diff.mat[, i] - diff.mat[, i+1])
               }
               )
               )
del.act <- do.call('cbind',
                   lapply(1:(ncol(surv.mat) - 1), function(i) {
                   return(surv.mat[, i] - surv.mat[, i+1])
               }
               )
               )

mean.del <- sapply(1:ncol(del.per), function(col){
    per.col <- del.per[,col]
    act.col <- del.act[,col]
    wm <- weighted.mean(per.col, act.col)
    return(wm)
})

mean.per <- sapply(1:ncol(del.per), function(col){
    wm <- mean(del.per[col])
    return(wm)
})

sapply(1:(ncol(diff.mat) - 1),
       function(i) {
           print(mean(diff.mat[, i] - diff.mat[, i+1]))
       }
       )

mean.per <- apply(del.per, 2, mean)

