library(xtable)
library(qcc)
library(plotrix)
library(zoo)


print(xtable(db.summary, digits=1),
      hline.after=NULL, only.contents=TRUE,
      include.colnames=FALSE,
      type="latex",
      file="/FRG/db_nsdl/DOC/TABLES/db_summary.tex")


load("/FRG/db_nsdl/SRC/DEMO-DATAMART/RESULTS/DEMO_SUMSTATS/returns_value2015-06-01.Rdata")
all.dat <- all.dat[!duplicated(all.dat$client_id), ]
all.dat$category_isin <- NA
all.dat$category_isin[which(all.dat$isin < 2)] <- 1
all.dat$category_isin[which(all.dat$isin >= 2 & all.dat$isin <= 5)] <-
    2
all.dat$category_isin[which(all.dat$isin > 5 & all.dat$isin <= 10)] <-
    3
all.dat$category_isin[which(all.dat$isin > 10 & all.dat$isin <= 15)] <-
    4
all.dat$category_isin[which(all.dat$isin > 15)] <-
    5

all.dat$category_value <- NA
all.dat$category_value[which(all.dat$val < 10000)] <- 1
all.dat$category_value[which(all.dat$val >= 10000 & all.dat$isin <= 50000)] <-
    2
all.dat$category_value[which(all.dat$val > 50000 & all.dat$val <= 100000)] <-
    3
all.dat$category_value[which(all.dat$val > 100000)] <-
    4


isin.breakup <- table(all.dat$category_isin) 
isin.per <- isin.breakup/sum(isin.breakup)*100
pdf("/FRG/db_nsdl/DOC/GRAPHS/isin_distribution.pdf")
lbls <- c("0-1",
          "2-5",
          "6-10",
          "11-15",
          "> 15")

names(isin.per) <- lbls
layout(matrix(c(1,2),nrow=2), heights=c(3,1))
par(mar=c(0,5,3,3), mgp = c(3,1,0),las = 1)
isin.dist <- barplot(isin.per,
                    beside = TRUE,
                    col = "darkred",
                    axis.lty = 1,
                    font.lab = 2,
                    names.arg = lbls,
                    xlab = "",
                  #  xaxt = "n",
                    ylab = "Percentage of total accounts",
                    ylim = c(0, max(isin.per) + 7),
                    main =
                        "Equity Positions",
                    las = 2)

box(lty = "solid")
mtext(expression(bold("categories")), side = 1, line = 7)

text(x = isin.dist,
     y = isin.per + 3,
     labels = paste0(
         round(isin.per, 1),
         sep = "%"),
     srt = 0)

dev.off()



value.breakup <- table(all.dat$category_value) 
isin.per <- isin.breakup/sum(isin.breakup)*100
pdf("/FRG/db_nsdl/DOC/GRAPHS/value_distribution.pdf")
lbls <- c("0-10000",
          "10000-50000",
          "50000-100000",
          "> 100000")

names(value.per) <- lbls
layout(matrix(c(1,2),nrow=2), heights=c(3,1))
par(mar=c(0,5,3,3), mgp = c(3,1,0),las = 1)
value.dist <- barplot(value.per,
                    beside = TRUE,
                    col = "darkred",
                    axis.lty = 1,
                    font.lab = 2,
                    names.arg = lbls,
                    xlab = "",
                  #  xaxt = "n",
                    ylab = "Percentage of total accounts",
                    ylim = c(0, max(value.per) + 7),
                    main =
                        "Equity Positions",
                    las = 2)

box(lty = "solid")
mtext(expression(bold("categories")), side = 1, line = 7)

text(x = value.dist,
     y = value.per + 3,
     labels = paste0(
         round(value.per, 1),
         sep = "%"),
     srt = 0)

dev.off()






