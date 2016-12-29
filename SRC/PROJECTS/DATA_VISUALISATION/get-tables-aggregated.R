options(scipen = 999, digits = 4)
library(xtable)
library(qcc)
library(plotrix)
library(zoo)
library(Hmisc)

all.files <- list.files("/FRG/db_nsdl/DOC/TABLES/DATA_VISUALISATION/AGGREGATED",full.names = TRUE)
load.f <- all.files[grep("Rdata", all.files)]
for(i in load.f) {
    load(i)
}

occ.per <- occupation/sum(occupation)*100
pdf("/FRG/db_nsdl/DOC/GRAPHS/DATA_VISUALISATION/AGGREGATED/occ_distribution_aggregated.pdf")
occ.per <- occ.per[sort(names(occ.per))]
lbls <- names(occ.per)
layout(matrix(c(1,2),nrow=2), heights=c(3,1))
par(mar=c(0,5,3,3), mgp = c(3,1,0),las = 1)
occ.dist <- barplot(occ.per,
                    beside = TRUE,
                    col = "darkred",
                    axis.lty = 1,
                    font.lab = 2,
                    names.arg = lbls,
                    xlab = "",
                  #  xaxt = "n",
                    ylab = "Percentage of total accounts",
                    ylim = c(0, max(occ.per) + 7),
                   # main =
                        #"Occupation",
                    las = 2)

box(lty = "solid")
mtext(expression(bold("Occupation categories (as provided by NSDL)")), side = 1, line = 7)

text(x = occ.dist,
     y = occ.per + 3,
     labels = paste0(
                     round(occ.per, 1),
                     sep = "%"),
     srt = 0)

dev.off()

#########################################

account.status
a.s <- data.frame(account_status = names(account.status),
                  number_of_account = as.numeric(account.status))

print(xtable(a.s, digits=0),
      hline.after=NULL, only.contents = TRUE,
include.rownames = FALSE,
      include.colnames=FALSE,
      type="latex",
      file="/FRG/db_nsdl/DOC/GRAPHS/DATA_VISUALISATION/AGGREGATED/account_status_aggregated.tex")


######################################### age


cty.per <- client.type/sum(client.type)*100
cty.per[11] <- cty.per[11] + cty.per[12]
cty.per[12] <- NA
cty.per <- cty.per[!is.na(cty.per)]
lbls <- c("Resident", "FI",
          "FII", "NRI", "Corporate",
          "Clearing member", "Foreign national",
          "Mutual fund", "Trust",
          "Bank", "Foreign qualified investor",
          "Foreign portfolio investor (Individual)",
          "Foreign portfolio investor (Corporate)")

names(cty.per) <- lbls
cty.per <- round(cty.per,3)
client.type[11] <- client.type[11] + client.type[12]
client.type <- client.type[-12]
c.t <- data.frame(categories = names(cty.per),
                  percentage = as.numeric(cty.per),
                  number = as.numeric(client.type))
c.t <- c.t[order(c.t[["number"]], decreasing = TRUE), ]

print(xtable(c.t, digits=c(0,0,2,0)),
      hline.after=NULL, only.contents = TRUE,
include.rownames = FALSE,
      include.colnames=FALSE,
      type="latex",
      file="/FRG/db_nsdl/DOC/GRAPHS/DATA_VISUALISATION/AGGREGATED/client_type_aggregated.tex")

############################################### opening year
load("/FRG/db_nsdl/DOC/TABLES/DATA_VISUALISATION/AGGREGATED/opening_year_distribution_aggregated.Rdata")
op.per <- opening.year/sum(opening.year)*100
op.per <- op.per[!is.na(names(op.per))]
if(length(which(names(op.per) %in% "9999")) > 0) {
    op.per <- op.per[-which(names(op.per) == "9999")]
}
pdf("/FRG/db_nsdl/DOC/GRAPHS/DATA_VISUALISATION/AGGREGATED/opening_year_distribution_aggregated.pdf")
op.per <- op.per[sort(names(op.per))]
lbls <- names(op.per)
layout(matrix(c(1,2),nrow=2), heights=c(3,1))
par(mar=c(0,5,3,3), mgp = c(3,1,0),las = 1)
op.dist <- barplot(op.per,
                    beside = TRUE,
                    col = "darkred",
                    axis.lty = 1,
                    font.lab = 2,
                    names.arg = lbls,
                    xlab = "",
                  #  xaxt = "n",
                    ylab = "Percentage of total accounts",
                    ylim = c(0, max(op.per) + 7),
                  #  main =
                       # "Year-wise breakup of account opening dates",
                    las = 2)

box(lty = "solid")
mtext(expression(bold("Years")), side = 1, line = 7)

text(x = op.dist,
     y = op.per + 1,
     labels = paste0(
         round(op.per, 1),
         sep = "%"),
     srt = 90)

dev.off()

############################################## opening month
load("/FRG/db_nsdl/DOC/TABLES/DATA_VISUALISATION/AGGREGATED/opening_month_distribution_aggregated.Rdata")

om.per <- opening.month/sum(opening.month)*100
om.per <- om.per[!is.na(names(om.per))]
pdf("/FRG/db_nsdl/DOC/GRAPHS/DATA_VISUALISATION/AGGREGATED/opening_month_distribution_aggregated.pdf")
lbls <- names(om.per)
layout(matrix(c(1,2),nrow=2), heights=c(3,1))
par(mar=c(0,5,3,3), mgp = c(3,1,0),las = 1)
om.dist <- barplot(om.per,
                    beside = TRUE,
                    col = "darkred",
                    axis.lty = 1,
                    font.lab = 2,
                    names.arg = lbls,
                    xlab = "",
                  #  xaxt = "n",
                    ylab = "Percentage of total accounts",
                    ylim = c(0, max(om.per) + 7),
                   # main =
                   #     "Accounts opened month-wise",
                    las = 2)

box(lty = "solid")
mtext(expression(bold("Months")), side = 1, line = 7)

text(x = om.dist,
     y = om.per + 1,
     labels = paste0(
         round(om.per, 1),
         sep = "%"),
     srt = 90)

dev.off()

################################################closing.year
names(closing.year) <- c(2000, 2001, 2010, 2011, 2012, 2013,
                         2014,2015, 2002, 2003, 2004, 2005, 2006,
                         2007, 2008,2009, 1997, 1998, 1999)
closing.year <- closing.year[order(names(closing.year))]

cp.per <- closing.year/sum(closing.year)*100
cp.per <- cp.per[!is.na(names(cp.per))]
if(length(which(names(cp.per) %in% "9999")) > 0) {
    cp.per <- cp.per[-which(names(cp.per) == "9999")]
}
pdf("/FRG/db_nsdl/DOC/GRAPHS/DATA_VISUALISATION/AGGREGATED/closing_year_distribution_aggregated.pdf")
cp.per <- cp.per[sort(names(cp.per))]
lbls <- names(cp.per)
layout(matrix(c(1,2),nrow=2), heights=c(3,1))
par(mar=c(0,5,3,3), mgp = c(3,1,0),las = 1)
cp.dist <- barplot(cp.per,
                    beside = TRUE,
                    col = "darkred",
                    axis.lty = 1,
                    font.lab = 2,
                    names.arg = lbls,
                    xlab = "",
                  #  xaxt = "n",
                    ylab = "Percentage of total closed accounts",
                    ylim = c(0, max(cp.per) + 7),
                    ## main =                                           ##
                    ##     "Year-wise breakup of account closing dates" ##
                  ,
                    las = 2)

box(lty = "solid")
mtext(expression(bold("Years")), side = 1, line = 3)

text(x = cp.dist,
     y = cp.per + 1,
     labels = paste0(
         round(cp.per, 1),
         sep = "%"),
     srt = 90)

dev.off()
###################################closing month

cm.per <- closing.month/sum(closing.month)*100
cm.per <- cm.per[!is.na(names(cm.per))]
pdf("/FRG/db_nsdl/DOC/GRAPHS/DATA_VISUALISATION/AGGREGATED/closing_month_distribution_aggregated.pdf")
lbls <- names(cm.per)
layout(matrix(c(1,2),nrow=2), heights=c(3,1))
par(mar=c(0,5,3,3), mgp = c(3,1,0),las = 1)
cm.dist <- barplot(cm.per,
                    beside = TRUE,
                    col = "darkred",
                    axis.lty = 1,
                    font.lab = 2,
                    names.arg = lbls,
                    xlab = "",
                  #  xaxt = "n",
                    ylab = "Percentage of total accounts",
                    ylim = c(0, max(cm.per) + 7),
                    #################################################
                    ## main =                                      ##
                    ##     "Month-wise closing dates of accounts", ##
                    #################################################
                    las = 2)

box(lty = "solid")
mtext(expression(bold("Months")), side = 1, line = 7)

text(x = cm.dist,
     y = cm.per + 1,
     labels = paste0(
         round(cm.per, 1),
         sep = "%"),
     srt = 90)

dev.off()

############################################### opening year
load("/FRG/db_nsdl/DOC/TABLES/DATA_VISUALISATION/AGGREGATED/opening_active_year_distribution_aggregated.Rdata")
op.per <- opening.year/sum(opening.year)*100
op.per <- op.per[!is.na(names(op.per))]
if(length(which(names(op.per) %in% "9999")) > 0) {
    op.per <- op.per[-which(names(op.per) == "9999")]
}
pdf("/FRG/db_nsdl/DOC/GRAPHS/DATA_VISUALISATION/AGGREGATED/opening_active_year_distribution_aggregated.pdf")
op.per <- op.per[sort(names(op.per))]
lbls <- names(op.per)
layout(matrix(c(1,2),nrow=2), heights=c(3,1))
par(mar=c(0,5,3,3), mgp = c(3,1,0),las = 1)
op.dist <- barplot(op.per,
                    beside = TRUE,
                    col = "darkred",
                    axis.lty = 1,
                    font.lab = 2,
                    names.arg = lbls,
                    xlab = "",
                  #  xaxt = "n",
                    ylab = "Percentage of total accounts",
                    ylim = c(0, max(op.per) + 7),
                    #######################################################
                    ## main =                                            ##
                    ##     "Year-wise breakup of account opening dates", ##
                    #######################################################
                    las = 2)

box(lty = "solid")
mtext(expression(bold("Years")), side = 1, line = 7)

text(x = op.dist,
     y = op.per + 1,
     labels = paste0(
         round(op.per, 1),
         sep = "%"),
     srt = 90)

dev.off()

##############################################active opening month
load("/FRG/db_nsdl/DOC/TABLES/DATA_VISUALISATION/AGGREGATED/opening_active_month_distribution_aggregated.Rdata")

om.per <- opening.month/sum(opening.month)*100
om.per <- om.per[!is.na(names(om.per))]
pdf("/FRG/db_nsdl/DOC/GRAPHS/DATA_VISUALISATION/AGGREGATED/opening_active_month_distribution_aggregated.pdf")
lbls <- names(om.per)
layout(matrix(c(1,2),nrow=2), heights=c(3,1))
par(mar=c(0,5,3,3), mgp = c(3,1,0),las = 1)
om.dist <- barplot(om.per,
                    beside = TRUE,
                    col = "darkred",
                    axis.lty = 1,
                    font.lab = 2,
                    names.arg = lbls,
                    xlab = "",
                  #  xaxt = "n",
                    ylab = "Percentage of total accounts",
                    ylim = c(0, max(om.per) + 7),
                    ######################################
                    ## main =                           ##
                    ##     "Accounts opened monthwise", ##
                    ######################################
                    las = 2)

box(lty = "solid")
mtext(expression(bold("Months")), side = 1, line = 7)

text(x = om.dist,
     y = om.per + 1,
     labels = paste0(
         round(om.per, 1),
         sep = "%"),
     srt = 90)

dev.off()



#################################### age

## tenure.op <- tenure.op[-length(tenure.op)]
tenure.op[14] <- tenure.op[14] + tenure.op[15] +
    tenure.op[16] + tenure.op[17] + tenure.op[18] +
    tenure.op[19] + tenure.op[20]
tenure.op <- tenure.op[-(15:20)]
names(tenure.op) <- c("0-1",
                      "1-2",
                      "2-3",
                      "3-4",
                      "4-5",
                      "5-6",
                      "6-7",
                      "7-8",
                      "8-9",
                      "9-10",
                      "10-11",
                      "11-12",
                      "12-13",
                      "> 13")
op.per <- tenure.op/sum(tenure.op)*100
names(op.per) <- names(tenure.op)
pdf("/FRG/db_nsdl/DOC/GRAPHS/DATA_VISUALISATION/AGGREGATED/active_account_age_distribution_aggregated.pdf")
lbls <- names(op.per)
layout(matrix(c(1,2),nrow=2), heights=c(3,1))
par(mar=c(0,5,3,3), mgp = c(3,1,0),las = 1)
op.dist <- barplot(op.per,
                   beside = TRUE,
                   col = "darkred",
                   axis.lty = 1,
                   font.lab = 2,
                   names.arg = lbls,
                   xlab = "",
                   # xaxt = "n",
                   ylab = "Percentage of total accounts",
                   ylim = c(0, max(op.per) + 3.5),
                   ###################################
                   ## main =                        ##
                   ##     "Age of active accounts", ##
                   ###################################
                   las = 2)

box(lty = "solid")
mtext(expression(bold("Account age in years")), side = 1, line = 4)


text(x = op.dist,          
     y = op.per + 1,       
     labels = paste0(      
         round(op.per, 1), 
         sep = "%"),       
     srt = 90)             


dev.off()



#########################################



tenure.cl[14] <- tenure.cl[14] + tenure.cl[15] +
    tenure.cl[16] + tenure.cl[17] + tenure.cl[18] +
    tenure.cl[19]
tenure.cl <- tenure.cl[-(15:19)]
names(tenure.cl) <- c("0-1",
                      "1-2",
                      "2-3",
                      "3-4",
                      "4-5",
                      "5-6",
                      "6-7",
                      "7-8",
                      "8-9",
                      "9-10",
                      "10-11",
                      "11-12",
                      "12-13",
                      "> 13")
cl.per <- tenure.cl/sum(tenure.cl)*100
names(cl.per) <- names(tenure.cl)
pdf("/FRG/db_nsdl/DOC/GRAPHS/DATA_VISUALISATION/inactive_account_age_distribution.pdf")
lbls <- names(cl.per)
layout(matrix(c(1,2),nrow=2), heights=c(3,1))
par(mar=c(0,5,3,3), mgp = c(3,1,0),las = 1)
cl.dist <- barplot(cl.per,
                   beside = TRUE,
                   col = "darkred",
                   axis.lty = 1,
                   font.lab = 2,
                   names.arg = lbls,
                   xlab = "",
                   # xaxt = "n",
                   ylab = "Percentage of total accounts",
                   ylim = c(0, max(cl.per) + 3.5),
                   ##########################################
                   ## main =                               ##
                   ##     "Survival of inactive accounts", ##
                   ##########################################
                   las = 2)

box(lty = "solid")
mtext(expression(bold("years")), side = 1, line = 7)


text(x = cl.dist,          
     y = cl.per + 1,       
     labels = paste0(      
         round(cl.per, 1), 
         sep = "%"),       
     srt = 90)             


dev.off()

###############################################
district.dat <- district.dat[-grep("unassigned",
                                   names(district.dat))]

district.dat <- sort(district.dat, decreasing = TRUE) 
dis.per <- district.dat/sum(district.dat)*100

dis <- data.frame(district_name =
                      capitalize(head(names(district.dat), 10)),
                  per_account = head(as.numeric(dis.per), 10),
                  accounts = head(as.numeric(district.dat), 10)
                  )

print(xtable(dis, digits=c(0,0,3,0)),
      hline.after=NULL, only.contents = TRUE,
      include.rownames = FALSE,
      include.colnames=FALSE,
      type="latex",
      file="/FRG/db_nsdl/DOC/GRAPHS/DATA_VISUALISATION/AGGREGATED/highest_district_aggregated.tex")

district.low <- district.dat[-grep("unassigned",
                                   names(district.dat))]
district.low <- district.low[which(district.low >= 10)]
district.low <- sort(district.low, decreasing = TRUE)
dis <- data.frame(district_name =
                      capitalize(tail(names(district.low), 10)),
                  per_account = tail(as.numeric(district.low), 10),
                  accounts = tail(as.numeric(district.low), 10)
                  )

print(xtable(dis, digits=c(0,0,3,0)),
      hline.after=NULL, only.contents = TRUE,
      include.rownames = FALSE,
      include.colnames=FALSE,
      type="latex",
      file="/FRG/db_nsdl/DOC/GRAPHS/DATA_VISUALISATION/AGGREGATED/lowest_district_aggregated.tex")



state.dat <- sort(state.dat, decreasing = TRUE) 
state.dat <- state.dat[-length(state.dat)]
st.per <- state.dat/sum(state.dat)*100

st <- data.frame(state_name =
                     head(names(state.dat), 10),
                 per_account = head(as.numeric(st.per), 10),
                 accounts = head(as.numeric(state.dat), 10)
                 )

print(xtable(st, digits=c(0,0,3,0)),
      hline.after=NULL, only.contents = TRUE,
      include.rownames = FALSE,
      include.colnames=FALSE,
      type="latex",
      file="/FRG/db_nsdl/DOC/GRAPHS/DATA_VISUALISATION/AGGREGATED/highest_state_aggregated.tex")

st <- data.frame(state_name =
                     tail(names(state.dat), 10),
                 per_account = tail(as.numeric(st.per), 10),
                 accounts = tail(as.numeric(state.dat), 10)
                 )

print(xtable(st, digits=c(0,0,3,0)),
      hline.after=NULL, only.contents = TRUE,
      include.rownames = FALSE,
      include.colnames=FALSE,
      type="latex",
      file="/FRG/db_nsdl/DOC/GRAPHS/DATA_VISUALISATION/AGGREGATED/lowest_state_aggregated.tex")

jp <- data.frame(category = c("Active PAN 2", "Active PAN 3"),
                 value = c(joint.pan2, joint.pan3),
                 percentage = c(joint.pan2*100/14920707,
                                joint.pan3*100/14920707))


print(xtable(jp, digits=3),
      hline.after=NULL, only.contents = TRUE,
include.rownames = FALSE,
      include.colnames=FALSE,
      type="latex",
      file="/FRG/db_nsdl/DOC/GRAPHS/DATA_VISUALISATION/AGGREGATED/joint_pan.tex")


########################################isin count
total.count[20] <- sum(total.count[20:length(total.count)])
total.count <- total.count[-(21:length(total.count))]
total.count <- c(10407234 - sum(total.count), total.count)
names(total.count) <- c(0:19, "> 19")


tc.per <- total.count/sum(total.count)*100
names(tc.per) <- names(total.count)
pdf("/FRG/db_nsdl/DOC/GRAPHS/DATA_VISUALISATION/AGGREGATED/isin_count_distribution_aggregated.pdf")
lbls <- names(tc.per)
layout(matrix(c(1,2),nrow=2), heights=c(3,1))
par(mar=c(0,5,3,3), mgp = c(3,1,0),las = 1)
tc.dist <- barplot(tc.per,
                   beside = TRUE,
                   col = "darkred",
                   axis.lty = 1,
                   font.lab = 2,
                   names.arg = lbls,
                   xlab = "",
                   # xaxt = "n",
                   ylab = "Percentage of total accounts",
                   ylim = c(0, max(tc.per) + 5.8),
                   ######################################################
                   ## main =                                           ##
                   ##     "Active accounts by number of unique isins", ##
                   ######################################################
                   las = 2)

box(lty = "solid")
mtext(expression(bold("years")), side = 1, line = 7)


text(x = tc.dist,          
     y = tc.per + 3,       
     labels = paste0(      
         round(tc.per, 1), 
         sep = "%"),       
     srt = 90)             


dev.off()



track.count[20] <- sum(track.count[20:length(track.count)])
track.count <- track.count[-(21:length(track.count))]
track.count <- c(10407234 - sum(track.count), track.count)
names(track.count) <- c(0:19, "> 19")
track.count <- track.count[-1]

tc.per <- track.count/sum(track.count)*100
names(tc.per) <- names(track.count)
pdf("/FRG/db_nsdl/DOC/GRAPHS/DATA_VISUALISATION/AGGREGATED/isin_tracked_count_distribution_aggregated.pdf")
lbls <- names(tc.per)
layout(matrix(c(1,2),nrow=2), heights=c(3,1))
par(mar=c(0,5,3,3), mgp = c(3,1,0),las = 1)
tc.dist <- barplot(tc.per,
                   beside = TRUE,
                   col = "darkred",
                   axis.lty = 1,
                   font.lab = 2,
                   names.arg = lbls,
                   xlab = "",
                   # xaxt = "n",
                   ylab = "Percentage of total accounts",
                   ylim = c(0, max(tc.per) + 5),
                   ###############################################
                   ## main =                                    ##
                   ##     "Accounts by number of tracked isin", ##
                   ###############################################
                   las = 1)

box(lty = "solid")
mtext(expression(bold("Number of ISINs per account")), side = 1, line = 2)


text(x = tc.dist,          
     y = tc.per + 3,       
     labels = paste0(      
         round(tc.per, 1), 
         sep = "%"),       
     srt = 90)             


dev.off()


