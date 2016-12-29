options(scipen = 999, digits = 4)
library(xtable)
library(qcc)
library(plotrix)
library(zoo)
library(Hmisc)

all.files <- list.files("/FRG/db_nsdl/DOC/TABLES/DATA_VISUALISATION/NON_EMPTY",full.names = TRUE)
load.f <- all.files[grep("Rdata", all.files)]
for(i in load.f) {
    load(i)
    print(load(i))
}



occ.per <- occupation/sum(occupation)*100
pdf("/FRG/db_nsdl/DOC/GRAPHS/DATA_VISUALISATION/NON_EMPTY/occ_distribution_non_empty.pdf" )
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
                    main =
                        "",
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
      file="/FRG/db_nsdl/DOC/GRAPHS/DATA_VISUALISATION/NON_EMPTY/account_status_non_empty.tex")


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
      file="/FRG/db_nsdl/DOC/GRAPHS/DATA_VISUALISATION/NON_EMPTY/client_type_non_empty.tex")

############################################### opening year
load("/FRG/db_nsdl/DOC/TABLES/DATA_VISUALISATION/NON_EMPTY/opening_year_distribution_aggregated.Rdata")
op.per <- opening.year/sum(opening.year)*100
op.per <- op.per[!is.na(names(op.per))]
if(length(which(names(op.per) %in% "9999")) > 0) {
    op.per <- op.per[-which(names(op.per) == "9999")]
}
pdf("/FRG/db_nsdl/DOC/GRAPHS/DATA_VISUALISATION/NON_EMPTY/opening_year_distribution_non_empty.pdf" )
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
                    main =
                        "",
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
load("/FRG/db_nsdl/DOC/TABLES/DATA_VISUALISATION/NON_EMPTY/opening_month_distribution_aggregated.Rdata")

om.per <- opening.month/sum(opening.month)*100
om.per <- om.per[!is.na(names(om.per))]
pdf("/FRG/db_nsdl/DOC/GRAPHS/DATA_VISUALISATION/NON_EMPTY/opening_month_distribution_non_empty.pdf" )
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
                    main =
                        "",
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

############################################### opening year
load("/FRG/db_nsdl/DOC/TABLES/DATA_VISUALISATION/NON_EMPTY/opening_active_year_distribution_aggregated.Rdata")
op.per <- opening.year/sum(opening.year)*100
op.per <- op.per[!is.na(names(op.per))]
if(length(which(names(op.per) %in% "9999")) > 0) {
    op.per <- op.per[-which(names(op.per) == "9999")]
}
pdf("/FRG/db_nsdl/DOC/GRAPHS/DATA_VISUALISATION/NON_EMPTY/opening_active_year_distribution_non_empty.pdf" )
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
                    main =
                        "",
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



#################################### age

## tenure.op <- tenure.op[-length(tenure.op)]



#########################################
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
      file="/FRG/db_nsdl/DOC/GRAPHS/DATA_VISUALISATION/NON_EMPTY/highest_district_non_empty.tex")

district.low <- district.dat[which(district.dat >=10)]
district.low <- sort(district.low, decreasing = TRUE)
district.low.per <- district.low*100/sum(district.low)
dis <- data.frame(district_name =
                      capitalize(tail(names(district.low), 10)),
                  per_account = tail(as.numeric(district.low.per), 10),
                  accounts = tail(as.numeric(district.low), 10)
                  )

print(xtable(dis, digits=c(0,0,5,0)),
      hline.after=NULL, only.contents = TRUE,
      include.rownames = FALSE,
      include.colnames=FALSE,
      type="latex",
      file="/FRG/db_nsdl/DOC/GRAPHS/DATA_VISUALISATION/NON_EMPTY/lowest_district_non_empty.tex")



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
      file="/FRG/db_nsdl/DOC/GRAPHS/DATA_VISUALISATION/NON_EMPTY/highest_state_non_empty.tex")

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
      file="/FRG/db_nsdl/DOC/GRAPHS/DATA_VISUALISATION/NON_EMPTY/lowest_state_non_empty.tex")

jp <- data.frame(category = c("Active PAN 2", "Active PAN 3"),
                 value = c(joint.pan2, joint.pan3),
                 percentage = c(joint.pan2*100/14920707,
                                joint.pan3*100/14920707))


print(xtable(jp, digits=3),
      hline.after=NULL, only.contents = TRUE,
      include.rownames = FALSE,
      include.colnames=FALSE,
      type="latex",
      file="/FRG/db_nsdl/DOC/GRAPHS/DATA_VISUALISATION/NON_EMPTY/joint_pan.tex")


#############################################


cty.per <- ac.client.per
cty.per <- cty.per[-12, ]
lbls <- c("Resident", "FI",
          "FII", "NRI", "Corporate",
          "Clearing member", "Foreign national",
          "Mutual fund", "Trust",
          "Bank", "Foreign qualified investor",
          "Foreign portfolio investor (Individual)",
          "Foreign portfolio investor (Corporate)")

cty.per <- data.frame(cty.per)
colnames(cty.per) <- c("10", "11", "19")
cty.per$client_type <- lbls
cty.per <- cty.per[, c("client_type", "10", "11",
                       "19")]

print(xtable(cty.per, digits=c(0,0,1,1,1)),
      hline.after=NULL, only.contents = TRUE,
      include.rownames = FALSE,
      include.colnames=FALSE,
      type="latex",
      file="/FRG/db_nsdl/DOC/GRAPHS/DATA_VISUALISATION/NON_EMPTY/client_account_category_non_empty.tex")





