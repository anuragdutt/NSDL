rm(list=ls())
library(data.table)
library(xts)
library(zoo)

nestle <- "INE239A01016"

## The function that just grabs retail holdings of Nestle holdings
## across dates.

getnestle <- function(x, isin){
    load(x)
    tmp <- data.table(result)
    tmp$date <- NULL
                                        # Keep only retail
    tmp <- subset(tmp, client_type==1)
    print(nrow(tmp))
                                        # Keep only nestle
    tmp <- subset(tmp, isin==nestle)
    tmp$date <- substr(x, nchar(x)-15, nchar(x)-6)
    print(nrow(tmp))
    tmp
}

files <- grep("nestle_client_holdings",
              list.files(path="/home/nsdl/RESULTS/NESTLE_HOLDINGS",
                         full.names = TRUE),
              value = TRUE)
date.seq <- as.character(seq(as.Date("2015-05-30"),
                             as.Date("2015-06-10"),
                             by = "day"))
f.name <- NULL
for(dt in date.seq) {
    
    f.name <<- c(f.name, grep(dt, files,
                              value = TRUE)
                 )
    print(grep(dt, files,
               value = TRUE))
}

d <- lapply(f.name, getnestle, isin=nestle)
data <- do.call("rbind", d)
new_data <- data[,c(1,4,9,10,16,17,24,25), with=FALSE]
new_data <- new_data[order(new_data$client_id, new_data$date),]

## Merging age bracket
age_data <- new_data[!duplicated(new_data$client_id),]
age_data <- age_data[,c(1,6), with=FALSE]
age_data$age_account <- as.numeric(age_data$age_account)

                                        # Number of accounts between
                                        # 5-10-15 years
mage <- summary(age_data$age_account)[6]
age_data$years <- cut(age_data$age_account, breaks = c(0,
                                                    1825,3650,
                                                    5475,mage))
levels(age_data$years) <- c("0-4", "5-9", "10-14", "15plus")
age_data$age_account <- NULL
new_data <- merge(new_data, age_data, by ="client_id", all.x=TRUE )
rm(age_data)
head(new_data)

## Number of accounts
acc_data <- new_data[!duplicated(new_data$client_id),]
acc_data <- acc_data[,c(1,7), with=FALSE]
acc_data$n_share <- as.numeric(acc_data$n_share)
mshare <- summary(acc_data$n_share)[6]
acc_data$shares <- cut(acc_data$n_share,
                       breaks = c(0,1,10,50,100, mshare))
levels(acc_data$shares) <- c("1", "2-10", "11-50",
                             "50-100", "100plus")
acc_data$n_share <- NULL
new_data <- merge(new_data, acc_data, by ="client_id", all.x=TRUE )
rm(acc_data)
head(new_data)

save(new_data, file="example_nestle_data.Rdata")

## Summary stats
date <- as.Date(unique(new_data$date), format="%Y-%m-%d")

holdings <- aggregate(new_data$qty, list(time=new_data$date), mean)
write.csv(holdings, "holdings.csv")
holdings$time <- as.Date(holdings$time)
write.csv(holdings, "holdings.csv")
A <- zoo(holdings$x, order.by = holdings$time)
pdf("holdings.pdf")
layout(matrix(c(1,2),nrow=2), heights=c(3,1))
par(mar=c(0,5,3,3), mgp = c(3,1,0),las = 1)
hol.dist <- plot(A,
                 lwd = 1.2,
                 xlab = "",
                 ylab = "holdings",
                 col = "red",
                 type = "l"
                 )

abline(v=as.Date("2015-06-03"))
abline(v=as.Date("2015-06-05"))
box(lty = "solid")
mtext(expression(bold("Average Holdings")), side = 1, line = 7)

dev.off()

##########################################################agewise

agewise <- aggregate(new_data$qty, list(age=new_data$years,
                                        time=new_data$date), mean)
agewise$time <- as.Date(agewise$time)
write.csv(agewise, "agewise.csv")
holdings <- agewise[which(agewise$age == "0-4"), ]
holdings$age <- NULL

##########################################category 0-4
A <- zoo(holdings$x, order.by = holdings$time)
pdf("agewise_0-4.pdf")
layout(matrix(c(1,2),nrow=2), heights=c(3,1))
par(mar=c(0,5,3,3), mgp = c(3,1,0),las = 1)
plot(A,
     lwd = 1.2,
     xlab = "",
     ylab = "holdings",
     ylim = c(min(A), max(A)),
     col = "red",
     type = "l"
     )
abline(v=as.Date("2015-06-03"))
abline(v=as.Date("2015-06-05"))
grid()
mtext(expression(bold("Agewise_0-4")), side = 1, line = 7)
box(lty = "solid")
par(mar = c(2,8,1,0), mgp = c(3,1,0), las = 1)
plot.new()
legend(x = "bottomleft", bty = "n",
       lwd = c(2),
       lty = c(1),
       col= "red",
       legend = "0-4")

dev.off()

#########################################category 5-9

holdings <- agewise[which(agewise$age == "5-9"), ]
holdings$age <- NULL
B <- zoo(holdings$x, order.by = holdings$time)
pdf("agewise_5-9.pdf")
layout(matrix(c(1,2),nrow=2), heights=c(3,1))
par(mar=c(0,5,3,3), mgp = c(3,1,0),las = 1)
plot(B,
     lwd = 1.2,
     xlab = "",
     ylab = "holdings",
     ylim = c(min(B), max(B)),
     col = "red",
     type = "l"
     )
abline(v=as.Date("2015-06-03"))
abline(v=as.Date("2015-06-05"))
grid()
mtext(expression(bold("Agewise_5-9")), side = 1, line = 7)
box(lty = "solid")
par(mar = c(2,8,1,0), mgp = c(3,1,0), las = 1)
plot.new()
legend(x = "bottomleft", bty = "n",
       lwd = c(2),
       lty = c(1),
       col= "red",
       legend = "5-9")

dev.off()

#################################################category 10-14

holdings <- agewise[which(agewise$age == "10-14"), ]
holdings$age <- NULL
C <- zoo(holdings$x, order.by = holdings$time)
pdf("agewise_10-14.pdf")
layout(matrix(c(1,2),nrow=2), heights=c(3,1))
par(mar=c(0,5,3,3), mgp = c(3,1,0),las = 1)
plot(C,
     lwd = 1.2,
     xlab = "",
     ylab = "holdings",
     ylim = c(min(C), max(C)),
     col = "red",
     type = "l"
     )
abline(v=as.Date("2015-06-03"))
abline(v=as.Date("2015-06-05"))
grid()
mtext(expression(bold("Agewise_10-14")), side = 1, line = 7)
box(lty = "solid")
par(mar = c(2,8,1,0), mgp = c(3,1,0), las = 1)
plot.new()
legend(x = "bottomleft", bty = "n",
       lwd = c(2),
       lty = c(1),
       col= "red",
       legend = "10-14")

dev.off()



################################################################category 15 plus

holdings <- agewise[which(agewise$age == "15plus"), ]
holdings$age <- NULL
D <- zoo(holdings$x, order.by = holdings$time)
pdf("agewise_15_plus.pdf")
layout(matrix(c(1,2),nrow=2), heights=c(3,1))
par(mar=c(0,5,3,3), mgp = c(3,1,0),las = 1)
plot(B,
     lwd = 1.2,
     xlab = "",
     ylab = "holdings",
     ylim = c(min(B), max(B)),
     col = "red",
     type = "l"
     )
abline(v=as.Date("2015-06-03"))
abline(v=as.Date("2015-06-05"))
grid()
mtext(expression(bold("Agewise 15 plus")), side = 1, line = 7)
box(lty = "solid")
par(mar = c(2,8,1,0), mgp = c(3,1,0), las = 1)
plot.new()
legend(x = "bottomleft", bty = "n",
       lwd = c(2),
       lty = c(1),
       col= "red",
       legend = "15 plus")

dev.off()

#############################################aggregated

pdf("agewise.pdf")
layout(matrix(c(1,2),nrow=2), heights=c(3,1))
par(mar=c(0,5,3,3), mgp = c(3,1,0),las = 1)
plot(A,
     lwd = 1.2,
     xlab = "",
     ylab = "holdings",
     ylim = c(min(A, B, C, D), max(A, B, C, D)),
     col = "red",
     type = "l"
     )
lines(B, col = "blue", lwd = 1.2)
lines(C, col = "green", lwd = 1.2)
lines(D, col = "orange", lwd = 1.2)

abline(v=as.Date("2015-06-03"))
abline(v=as.Date("2015-06-05"))
grid()
box(lty = "solid")
mtext(expression(bold("Agewise")), side = 1, line = 5)
par(mar = c(0,8,1,0), mgp = c(3,1,0), las = 1)
plot.new()

legend(x = "bottomleft", bty = "n",
       lwd = c(2,2,2,2),
       lty = c(1,1,1,1),
       col= c("red", "blue", "green", "orange"),
       legend = c("0-4", "5-9", "10-14", "15 plus"))

dev.off()

##########################################sharewise

sharewise <- aggregate(new_data$qty, list(age=new_data$shares,
                                        time=new_data$date), mean)
sharewise$time <- as.Date(sharewise$time)
write.csv(sharewise, "sharewise.csv")

##############################################################category 1

holdings <- sharewise[which(sharewise$age == "1"), ]
holdings$age <- NULL
A <- zoo(holdings$x, order.by = holdings$time)
pdf("sharewise_1.pdf")
layout(matrix(c(1,2),nrow=2), heights=c(3,1))
par(mar=c(0,5,3,3), mgp = c(3,1,0),las = 1)
plot(A,
     lwd = 1.2,
     xlab = "",
     ylab = "holdings",
     ylim = c(min(A), max(A)),
     col = "red",
     type = "l"
     )
abline(v=as.Date("2015-06-03"))
abline(v=as.Date("2015-06-05"))
grid()
mtext(expression(bold("sharewise_1")), side = 1, line = 7)
box(lty = "solid")
par(mar = c(2,8,1,0), mgp = c(3,1,0), las = 1)
plot.new()
legend(x = "bottomleft", bty = "n",
       lwd = c(2),
       lty = c(1),
       col= "red",
       legend = "1")

dev.off()

#############################################################category 2-10

holdings <- sharewise[which(sharewise$age == "2-10"), ]
holdings$age <- NULL
B <- zoo(holdings$x, order.by = holdings$time)
pdf("sharewise_2-10.pdf")
layout(matrix(c(1,2),nrow=2), heights=c(3,1))
par(mar=c(0,5,3,3), mgp = c(3,1,0),las = 1)
plot(B,
     lwd = 1.2,
     xlab = "",
     ylab = "holdings",
     ylim = c(min(B), max(B)),
     col = "red",
     type = "l"
     )
abline(v=as.Date("2015-06-03"))
abline(v=as.Date("2015-06-05"))
grid()
mtext(expression(bold("sharewise_2-10")), side = 1, line = 7)
box(lty = "solid")
par(mar = c(2,8,1,0), mgp = c(3,1,0), las = 1)
plot.new()
legend(x = "bottomleft", bty = "n",
       lwd = c(2),
       lty = c(1),
       col= "red",
       legend = "2-10")

dev.off()


#####################################################category 11-50

holdings <- sharewise[which(sharewise$age == "11-50"), ]
holdings$age <- NULL
C <- zoo(holdings$x, order.by = holdings$time)
pdf("sharewise_11-50.pdf")
layout(matrix(c(1,2),nrow=2), heights=c(3,1))
par(mar=c(0,5,3,3), mgp = c(3,1,0),las = 1)
plot(C,
     lwd = 1.2,
     xlab = "",
     ylab = "holdings",
     ylim = c(min(C), max(C)),
     col = "red",
     type = "l"
     )
abline(v=as.Date("2015-06-03"))
abline(v=as.Date("2015-06-05"))
grid()
mtext(expression(bold("sharewise_11-50")), side = 1, line = 7)
box(lty = "solid")
par(mar = c(2,8,1,0), mgp = c(3,1,0), las = 1)
plot.new()
legend(x = "bottomleft", bty = "n",
       lwd = c(2),
       lty = c(1),
       col= "red",
       legend = "11-50")
dev.off() 


############################################################category 50-100

holdings <- sharewise[which(sharewise$age == "50-100"), ]
holdings$age <- NULL
D <- zoo(holdings$x, order.by = holdings$time)
pdf("sharewise_50-100.pdf")
layout(matrix(c(1,2),nrow=2), heights=c(3,1))
par(mar=c(0,5,3,3), mgp = c(3,1,0),las = 1)
plot(D,
     lwd = 1.2,
     xlab = "",
     ylab = "holdings",
     ylim = c(min(D), max(D)),
     col = "red",
     type = "l"
     )
abline(v=as.Date("2015-06-03"))
abline(v=as.Date("2015-06-05"))
grid()
mtext(expression(bold("sharewise_50-100")), side = 1, line = 7)
box(lty = "solid")
par(mar = c(2,8,1,0), mgp = c(3,1,0), las = 1)
plot.new()
legend(x = "bottomleft", bty = "n",
       lwd = c(2),
       lty = c(1),
       col= "red",
       legend = "50-100")
dev.off() 

######################################################categor 100 plus

holdings <- sharewise[which(sharewise$age == "100plus"), ]
holdings$age <- NULL
E <- zoo(holdings$x, order.by = holdings$time)
pdf("sharewise_100plus.pdf")
layout(matrix(c(1,2),nrow=2), heights=c(3,1))
par(mar=c(0,5,3,3), mgp = c(3,1,0),las = 1)
plot(E,
     lwd = 1.2,
     xlab = "",
     ylab = "holdings",
     ylim = c(min(E), max(E)),
     col = "red",
     type = "l"
     )
abline(v=as.Date("2015-06-03"))
abline(v=as.Date("2015-06-05"))
grid()
mtext(expression(bold("sharewise_100plus")), side = 1, line = 7)
box(lty = "solid")
par(mar = c(2,8,1,0), mgp = c(3,1,0), las = 1)
plot.new()
legend(x = "bottomleft", bty = "n",
       lwd = c(2),
       lty = c(1),
       col= "red",
       legend = "100plus")
dev.off() 


################################################aggregated

pdf("sharewise.pdf")
layout(matrix(c(1,2),nrow=2), heights=c(3,1))
par(mar=c(0,5,3,3), mgp = c(3,1,0),las = 1)
plot(A,
     lwd = 1.2,
     xlab = "",
     ylab = "holdings",
     ylim = c(min(A, B, C, D, E), max(A, B, C, D, E)),
     col = "red",
     type = "l"
     )
lines(B, col = "blue", lwd = 1.2)
lines(C, col = "green", lwd = 1.2)
lines(D, col = "orange", lwd = 1.2)
lines(E, col = "black", lwd = 1.2)

abline(v=as.Date("2015-06-03"))
abline(v=as.Date("2015-06-05"))
grid()
mtext(expression(bold("Sharewise")), side = 1, line = 5)
box(lty = "solid")
par(mar = c(0,8,1,0), mgp = c(3,1,0), las = 1)
plot.new()
legend(x = "bottomleft", bty = "n",
       lwd = c(2,2,2,2),
       lty = c(1,1,1,1),
       col= c("red", "blue", "green", "orange", "black"),
       legend = c("1", "2-10", "11-50", "50-100", "100 plus"))


dev.off()





