rm(list=ls())
library("zoo")

path.static <- "/FRG/db_nsdl/SRC/RESULTS/NESTLE_HOLDINGS/GRAPHS/"
load("/FRG/db_nsdl/SRC/RESULTS/NESTLE_HOLDINGS/age_v_holdings.Rdata")
load("/FRG/db_nsdl/SRC/RESULTS/NESTLE_HOLDINGS/both_v_holdings.Rdata")
load("/FRG/db_nsdl/SRC/RESULTS/NESTLE_HOLDINGS/nifty_v_holdings.Rdata")
load("/FRG/db_nsdl/SRC/RESULTS/NESTLE_HOLDINGS/niftyjr_v_holdings.Rdata")

pdf(paste0(path.static, "age_v_holdings_0_5.pdf"))
par(mar = c(2, 4, 2, 2))
rng <- age.var$"0-5"
df.temp <- read.zoo(data.frame(rownames(age.var), age.var$"0-5"))
plot(df.temp,
     ylim = range(rng), lwd = 1.5, xlab = "",
     ylab = "Average Holdings",
     col = "black", type = 'l')
legend(x = "bottomleft", bty = "n", lwd = c(2),
       lty = c(1),
       col= c("black"),
       legend = c("Below 5 years"))

grid()
dev.off()

pdf(paste0(path.static, "age_v_holdings_5_10.pdf"))
par(mar = c(2, 4, 2, 2))
rng <- age.var$"5-10"
df.temp <- read.zoo(data.frame(rownames(age.var), age.var$"5-10"))
plot(df.temp,
     ylim = range(rng), lwd = 1.5, xlab = "",
     ylab = "Average Holdings",
     col = "black", type = 'l')
legend(x = "bottomleft", bty = "n", lwd = c(2),
       lty = c(1),
       col= c("black"),
       legend = c("5 to 10 years"))

grid()
dev.off()

pdf(paste0(path.static, "age_v_holdings_10_15.pdf"))
par(mar = c(2, 4, 2, 2))
rng <- age.var$"10-15"
df.temp <- read.zoo(data.frame(rownames(age.var), age.var$"10-15"))
plot(df.temp,
     ylim = range(rng), lwd = 1.5, xlab = "",
     ylab = "Average Holdings",
     col = "black", type = 'l')
legend(x = "bottomleft", bty = "n", lwd = c(2),
       lty = c(1),
       col= c("black"),
       legend = c("10 to 15 years"))

grid()
dev.off()


pdf(paste0(path.static, "age_v_holdings_15_inf.pdf"))
par(mar = c(2, 4, 2, 2))
rng <- age.var$">15"
df.temp <- read.zoo(data.frame(rownames(age.var), age.var$">15"))
plot(df.temp,
     ylim = range(rng), lwd = 1.5, xlab = "",
     ylab = "Average Holdings",
     col = "black", type = 'l')
legend(x = "bottomleft", bty = "n", lwd = c(2),
       lty = c(1),
       col= c("black"),
       legend = c("greater than 15 years"))

grid()
dev.off()

###################################################

pdf(paste0(path.static, "both_v_holdings_0_25.pdf"))
par(mar = c(2, 4, 2, 2))
rng <- both.var$"0-25"
df.temp <- read.zoo(data.frame(rownames(both.var), both.var$"0-25"))
plot(df.temp,
     ylim = range(rng), lwd = 1.5, xlab = "",
     ylab = "Average Holdings",
     col = "black", type = 'l')
legend(x = "bottomleft", bty = "n", lwd = c(2),
       lty = c(1),
       col= c("black"),
       legend = c("Below 5 years"))

grid()
dev.off()

pdf(paste0(path.static, "both_v_holdings_25_50.pdf"))
par(mar = c(2, 4, 2, 2))
rng <- both.var$"25-50"
df.temp <- read.zoo(data.frame(rownames(both.var), both.var$"25-50"))
plot(df.temp,
     ylim = range(rng), lwd = 1.5, xlab = "",
     ylab = "Average Holdings",
     col = "black", type = 'l')
legend(x = "bottomleft", bty = "n", lwd = c(2),
       lty = c(1),
       col= c("black"),
       legend = c("5 to 10 years"))

grid()
dev.off()

pdf(paste0(path.static, "both_v_holdings_50_75.pdf"))
par(mar = c(2, 4, 2, 2))
rng <- both.var$"50-75"
df.temp <- read.zoo(data.frame(rownames(both.var), both.var$"50-75"))
plot(df.temp,
     ylim = range(rng), lwd = 1.5, xlab = "",
     ylab = "Average Holdings",
     col = "black", type = 'l')
legend(x = "bottomleft", bty = "n", lwd = c(2),
       lty = c(1),
       col= c("black"),
       legend = c("10 to 15 years"))

grid()
dev.off()


pdf(paste0(path.static, "both_v_holdings_75_inf.pdf"))
par(mar = c(2, 4, 2, 2))
rng <- both.var$">75"
df.temp <- read.zoo(data.frame(rownames(both.var), both.var$">75"))
plot(df.temp,
     ylim = range(rng), lwd = 1.5, xlab = "",
     ylab = "Average Holdings",
     col = "black", type = 'l')
legend(x = "bottomleft", bty = "n", lwd = c(2),
       lty = c(1),
       col= c("black"),
       legend = c("greater than 15 years"))

grid()
dev.off()

###################################################

pdf(paste0(path.static, "nifty_v_holdings_0_25.pdf"))
par(mar = c(2, 4, 2, 2))
rng <- nifty.var$"0-25"
df.temp <- read.zoo(data.frame(rownames(nifty.var), nifty.var$"0-25"))
plot(df.temp,
     ylim = range(rng), lwd = 1.5, xlab = "",
     ylab = "Average Holdings",
     col = "black", type = 'l')
legend(x = "bottomleft", bty = "n", lwd = c(2),
       lty = c(1),
       col= c("black"),
       legend = c("Below 5 years"))

grid()
dev.off()

pdf(paste0(path.static, "nifty_v_holdings_25_50.pdf"))
par(mar = c(2, 4, 2, 2))
rng <- nifty.var$"25-50"
df.temp <- read.zoo(data.frame(rownames(nifty.var), nifty.var$"25-50"))
plot(df.temp,
     ylim = range(rng), lwd = 1.5, xlab = "",
     ylab = "Average Holdings",
     col = "black", type = 'l')
legend(x = "bottomleft", bty = "n", lwd = c(2),
       lty = c(1),
       col= c("black"),
       legend = c("5 to 10 years"))

grid()
dev.off()

pdf(paste0(path.static, "nifty_v_holdings_50_75.pdf"))
par(mar = c(2, 4, 2, 2))
rng <- nifty.var$"50-75"
df.temp <- read.zoo(data.frame(rownames(nifty.var), nifty.var$"50-75"))
plot(df.temp,
     ylim = range(rng), lwd = 1.5, xlab = "",
     ylab = "Average Holdings",
     col = "black", type = 'l')
legend(x = "bottomleft", bty = "n", lwd = c(2),
       lty = c(1),
       col= c("black"),
       legend = c("10 to 15 years"))

grid()
dev.off()


pdf(paste0(path.static, "nifty_v_holdings_75_inf.pdf"))
par(mar = c(2, 4, 2, 2))
rng <- nifty.var$">75"
df.temp <- read.zoo(data.frame(rownames(nifty.var), nifty.var$">75"))
plot(df.temp,
     ylim = range(rng), lwd = 1.5, xlab = "",
     ylab = "Average Holdings",
     col = "black", type = 'l')
legend(x = "bottomleft", bty = "n", lwd = c(2),
       lty = c(1),
       col= c("black"),
       legend = c("greater than 15 years"))

grid()
dev.off()




###################################################

pdf(paste0(path.static, "niftyjr_v_holdings_0_25.pdf"))
par(mar = c(2, 4, 2, 2))
rng <- niftyjr.var$"0-25"
df.temp <- read.zoo(data.frame(rownames(niftyjr.var), niftyjr.var$"0-25"))
plot(df.temp,
     ylim = range(rng), lwd = 1.5, xlab = "",
     ylab = "Average Holdings",
     col = "black", type = 'l')
legend(x = "bottomleft", bty = "n", lwd = c(2),
       lty = c(1),
       col= c("black"),
       legend = c("Below 5 years"))

grid()
dev.off()

pdf(paste0(path.static, "niftyjr_v_holdings_25_50.pdf"))
par(mar = c(2, 4, 2, 2))
rng <- niftyjr.var$"25-50"
df.temp <- read.zoo(data.frame(rownames(niftyjr.var), niftyjr.var$"25-50"))
plot(df.temp,
     ylim = range(rng), lwd = 1.5, xlab = "",
     ylab = "Average Holdings",
     col = "black", type = 'l')
legend(x = "bottomleft", bty = "n", lwd = c(2),
       lty = c(1),
       col= c("black"),
       legend = c("5 to 10 years"))

grid()
dev.off()

pdf(paste0(path.static, "niftyjr_v_holdings_50_75.pdf"))
par(mar = c(2, 4, 2, 2))
rng <- niftyjr.var$"50-75"
df.temp <- read.zoo(data.frame(rownames(niftyjr.var), niftyjr.var$"50-75"))
plot(df.temp,
     ylim = range(rng), lwd = 1.5, xlab = "",
     ylab = "Average Holdings",
     col = "black", type = 'l')
legend(x = "bottomleft", bty = "n", lwd = c(2),
       lty = c(1),
       col= c("black"),
       legend = c("10 to 15 years"))

grid()
dev.off()


pdf(paste0(path.static, "niftyjr_v_holdings_75_inf.pdf"))
par(mar = c(2, 4, 2, 2))
rng <- niftyjr.var$">75"
df.temp <- read.zoo(data.frame(rownames(niftyjr.var), niftyjr.var$">75"))
plot(df.temp,
     ylim = range(rng), lwd = 1.5, xlab = "",
     ylab = "Average Holdings",
     col = "black", type = 'l')
legend(x = "bottomleft", bty = "n", lwd = c(2),
       lty = c(1),
       col= c("black"),
       legend = c("greater than 15 years"))

grid()
dev.off()
