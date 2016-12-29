rm(list = ls())
library("zoo")
date.seq <- seq(from = as.Date("2015-05-01"),
                to = as.Date("2015-05-10"),
                by = "day")
date.day <- format(date.seq, "%a")
date.seq <- date.seq[which(date.day != "Sun")]
date.seq <- as.character(date.seq)
date.seq <- date.seq[-1]

path.static <- "/home/nsdl/RESULTS/DEMO_SUMSTATS/"
nifty.var   <- NULL
niftyjr.var <- NULL
both.var    <- NULL
age.var     <-NULL

for(dt in date.seq) {

    file.path <-
        paste0(path.static, "Client_holdings_",
               dt, ".Rdata")
    load(file.path)

    age.diff <- result[!duplicated(result$client_id),
                       c("client_id", "dp_id", 
                         "age_account")]
    age.diff$age_account <- as.numeric(age.diff$age_account)
    age.diff$age_bracket <- NA
    age.diff$age_bracket[which(age.diff$age_account <= (365*5 + 1))] <- 1
    age.diff$age_bracket[which(age.diff$age_account > (365*5 + 1) &
                               age.diff$age_account <= (2*365*5 + 2))] <-
        2
    age.diff$age_bracket[which(age.diff$age_account > (2*365*5 + 2) &
                               age.diff$age_account <= (3*365*5 + 3))] <-
        3
    age.diff$age_bracket[which(age.diff$age_account > (3*365*5 + 3))] <- 4
    
    
    df.a <- table(age.diff$age_bracket)
    df.p <- df.a/sum(df.a)*100
    names(df.a) <- c("0-5", "5-10", "10-15", ">15")
    names(df.p) <- c("0-5", "5-10", "10-15", ">15")
    fn.a <- paste0(path.static, "table_a_age_", dt, ".Rdata")
    fn.p <- paste0(path.static, "table_p_age_", dt, ".Rdata")
    save(df.a, file = fn.a) 
    save(df.p, file = fn.p)
    age.dat  <- age.diff[, c("client_id", "age_bracket")]
    isin.dat <- tapply(result$qty, result$client_id, sum)
    isin.all <- data.frame(client_id = names(isin.dat), qty = isin.dat)
    age.merge <- merge(age.dat, isin.all, by = "client_id", all.x =
                                                                TRUE)
    age.merge$qty[is.na(age.merge$qty)] <- 0
    age.v <- tapply(age.merge$qty, age.merge$age_bracket, mean)
    age.var <- rbind(age.var, age.v)

    
    nifty.diff <- result[!duplicated(result$client_id),
                         c("client_id", "dp_id", "qty",
                           "nifty_per")]
    quartiles  <- quantile(unique(nifty.diff$nifty_per), c(0.25, 0.5, 0.75))
    nifty.diff$nifty_per <- as.numeric(nifty.diff$nifty_per)
    nifty.diff$nifty_bracket <- NA
    nifty.diff$nifty_bracket[which(nifty.diff$nifty_per <= as.numeric(quartiles[1]))] <- 1
    nifty.diff$nifty_bracket[which(nifty.diff$nifty_per > as.numeric(quartiles[1]) &
                                   nifty.diff$nifty_per <= as.numeric(quartiles[2]))] <-
        2
    nifty.diff$nifty_bracket[which(nifty.diff$nifty_per > as.numeric(quartiles[2]) &
                                   nifty.diff$nifty_per <= as.numeric(quartiles[3]))] <-
        3
    nifty.diff$nifty_bracket[which(nifty.diff$nifty_per > as.numeric(quartiles[3]))] <- 4

    df.a <- table(nifty.diff$nifty_bracket)
    df.p <- df.a/sum(df.a)*100
    names(df.a) <- c("0-25", "25-50", "50-75", ">75")
    names(df.p) <- c("0-25", "25-50", "50-75", ">75")
    fn.a <- paste0(path.static, "table_a_nifty_", dt, ".Rdata")
    fn.p <- paste0(path.static, "table_p_nifty_", dt, ".Rdata")
    save(df.a, file = fn.a) 
    save(df.p, file = fn.p)
    nifty.dat  <- nifty.diff[, c("client_id", "nifty_bracket")]
    isin.dat <- tapply(result$qty, result$client_id, sum)
    isin.all <- data.frame(client_id = names(isin.dat), qty = isin.dat)
    nifty.merge <- merge(nifty.dat, isin.all, by = "client_id", all.x =
                                                                TRUE)
    nifty.merge$qty[is.na(nifty.merge$qty)] <- 0
    nifty.v <- tapply(nifty.merge$qty, nifty.merge$nifty_bracket, mean)
    nifty.var <- rbind(nifty.var, nifty.v)



    niftyjr.diff <- result[!duplicated(result$client_id),
                         c("client_id", "dp_id", "qty",
                           "niftyjr_per")]
    quartiles  <- quantile(unique(niftyjr.diff$niftyjr_per), c(0.25, 0.5, 0.75))
    niftyjr.diff$niftyjr_per <- as.numeric(niftyjr.diff$niftyjr_per)
    niftyjr.diff$niftyjr_bracket <- NA
    niftyjr.diff$niftyjr_bracket[which(niftyjr.diff$niftyjr_per <= as.numeric(quartiles[1]))] <- 1
    niftyjr.diff$niftyjr_bracket[which(niftyjr.diff$niftyjr_per > as.numeric(quartiles[1]) &
                                   niftyjr.diff$niftyjr_per <= as.numeric(quartiles[2]))] <-
        2
    niftyjr.diff$niftyjr_bracket[which(niftyjr.diff$niftyjr_per > as.numeric(quartiles[2]) &
                                   niftyjr.diff$niftyjr_per <= as.numeric(quartiles[3]))] <-
        3
    niftyjr.diff$niftyjr_bracket[which(niftyjr.diff$niftyjr_per > as.numeric(quartiles[3]))] <- 4

    df.a <- table(niftyjr.diff$niftyjr_bracket)
    df.p <- df.a/sum(df.a)*100
    names(df.a) <- c("0-25", "25-50", "50-75", ">75")
    names(df.p) <- c("0-25", "25-50", "50-75", ">75")
    fn.a <- paste0(path.static, "table_a_niftyjr_", dt, ".Rdata")
    fn.p <- paste0(path.static, "table_p_niftyjr_", dt, ".Rdata")
    save(df.a, file = fn.a) 
    save(df.p, file = fn.p)
    niftyjr.dat  <- niftyjr.diff[, c("client_id", "niftyjr_bracket")]
    isin.dat <- tapply(result$qty, result$client_id, sum)
    isin.all <- data.frame(client_id = names(isin.dat), qty = isin.dat)
    niftyjr.merge <- merge(niftyjr.dat, isin.all, by = "client_id", all.x =
                                                                TRUE)
    niftyjr.merge$qty[is.na(niftyjr.merge$qty)] <- 0
    niftyjr.v <- tapply(niftyjr.merge$qty, niftyjr.merge$niftyjr_bracket, mean)
    niftyjr.var <- rbind(niftyjr.var, niftyjr.v)

    

    result$both_per <- result$nifty_per + result$niftyjr_per
    both.diff <- result[!duplicated(result$client_id),
                         c("client_id", "dp_id", "qty",
                           "both_per")]
    quartiles  <- quantile(unique(both.diff$both_per), c(0.25, 0.5, 0.75))
    both.diff$both_per <- as.numeric(both.diff$both_per)
    both.diff$both_bracket <- NA
    both.diff$both_bracket[which(both.diff$both_per <= as.numeric(quartiles[1]))] <- 1
    both.diff$both_bracket[which(both.diff$both_per > as.numeric(quartiles[1]) &
                                   both.diff$both_per <= as.numeric(quartiles[2]))] <-
        2
    both.diff$both_bracket[which(both.diff$both_per > as.numeric(quartiles[2]) &
                                   both.diff$both_per <= as.numeric(quartiles[3]))] <-
        3
    both.diff$both_bracket[which(both.diff$both_per > as.numeric(quartiles[3]))] <- 4

    df.a <- table(both.diff$both_bracket)
    df.p <- df.a/sum(df.a)*100
    names(df.a) <- c("0-25", "25-50", "50-75", ">75")
    names(df.p) <- c("0-25", "25-50", "50-75", ">75")
    fn.a <- paste0(path.static, "table_a_both_", dt, ".Rdata")
    fn.p <- paste0(path.static, "table_p_both_", dt, ".Rdata")
    save(df.a, file = fn.a) 
    save(df.p, file = fn.p)
    both.dat  <- both.diff[, c("client_id", "both_bracket")]
    isin.dat <- tapply(result$qty, result$client_id, sum)
    isin.all <- data.frame(client_id = names(isin.dat), qty = isin.dat)
    both.merge <- merge(both.dat, isin.all, by = "client_id", all.x =
                                                                TRUE)
    both.merge$qty[is.na(both.merge$qty)] <- 0
    both.v <- tapply(both.merge$qty, both.merge$both_bracket, mean)
    both.var <- rbind(both.var, both.v)
    
    
}


age.var <- data.frame(age.var)
nifty.var <- data.frame(nifty.var)
niftyjr.var <- data.frame(niftyjr.var)
both.var <- data.frame(both.var)

rownames(age.var) <- date.seq
colnames(age.var) <- c("0-5", "5-10", "10-15", ">15")
rownames(nifty.var) <- date.seq
colnames(nifty.var) <- c("0-25", "25-50", "50-75", ">75")
rownames(niftyjr.var) <- date.seq
colnames(niftyjr.var) <- c("0-25", "25-50", "50-75", ">75")
rownames(both.var) <- date.seq
colnames(both.var) <- c("0-25", "25-50", "50-75", ">75")

save(age.var, file = paste0(path.static, "age_v_holdings.Rdata"))
save(nifty.var, file = paste0(path.static, "nifty_v_holdings.Rdata"))
save(niftyjr.var, file = paste0(path.static, "niftyjr_v_holdings.Rdata"))
save(both.var, file = paste0(path.static, "both_v_holdings.Rdata"))

pdf(paste0(path.static, "age_v_holdings.pdf"))
par(mar = c(2, 4, 2, 2))
rng <- c(age.var$"0-5",
         age.var$"5-10",
         age.var$"10-15",
         age.var$">15"
         )
df.temp <- read.zoo(data.frame(rownames(age.var), age.var$"0-5"))
plot(df.temp,
     ylim = range(rng), lwd = 1.5, xlab = "", ylab = "Average Holdings",
     col = "black", type = 'l')
df.temp <- read.zoo(data.frame(rownames(age.var), age.var$"5-10"))
lines(df.temp,
      lwd = 1.5, col = "red")
df.temp <- read.zoo(data.frame(rownames(age.var), age.var$"10-15"))
lines(df.temp,
      lwd = 1.5, col = "orange")
df.temp <- read.zoo(data.frame(rownames(age.var), age.var$">15"))
lines(df.temp,
      lwd = 1.5, col = "green")
legend(x = "bottomleft", bty = "n", lwd = c(2,2,2,2),
       lty = c(1,1,1,1),
       col= c("black", "red", "blue", "green", "orange"),
       legend = c("Below 5 years", "5-10 years", "10-15 years",
           "greater than 15 years"))
grid()
dev.off()


pdf(paste0(path.static, "nifty_v_holdings.pdf"))
par(mar = c(2, 4, 2, 2))
rng <- c(nifty.var$"0-25",
         nifty.var$"25-50",
         nifty.var$"50-75",
         nifty.var$">75"
         )
df.temp <- read.zoo(data.frame(rownames(nifty.var), nifty.var$"0-25"))
plot(df.temp,
     ylim = range(rng), lwd = 1.5, xlab = "", ylab = "Average Holdings",
     col = "black", type = 'l')
df.temp <- read.zoo(data.frame(rownames(nifty.var), nifty.var$"25-50"))
lines(df.temp,
      lwd = 1.5, col = "red")
df.temp <- read.zoo(data.frame(rownames(nifty.var), nifty.var$"50-75"))
lines(df.temp,
      lwd = 1.5, col = "orange")
df.temp <- read.zoo(data.frame(rownames(nifty.var), nifty.var$">75"))
lines(df.temp,
      lwd = 1.5, col = "green")
legend(x = "bottomleft", bty = "n", lwd = c(2,2,2,2),
       lty = c(1,1,1,1),
       col= c("black", "red", "blue", "green", "orange"),
       legend = c("1st quartile", "2nd quartile", "3rd quartile",
           "4th quartile"))
grid()
dev.off()


pdf(paste0(path.static, "niftyjr_v_holdings.pdf"))
par(mar = c(2, 4, 2, 2))
rng <- c(niftyjr.var$"0-25",
         niftyjr.var$"25-50",
         niftyjr.var$"50-75",
         niftyjr.var$">75"
         )
df.temp <- read.zoo(data.frame(rownames(niftyjr.var), niftyjr.var$"0-25"))
plot(df.temp,
     ylim = range(rng), lwd = 1.5, xlab = "", ylab = "Average Holdings",
     col = "black", type = 'l')
df.temp <- read.zoo(data.frame(rownames(niftyjr.var), niftyjr.var$"25-50"))
lines(df.temp,
      lwd = 1.5, col = "red")
df.temp <- read.zoo(data.frame(rownames(niftyjr.var), niftyjr.var$"50-75"))
lines(df.temp,
      lwd = 1.5, col = "orange")
df.temp <- read.zoo(data.frame(rownames(niftyjr.var), niftyjr.var$">75"))
lines(df.temp,
      lwd = 1.5, col = "green")
legend(x = "bottomleft", bty = "n", lwd = c(2,2,2,2),
       lty = c(1,1,1,1),
       col= c("black", "red", "blue", "green", "orange"),
       legend = c("1st quartile", "2nd quartile", "3rd quartile",
           "4th quartile"))
grid()
dev.off()


pdf(paste0(path.static, "both_v_holdings.pdf"))
par(mar = c(2, 4, 2, 2))
rng <- c(both.var$"0-25",
         both.var$"25-50",
         both.var$"50-75",
         both.var$">75"
         )
df.temp <- read.zoo(data.frame(rownames(both.var), both.var$"0-25"))
plot(df.temp,
     ylim = range(rng), lwd = 1.5, xlab = "", ylab = "Average Holdings",
     col = "black", type = 'l')
df.temp <- read.zoo(data.frame(rownames(both.var), both.var$"25-50"))
lines(df.temp,
      lwd = 1.5, col = "red")
df.temp <- read.zoo(data.frame(rownames(both.var), both.var$"50-75"))
lines(df.temp,
      lwd = 1.5, col = "orange")
df.temp <- read.zoo(data.frame(rownames(both.var), both.var$">75"))
lines(df.temp,
      lwd = 1.5, col = "green")
legend(x = "bottomleft", bty = "n", lwd = c(2,2,2,2),
       lty = c(1,1,1,1),
       col= c("black", "red", "blue", "green", "orange"),
       legend = c("1st quartile", "2nd quartile", "3rd quartile",
           "4th quartile"))
grid()
dev.off()
