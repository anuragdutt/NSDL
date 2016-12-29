options(scipen = 999, digit = 6)
load("/home/nsdl/DATAMART1/holdings_aggregated.Rdata")
load("/home/nsdl/CONSTANTS/close_price_all_stocks.Rdata")
load("/home/nsdl/DATAMART1/client_master_aggregated.Rdata")
load("/home/nsdl/RESULTS/DATA_VISUALISATION/AGGREGATED/total_isin_count_aggregated.Rdata")
colnames(isin.dat) <- tolower(colnames(isin.dat))
##############do isin analysis
isin.unique <- unique(isin.dat$isin)

holdings <- holdings[order(holdings[["client_id"]],
                           holdings[["isin"]]), ]
holdings.trackable <- holdings[which(holdings$isin %in% isin.unique),
                               ]

ctype <- pan.data[, c("client_id", "client_type")]
st.in <-
    seq_along(holdings$client_id)[!duplicated(holdings$client_id)]
en.in <- seq_along(holdings$client_id)[!duplicated(holdings$client_id,
                                                   fromLast = TRUE)]


isin.total <- aggregate(isin ~ client_id,
                        data = holdings,
                        length
                        )


isin.track <- aggregate(isin ~ client_id,
                        data = holdings.trackable,
                        length
                        )


total.count <- table(isin.total$isin)
save(total.count,
     file = "/home/nsdl/RESULTS/DATA_VISUALISATION/AGGREGATED/total_isin_count_aggregated.Rdata")


track.count <- table(isin.track$isin)
save(track.count,
     file = "/home/nsdl/RESULTS/DATA_VISUALISATION/AGGREGATED/track_isin_count_aggregated.Rdata")

##########################################################

load("/home/nsdl/RESULTS/DATA_VISUALISATION/portfolio_returns_aggregated.Rdata")

portfolio.ret <- portfolio.ret[order(portfolio.ret[["client_id"]],           ##
                                     portfolio.ret[["val"]]), ]              ##
portfolio.ret <- portfolio.ret[!duplicated(portfolio.ret$client_id), ]       ##
portfolio.ret <- portfolio.ret[-which(portfolio.ret$val ==                   ##
                                      max(portfolio.ret$val)), ]             ##
save(portfolio.ret,                                                          ##
     file = "/home/nsdl/RESULTS/DATA_VISUALISATION/portfolio_returns_aggregated.Rdata") ##

vec <- NULL
series.g <- c(seq(from = 0,
                  to = 9000,
                  by = 1000),
              seq(from = 10000,
                  to = 90000,
                  by = 10000),
              seq(from = 100000,
                  to = 900000,
                  by = 100000),
              seq(from = 1000000,
                  to = 9000000,
                  by = 1000000))


series.l <- c(seq(from = 1000,
                  to = 10000,
                  by = 1000),
              seq(from = 20000,
                  to = 100000,
                  by = 10000),
              seq(from = 200000,
                  to = 1000000,
                  by = 100000),
              seq(from = 2000000,
                  to = 10000000,
                  by = 1000000))
series <- paste0(sep = "length(which(portfolio.ret$val >= ",
                 series.g,
                 sep = " & portfolio.ret$val < ",
                 series.l,
                 sep = "))")

vec <-  NULL
for(i in series) {
    print(i)
    vec <- c(vec, eval(parse(text = i)))
}
vec <- c(vec, length(which(portfolio.ret$val > 10000000)))
names(vec) <- c(series.l, ">10000000")

save(vec, file = "/home/nsdl/RESULTS/DATA_VISUALISATION/AGGREGATED/account_value_distribution_aggregated.Rdata")


mean.holdings <- median(portfolio.ret$val)

sum(portfolio.ret$val
    )
sum(holdings$qty)


####################################################
## kernel distribution
#############################################

load("/home/nsdl/RESULTS/DATA_VISUALISATION/portfolio_returns_aggregated.Rdata")
portfolio.ret <- portfolio.ret[which(portfolio.ret$val > 1000 &
                                     portfolio.ret$val < 1000000000), ]
portfolio.ret$log_val <- log(portfolio.ret$val)
portfolio.ret$val_sd  <- sd(portfolio.ret$log_val)
den <- density(log(portfolio.ret$val))

pdf("/home/nsdl/RESULTS/DATA_VISUALISATION/portfolio_value_kernel_density.pdf")
layout(matrix(c(1,2),nrow=2), heights=c(3,1))
par(mar=c(0,5,7,3), mgp = c(3,1,0), las = 1)
plot(den,
#     log = "x",
    # ylim = range(den$y),
     lwd = 1.2,
     xlab = " Log portfolio value",
     ylab = "",
     col = "black",
     type = 'l',
     xaxt = 'n',
     main = "Portfolio value density")

abline(v = log(as.numeric(names(sort(table(portfolio.ret$val),
                                     decreasing = TRUE))[1])), lty = 2)
abline(v = log(mean(portfolio.ret$val)), lty = 2)
abline(v = log(median(portfolio.ret$val)), lty = 2)

mtext(expression(bold("Portfolio value (in Rs millions)")),
      side = 1, line = 3)
axis(1,
     at = c(0, 4, 8, 12, 16, 20),
     labels = round(exp(c(0, 4, 8, 12, 16, 20))/1000000, 3),
     las = 1)

grid()

mtext(side= 3,
      paste0("mode = ",
             round(as.numeric(names(sort(table(portfolio.ret$val),
                                         decreasing = TRUE))[1])/1000000, 3)
            ),
      at = log(as.numeric(names(sort(table(portfolio.ret$val),
                                     decreasing = TRUE))[1])) ,
      line = 0.5,
      cex = 0.65
      )


mtext(side= 3,
      paste0("median = ",
             round(median(portfolio.ret$val)/1000000, 2)
             ),
      at = log(median(portfolio.ret$val)),
      line = 0.5,
      cex = 0.65
      )

mtext(side= 3,
      paste0("mean = ",
             round(mean(portfolio.ret$val)/1000000, 2)
             ),
      at = log(mean(portfolio.ret$val)),
      line = 0.5,
      cex = 0.65
      )

dev.off()

############################################################
load("/home/nsdl/DATAMART1/holdings_non_aggregated.Rdata")
load("/home/nsdl/CONSTANTS/close_price_all_stocks.Rdata")
load("/home/nsdl/DATAMART1/client_master_non_aggregated.Rdata")
holdings$security_type     <- substr(holdings$isin, 8, 9)
holdings$security_category <- substr(holdings$isin, 1, 3)
sec.qty <- aggregate(qty ~ security_type,
                     data = holdings,
                     sum
                     )
save(sec.qty, file = "/home/nsdl/RESULTS/DATA_VISUALISATION/security_type.Rdata") 

client.type <- pan.data[, c("client_id", "client_type")]
client.holdings <- merge(holdings, client.type,
                         by = "client_id",
                         all.x = TRUE)
client.holdings <-
    client.holdings[!is.na(client.holdings$client_type), ]
client.holdings$security_category[-which(client.holdings$security_category
                                         %in% c("INE", "INF"))] <- "OTH"
client.holdings$security_client <- paste0(client.holdings$client_type,
                                          sep = "-",
                                          client.holdings$security_category)
typ.qty <- aggregate(qty ~ security_client,
                     data = client.holdings,
                     sum
                     )
save(typ.qty, file = "/home/nsdl/RESULTS/DATA_VISUALISATION/security_distribution.Rdata") 

#############################################################


client.val  <- merge(portfolio.ret, client.type,
                     by = "client_id",
                     all.x = TRUE)
client.val   <- client.val[!is.na(client.val$client_type), ]
client.val$client_type[which(client.val$client_type == 12)] <- 11
client.val$client_type[which(client.val$client_type == 14)] <- 13
average.type <-  aggregate(val ~ client_type,
                           data = client.val,
                           FUN = mean)
median.type  <- aggregate(val ~ client_type,
                          data = client.val,
                          FUN = median)



client.val$category <- NA
client.val$category[which(client.val$client_type == 1)] <- 1
client.val$category[which(client.val$client_type%in% c(2,3,5,6,8,9,10))]  <- 2
client.val$category[which(client.val$client_type%in% c(4,7,11,12,13,14))] <- 3

average.category <-  aggregate(val ~ category,
                           data = client.val,
                           FUN = mean)
median.category  <- aggregate(val ~ category,
                          data = client.val,
                          FUN = median)

type.val     <- merge(average.type, median.type, by = "client_type")
colnames(type.val) <- c("client_type", "mean", "median")
category.val <- merge(average.category, median.category, by = "category")
colnames(category.val) <- c("client_category", "mean", "median")
type.val$client_type<- c("Resident",
                        "FI",
                        "FII",
                        "NRI",
                        "Corporate",
                        "Clearing Member",
                        "Foreign National",
                        "Mutual Fund",
                        "Trust",
                        "Bank",
                        "Qualified Foreign Investor",
                        "Foreign Portfolio Investor"
                        )
category.val$client_id <- c("Retail",
                            "Institutional",
                            "Foreign"
                            )

save(type.val,
     file = "/home/nsdl/RESULTS/DATA_VISUALISATION/client_type_portfolio_value.Rdata")
save(category.val,
     file = "/home/nsdl/RESULTS/DATA_VISUALISATION/client_category_portfolio_value.Rdata")
