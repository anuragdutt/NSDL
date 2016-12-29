rm(list=ls())
load("portfolio_returns_2015-05-01.Rdata")

                                        # Number of accounts
nrow(h.sumstats)

                                        # Summary stat on value of
                                        # portfolio (in Rs.million)

summary(h.sumstats$val)/1000000

                                        # Summary stats on age
h.sumstats$age_account <- as.numeric(h.sumstats$age_account)
round(summary(h.sumstats$age_account)/365,2)
                                        # Number of accounts between
                                        # 5-10-15 years
h.sumstats$years <- cut(h.sumstats$age_account, breaks = c(365,
                                                    1825,3650,
                                                    5475,5561))
levels(h.sumstats$years) <- c("0-4", "5-9", "10-14", "15-20")
prop.table(table(h.sumstats$years))

                                        # Value by number of years
                                        # (maybe a silly metric)
tapply(h.sumstats$val/1000000, h.sumstats$years, mean)

                                        # Accounts in districts
table(h.sumstats$district)

                                        # Districts that have seen the
                                        # most accounts in the recent
                                        # years
table(h.sumstats$district[h.sumstats$years=="0-4"])
