library(xtable)
library(parallel)
load("/home/nsdl/DATAMART1/holdings_aggregated.Rdata")
load("/home/nsdl/DATAMART1/client_master_aggregated.Rdata")
load("/home/nsdl/DATAMART1/client_master_non_aggregated.Rdata")
cli.data <- cli.data[!is.na(cli.data$pan_1), ]
cli.data$account_closing_date <-
    as.Date(as.character(cli.data$account_closing_date),
            format = "%y-%m-%d")
client.type <- as.numeric(table(pan.data$client_type))
names(client.type) <- names(table(pan.data$client_type))
cli.data$opening_year <-
    format(as.Date(cli.data$account_opening_date),
           format = "%Y")
cli.data$opening_year <- as.numeric(cli.data$opening_year)
cli.data$closing_year <-
    format(as.Date(cli.data$account_closing_date),
           format = "%Y")
cli.data$closing_year <- as.numeric(cli.data$closing_year)
cli.data$closing_year[which(is.na(cli.data$closing_year))] <- 2016

pan.mul <-
    unique(cli.data$pan_1[which(duplicated(cli.data$pan_1))])
mul.data <- cli.data[which(cli.data$pan_1 %in% pan.mul), ]

mul.data <- mul.data[order(mul.data[["pan_1"]],
                           mul.data[["closing_year"]],
                           mul.data[["account_opening_date"]]), ]

save(mul.data,
     file = "/home/nsdl/DATAMART1/multiple_accounts.Rdata")


c.st <-                                                                         
    seq_along(mul.data$pan_1)[!duplicated(mul.data$pan_1)]              
c.en <- c((c.st[-1] - 1), length(mul.data$pan_1))                           
c.in <- paste0(sep = "c(", c.st, sep = ":", c.en, sep = ")")                    

removeSwitching <- function(inx) {
    c.dat <- mul.data[eval(parse(text = inx)), ]
##     c.dat <- mul[eval(parse(text = inx)), ]
    c.dat$overlap <- c((c.dat$opening_year[-1] -
                        c.dat$closing_year[-nrow(c.dat)]), NA)
    if(length(which(c.dat$overlap >= 0)) > 0) { 
        c.dat <- c.dat[-which(c.dat$overlap >= 0), ]
    }
    c.dat$overlap <- NULL
    return(c.dat)
}

hosts <- c(rep(paste0("user@192.9.11.", c(55, 57, 61)),                             
               each = 8),                                                       
           rep(paste0("user@192.9.11.", c(59)),                             
               each = 4),                                                       
           )                                                                    

my.cluster <- makePSOCKcluster(hosts,                                           
                               master = "192.9.11.50",                          
                               rshcmd = "ssh -i $HOME/.ssh/igidr-ws",           
                               outfile = "",                                    
                               homogenous = TRUE                                
                               )                                                

clusterExport(cl=my.cluster,                                                    
              varlist = c("c.in",                                               
                          "mul.data",
                          ## "mul",
                          "removeSwitching"
                          )                            
              )                                                                 

h.list <- parLapplyLB(cl = my.cluster,                                         
                      X = c.in,                                                
                      fun = removeSwitching                                              
                      )                                                        

save(h.list, file = "/home/nsdl/RESULTS/multiple_accounts_list.Rdata")

stopCluster(my.cluster)
library(data.table)
mul.acc <- rbindlist(h.list)

save(mul.acc, file = "/home/nsdl/RESULTS/multiple_accounts.Rdata")

acc <- NULL
pan <- NULL

for(i in sort(unique(cli.data$opening_year))) {
    nsurv.cli <- cli.data[which(cli.data$opening_year <= i &
                                cli.data$closing_year > i), ]
    total.accounts <- nrow(nsurv.cli)
    nsurv.mul <- mul.data[which(mul.data$opening_year <= i &
                                mul.data$closing_year > i), ]
    multi.accounts <- nrow(nsurv.mul)
    total.pan <- length(unique(cli.data$pan_1))
    multi.pan <- length(unique(mul.data$pan_1))

    account.per <- multi.accounts*100/total.accounts
    pan.per <- multi.pan*100/total.pan
    acc <- c(acc, account.per)
    pan <- c(pan, pan.per)
}



