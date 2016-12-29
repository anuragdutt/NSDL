rm(list=ls())
library(data.table)

## From Anurag: The data consists of holdings of all the people who
## have held nestle on any day between 01-06-2015 and 30-06-2015. The
## account modification date tells the last date on which the account
## was active.  We would need a base date to figure out when and how
## trading happened. So if you set the base date on 01-06-2015, We can
## figure out bought and sold from that specific date, by reading the
## subsequent holdings dates.

## Original data
orgdata <- read.csv("dummy_nestle.csv", sep=",", header=T)
odata  <- data.table(orgdata)

## Remove absurd quantities - here defining absurd as greater than
## 100000 qty.
odata <- odata[qty %in% c(0:100000)]

## Create a data-set with the relevant column names for the change in
## holding metrics
data <- subset(odata, select=c("client_id", "date", "isin", "qty"))

## The stocks and the date
nestle <- "INE239A01016"
chosendate <- "01-05-2015"
eventdate <- "03-05-2015"

## Functions

change_in_holding <- function(stock=nestle, dataset=data){
    new_data <- subset(dataset, isin==stock)
    new_data_1 <- new_data[,.(val=c(NA, diff(qty)), qty=qty, date=date),
                           by=client_id]
    new_data_1 <- new_data_1[, .(date=date, qty=qty, val=val,
                                 incholding = ifelse(val < 0, 0, 1)),
                             by=client_id]
    return(new_data_1)
}

numbershares_portfolio <- function(chosendate=chosendate, dataset=data){
    new_data <- subset(dataset, date==chosendate)
    new_data_1 <- new_data[,.N, by=c("client_id")]
    return(new_data_1)
}

age_account <- function(chosendate, dataset){
    new_data <- subset(dataset, date==chosendate)
    new_data_1 <- new_data[, .SD[1], by="client_id"]
    new_data_1 <- new_data_1[,.(age=
                                ceiling(difftime(as.Date(chosendate,
                                                         format="%d-%m-%Y"), 
                                    as.Date(account_opening_date, format="%Y-%m-%d"),
                                    units = c("days"))/365)), by="client_id"]
    return(new_data_1)
} 

niftyshares_portfolio <- function(both=0, chosendate=chosendate, dataset){
    new_data <- subset(dataset, date==chosendate)
    new_data_1 <- new_data[, .SD[1], by="client_id"]
    new_data_1 <- new_data_1[,.(nifty= ifelse(both==0, nifty,
                                    sum(nifty, niftyjr, na.rm=TRUE))),
                             by="client_id"]
    return(new_data_1)        
}



## Run the functions

change_in_holding(stock=nestle, dataset=data)
numbershares_portfolio(chosendate, data)
age_account(chosendate, odata)
niftyshares_portfolio(both=0, chosendate, odata)
