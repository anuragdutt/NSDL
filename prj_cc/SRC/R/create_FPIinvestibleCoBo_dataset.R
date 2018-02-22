############################################################################
## The program creates the dataset consisting, all FPI investible
## corporate debt securities by using data from two different sources:
## 1) NSDL website: Trade-wise debt data of FPI/FII from 2002 -
## 2016(Jan).
## 2) NSDL website: Details of all corporate bonds.
## The final dataset contains the unique ISINs of FPI investible
## corporate debt securities (between 2014-2016(Jan)), along with
## maturity, type of instrument(CP/Warrants/Convertible bonds etc.),
## and issuing compay's name.
###########################################################################


##############
## Read files
##############
load("../../RESOURCES/FROM_NSDL/FPI_debtSecTradingData_201601.rda")
load("../../RESOURCES/FROM_NSDL/debt_master.rda")


## FPI trades in debt securities data
                                        # Pick only relevant fields
debttrades_fpi <- debtTrades_FPI[, which(colnames(debtTrades_FPI) %in%
                                         c("FII",
                                           "SCRIP_NAME",
                                           "ISIN",
                                           "TR_DATE",
                                           "TR_TYPE...",
                                           "RFDD_SE_REG_NUM",
                                           "RFDD_INSTR_TYPE"))]
                                        # Change column names
colnames(debttrades_fpi) <- c("FII_ID", "Scrip_Name", "ISIN",
                              "TransactionDate", "TranscationType",
                              "StockExch_code", "Instrument_Type")
  
                                        # Pick transactions only after 2014
debttrades_fpi$TransactionDate <- as.Date(debttrades_fpi$TransactionDate,
                                          format = "%m/%d/%Y")
debttrades_fpi$TR_YR <- format(debttrades_fpi$TransactionDate, "%Y")
debttrades_fpi <- debttrades_fpi[which(debttrades_fpi$TR_YR > 2014), ]


## Debt master: All corporate bonds details 
colnames(debt_master) <- c("IssuingCompany", "ISIN_num",
                           "Instrument_name",
                           "InstrumentDesc_NSDL",
                           "IssuePrice_perSec",
                           "FV_perSec",
                           "IssueDate_allotment",
                           "Redemption",
                           "Coupon_rate",
                           "InterestPayment_freq",
                           "PutCallOptions",
                           "CertificateNos",
                           "Registrar",
                           "AddressForPhysicalDelivery",
                           "CreditRating",
                           "Arranger_LeadManager",
                           "Remarks")

debt_master <- debt_master[, which(colnames(debt_master) %in%
                                   c("IssuingCompany", "ISIN_num",
                                     #"Instrument_name",
                                     "Coupon_rate",
                                     "InstrumentDesc_NSDL",
                                     "IssuePrice_perSec",
                                     "FV_perSec",
                                     "IssueDate_allotment",
                                     "Redemption",
                                     "CreditRating"))]
                                   
debt_master$IssueDate_allotment <- as.Date(debt_master$IssueDate_allotment,
                                           format = "%d/%m/%Y")

debt_master$Redemption <- as.Date(debt_master$Redemption,
                                  format = "%d/%m/%Y")
                                        # Compute maturity in years
debt_master$Maturity_yrs <- (as.double(debt_master$Redemption -
                          debt_master$IssueDate_allotment)/365)


## Final debt data: FPI investible corporate bonds between 2014 - 2016(Jan)
cobo_FPIinvestible_data <- merge(debttrades_fpi, debt_master,
                                 by.x = "ISIN",
                                 by.y = "ISIN_num")


##########################################################################
## save(cobo_FPIinvestible_data,                                        ##
##      file = "../../RESOURCES/FROM_NSDL/cobo_FPIinvestible_data.rda") ##
##########################################################################





