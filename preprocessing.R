### DATA PREPARATION ###
########################

install.packages("plyr")
library(plyr)


transactions <- read.csv('./data/transactions.csv',header=TRUE, sep=";")
transactions <- rename(transactions, c("acount"="partner_account_id"))
############################
levels(transactions$type)
transactions$type <- revalue(transactions$type, c("PRIJEM"="income", "VYDAJ"="expenditure", "VYBER" = "withdraw" ))
table(transactions$type)

levels(transactions$operation)
table(transactions$operation)
transactions$operation <- revalue(transactions$operation, c("PREVOD NA UCET"="remittance to another bank", "PREVOD Z UCTU"="collection from another bank", "VKLAD" = "cash deposit", "VYBER" = "cash withdraw", "VYBER KARTOU" = "card withdraw", " " = "other" ))

levels(transactions$k_symbol)
table(transactions$k_symbol)
transactions$k_symbol <- revalue(transactions$k_symbol, c("DUCHOD"="old-age pension", "POJISTNE"="insurrance payment", "SANKC. UROK" = "sanction", "SIPO" = "household", "SLUZBY" = "payment for statement", "UROK" = "interest credited", "UVER" = "loan payment", " " = "other" ))

for (i in 475305:nrow(transactions)){
  x <- substring(transactions[i,"date"], seq(1,5,2), seq(2,6,2))
  x2 <- paste(x[2], x[3], x[1], sep ="/")
  transactions[i,"betterDates"] <- as.Date(x2,format = "%m/%d/%y") + (19*365.25)
  #transactions[i,"betterDates"] <- transactions[i,"betterDates"] + (19*365.25)
}

transactions$date2 <- NULL
transactions$year <- NULL
transactions$month <- NULL
transactions$day <- NULL
transactions$data <- NULL

transactions$date <- transactions$betterDates
transactions$betterDates <- NULL

transactions$year <- NULL
transactions$month <- NULL
transactions$day <- NULL
transactions$data <- NULL
transactions$date2 <- NULL

transactions$date <- transactions$betterDates
transactions$betterDates <- NULL

transactions$partner_account_id <- as.numeric(transactions$partner_account_id)

#transactions$partner_account_id[transactions$partner_account_id == 0] <- 0
transactions$partner_account_id[is.na(transactions$partner_account_id)] <- 0

saveRDS(transactions, file="transactions.rds")
##############################

disp$disp_id <- NULL
disp2 <- reshape(disp, idvar="account_id", timevar="type", direction="wide")
disp2 <- rename(disp2, c("client_id.OWNER"="owner_id", "client_id.DISPONENT"="disponent_id"))

accounts_data <- disp2
saveRDS(accounts_data, file="accounts_data.rds")



partner_accounts_data <- accounts_data
partner_accounts_data <- rename(accounts_data, c("account_id"="partner_account_id","owner_id" ="partner_owner_id",
                                                 "disponent_id" = "partner_disponent_id"))
saveRDS(partner_accounts_data, file="partner_accounts_data.rds")

trans <- na.omit(transactions)
aggregated <- aggregate(trans_id ~ account_id + partner_account_id + type, transactions, FUN = "length")
aggregated  <- dplyr::left_join(aggregated , accounts_data, by = "account_id")
aggregated  <- dplyr::left_join(aggregated , partner_accounts_data, by = "partner_account_id")


