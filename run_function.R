if (!require("plyr")) install.packages("plyr")

library(plyr)
source("FraudDetection.R")

transactions <- readRDS("./data/transactions.rds")


FraudDetection(2, "median", 2)
FraudDetection(transactions, "medi", 2)
FraudDetection(transactions, "median", -2)
aaa <- FraudDetection_2(transactions, "median", 10)