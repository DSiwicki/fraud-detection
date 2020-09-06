#####     FUNCTION V.1 

FraudDetection <-  function(transactions, central_tendency_measure, CTN_multiplier, amount_limit = 15000, count_limit = 6, amount_multiplier = 0.8){
  # Checking correctness of provided arguments
  if (central_tendency_measure %in% c('mean', "mode","median") && is.numeric(CTN_multiplier) && CTN_multiplier > 0 && is.data.frame(transactions) && is.numeric(amount_limit)){
  
    ##### PART 2
    # If amount of transaction is CTN_multipier times higher than CTN, transaction should be checked.
    if (central_tendency_measure == "mean"){
      tendecy_values <- ddply(transactions,~account_id,summarise,CTN=mean(amount))
    }
    else if(central_tendency_measure == "mode"){
      tendecy_values <- ddply(transactions,~account_id,summarise, CTN=mode(amount))}

    else if(central_tendency_measure == "median"){
      tendecy_values <- ddply(transactions,~account_id,summarise, CTN=median(amount))
    }
    transactions_tendency <- dplyr::left_join(transactions, tendecy_values, by = "account_id")
    
    frauds_1 <- transactions_tendency[transactions_tendency$amount >= transactions_tendency$CTN * CTN_multiplier,]
    
    ##### PART 2
    # Transactions grouped by account_id, partner_account_id and type of transaction (salaries and some payments are not frauds, we should drop them)
    
    data_to_aggregate <- transactions[transactions$partner_account_id != 0,]
    aggregated <- aggregate(trans_id ~ account_id + partner_account_id + type + k_symbol, data_to_aggregate, FUN = "length")
    aggregated  <- dplyr::left_join(aggregated , accounts_data, by = "account_id")
    aggregated  <- dplyr::left_join(aggregated , partner_accounts_data, by = "partner_account_id")
    
    sub <- subset(aggregated, owner_id == partner_owner_id | owner_id == partner_disponent_id |
                    disponent_id == partner_owner_id | disponent_id == partner_disponent_id)
    frauds_2 <- transactions[transactions$account_id %in% sub$account_id & transactions$partner_account_id %in% sub$partner_account_id,]
    
    ##### PART 3
    sub_2 <- subset(transactions, amount > amount_limit & k_symbol == "")

    sub_2bis <- aggregate(trans_id ~ account_id + partner_account_id + type + k_symbol + amount, sub_2, FUN = "length")
    sub_2bis <- rename(sub_2bis, c("trans_id"="trans_count"))
    sub_2bis2 <- subset(sub_2bis, trans_count < count_limit)
    
    frauds_3 <- data.frame()
    for (i in 1:nrow(sub_2bis2)){
      subset_i <- subset(transactions, account_id == sub_2bis2[i,"account_id"] & type == sub_2bis2[i,"type"] & partner_account_id == sub_2bis2[i,"partner_account_id"]  & amount == sub_2bis2[i,"amount"])
      if (nrow(subset_i) > 0){
        
        for (j in 1:nrow(subset_i)){
          if (subset_i[j,"type"] == "income"){
            transactions_types = c("expenditure","withdraw")
            sub_3 <- subset(transactions, account_id == subset_i[j,"account_id"] & (date == subset_i[j,"date"] | date == subset_i[j,"date"]+1) & type %in% transactions_types )
            if (nrow(sub_3) > 0){
              if (sum(sub_3$amount) >= amount_multiplier * sub_2bis2[i,"amount"]){
                frauds_3 <- rbind(frauds_3, subset_i[j,])
              }
            }
          }
          else{
            sub_4 <- subset(transactions, account_id == subset_i[j,"account_id"] & type == subset_i[j,"type"] & date < subset_i[j,"date"])
            if (nrow(sub_4) > 0){
              if (median(sub_4$amount) < subset_i[j,"amount"]){
                frauds_3 <- rbind(frauds_3, subset_i[j,])
              }
            }
          }
        }
      }
    }
    
    frauds <- dplyr::bind_rows(frauds_1, frauds_2)
    frauds <- dplyr::bind_rows(final, frauds_3)
  }
  else{
    frauds = "Wrong arguments provided! Please provide correct arguments."
  }
  return(frauds)
}

####################################################################################################################################
####################################################################################################################################
####################################################################################################################################
#####     FUNCTION V.2 

FraudDetection_2 <-  function(transactions, central_tendency_measure, CTN_multiplier, amount_limit = 15000, count_limit = 6, amount_multiplier = 0.8){
  # Checking correctness of provided arguments
  if (central_tendency_measure %in% c('mean', "mode","median") && is.numeric(CTN_multiplier) && CTN_multiplier > 0 && is.data.frame(transactions)){
    
    ### PART 1
    # If amount of transaction is CTN_multipier times higher than CTN, transaction should be checked.
    if (central_tendency_measure == "mean"){
      tendecy_values <- ddply(transactions,~account_id,summarise,CTN=mean(amount))
    }
    else if(central_tendency_measure == "mode"){
      tendecy_values <- ddply(transactions,~account_id,summarise, CTN=mode(amount))}
    
    else if(central_tendency_measure == "median"){
      tendecy_values <- ddply(transactions,~account_id,summarise, CTN=median(amount))
    }
    transactions_tendency <- dplyr::left_join(transactions, tendecy_values, by = "account_id")
    
    frauds_1 <- transactions_tendency[transactions_tendency$amount >= transactions_tendency$CTN * CTN_multiplier,]
    
    ##### PART 2
    # Transactions grouped by account_id, partner_account_id and type of transaction (salaries and some payments are not frauds, we should drop them)
    
    data_to_aggregate <- transactions[transactions$partner_account_id != 0,]
    
    data <- data.frame()
    unique_users <- unique(transactions$account_id)
    for (i in 1:length(unique_users)){
      data_1 <- transactions[transactions$account_id == unique_users[i],]
      unique_partner <- unique(data_1$partner_account_id)
      for (j in 1:length(unique_partner)){
        data_2 <- data_1[data_1$partner_account_id == unique_partner[j],]
        unique_type <- unique(data_2$type)
        for(k in 1:length(unique_type)){
          data_3 <- data_2[data_2$type == unique_type[k],]
          unique_symbol <- unique(data_3$k_symbol)
          for(l in length(unique_symbol)){
            data_4 <- data_3[data_3$k_symbol == unique_symbol[l],]
            data_5 <- data.frame(data_4$account_id[1],data_4$partner_account_id[1],data_4$type[1], data_4$k_symbol[1], nrow(data_4))
            data <- rbind(data, data_5)
          }
        }
      }
    }
    names(data) = c("account_id","partner_account_id","type","k_symbol","trans_id")
    data <- dplyr::left_join(data , accounts_data, by = "account_id")
    data <- dplyr::left_join(data , partner_accounts_data, by = "partner_account_id")
    
    sub <- data.frame()

    for (i in 1:nrow(data)){
      if (data[i, "owner_id"] == data[i, "partner_owner_id"] | data[i, "owner_id"] == data[i, "partner_disponent_id"] |
          data[i, "disponent_id"] == data[i, "parter_owner_id"] | data[i, "disponent_id"] == data[i, "partner_disponent_id"] ){
        
        data_6 <- data[i,]
        sub <- rbind(sub, data_6)
      }
    }

    frauds_2 <- transactions[transactions$account_id %in% sub$account_id & transactions$partner_account_id %in% sub$partner_account_id,]

    ##### PART 3
    sub_2 <- subset(transactions, amount > amount_limit & k_symbol == "")
    
    sub_2bis <- aggregate(trans_id ~ account_id + partner_account_id + type + k_symbol + amount, sub_2, FUN = "length")
    sub_2bis <- rename(sub_2bis, c("trans_id"="trans_count"))
    sub_2bis2 <- subset(sub_2bis, trans_count < count_limit)
    
    frauds_3 <- data.frame()
    for (i in 1:nrow(sub_2bis2)){
      subset_i <- subset(transactions, account_id == sub_2bis2[i,"account_id"] & type == sub_2bis2[i,"type"] & partner_account_id == sub_2bis2[i,"partner_account_id"]  & amount == sub_2bis2[i,"amount"])
      if (nrow(subset_i) > 0){
        
        for (j in 1:nrow(subset_i)){
          if (subset_i[j,"type"] == "income"){
            transactions_types = c("expenditure","withdraw")
            sub_3 <- subset(transactions, account_id == subset_i[j,"account_id"] & (date == subset_i[j,"date"] | date == subset_i[j,"date"]+1) & type %in% transactions_types )
            if (nrow(sub_3) > 0){
              if (sum(sub_3$amount) >= amount_multiplier * sub_2bis2[i,"amount"]){
                frauds_3 <- rbind(frauds_3, subset_i[j,])
              }
            }
          }
          else{
            sub_4 <- subset(transactions, account_id == subset_i[j,"account_id"] & type == subset_i[j,"type"] & date < subset_i[j,"date"])
            if (nrow(sub_4) > 0){
              if (median(sub_4$amount) < subset_i[j,"amount"]){
                frauds_3 <- rbind(frauds_3, subset_i[j,])
              }
            }
          }
        }
      }
    }

    frauds <- dplyr::bind_rows(frauds_1, frauds_2)
    frauds <- dplyr::bind_rows(final, frauds_3)
  }
  else{
    frauds = "Wrong arguments provided! Please provide correct arguments."
  }
  return(frauds)
}

########################################
### CHECK FUNCTION
setwd()
transactions <- readRDS("transactions.rds")
accounts_data <- readRDS("accounts_data.rds")
partner_accounts_data <- readRDS("partner_accounts_data.rds")

FraudDetection(2, "median", 2)
FraudDetection(transactions, "medi", 2)
FraudDetection(transactions, "median", -2)
aaa <- FraudDetection(transactions, "median", 10)


