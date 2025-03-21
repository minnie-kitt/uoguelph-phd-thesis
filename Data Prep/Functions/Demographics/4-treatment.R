recode_treatment <- function(datalist){
  data <- datalist[["treatment"]]
  
  previous_other_txt <-  c(
    "Counseling",
    "Counselling",
    "I was on medication but I can’t get disability insurance if I stay on it :(", 
    "currently neither med or therapy" 
  )
  current_other_txt <- na.omit(data[data$other_txt!="",c("other_txt")])
  diff_other_text <- setdiff(current_other_txt,previous_other_txt)
  
  if(length(diff_other_text)>0){
    print("There are 'other_txt's that were not included in this code.")
  }
  
  data <- data[, -which(names(data) == c("other"))]
  
  ## Make sure those who specified mh as disab were included
  data$mh_diag <- ifelse(data$from_disab==1,"yes_diag",data$mh_diag)
  data <- data[, -which(names(data) == c("from_disab"))]
  
  data$mh_diag <- ifelse(data$mh_diag=="",NA,data$mh_diag)
  data$mh_diag <- ifelse(data$mh_diag=="Non" | data$mh_diag=="No","no",data$mh_diag)
  data$mh_diag <- ifelse(data$mh_diag=="Prefer not to say","prefer_no",data$mh_diag)
  data$mh_diag <- ifelse(data$mh_diag=="Oui, j'ai déjà reçu un diagnostic." | data$mh_diag=="Yes, and I have been diagnosed.","yes_diag",data$mh_diag)
  data$mh_diag <- ifelse(data$mh_diag=="Oui, mais je n'ai pas reçu de diagnostic officiel." | data$mh_diag=="Yes, but I have not received a formal diagnosis.","yes",data$mh_diag)
  
  recode_ther <- c(
    "Counseling",
    "Counselling"
  )
  
  for(i in recode_ther){
    data[["tx_ther"]] <- ifelse(data[["other_txt"]]==i,1,data$tx_ther)
  }
  
  recode_med <- c(
    "I was on medication but I can’t get disability insurance if I stay on it :("

  )
  
  for(i in recode_med){
    data[["tx_med"]] <- ifelse(data[["other_txt"]]==i,1,data$tx_med)
  }
  
  recode_both <- c(
  )
  
  for(i in recode_both){
    data[["tx_ther"]] <- ifelse(data[["other_txt"]]==i,1,data$tx_ther)
    data[["tx_med"]] <- ifelse(data[["other_txt"]]==i,1,data$tx_med)
  }
  
  recode_no_tx <- c(
    "currently neither med or therapy" 
  )
  
  for(i in recode_no_tx){
    data[["no_tx"]] <- ifelse(data[["other_txt"]]==i,1,data$no_tx)
  }
  
  for (i in c("tx_med","tx_ther","no_tx","prefer_no")){
    data[[i]] <- as.numeric(ifelse(data[[i]]==0,0,
                                   ifelse(data[[i]]=="",NA,
                                          ifelse(is.na(data[[i]]),NA,
                                                 1))))}
  
  data <- data[, -which(names(data) == c("other_txt"))]
  
  data1 <- data
  data1[is.na(data1)] <- 0
  
  data$sum <- apply(data1[2:3], 1, sum)
  data$mh_tx <- 0
  data$mh_tx <- ifelse(data$mh_diag=="no","no_hx",data$mh_tx)
  data$mh_tx <- ifelse(data$mh_diag=="yes" | data$mh_diag == "yes_diag","blank",data$mh_tx)
  data$mh_tx <- ifelse(data$sum==0 & data$mh_tx==0,NA,data$mh_tx)
  data$mh_tx <- ifelse(data$sum==1 & data$tx_med==1,"med",data$mh_tx)
  data$mh_tx <- ifelse(data$sum==1 & data$tx_ther==1,"therapy",data$mh_tx)
  data$mh_tx <- ifelse(data$sum==2,"both",data$mh_tx)
  data$mh_tx <- ifelse(data$no_tx==1 & data$mh_tx==0,"no",data$mh_tx)
  data$mh_tx <- ifelse(data$prefer_no==1 & data$mh_tx==0,"prefer_no",data$mh_tx)

  

  
  
  cleaned_data <- data[,c("mh_diag","mh_tx")]
  
  metadata <- datalist[["metadata"]]
  new_data <- cbind(metadata,cleaned_data)
  
  datalist[["treatment"]] <- new_data
  new_datalist <- datalist
  
  return(new_datalist)

}