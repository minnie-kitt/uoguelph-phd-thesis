recode_disab <- function(datalist){
  metadata <- datalist[["metadata"]]
  data <- datalist[["disab"]]
  
  data <- cbind(data,metadata)
  
  previous_other_txt <-  c(
    "bilateral thumb tendonitis and carpal tunnel ",
    "Generalized anxiety/depression",
    "Chronic Migraines",
    "Chronic Migraines",
    "chronic injury",
    "anxiety",
    "Sero-negative immune-mediated arthritis",
    "Spasmodic dysphonia",
    "obsessive compulsive disorder, generalize anxiety disorder",
    "Douleur chronique",
    "inflammatory arthritis",
    "Spasmodic dysphonia",
    "General Anxiety Disorder",
    "GAD, OCD",
    "scoliosis",
    "Anxiety", "Depression, anxiety", "anxiety ",
    "spasmodic dysphonia"
  )
  current_other_txt <- na.omit(data[data$other_txt!="",c("other_txt")])
  diff_other_text <- setdiff(current_other_txt,previous_other_txt)
  
  if(length(diff_other_text)>0){
    print("There are 'other_txt's that were not included in this code.")
  }
  
  for (i in c("sensory","mobility","neurodevelop","chronic","other","yes","no","prefer_no")){
    data[[i]] <- as.numeric(ifelse(data[[i]]==0,0,
                                   ifelse(data[[i]]=="",NA,
                                          ifelse(is.na(data[[i]]),NA,
                                                 1))))}
  
  recode_chronic <- c(
    "bilateral thumb tendonitis and carpal tunnel ",
    "Chronic Migraines",
    "chronic injury",
    "Sero-negative immune-mediated arthritis",
    "Spasmodic dysphonia",
    "spasmodic dysphonia",
    "Douleur chronique",
    "inflammatory arthritis"
  )
  
  for(i in recode_chronic){
    data[["chronic"]] <- ifelse(data[["other_txt"]]==i,1,data$chronic)
  }
  
  # Make a note of folks who listed their MH conditions in here and make sure they are consistent with [["treatment"]]
  
  makenote_treatment <- c(
    "Generalized anxiety/depression",
    "anxiety",
    "obsessive compulsive disorder, generalize anxiety disorder",
    "General Anxiety Disorder",
    "GAD, OCD",
    "Anxiety",
    "Depression, anxiety", "anxiety "
  )
  
  datalist[["treatment"]][["from_disab"]] <- 0
  for(i in recode_chronic){
    datalist[["treatment"]][["from_disab"]] <- ifelse(data[["other_txt"]]==i,1,0)
  }
  
  # Remove "other" and "other_txt" column
  data <- data[, -which(names(data) == c("other"))]
  data <- data[, -which(names(data) == c("other_txt"))]
  
  data1 <- data
  data1[is.na(data1)] <- 0
  
  data$sum <- apply(data1[,1:5], 1, sum)
  data$disab_simple <- NA
  data$disab_simple <- ifelse(data$sum>0,"yes",data$disab_simple)
  for (i in c("yes","no","prefer_no")){
    data$disab_simple <- ifelse(data[[i]]==1,paste(i),data$disab_simple)
  }
  data$disab_type <- data$disab_simple
  for (i in c("sensory","mobility","neurodevelop","chronic")){
    data$disab_type <- ifelse(data[[i]]==1,paste(data$disab_type,i),data$disab_type)
  }
  
  new_data <- data[,c("ID","class","count","disab_simple","disab_type")]
  datalist[["disab"]] <- new_data
  new_datalist <- datalist
  
  return(new_datalist)

}
