#datalist <- cleaned_demo_list

recode_cohab <- function(datalist){
  metadata <- datalist[["metadata"]]
  data <- datalist[["cohab"]]
  
  data <- cbind(data,metadata)
  
  previous_other_txt <- c(
    "brother and nephew",
    "brother and nephew",
    "Currently staying with a tech and looking for a home to live with my pet. ",
    "Live with the other interns veterinarians in provided accomodation ",
    "siblings",
    "I rent from an older couple",
    "Live in an apartment alone but with landlord on same property",
    "Couch surfing at friend's during fourth year",
    "Colleagues ",
    "Grandmother ",
    "Colleagues",
    "My sister","brother","Sibling ", "Sibling (Housemate)"
    )
  current_other_txt <- na.omit(data[data$other_txt!="",c("other_txt")])
  diff_other_text <- setdiff(current_other_txt,previous_other_txt)
  
  if(length(diff_other_text)>0){
    print("There are 'other_txt's that were not included in this code.")
    }
  
  for (i in c("alone","pet","parent","partner","child","roommate","other","prefer_no")){
    data[[i]] <- as.numeric(ifelse(data[[i]]==0,0,
                                   ifelse(data[[i]]=="",NA,
                                          ifelse(is.na(data[[i]]),NA,
                                                 1))))
  }

  recode_parent <- c("brother and nephew", "siblings","Grandmother ","My sister","brother","Sibling ", "Sibling (Housemate)")
  for(i in recode_parent){
    data[["parent"]] <- as.numeric(ifelse(data[["other_txt"]]==i,1,data$parent))
  }
  
  recode_roommate <- c("Currently staying with a tech and looking for a home to live with my pet. ",
                        "Live with the other interns veterinarians in provided accomodation ",
                        "Couch surfing at friend's during fourth year",
                        "Colleagues ",
                        "Colleagues")
  for(i in recode_roommate){
    data[["roommate"]] <- as.numeric(ifelse(data[["other_txt"]]==i,1,data$roommate))
  }
  
  recode_alone <- c("I rent from an older couple",
                    "Live in an apartment alone but with landlord on same property")
  for(i in recode_alone){
    data[["alone"]] <- as.numeric(ifelse(data[["other_txt"]]==i,1,data$alone))
  }
  
  # Remove "other" and "other_txt" column
  data <- data[, -which(names(data) == c("other"))]
  data <- data[, -which(names(data) == c("other_txt"))]
  
  #####
  data1 <- data
  data1[is.na(data1)] <- 0
  data1$alone <- ifelse(data1$alone==1,10,data1$alone)
  data1$prefer_no <- ifelse(data1$prefer_no==1,99,data1$prefer_no)
  data$sum <- apply(data1[,1:7], 1, sum)
  data$cohab<-0
  
  for (i in c("parent","partner","child","roommate")){
    data$cohab <- (ifelse(data$sum==1 & data[[i]]==1,i,data$cohab))}
  
  data$cohab <- ifelse(data$sum==0,NA,data$cohab)
  data$cohab <- ifelse(data$sum==99,"prefer_no",data$cohab)
  
  ## only considering truly "alone" if they choose "alone" only and nothing else
  data$cohab <- ifelse(data$sum==10,"alone",data$cohab)
  
  ## considering all pet variables
  data$cohab <- ifelse(data$sum==1 & data$pet ==1 | data$sum==11 & data$pet==1 ,"pet",data$cohab)
  
  data$cohab <- ifelse(data$sum==2 & data$pet==1,"pets_humans",data$cohab)
  data$cohab <- ifelse(data$sum==3 & data$pet==1,"pets_humans",data$cohab)
  data$cohab <- ifelse(data$sum==2 & data$pet==1,"pets_humans",data$cohab)
  
  ## considering various humans as humans
  data$cohab <- ifelse(data$sum==2 & data$pet==0,"humans",data$cohab)
  
  ## considering all "alone" with other humans as "multiple living situations"
  data$cohab <- ifelse(data$sum==13 & data$pet==1,"multiple",data$cohab)
  data$cohab <- ifelse(data$sum==12 & data$pet==1,"multiple",data$cohab)
  data$cohab <- ifelse(data$sum==11 & data$pet==0,"multiple",data$cohab)
  
  new_data <- cbind(metadata,data[,"cohab"])
  colnames(new_data)[4] <- c("cohab")
  datalist[["cohab"]] <- new_data
  new_datalist <- datalist
  
  return(new_datalist)
  
}
  