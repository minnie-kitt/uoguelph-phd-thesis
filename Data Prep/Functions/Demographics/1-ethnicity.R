recode_ethnicity <- function(datalist){
  metadata <- datalist[["metadata"]]
  data <- datalist[["ethnic"]]
  
  data <- cbind(data,metadata)

    for (i in c("indigenous","white","black","east_asian","southeast_asian","arab","south_asian","latin","west_asian","other","prefer_no")){
        data[[i]] <- as.numeric(ifelse(data[[i]]==0,0,
                          ifelse(data[[i]]=="",NA,
                                 ifelse(is.na(data[[i]]),NA,
                                 1))))
        }
  data1 <- data
  data1[is.na(data1)] <- 0
  data$sum <- apply(data1[,1:11], 1, sum)
  data$mixed <- ifelse(data$sum>1,1,0)
  data$blank <- ifelse(data$sum<1,1,0)
  data <- data[, -which(names(data) == "sum"), drop = FALSE]
  
  for (i in c("indigenous","white","black","east_asian","southeast_asian","arab","south_asian","latin","west_asian","other","prefer_no")){
    data[[i]] <- ifelse(data$mixed>0,0,data[[i]])
    }
  
  library(tidyr)
  piv_data <- data %>% 
  pivot_longer(cols = c("indigenous","white","black","east_asian","southeast_asian","arab","south_asian","latin","west_asian","other","mixed","prefer_no","blank"), names_to = "ethnicity", values_to = "num")
  
  data <- data.frame(na.omit(piv_data[piv_data$num>0,]))
  
  data$ethnicity <- ifelse(data$other_txt=="Israeli","west_asian",data$ethnicity)
  data$ethnicity <- ifelse(data$other_txt=="Punjabi","south_asian",data$ethnicity)
  data$ethnicity<- ifelse(data$ethnicity=="blank",NA,data$ethnicity)
  
  new_data <- cbind(metadata,data[,"ethnicity"])
  colnames(data)[4] <- c("ethnicity")
  datalist[["ethnic"]] <- new_data
  new_datalist <- datalist
  
  return(new_datalist)
  
}
