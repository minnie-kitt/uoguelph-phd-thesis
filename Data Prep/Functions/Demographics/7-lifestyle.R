recode_lifestyle <- function(datalist){
  data <- datalist[["lifestyle"]]

  
  for (i in c("active","outdoors","finance","social")){
    data[[i]] <- ifelse(data[[i]]=="",NA,data[[i]])}
  
  
  likert <- c("A lot"=3, "A little"=1, "A moderate amount"=2, "None at all"=0, 
              "A great deal"=4, "Un peu\t"=1, "Une quantité modérée\t"=2, 
              "Pas du tout\t"=0, "Beaucoup\t"=3,"Énormément"=4)
  
  data[2:4] <- sapply(data[2:4],function(x) likert[x])
  
  a_score <- c("Less than 1 hour"=0, "1 - 2 hours"=1,  "3 - 4 hours"=2, "5 - 6 hours"=3, "7 - 8 hours"=4, "More than 8 hours"=5, 
               "Moins d'une heure"=0,"1 à 2 heures"=1, "3 à 4 heures"=2, "5 à 6 heures"=3, "7 à 8 heures"=4,  "Plus de 8 heures"=5)
  
  data$active <- a_score[data$active]
  
  metadata <- datalist[["metadata"]]
  new_data <- cbind(metadata,data)
  datalist[["lifestyle"]] <- new_data
  new_datalist <- datalist
  
  return(new_datalist)
}