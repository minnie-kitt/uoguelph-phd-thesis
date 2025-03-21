## Objective: Replace missing data with within-time-point mean imputation and Last Value Carried Forward/Backward (LVCF/B) 

all_scores_demo <- read.csv("Output/all_scores_demographic.csv", header = TRUE, row.names = "X")
row.names(all_scores_demo) <- NULL

## Create separate dataframes for each time-point
time1 <- all_scores_demo[all_scores_demo$time==1,]
row.names(time1) <- NULL
time2 <- all_scores_demo[all_scores_demo$time==2,]
row.names(time2) <- NULL
time3 <- all_scores_demo[all_scores_demo$time==3,]
row.names(time3) <- NULL
time4 <- all_scores_demo[all_scores_demo$time==4,]
row.names(time4) <- NULL

## The function 'mean_rep_na' replaces NA with the average value/score of each time-point
mean_rep_na <- function(variable = "age", data = time1, round = FALSE){
  temp_df_to_impute <- data[is.na(data[variable]),]

  temp_n_rows_to_impute <- as.integer(row.names(temp_df_to_impute))
  if( round == TRUE){
  temp_mean_to_impute <- round(colMeans(data[variable],na.rm=T))  
  } else {
  temp_mean_to_impute <- colMeans(data[variable],na.rm=T)
  }
  
  for(n in temp_n_rows_to_impute){
      data[temp_n_rows_to_impute,variable][is.na(data[temp_n_rows_to_impute,variable])] <- temp_mean_to_impute
  }
  
  return(data)
}

colSums(is.na(time1))

time1 <- mean_rep_na("pwb")
time1 <- mean_rep_na("p_self_acc")
time1 <- mean_rep_na("p_autonomy")
time1 <- mean_rep_na("swls")
time1 <- mean_rep_na("active", round = TRUE)
time1 <- mean_rep_na("outdoors", round = TRUE)
time1 <- mean_rep_na("finance", round = TRUE)
time1 <- mean_rep_na("social", round = TRUE)

## The function 'lvcf' replaces the NA in data (current) with the value from prev_data (can be previous or future) if it exists
lvcf <- function(variable = "disab_simple", data = time1, prev_data = time234){
  temp_df_to_impute <- data[is.na(data[variable]),]
  temp_n_rows_to_impute <- as.integer(row.names(temp_df_to_impute))
  also_print <- paste0(
    "A total of ", NROW(temp_n_rows_to_impute), " observations were considered to be carried forward / backward.")
  print(also_print)
  for(n in 1:NROW(temp_n_rows_to_impute)){
      temp_row <- temp_n_rows_to_impute[n]
      temp_ID <- data[temp_row,"ID"]
      last_value <- c()
      last_value <- prev_data[prev_data["ID"]==temp_ID,]
      print_this <- c()
      print_that <- c()
      if(NROW(last_value)==0){
        temp_ID <- data[temp_row,"ID"]
        
        print_that <- paste0(
        "Participant ", temp_ID, " does not have a response in a previous  timepoint.", "
        ","Please proceed with function `choice_rep_na()`")
        if(is.null(print_that)==FALSE){
        print(print_that)
      }
      } else{
        latest_row <- NROW(last_value)
        replacement <- prev_data[1,variable]
        data[temp_row,variable][is.na(data[temp_row,variable])] <- replacement
      
      print_this <- paste0(
        "The last value from ", temp_ID, "'s response in timepoint ", last_value[["time"]][1], " : ", 
        variable, " = ", replacement, " was carried forward.")
        if(is.null(print_this)==FALSE){
          print(print_this)
      }
  }


      }

  
  return(data)
  }

##  The function 'lvcf' replaces the NA with a chosen replacement value specified as a string the replacement variable
choice_rep_na <- function(variable = "ethnicity",replacement = "prefer_no", data = time1){
  temp_df_to_impute <- data[is.na(data[variable]),]

  temp_n_rows_to_impute <- as.integer(row.names(temp_df_to_impute))
  
  for(n in temp_n_rows_to_impute){
      data[temp_n_rows_to_impute,variable][is.na(data[temp_n_rows_to_impute,variable])] <- replacement
  }
  message <- paste0(
    "A total of ", NROW(temp_n_rows_to_impute), " observations were replaced with `", replacement, "`.")
  print(message)
  return(data)
}

newtime1 <- time1
time234 <- rbind(time2,time3,time4)

## Function 'recalc_age' fixes the age calculation
recalc_age <- function(data = time1){
  data$age <- data$yob
  data$age <- ifelse(data$age=="Before 1980",1979,data$age)
  data$age <- ifelse(data$age=="Prefer not to say",NA,data$age)
  data$age <- as.numeric(data$class) - as.numeric(data$age)
  
  data$age_cat <- as.numeric(data$age)
  data$age_cat <- ifelse(data$age<25,"24 or less", data$age_cat)
  data$age_cat <- ifelse(data$age>24 & data$age<30,"25 to 29", data$age_cat)
  data$age_cat <- ifelse(data$age>29,"30 or more", data$age_cat)
  return(data)
}

table(time1$yob, useNA = "ifany")
newtime1 <- lvcf("yob",data=newtime1,prev_data = time234)
newtime1 <- choice_rep_na("yob", replacement = 1996, data = newtime1)
newtime1 <- recalc_age(data = newtime1)
newtime1 <- choice_rep_na("age",replacement = 27,data = newtime1)
table(newtime1$age, useNA = "ifany")
newtime1 <- choice_rep_na("age_cat",replacement = "25 to 29",data = newtime1)
table(newtime1$age_cat, useNA = "ifany")

## ---------------------------------------------------------------------------------------------------------------------------------------------------
table(time1$ethnicity, useNA = "ifany")
newtime1 <- lvcf("ethnicity",data=newtime1,prev_data = time234)
newtime1 <- choice_rep_na("ethnicity", data = newtime1)
table(newtime1$ethnicity, useNA = "ifany")

## ---------------------------------------------------------------------------------------------------------------------------------------------------
newtime1 <- lvcf("relationship",data=newtime1,prev_data = time234)
newtime1 <- choice_rep_na("relationship", data = newtime1)
table(newtime1$relationship, useNA = "ifany")


## ---------------------------------------------------------------------------------------------------------------------------------------------------
newtime1 <- lvcf("child",data=newtime1,prev_data = time234)
newtime1 <- choice_rep_na("child",0, data = newtime1)
table(newtime1$child, useNA = "ifany")


## ---------------------------------------------------------------------------------------------------------------------------------------------------
newtime1 <- lvcf("cohabitation",data=newtime1,prev_data = time234)
newtime1 <- choice_rep_na("cohabitation", data = newtime1)
table(newtime1$cohabitation, useNA = "ifany")


## ---------------------------------------------------------------------------------------------------------------------------------------------------
newtime1 <- lvcf("disab_simple",data=newtime1,prev_data = time234)
newtime1 <- choice_rep_na("disab_simple", data = newtime1)
table(newtime1$disab_simple, useNA = "ifany")


## ---------------------------------------------------------------------------------------------------------------------------------------------------
newtime1 <- lvcf("mh_diag",data=newtime1,prev_data = time234)
newtime1 <- choice_rep_na("mh_diag", data = newtime1)
table(newtime1$mh_diag, useNA = "ifany")


## ---------------------------------------------------------------------------------------------------------------------------------------------------
newtime1 <- lvcf("mh_tx",data=newtime1,prev_data = time234)
newtime1 <- choice_rep_na("mh_tx", data = newtime1)
table(newtime1$mh_tx, useNA = "ifany")

colSums(is.na(newtime1))

## Time-point 2   ----

colSums(is.na(time2))


time2 <- mean_rep_na("flo", data = time2)
time2 <- mean_rep_na("p_pers_growth", data = time2)
time2 <- mean_rep_na("p_purpose", data = time2)
time2 <- mean_rep_na("pwb", data = time2)

time2 <- mean_rep_na("active", data = time2, round = TRUE)
time2 <- mean_rep_na("outdoors", data = time2, round = TRUE)
time2 <- mean_rep_na("finance", data = time2, round = TRUE)
time2 <- mean_rep_na("social", data = time2, round = TRUE)


## ---------------------------------------------------------------------------------------------------------------------------------------------------
time134 <- rbind(time1,time3,time4)
newtime2 <- time2


## ---------------------------------------------------------------------------------------------------------------------------------------------------
newtime2 <- lvcf("career",data = newtime2, prev_data = time134)
table(newtime2$career, useNA = "ifany")

## ---------------------------------------------------------------------------------------------------------------------------------------------------
newtime2 <- lvcf("disab_simple",data=newtime2,prev_data = time134)
newtime2 <- choice_rep_na("disab_simple", data = newtime2)
table(newtime2$disab_simple, useNA = "ifany")

## ---------------------------------------------------------------------------------------------------------------------------------------------------
newtime2 <- lvcf("child",data=newtime2,prev_data = time134)
table(newtime2$child, useNA = "ifany")

## ---------------------------------------------------------------------------------------------------------------------------------------------------
newtime2 <- lvcf("mh_diag",data=newtime2,prev_data = time134)
table(newtime2$mh_diag, useNA = "ifany")

## ---------------------------------------------------------------------------------------------------------------------------------------------------
newtime2 <- lvcf("mh_tx",data=newtime2,prev_data = time134)
newtime2 <- choice_rep_na("mh_tx","blank", data = newtime2)
table(newtime2$mh_tx, useNA = "ifany")


## ---------------------------------------------------------------------------------------------------------------------------------------------------
newtime2 <- recalc_age(data = newtime2)

## ---------------------------------------------------------------------------------------------------------------------------------------------------
colSums(is.na(newtime2))

## Time-point 3 ----
colSums(is.na(time3))

## ---------------------------------------------------------------------------------------------------------------------------------------------------
time3 <- mean_rep_na("p_self_acc", data = time3)
time3 <- mean_rep_na("pwb", data = time3)
time3 <- mean_rep_na("active", data = time3, round = TRUE)
time3 <- mean_rep_na("outdoors", data = time3, round = TRUE)
time3 <- mean_rep_na("finance", data = time3, round = TRUE)
time3 <- mean_rep_na("social", data = time3, round = TRUE)


time124 <- rbind(time1,time2,time4)
newtime3 <- time3

## ---------------------------------------------------------------------------------------------------------------------------------------------------
table(time3$yob, useNA = "ifany")
newtime3 <- lvcf("yob",data=newtime3,prev_data = time124)
newtime3 <- recalc_age(data = newtime3)
table(newtime1$age, useNA = "ifany")

## ---------------------------------------------------------------------------------------------------------------------------------------------------
newtime3 <- lvcf("school",data=newtime3,prev_data = time124)
table(newtime3$school, useNA = "ifany")

## ---------------------------------------------------------------------------------------------------------------------------------------------------
newtime3 <- lvcf("child",data=newtime3,prev_data = time124)
table(newtime3$child, useNA = "ifany")

## ---------------------------------------------------------------------------------------------------------------------------------------------------
newtime3 <- lvcf("disab_simple",data=newtime3,prev_data = time124)
table(newtime3$disab_simple, useNA = "ifany")

## ---------------------------------------------------------------------------------------------------------------------------------------------------
newtime3 <- lvcf("mh_tx",data=newtime3,prev_data = time124)

table(newtime3$mh_tx, useNA = "ifany")

## ---------------------------------------------------------------------------------------------------------------------------------------------------
newtime3 <- lvcf("relationship",data=newtime3,prev_data = time124)
table(newtime3$relationship, useNA = "ifany")

## Time-point 4 ----
colSums(is.na(time4))

time4 <- mean_rep_na("active", data = time4, round = TRUE)
time4 <- mean_rep_na("outdoors", data = time4, round = TRUE)
time4 <- mean_rep_na("finance", data = time4, round = TRUE)
time4 <- mean_rep_na("social", data = time4, round = TRUE)

time123 <- rbind(time1,time2,time3)
newtime4 <- time4

# -------------------------------------------------------------------------

newtime4 <- lvcf("lgbt",data=newtime4,prev_data = time123)
table(newtime4$lgbt, useNA = "ifany")

newtime4 <- lvcf("child",data=newtime4,prev_data = time123)
table(newtime4$child, useNA = "ifany")

newtime4 <- lvcf("career",data=newtime4,prev_data = time123)
table(newtime4$career, useNA = "ifany")

newtime4 <- lvcf("disab_simple",data=newtime4,prev_data = time123)
newtime4 <- choice_rep_na("disab_simple", data = newtime4)
table(newtime4$disab_simple, useNA = "ifany")

newtime4 <- lvcf("mh_tx",data=newtime4,prev_data = time123)
table(newtime4$mh_tx, useNA = "ifany")

colSums(is.na(newtime4))

## Combine the imputed dataframes
new_completed_df <- rbind(newtime1,newtime2, newtime3,newtime4)
rownames(new_completed_df) <- NULL

colSums(is.na(new_completed_df))

write.csv(new_completed_df, file = "Output/all_scores_demo-na_replaced.csv")

baseline <- new_completed_df[new_completed_df[["time"]]==1,]
write.csv(baseline, file = "Output/baseline-na_replaced.csv")
