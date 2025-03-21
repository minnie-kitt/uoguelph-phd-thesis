### Use 15% or less for questionnaire scales that did not specify 
### 

mean_imputation <- function(datalist = scores_list){
  for( n in 1:nrow(datalist[["cdrisc"]])){
    temp_df <- datalist[["cdrisc"]]
    temp_df_to_impute <- temp_df[rowSums(is.na(temp_df))<4 & rowSums(is.na(temp_df))>0,]
    if(nrow(temp_df_to_impute)!=0){
    temp_n_rows_to_impute <- as.integer(row.names(temp_df_to_impute))
    temp_mean_to_impute <- rowMeans(temp_df[temp_n_rows_to_impute,1:10],na.rm=T)
    datalist[["cdrisc"]][temp_n_rows_to_impute, ][is.na(datalist[["cdrisc"]][temp_n_rows_to_impute,])] <- temp_mean_to_impute
    datalist[["cdrisc"]][["total"]] <- rowSums(datalist[["cdrisc"]][,1:10])
  }}
  for(n in 1:nrow(datalist[["teique"]])){
    temp_df <- datalist[["teique"]]
    temp_df_to_impute <- temp_df[rowSums(is.na(temp_df))<5 & rowSums(is.na(temp_df))>0,]
    if(nrow(temp_df_to_impute)!=0){
    temp_n_rows_to_impute <- as.integer(row.names(temp_df_to_impute))
    temp_mean_to_impute <- 4
    #temp_mean_to_impute <- rowMeans(temp_df[temp_n_rows_to_impute,1:30],na.rm=T)
    datalist[["teique"]][temp_n_rows_to_impute, ][is.na(datalist[["teique"]][temp_n_rows_to_impute,])] <- temp_mean_to_impute
    datalist[["teique"]][["total"]] <- rowSums(datalist[["teique"]][,1:30])
    datalist[["teique"]][["global_ei"]] <- rowSums(datalist[["teique"]][,1:30])/30
    
    cols <- colnames(datalist[["teique"]])
    all_cols <- cols[1:30]
    
    well_being <- all_cols[c(5,20,9,24,12,27)]
    self_ctrl <- all_cols[c(4,19,7,22,15,30)]
    emotionality <- all_cols[c(1,16,2,17,8,23,13,28)]
    sociability <- all_cols[c(6,21,10,25,11,26)]
    
    datalist[["teique"]][["wellbeing"]] <- rowSums(datalist[["teique"]][,well_being])/6
    datalist[["teique"]][["self_ctrl"]] <- rowSums(datalist[["teique"]][,self_ctrl])/6
    datalist[["teique"]][["emotionality"]] <- rowSums(datalist[["teique"]][,emotionality])/8
    datalist[["teique"]][["sociability"]] <- rowSums(datalist[["teique"]][,sociability])/6
    }}
  for(n in 1:nrow(datalist[["ngse"]])){
    temp_df_to_impute <- datalist$ngse[rowSums(is.na(datalist$ngse))<3 & rowSums(is.na(datalist$ngse))>0,]
    if(nrow(temp_df_to_impute)!=0){
    temp_n_rows_to_impute <- as.integer(row.names(temp_df_to_impute))
    temp_mean_to_impute <- rowMeans(datalist$ngse[temp_n_rows_to_impute,1:8],na.rm=T)
    datalist[["ngse"]][temp_n_rows_to_impute, ][is.na(datalist[["ngse"]][temp_n_rows_to_impute,])] <- temp_mean_to_impute
    datalist[["ngse"]][["total"]] <- rowSums(datalist[["ngse"]][,1:8])}}
  
    temp_df <- datalist[["pwb"]][1:42]
    temp_df_to_impute <- temp_df[rowSums(is.na(temp_df))<7 & rowSums(is.na(temp_df))>0,]
    if(nrow(temp_df_to_impute)!=0){
      temp_n_rows_to_impute <- as.integer(row.names(temp_df_to_impute))
      temp_mean_to_impute <- rowMeans(temp_df[temp_n_rows_to_impute,],na.rm=T)
      temp_reps_per_row <- rowSums(is.na(temp_df_to_impute))
      for(n in 1:NROW(temp_n_rows_to_impute)){
        row_name <- temp_n_rows_to_impute[n]
        datalist[["pwb"]][row_name,1:42][is.na(datalist[["pwb"]][row_name,1:42])] <- rep(temp_mean_to_impute[n],temp_reps_per_row[n])
      }
      datalist[["pwb"]][["total"]] <- rowSums(datalist[["pwb"]][,1:42])
      
      cols <- colnames(datalist[["pwb"]])
      all_cols <- cols[1:42]
      
      autonomy <- all_cols[c(1,7,13,19,25,31,37)]
      envi_master <- all_cols[c(2,8,14,20,26,32,38)]
      pers_growth <- all_cols[c(3,9,15,21,27,33,39)]
      pos_relation <- all_cols[c(4,10,16,22,28,34,40)]
      purpose <- all_cols[c(5,11,17,23,29,35,41)]
      self_acc <- all_cols[c(6,12,18,24,30,36,42)]
      
      datalist[["pwb"]][["autonomy"]] <- rowSums(datalist[["pwb"]][,autonomy])
      datalist[["pwb"]][["envi_master"]] <- rowSums(datalist[["pwb"]][,envi_master])
      datalist[["pwb"]][["pers_growth"]] <- rowSums(datalist[["pwb"]][,pers_growth])
      datalist[["pwb"]][["pos_relation"]] <- rowSums(datalist[["pwb"]][,pos_relation])
      datalist[["pwb"]][["purpose"]] <- rowSums(datalist[["pwb"]][,purpose])
      datalist[["pwb"]][["self_acc"]] <- rowSums(datalist[["pwb"]][,self_acc])
    }
  
  for(n in 1:nrow(datalist[["swls"]])){
    temp_df_to_impute <- datalist$swls[rowSums(is.na(datalist$swls))<2 & rowSums(is.na(datalist$swls))>0,]
    if(nrow(temp_df_to_impute)!=0){
      temp_n_rows_to_impute <- as.integer(row.names(temp_df_to_impute))
      temp_mean_to_impute <- rowMeans(datalist$swls[temp_n_rows_to_impute,1:5],na.rm=T)
      datalist[["swls"]][temp_n_rows_to_impute, ][is.na(datalist[["swls"]][temp_n_rows_to_impute,])] <- temp_mean_to_impute
      datalist[["swls"]][["total"]] <- rowSums(datalist[["swls"]][,1:5])}}
  
  for(n in 1:nrow(datalist[["flo"]])){
    temp_df_to_impute <- datalist$flo[rowSums(is.na(datalist$flo))<3 & rowSums(is.na(datalist$flo))>0,]
    if(nrow(temp_df_to_impute)!=0){
      temp_n_rows_to_impute <- as.integer(row.names(temp_df_to_impute))
      temp_mean_to_impute <- rowMeans(datalist$flo[temp_n_rows_to_impute,1:8],na.rm=T)
      datalist[["flo"]][temp_n_rows_to_impute, ][is.na(datalist[["flo"]][temp_n_rows_to_impute,])] <- temp_mean_to_impute
      datalist[["flo"]][["total"]] <- rowSums(datalist[["flo"]][,1:8])}}
  
  return(datalist)
}

