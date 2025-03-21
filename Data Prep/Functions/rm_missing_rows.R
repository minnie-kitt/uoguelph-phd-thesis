rm_missing_rows <- function(dataframe, cutoff){
  rows_to_rm <- c()
  temp_df <- dataframe
  temp_df$missing <- 0
  for(i in 1:nrow(dataframe)){
    temp_df[[i,"missing"]] <- sum(is.na(dataframe[i,]==TRUE))
    if (temp_df[[i,"missing"]] > cutoff){
      new_row <- i
      rows_to_rm <- c(rows_to_rm,new_row)
    }}
  new_dataframe <- dataframe[-c(rows_to_rm),]
  return(new_dataframe)
}