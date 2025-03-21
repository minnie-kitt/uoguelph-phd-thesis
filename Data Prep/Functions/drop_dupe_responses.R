drop_dupe_responses <- function(dataframe){
  duplicates <- dataframe[duplicated(dataframe[c("ID", "count")]),c("ID","count")]
  list_rows_to_rm <- c()
  for (i in seq_len(nrow(duplicates))) {
    id <- as.character(duplicates[i, "ID"])
    count <- as.character(duplicates[i, "count"])
    
    temp_df <- dataframe[dataframe$ID==id & dataframe$count==count,]
    temp_max_progress <- max(temp_df[["progress"]])
    temp_df_rows_to_rm <- row.names(temp_df[temp_df[["progress"]]!=temp_max_progress,])
    temp_df_rows_to_rm <- as.integer(temp_df_rows_to_rm)
    list_rows_to_rm <- c(list_rows_to_rm,temp_df_rows_to_rm)
  }
  if(is.null(list_rows_to_rm)==FALSE){
    dataframe <- dataframe[-c(list_rows_to_rm),]
  }
  return (dataframe)
}
