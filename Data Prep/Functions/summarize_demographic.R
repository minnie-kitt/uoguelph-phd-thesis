summarize_demographic <- function(datalist){
  dfs_to_merge <- names(datalist)
  new_df <- data.frame(datalist[["metadata"]][["ID"]],
                       datalist[["metadata"]][["class"]],
                       datalist[["metadata"]][["count"]])
  colnames(new_df) <- c("ID","class","count")
  
  for (df in dfs_to_merge[2:length(dfs_to_merge)]){
    new_df <- cbind(new_df,
                    datalist[[df]][4:length(datalist[[df]])])
  }
  
  sorted_merged_df <- new_df <- new_df[order(new_df$class, new_df$count), ]
  
  colnames(sorted_merged_df) <- c( "ID","class","time","career",
                                   "career_type","practice","active","outdoors",
                                   "finance","social","ethnicity","province",
                                   "school","yob","age","gender",
                                   "lgbt","relationship","child","cohabitation",
                                   "disab_simple","disab_type","mh_diag","mh_tx")
  
  return(sorted_merged_df)
}