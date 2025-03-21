combine_files <- function(df1,df2,df3,df4){
  combined_df <- rbind(df1,df2,df3,df4)
  if (is.null(combined_df$class)==FALSE & is.null(combined_df$count)==FALSE){
    combined_df <- combined_df[order(combined_df$class, combined_df$count), ]
  } else if (is.null(combined_df$class)==FALSE & is.null(combined_df$time)==FALSE){
    combined_df <- combined_df[order(combined_df$class, combined_df$time), ]
  }
  return(combined_df)
}