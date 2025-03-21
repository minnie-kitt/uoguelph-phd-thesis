extract_cols <- function(dataframe, choice){
  if(choice=="demographic"){
    extracted_df <- dataframe[,c("ID","class","count",paste0("demo_", 1:65))]
  } else if (choice=="scores"){
    extracted_df <- dataframe[,c("ID","class","count",
                                 paste0("cdr_",1:10),
                                 paste0("pwb_",1:42),
                                 paste0("ngse_",1:8),
                                 paste0("flo_",1:8),
                                 paste0("swls_",1:5),
                                 "sise",
                                 paste0("tei_",1:30),
                                 paste0("proqol_",1:9),
                                 "burnout")]}
  return(extracted_df)
}