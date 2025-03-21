import_data <- function(file_path,rows_to_rm,class,language){
  df <- read.csv(file_path, header = T)
  rows_to_rm <- as.integer(rows_to_rm)
  df <- df[-c(rows_to_rm),]
  df[["progress"]] <- as.integer(df[["progress"]])
  df[["class"]] <- as.character(class)
  
  # rename columns
  colnames(df)[19:28] <- c(paste0("cdr_",1:10))
  colnames(df)[29:70] <- c(paste0("pwb_",1:42))
  colnames(df)[71:78] <- c(paste0("ngse_",1:8))
  colnames(df)[79:86] <- c(paste0("flo_",1:8))
  colnames(df)[87:91] <- c(paste0("swls_",1:5))
  colnames(df)[92] <- c("sise")
  colnames(df)[93:122] <- c(paste0("tei_",1:30))
  colnames(df)[123:131] <- c(paste0("proqol_",1:9))
  colnames(df)[132] <- c("burnout")
  
  if(class==2022){
    if(language=="en"){
      colnames(df)[c(134:143,145:152,154:200)] <- c(paste0("demo_", 1:65)) #dropped covid question in demographics
    } 
    else if(language=="fr"){
      colnames(df)[c(134:151,153:199)] <- c(paste0("demo_", 1:65))
    }}
  else if(class==2023){
    colnames(df)[c(134:198)] <- c(paste0("demo_", 1:65))
  }
  
  row.names(df) <- NULL
  
  return(df)
}