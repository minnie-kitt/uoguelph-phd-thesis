mnar_check <- function(datalist){
  metadata <- datalist[["metadata"]]
  cdrisc <- datalist[["cdrisc"]]
  
  all_scales <- c("cdrisc", "pwb","ngse","flo","swls","teique","proqol","burnout","sise")
  missing_count <- expand.grid(ID = metadata$ID,cdrisc="",pwb="",
                         ngse = "",flo="",swls="",teique="",
                         proqol = "", burnout = "", sise = "")
  
  for( scale in all_scales){
    for( n in nrow(datalist[[scale]])){
      temp_df <- datalist[[scale]]
      missing_count[[scale]] <- rowSums(is.na(temp_df))
      
    }}
  missing_count$total <- rowSums(missing_count[2:10])
  return(missing_count)
  }
    
  

