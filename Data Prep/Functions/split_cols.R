split_cols <- function(dataframe,choice){
  if(choice=="demographic"){
    demographic_df <- dataframe[,c("ID","class","count",paste0("demo_", 1:65))] 
    career <- demographic_df[,c(paste0("demo_", 1:14))] #career and practice
    colnames(career) <- c("small","equine","ruminants","exotic","farm","public_h","academia","non_vet","undecided","other","prefer_no","other_txt","practice","practice_txt")
    
    lifestyle <- demographic_df[,c(paste0("demo_", 15:18))] # lifestyle
    colnames(lifestyle) <- c("active","outdoors","finance","social")
    
    ethnic <- demographic_df[,c(paste0("demo_", 23:34))] #ethnicity
    colnames(ethnic) <- c("indigenous","white","black","east_asian","southeast_asian","arab","south_asian","latin","west_asian","other","prefer_no","other_txt")
    
    demo1 <- demographic_df[,c(paste0("demo_", c(19:22,35:40)))] #demographics 1
    colnames(demo1) <- c("prov","prov_txt","school","yob","gender","gender_txt","lgbt","relat","relat_txt","child")
    cohab <-  demographic_df[,c(paste0("demo_", 41:49))] #cohabitation
    colnames(cohab) <- c("alone","pet","parent","partner","child","roommate","other","prefer_no","other_txt")
    
    disab <- demographic_df[,c(paste0("demo_", 50:58))]  #disability
    colnames(disab)<- c("sensory","mobility","neurodevelop","chronic","other","yes","no","prefer_no","other_txt")
    
    treatment <- demographic_df[,c(paste0("demo_", 59:65))] #treatment
    colnames(treatment) <- c("mh_diag","tx_med","tx_ther","other","no_tx","prefer_no","other_txt")
    
    cleaned_datalist <- list(demographic_df[,c("ID","class","count")],career,lifestyle,ethnic,demo1,cohab,disab,treatment)
    names(cleaned_datalist) <- c("metadata","career","lifestyle","ethnic","demo1","cohab","disab","treatment")
    
  } else if (choice=="scores"){
    scores_df <- dataframe[,c("ID","class","count",paste0("cdr_",1:10),
                              paste0("ngse_",1:8), paste0("tei_",1:30),
                              "burnout", "sise")]
    
    metadata <- dataframe[,c("ID","class","count")]
    cdrisc <- dataframe[,c(paste0("cdr_",1:10))]
    pwb <- dataframe[,c(paste0("pwb_",1:42))]
    ngse <- dataframe[,c(paste0("ngse_",1:8))]
    flo <- dataframe[,c(paste0("flo_",1:8))]
    swls <- dataframe[,c(paste0("swls_",1:5))]
    teique <- dataframe[,c(paste0("tei_",1:30))]
    proqol <- dataframe[,c(paste0("proqol_",1:9))]
    burnout <- dataframe[,c("burnout")]
    sise <- dataframe[,c("sise")]
    
    cleaned_datalist <- list(metadata,cdrisc,pwb,ngse,flo,swls,teique,proqol,burnout,sise)
    names(cleaned_datalist) <- c("metadata","cdrisc", "pwb","ngse","flo","swls","teique","proqol","burnout","sise")
  }
  return(cleaned_datalist)
}