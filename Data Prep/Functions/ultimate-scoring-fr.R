scoring_fr <- function(data){
    
  ##### CD-RISC #####
  cdrisc <- cbind(data$ID, data[19:28],data$class,data$count)
  colnames(cdrisc) <- c("ID",paste0("cdr_",1:10),"class","time")
  score <- c("Pas vrais du tout" = 0, "Rarement vrai" = 1, "Quelquefois vrai" = 2,
             "Souvent vrai" = 3, "Presque toujours vrai" = 4)
  cdrisc[2:11] <- sapply(cdrisc[2:11],function(x) score[x])
  cdrisc$total <- rowSums(cdrisc[,2:11])
  cdrisc <- as.data.frame(lapply(cdrisc, as.numeric))
  cdrisc$ID <- as.character(cdrisc$ID)
  
  ##### New General Self-Efficacy (NGSE) #####
  ngse <- cbind(data$ID, data[71:78],data$class,data$count)
  colnames(ngse) <- c("ID",paste0("ngse_",1:8),"class","time")
  score <- c("Fortement en désaccord" = 1, "Plutôt en désaccord" = 2, 
             "Plutôt en désaccord " = 2, 
             "Ni d'accord ni en désaccord" = 3, "Plutôt d'accord"= 4,
             "Tout à fait d'accord" = 5)
  ngse[2:9] <- sapply(ngse[2:9],function(x) score[x])
  ngse$total <- rowSums(ngse[,2:9])/8
  
  ##### TEIque #####
  tei <- cbind(data$ID, data[93:122],data$class,data$count)
  colnames(tei) <- c("ID", paste0("tei_",1:30),"class","time")
  score <- c("1 (Pas du tout d'accord)" = 1, "2" = 2, 
             "3" = 3,"4" = 4, "5" = 5, "6" = 6, "7 (Tout à fait d'accord)" = 7)
  reverse <- c("1 (Pas du tout d'accord)" = 7, "2" = 6, 
               "3" = 5,"4" = 4, "5" = 3, "6" = 2, "7 (Tout à fait d'accord)" = 1)
  tei_sc <- tei
  rm(tei)
  cols <- colnames(tei_sc)
  cols <- cols[2:31]
  all_cols<-cols
  rev_cols <- cols[c(2,4,5,7,8,10,12,13,14,16,18,22,25,26,28)]
  cols <- cols[c(1,3,6,9,11,15,17,19:21,23:24,27,29:30)]
  
  tei_sc[cols] <- sapply(tei_sc[cols],function(x) score[x])
  tei_sc[rev_cols] <- sapply(tei_sc[rev_cols],function(x) reverse[x])
  tei_sc$total <- rowSums(tei_sc[,2:31])
  tei_sc$global_ei <- rowSums(tei_sc[,2:31])/30
  
  wellbeing <- all_cols[c(5,20,9,24,12,27)]
  self_ctrl <- all_cols[c(4,19,7,22,15,30)]
  emotionality <- all_cols[c(1,16,2,17,8,23,13,28)]
  sociability <- all_cols[c(6,21,10,25,11,26)]
  
  tei_sc$wellbeing <- rowSums(tei_sc[,wellbeing])/6
  tei_sc$self_ctrl <- rowSums(tei_sc[,self_ctrl])/6
  tei_sc$emotionality <- rowSums(tei_sc[,emotionality])/8
  tei_sc$sociability <- rowSums(tei_sc[,sociability])/6
  
  ##### Burnout #####
  
  burnout <- cbind(data$ID, data[132],data$class,data$count)
  colnames(burnout) <- c("ID", "burnout","class","time")
  burnout_score <- c("J'aime mon travail. Je n'ai pas de symptômes d'épuisement professionnel." = 1, 
                     "Il m'arrive d'être stressé(e) et je n'ai pas toujours autant d'énergie qu'avant, mais je ne me sens pas épuisé(e)." = 2, 
                     "Je suis définitivement en épuisement professionnel et je présente un ou plusieurs symptômes de burnout, comme l'épuisement physique et émotionnel." = 3,
                     "Les symptômes d'épuisement que j'éprouve ne disparaissent pas. Je pense souvent à ma frustration au travail."= 4,
                     "Je me sens complètement épuisé(e) et je me demande souvent si je peux continuer. J'en suis au point où j'ai peut-être besoin de changements ou d'une aide quelconque." = 5)
  
  burnout$burnout <- sapply(burnout$burnout,function(x) burnout_score[x])
  
  ##### Single Item Self-Esteem (SISE) #####
  
  sise <- cbind(data$ID, data[92],data$class,data$count)
  colnames(sise) <- c("ID", "sise","class","time")
  sise_score <- c("1 (Pas très vrai pour moi)" = 1, 
                  "2" = 2, "3" = 3, "4" = 4,
                  "5" = 5, "6" = 6, "7 (Très vrai pour moi)" = 7)
  sise$sise <- sapply(sise$sise,function(x) sise_score[x])

  
  output <- list(cdrisc,ngse,tei_sc,sise,burnout)
  names(output) <- c("cdrisc","ngse","tei_sc","sise","burnout")
  
  return(output)
}