scoring_en <- function(data){
  
  ## CD-RISC
  cdrisc <- cbind(data$ID, data[19:28],data$class,data$count)
  colnames(cdrisc) <- c("ID",paste0("cdr_",1:10),"class","time")
  score <- c("Not true at all." = 0, "Rarely true." = 1, "Sometimes true." = 2,"Often true." = 3, "True nearly all the time." = 4)
  cdrisc[2:11] <- sapply(cdrisc[2:11],function(x) score[x])
  cdrisc$total <- rowSums(cdrisc[,2:11])
  cdrisc <- as.data.frame(lapply(cdrisc, as.numeric))
  cdrisc$ID <- as.character(cdrisc$ID)
  
  
  ## New General Self-Efficacy (NGSE)
  ngse <- cbind(data$ID, data[71:78],data$class,data$count)
  colnames(ngse) <- c("ID",paste0("ngse_",1:8),"class","time")
  score <- c("Strongly disagree" = 1, "Somewhat disagree" = 2, 
             "Neither agree nor disagree" = 3, "Somewhat agree"= 4,
             "Strongly agree" = 5)
  ngse[2:9] <- sapply(ngse[2:9],function(x) score[x])
  ngse$total <- rowSums(ngse[,2:9])/8
  
  
  
  ## TEIque
  tei <- cbind(data$ID, data[93:122],data$class,data$count)
  colnames(tei) <- c("ID", paste0("tei_",1:30),"class","time")
  score <- c("1 (Completely Disagree)" = 1, "2" = 2, 
             "3" = 3,"4" = 4, "5" = 5, "6" = 6, "7 (Completely Agree)" = 7)
  reverse <- c("1 (Completely Disagree)" = 7, "2" = 6, 
               "3" = 5,"4" = 4, "5" = 3, "6" = 2, "7 (Completely Agree)" = 1)
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
  
  burnout_score <- c("I enjoy my work. I have no symptoms of burnout." = 1, 
                     "Occasionally I am under stress, and I don't always have as much energy as I once did, but I don't feel burned out." = 2, 
                     "I am definitely burning out and have one or more symptoms of burnout, such as physical and emotional exhaustion." = 3,
                     "The symptoms of burnout that I'm experiencing won't go away. I think about frustration at work a lot."= 4,
                     "I feel completely burned out and often wonder if I can go on. I am at the point where I may need some changes or may need to seek some sort of help." = 5)
  
  burnout$burnout  <- sapply(burnout$burnout,function(x) burnout_score[x])
  burnout
  
  ##### Single Item Self-Esteem (SISE) #####
  
  sise <- cbind(data$ID, data[92],data$class,data$count)
  colnames(sise) <- c("ID", "sise","class","time")
  row.names(sise) <- NULL 
  sise_score <- c("1\n(Not very true of me)" = 1, 
                  "2" = 2, "3" = 3, "4" = 4,
                  "5" = 5, "6" = 6, "7\n(Very true of me)" = 7)
  sise$sise <- sapply(sise$sise,function(x) sise_score[x])

  
  datalist <- list(cdrisc,ngse,tei_sc,sise,burnout)
  names(datalist) <- c("cdrisc","ngse","tei_sc","sise","burnout")

  return(datalist)
  
}