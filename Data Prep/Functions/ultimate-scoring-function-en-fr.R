scoring <- function(datalist){
  metadata <- datalist[["metadata"]]
  
  ## CD-RISC
  cdrisc <- datalist[["cdrisc"]]
  cdr_score <- c("Not true at all." = 0, "Rarely true." = 1, "Sometimes true." = 2,
             "Often true." = 3, "True nearly all the time." = 4,
             "Pas vrais du tout" = 0, "Rarement vrai" = 1, "Quelquefois vrai" = 2,
             "Souvent vrai" = 3, "Presque toujours vrai" = 4)
  cdrisc <- data.frame(sapply(cdrisc,function(x) cdr_score[x]))
  cdrisc$total <- rowSums(cdrisc)
  cdrisc <- as.data.frame(lapply(cdrisc, as.numeric))
  
  ## New General Self-Efficacy (NGSE)
  ngse <- datalist[["ngse"]]
  ngse_score <- c("Strongly disagree" = 1, "Somewhat disagree" = 2, 
             "Neither agree nor disagree" = 3, "Somewhat agree"= 4,
             "Strongly agree" = 5, "Fortement en désaccord" = 1, 
             "Plutôt en désaccord" = 2, "Ni d'accord ni en désaccord" = 3, 
             "Plutôt d'accord"= 4, "Tout à fait d'accord" = 5)
  ngse <- data.frame(sapply(ngse,function(x) ngse_score[x]))
  ngse$total <- rowSums(ngse)/8
  ngse <- as.data.frame(lapply(ngse,as.numeric))
  
  ## TEIque
  teique <- datalist[["teique"]]
  tei_score <- c("1 (Completely Disagree)" = 1, "1 (Pas du tout d'accord)" = 1, "2" = 2, 
             "3" = 3,"4" = 4, "5" = 5, "6" = 6, 
             "7 (Completely Agree)" = 7,  "7 (Tout à fait d'accord)" = 7)
  tei_reverse <- c("1 (Completely Disagree)" = 7, "1 (Pas du tout d'accord)" = 7, "2" = 6, 
               "3" = 5,"4" = 4, "5" = 3, "6" = 2, 
               "7 (Completely Agree)" = 1, "7 (Tout à fait d'accord)" = 1)
  cols <- colnames(teique)
  all_cols<-cols
  rev_cols <- cols[c(2,4,5,7,8,10,12,13,14,16,18,22,25,26,28)]
  cols <- cols[c(1,3,6,9,11,15,17,19:21,23:24,27,29:30)]
  
  teique[cols] <- sapply(teique[cols],function(x) tei_score[x])
  teique[rev_cols] <- sapply(teique[rev_cols],function(x) tei_reverse[x])
  teique$total <- rowSums(teique)
  teique$global_ei <- rowSums(teique)/30
  
  wellbeing <- all_cols[c(5,20,9,24,12,27)]
  self_ctrl <- all_cols[c(4,19,7,22,15,30)]
  emotionality <- all_cols[c(1,16,2,17,8,23,13,28)]
  sociability <- all_cols[c(6,21,10,25,11,26)]
  motivation <- all_cols[c(3,18)]
  adaptability <- all_cols[c(14,29)]
  
  teique$wellbeing <- rowSums(teique[,wellbeing])/6
  teique$self_ctrl <- rowSums(teique[,self_ctrl])/6
  teique$emotionality <- rowSums(teique[,emotionality])/8
  teique$sociability <- rowSums(teique[,sociability])/6
  teique$motivation <- rowSums(teique[,motivation])/2
  teique$adaptability <- rowSums(teique[,adaptability])/2
  rownames(teique) <- NULL
  
  ##### Burnout #####
  burnout <- data.frame(datalist[["burnout"]])
  burnout_score <- c("I enjoy my work. I have no symptoms of burnout." = 1, 
                     "Occasionally I am under stress, and I don't always have as much energy as I once did, but I don't feel burned out." = 2, 
                     "I am definitely burning out and have one or more symptoms of burnout, such as physical and emotional exhaustion." = 3,
                     "The symptoms of burnout that I'm experiencing won't go away. I think about frustration at work a lot."= 4,
                     "I feel completely burned out and often wonder if I can go on. I am at the point where I may need some changes or may need to seek some sort of help." = 5,
                     "J'aime mon travail. Je n'ai pas de symptômes d'épuisement professionnel." = 1, 
                     "Il m'arrive d'être stressé(e) et je n'ai pas toujours autant d'énergie qu'avant, mais je ne me sens pas épuisé(e)." = 2, 
                     "Je suis définitivement en épuisement professionnel et je présente un ou plusieurs symptômes de burnout, comme l'épuisement physique et émotionnel." = 3,
                     "Les symptômes d'épuisement que j'éprouve ne disparaissent pas. Je pense souvent à ma frustration au travail."= 4,
                     "Je me sens complètement épuisé(e) et je me demande souvent si je peux continuer. J'en suis au point où j'ai peut-être besoin de changements ou d'une aide quelconque." = 5)
  
  burnout$burnout <- sapply(burnout,function(x) burnout_score[x])
  
  ##### Single Item Self-Esteem (SISE) #####
  
  sise <- data.frame(datalist[["sise"]])
  sise_score <- c("1 (Pas très vrai pour moi)" = 1, "1\n(Not very true of me)" = 1, 
                  "2" = 2, "3" = 3, "4" = 4,
                  "5" = 5, "6" = 6, "7\n(Very true of me)" = 7, "7 (Très vrai pour moi)" = 7)
  sise$sise <- sapply(sise,function(x) sise_score[x])
  
  ##### Emily's Scales #####
  
  ##### Psychological Well-Being (PWB) #####
  pwb <- data.frame(datalist[["pwb"]])
  
  pwb_score <- c("Strongly Disagree" = 1, "Disagree Somewhat" = 2, 
             "Disagree Slightly" = 3,"Agree Slightly" = 4, 
             "Agree Somewhat" = 5,"Strongly Agree" = 6, 
             "Tout à fait en désaccord, tout à fait faux" = 1, "Passablement en désaccord" = 2, 
             "Plus ou moins en désaccord" = 3,"Plus ou moins d'accord" = 4, 
             "Passablement d'accord" = 5,"Tout à fait d’accord, tout à fait vrai" = 6)
  
  pwb_reverse <- c("Strongly Disagree" = 6, "Disagree Somewhat" = 5, 
               "Disagree Slightly" = 4,"Agree Slightly" = 3, 
               "Agree Somewhat" = 2,"Strongly Agree" = 1,
               "Tout à fait en désaccord, tout à fait faux"= 6, "Passablement en désaccord" = 5, 
               "Plus ou moins en désaccord" = 4,"Plus ou moins d'accord" = 3, 
               "Passablement d'accord" = 2,"Tout à fait d’accord, tout à fait vrai" = 1)
  
  all_cols <- colnames(pwb)
  rev_cols <- all_cols[c(3,5,8,10,13,14,15,16,17,18,19,23,26,27,30,31,32,34,36,39,41)]
  cols <- all_cols[c(1:2,4,6:7,9,11:12,20:22,24:25,28:29,33,35,37:38,40,42)]
  pwb[cols] <- sapply(pwb[cols],function(x) pwb_score[x])
  pwb[rev_cols] <- sapply(pwb[rev_cols],function(x) pwb_reverse[x])
  pwb$total <- rowSums(pwb)
  
  autonomy <- all_cols[c(1,7,13,19,25,31,37)]
  envi_master <- all_cols[c(2,8,14,20,26,32,38)]
  pers_growth <- all_cols[c(3,9,15,21,27,33,39)]
  pos_relation <- all_cols[c(4,10,16,22,28,34,40)]
  purpose <- all_cols[c(5,11,17,23,29,35,41)]
  self_acc <- all_cols[c(6,12,18,24,30,36,42)]
  
  pwb$autonomy <- rowSums(pwb[,autonomy])
  pwb$envi_master <- rowSums(pwb[,envi_master])
  pwb$pers_growth <- rowSums(pwb[,pers_growth])
  pwb$pos_relation <- rowSums(pwb[,pos_relation])
  pwb$purpose <- rowSums(pwb[,purpose])
  pwb$self_acc <- rowSums(pwb[,self_acc])

  ##### Satisfaction with life scale (SWLS) #####
  swls <- data.frame(datalist[["swls"]])
  swls_score <- c("Strongly Disagree" = 1, "Disagree" = 2, 
             "Slightly Disagree" = 3, "Neither Agree or Disagree"= 4,"Slightly Agree" = 5, 
             "Agree" = 6,"Strongly Agree" = 7, 
             "Fortement  en désaccord" = 1, "En désaccord" = 2, 
             "Légèrement en désaccord" = 3, "Ni en désaccord ni en accord"= 4,
             "Légèrement en accord" = 5, 
             "En accord" = 6,"Fortemente en accord" = 7)
  swls <- data.frame(sapply(swls,function(x) swls_score[x]))
  swls$total <- rowSums(swls)/5

  ## Flo
  flo <- data.frame(datalist[["flo"]])
  flo_score <- c("Strongly Disagree" = 1, "Disagree" = 2, 
             "Slightly Disagree" = 3, "Mixed or Neither Agree Nor Disagree"= 4,
             "Slightly Agree" = 5, "Agree" = 6,"Strongly Agree" = 7, 
             "Pas du tout d’accord" = 1, "Pas d’accord" = 2, 
             "Plutôt pas d’accord" = 3, "Ni d’accord, ni pas d’accord"= 4,
             "Plutôt d’accord" = 5, "D’accord" = 6,"Tout à fait d’accord" = 7)
  flo <- data.frame(sapply(flo,function(x) flo_score[x]))
  flo$total <- rowSums(flo)/8
  
  ##### Professional Quality of Life (ProQOL) #####
  proqol <- data.frame(datalist[["proqol"]])
  proqol_score <- c("Never" = 1, "Rarely" = 2, 
             "Sometimes" = 3, "Often"= 4,
             "Very Often" = 5,
             "Jamais" = 1, "Rarement" = 2, 
             "Parfois" = 3, "Souvent"= 4,
             "Très souvent" = 5)
  
  proqol <- data.frame(sapply(proqol,function(x) proqol_score[x]))
  proqol$total <- rowSums(proqol)
  proqol$cf <- rowSums(proqol[,c(1,4,8)])
  proqol$bo <- rowSums(proqol[,c(2,6,7)])
  proqol$cs <- rowSums(proqol[,c(3,5,9)])
  
  new_datalist <- list(metadata,cdrisc,pwb,ngse,flo,swls,teique,proqol,burnout,sise)
  names(new_datalist) <- c("metadata","cdrisc", "pwb","ngse","flo","swls","teique","proqol","burnout","sise")

  return(new_datalist)
  
}